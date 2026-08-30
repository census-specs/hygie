# ============================================================
# MODULE CONTROLE QUALITE — Analyse automatique du jeu de données
# ============================================================

modal_qc_analyser <- function() {
  df <- isolate(rv$donnees_courantes)
  if (is.null(df)) {
    showNotification("Aucune donnée chargée.", type = "warning")
    return()
  }
  resultats <- analyser_qualite(df)
  showModal(modalDialog(
    title = "Controle qualite — Analyse du jeu de données",
    size  = "l",
    tags$div(
      style = "font-size:12px; color:#4A5568; margin-bottom:12px;",
      glue::glue(
        "Analyse de {nrow(df)} observations x {ncol(df)} variables — ",
        "{sum(sapply(resultats, function(r) r$niveau != 'ok'))} point(s) d'attention."
      )
    ),
    tags$div(
      lapply(resultats, function(r) {
        tags$div(class = paste0("h-qc-item ", r$niveau),
          tags$span(class = paste0("h-qc-badge ", r$niveau),
            switch(r$niveau, ok="OK", warning="ATTENTION", error="ERREUR")
          ),
          tags$div(
            tags$div(class = "h-qc-msg",  r$message),
            if (!is.null(r$detail)) tags$div(class = "h-qc-action", r$detail)
          )
        )
      })
    ),
    footer = tagList(
      modalButton("Fermer"),
      if (any(sapply(resultats, function(r) r$niveau != "ok"))) {
        tags$button(
          class   = "h-btn-ok", style="padding:5px 14px; background:#1A56C4; color:#fff; border:none; border-radius:3px; cursor:pointer; font-family:'Noto Sans',sans-serif; font-size:12.5px;",
          onclick = "Shiny.setInputValue('qc_exporter_rapport', Math.random(), {priority:'event'})",
          "Exporter le rapport"
        )
      }
    )
  ))
}

#' Analyser la qualité d'un data.frame
analyser_qualite <- function(df) {
  resultats <- list()
  n <- nrow(df)

  # 1. Doublons
  n_doublons <- n - nrow(dplyr::distinct(df))
  resultats <- c(resultats, list(list(
    niveau  = if (n_doublons == 0) "ok" else "warning",
    message = if (n_doublons == 0)
      "Aucun doublon détecté."
    else
      glue::glue("{n_doublons} ligne(s) dupliquée(s) ({round(n_doublons/n*100,1)} % des observations)"),
    detail  = if (n_doublons > 0) "Menu Nettoyer > Supprimer les doublons" else NULL
  )))

  # 2. Valeurs manquantes par colonne
  na_par_col <- sapply(df, function(x) sum(is.na(x)))
  cols_na    <- names(na_par_col[na_par_col > 0])
  if (length(cols_na) == 0) {
    resultats <- c(resultats, list(list(
      niveau="ok", message="Aucune valeur manquante.", detail=NULL
    )))
  } else {
    pct_total <- round(sum(na_par_col) / (n * ncol(df)) * 100, 1)
    detail_cols <- paste(sapply(cols_na, function(col) {
      glue::glue("{col} : {na_par_col[col]} ({round(na_par_col[col]/n*100,1)} %)")
    }), collapse=" | ")
    resultats <- c(resultats, list(list(
      niveau  = if (pct_total < 5) "warning" else "error",
      message = glue::glue("{length(cols_na)} colonne(s) avec des valeurs manquantes ({pct_total} % du total)"),
      detail  = detail_cols
    )))
  }

  # 3. Colonnes vides
  cols_vides <- names(df)[sapply(df, function(x) all(is.na(x)))]
  resultats <- c(resultats, list(list(
    niveau  = if (length(cols_vides) == 0) "ok" else "error",
    message = if (length(cols_vides) == 0)
      "Aucune colonne entièrement vide."
    else
      glue::glue("{length(cols_vides)} colonne(s) entièrement vide(s) : {paste(cols_vides, collapse=', ')}"),
    detail  = if (length(cols_vides) > 0) "Menu Nettoyer > Supprimer les colonnes vides" else NULL
  )))

  # 4. Lignes vides
  n_lignes_vides <- sum(apply(df, 1, function(r) all(is.na(r))))
  resultats <- c(resultats, list(list(
    niveau  = if (n_lignes_vides == 0) "ok" else "warning",
    message = if (n_lignes_vides == 0)
      "Aucune ligne entièrement vide."
    else
      glue::glue("{n_lignes_vides} ligne(s) entièrement vide(s)"),
    detail  = if (n_lignes_vides > 0) "Menu Nettoyer > Supprimer les lignes vides" else NULL
  )))

  # 5. Variables constantes
  cols_const <- names(df)[sapply(df, function(x) {
    vals <- x[!is.na(x)]
    length(vals) > 0 && length(unique(vals)) == 1
  })]
  resultats <- c(resultats, list(list(
    niveau  = if (length(cols_const) == 0) "ok" else "warning",
    message = if (length(cols_const) == 0)
      "Aucune variable constante (même valeur pour toutes les lignes)."
    else
      glue::glue("{length(cols_const)} variable(s) constante(s) : {paste(cols_const, collapse=', ')}"),
    detail  = if (length(cols_const) > 0) "Ces variables n'apportent aucune information." else NULL
  )))

  # 6. Incohérences de modalités (texte — ex: M, Masculin, masculin)
  cols_cat <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  problemes_cat <- list()
  for (col in cols_cat) {
    vals   <- unique(as.character(df[[col]][!is.na(df[[col]])]))
    vals_l <- tolower(trimws(vals))
    doublons_casse <- vals[duplicated(vals_l) | duplicated(vals_l, fromLast=TRUE)]
    if (length(doublons_casse) >= 2) {
      problemes_cat <- c(problemes_cat,
        glue::glue("{col} : {paste(head(doublons_casse,4), collapse=' / ')}"))
    }
  }
  if (length(problemes_cat) == 0) {
    resultats <- c(resultats, list(list(
      niveau="ok", message="Aucune incohérence de modalités détectée (casse, espaces).", detail=NULL
    )))
  } else {
    resultats <- c(resultats, list(list(
      niveau  = "warning",
      message = glue::glue("{length(problemes_cat)} variable(s) avec des modalités incohérentes (casse ou espaces)"),
      detail  = paste(head(problemes_cat, 5), collapse=" | ")
    )))
  }

  # 7. Outliers potentiels (colonnes numériques)
  cols_n <- names(df)[sapply(df, is.numeric)]
  n_cols_out <- 0
  detail_out <- character(0)
  for (col in cols_n) {
    x <- df[[col]][!is.na(df[[col]])]
    if (length(x) < 10) next
    q1 <- quantile(x, .25); q3 <- quantile(x, .75); iqr <- q3 - q1
    n_out <- sum(x < q1 - 3*iqr | x > q3 + 3*iqr)
    if (n_out > 0) {
      n_cols_out <- n_cols_out + 1
      detail_out <- c(detail_out, glue::glue("{col} : {n_out} valeur(s)"))
    }
  }
  resultats <- c(resultats, list(list(
    niveau  = if (n_cols_out == 0) "ok" else "warning",
    message = if (n_cols_out == 0)
      "Aucun outlier extrême détecté (méthode IQR x3)."
    else
      glue::glue("{n_cols_out} variable(s) numérique(s) avec des valeurs potentiellement aberrantes"),
    detail  = if (n_cols_out > 0) paste(detail_out, collapse=" | ") else NULL
  )))

  # 8. Types suspects (colonnes texte contenant des nombres)
  cols_txt <- names(df)[sapply(df, is.character)]
  suspects <- character(0)
  for (col in cols_txt) {
    vals <- df[[col]][!is.na(df[[col]])]
    if (length(vals) < 2) next
    pct_num <- mean(!is.na(suppressWarnings(as.numeric(vals))))
    if (pct_num > 0.9) suspects <- c(suspects, col)
  }
  resultats <- c(resultats, list(list(
    niveau  = if (length(suspects) == 0) "ok" else "warning",
    message = if (length(suspects) == 0)
      "Aucun type de variable suspect détecté."
    else
      glue::glue("{length(suspects)} variable(s) de type texte contiennent principalement des nombres"),
    detail  = if (length(suspects) > 0)
      paste0(paste(suspects, collapse=", "), " — Corriger via Nettoyer > Corriger le type")
    else NULL
  )))

  resultats
}

observeEvent(input$qc_analyser, {
  modal_qc_analyser()
}, ignoreInit = TRUE)
