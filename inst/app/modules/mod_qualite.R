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

# ============================================================
# INSPECTION INTERACTIVE DES PROBLEMES PAR VARIABLE
# ============================================================

# Cette couche est indépendante du pipeline : les filtres servent
# uniquement à inspecter les lignes et ne créent aucune transformation.

hygie_analyser_problemes <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list())
  }

  lignes_doublons <- which(
    duplicated(df) | duplicated(df, fromLast = TRUE)
  )

  out <- setNames(vector("list", ncol(df)), names(df))

  for (nm in names(df)) {
    x <- df[[nm]]
    lignes_manquantes <- which(is.na(x))
    lignes_outliers <- integer(0)

    if (is.numeric(x)) {
      idx <- which(!is.na(x))
      vals <- x[idx]

      # Même règle que les moustaches du boxplot.
      if (length(vals) >= 5) {
        q1 <- as.numeric(quantile(vals, .25, names = FALSE, na.rm = TRUE))
        q3 <- as.numeric(quantile(vals, .75, names = FALSE, na.rm = TRUE))
        iqr <- q3 - q1

        if (is.finite(iqr) && iqr > 0) {
          bas <- q1 - 1.5 * iqr
          haut <- q3 + 1.5 * iqr
          lignes_outliers <- idx[vals < bas | vals > haut]
        }
      }
    }

    out[[nm]] <- list(
      manquants = lignes_manquantes,
      outliers = lignes_outliers,
      doublons = lignes_doublons
    )
  }

  attr(out, "lignes_doublons") <- lignes_doublons
  out
}

hygie_filtrer_problemes <- function(df, filtre) {
  if (is.null(df) || is.null(filtre)) return(df)

  problemes <- hygie_analyser_problemes(df)
  if (length(problemes) == 0) {
    return(df[FALSE, , drop = FALSE])
  }

  type <- filtre$type %||% NULL
  variable <- filtre$variable %||% NULL

  if (type == "duplicate" && is.null(variable)) {
    idx <- attr(problemes, "lignes_doublons")
  } else if (!is.null(variable) && variable %in% names(problemes)) {
    cle <- switch(
      type,
      missing = "manquants",
      outlier = "outliers",
      duplicate = "doublons",
      NULL
    )
    idx <- if (is.null(cle)) integer(0) else problemes[[variable]][[cle]]
  } else {
    idx <- integer(0)
  }

  idx <- sort(unique(as.integer(idx)))
  if (length(idx) == 0) {
    return(df[FALSE, , drop = FALSE])
  }

  df[idx, , drop = FALSE]
}

hygie_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", ensure_ascii = FALSE)
}

hygie_badge <- function(type, n, variable, action = "filter") {
  if (length(n) != 1 || is.na(n) || n <= 0) return(NULL)

  libelle <- switch(
    type,
    missing = "manquants",
    outlier = "aberrantes",
    duplicate = "doublons",
    type
  )

  couleur <- switch(
    type,
    missing = list(fond = "#FFF7ED", texte = "#9A3412", bord = "#FDBA74"),
    outlier = list(fond = "#FEF2F2", texte = "#B91C1C", bord = "#FCA5A5"),
    duplicate = list(fond = "#F5F3FF", texte = "#6D28D9", bord = "#C4B5FD"),
    list(fond = "#F3F4F6", texte = "#374151", bord = "#D1D5DB")
  )

  variable_js <- hygie_json(variable)

  if (identical(action, "boxplot")) {
    onclick <- sprintf(
      "Shiny.setInputValue('hygie_boxplot', {variable:%s}, {priority:'event'});",
      variable_js
    )
    title <- "Afficher le boxplot et les valeurs aberrantes"
  } else {
    type_js <- hygie_json(type)
    onclick <- sprintf(
      "Shiny.setInputValue('hygie_probleme', {type:%s, variable:%s}, {priority:'event'});",
      type_js,
      variable_js
    )
    title <- "Afficher uniquement les lignes concernées"
  }

  tags$button(
    type = "button",
    class = paste("h-qualite-badge", paste0("h-qualite-", type)),
    title = title,
    onclick = onclick,
    style = paste0(
      "background:", couleur$fond, ";",
      "color:", couleur$texte, ";",
      "border:1px solid ", couleur$bord, ";",
      "font-size:10px;font-weight:600;line-height:1.2;",
      "padding:2px 6px;border-radius:10px;cursor:pointer;",
      "white-space:nowrap;"
    ),
    paste0(n, " ", libelle)
  )
}

hygie_header <- function(value, name, problemes) {
  p <- problemes[[name]]
  if (is.null(p)) return(value)

  tags$div(
    class = "h-qualite-header",
    tags$span(class = "h-qualite-header-name", value),
    tags$span(
      class = "h-qualite-header-badges",
      hygie_badge("missing", length(p$manquants), name),
      hygie_badge("outlier", length(p$outliers), name, action = "boxplot"),
      hygie_badge("duplicate", length(p$doublons), name)
    )
  )
}

# Etat de l'inspection. Ne jamais utiliser names(rv) ici :
# names.reactivevalues exige un contexte réactif actif.
rv$hygie_probleme_actif <- NULL
rv$hygie_boxplot_variable <- NULL

hygie_donnees_inspection_base <- reactive({
  id <- input$apercu_etape_id

  if (is.null(id) || identical(id, "final")) {
    rv$donnees_courantes
  } else {
    tryCatch(
      apercu_etape(rv$pipeline, id),
      error = function(e) NULL
    )
  }
})

hygie_donnees_inspection <- reactive({
  df <- hygie_donnees_inspection_base()
  req(!is.null(df))
  hygie_filtrer_problemes(df, rv$hygie_probleme_actif)
})

observeEvent(input$apercu_etape_id, {
  rv$hygie_probleme_actif <- NULL
}, ignoreInit = TRUE)

observeEvent(input$hygie_probleme, {
  req(input$hygie_probleme$type)

  nouveau <- list(
    type = as.character(input$hygie_probleme$type),
    variable = if (!is.null(input$hygie_probleme$variable)) {
      as.character(input$hygie_probleme$variable)
    } else NULL
  )

  ancien <- rv$hygie_probleme_actif

  if (!is.null(ancien) && identical(ancien, nouveau)) {
    rv$hygie_probleme_actif <- NULL
    showNotification("Filtre d'inspection retiré.", type = "message", duration = 2)
  } else {
    rv$hygie_probleme_actif <- nouveau
  }
}, ignoreInit = TRUE)

observeEvent(input$hygie_boxplot, {
  variable <- input$hygie_boxplot$variable %||% NULL
  req(variable)

  df <- hygie_donnees_inspection_base()
  req(!is.null(df), variable %in% names(df), is.numeric(df[[variable]]))

  rv$hygie_boxplot_variable <- variable

  showModal(modalDialog(
    title = paste("Valeurs aberrantes —", variable),
    size = "l",
    tags$div(
      class = "h-boxplot-intro",
      "Les points au-delà des moustaches sont identifiés avec la règle IQR × 1,5. ",
      "Ils sont potentiellement aberrants : l'utilisateur décide ensuite quoi en faire."
    ),
    plotOutput("hygie_boxplot_plot", height = "380px"),
    footer = tagList(
      modalButton("Fermer"),
      tags$button(
        type = "button",
        class = "h-btn-ok",
        onclick = sprintf(
          "Shiny.setInputValue('hygie_probleme', {type:'outlier', variable:%s}, {priority:'event'}); $(this).closest('.modal').modal('hide');",
          hygie_json(variable)
        ),
        "Voir les lignes concernées"
      )
    )
  ))
}, ignoreInit = TRUE)

output$hygie_boxplot_plot <- renderPlot({
  variable <- rv$hygie_boxplot_variable
  req(variable)

  df <- hygie_donnees_inspection_base()
  req(!is.null(df), variable %in% names(df))

  x <- df[[variable]]
  req(is.numeric(x))

  idx <- which(!is.na(x))
  vals <- x[idx]
  req(length(vals) >= 5)

  q1 <- as.numeric(quantile(vals, .25, names = FALSE, na.rm = TRUE))
  q3 <- as.numeric(quantile(vals, .75, names = FALSE, na.rm = TRUE))
  iqr <- q3 - q1
  req(is.finite(iqr), iqr > 0)

  bas <- q1 - 1.5 * iqr
  haut <- q3 + 1.5 * iqr
  idx_out <- idx[vals < bas | vals > haut]

  boxplot(
    vals,
    horizontal = TRUE,
    outline = FALSE,
    main = paste("Boxplot —", variable),
    xlab = variable
  )

  if (length(idx_out) > 0) {
    points(df[[variable]][idx_out], rep(1, length(idx_out)), pch = 19, cex = 1.1)
    text(
      df[[variable]][idx_out],
      rep(1, length(idx_out)),
      labels = paste0("L", idx_out),
      pos = 3,
      cex = 0.72
    )
  }

  mtext(
    paste0(length(idx_out), " valeur(s) potentiellement aberrante(s)"),
    side = 3,
    line = 0.25,
    cex = 0.85
  )
})

# Le tableau est déjà défini dans server.R. On remplace son renderer
# après le premier flush afin de conserver l'architecture existante et
# d'éviter de lire donnees_affichees() avant sa définition.
session$onFlushed(function() {
  output$tableau_donnees <- renderReactable({
    df_base <- hygie_donnees_inspection_base()
    df <- hygie_donnees_inspection()
    req(!is.null(df_base), !is.null(df), nrow(df) > 0)

    df_propre <- nettoyer_df_pour_dt(df)
    problemes <- hygie_analyser_problemes(df_base)

    colonnes <- setNames(
      lapply(names(df_propre), function(nm) {
        reactable::colDef(
          header = function(value, name) {
            hygie_header(value, name, problemes)
          },
          html = TRUE
        )
      }),
      names(df_propre)
    )

    reactable::reactable(
      df_propre,
      columns = colonnes,
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      bordered = TRUE,
      resizable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100),
      language = reactable::reactableLang(
        searchPlaceholder = "Rechercher...",
        noData = "Aucune donnée disponible",
        pageNext = "Suivant",
        pagePrevious = "Précédent",
        pageInfo = "{rowStart} à {rowEnd} sur {rows} lignes",
        pageSizeOptions = "Afficher {rows} lignes"
      )
    )
  })
}, once = TRUE)

observeEvent(input$qc_analyser, {
  modal_qc_analyser()
}, ignoreInit = TRUE)
