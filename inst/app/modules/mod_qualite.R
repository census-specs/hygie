# ============================================================
# MODULE CONTROLE QUALITE — Analyse + inspection interactive
# ============================================================

# ------------------------------------------------------------
# Analyse globale existante
# ------------------------------------------------------------

modal_qc_analyser <- function() {
  df <- isolate(rv$donnees_courantes)

  if (is.null(df)) {
    showNotification("Aucune donnée chargée.", type = "warning")
    return(invisible(NULL))
  }

  resultats <- analyser_qualite(df)

  showModal(modalDialog(
    title = "Contrôle qualité — Analyse du jeu de données",
    size = "l",

    tags$div(
      style = "font-size:12px; color:#4A5568; margin-bottom:12px;",
      glue::glue(
        "Analyse de {nrow(df)} observations × {ncol(df)} variables — ",
        "{sum(sapply(resultats, function(r) r$niveau != 'ok'))} point(s) d'attention."
      )
    ),

    tags$div(
      lapply(resultats, function(r) {
        tags$div(
          class = paste0("h-qc-item ", r$niveau),
          tags$span(
            class = paste0("h-qc-badge ", r$niveau),
            switch(
              r$niveau,
              ok = "OK",
              warning = "ATTENTION",
              error = "ERREUR"
            )
          ),
          tags$div(
            tags$div(class = "h-qc-msg", r$message),
            if (!is.null(r$detail)) {
              tags$div(class = "h-qc-action", r$detail)
            }
          )
        )
      })
    ),

    footer = tagList(
      modalButton("Fermer"),
      if (any(sapply(resultats, function(r) r$niveau != "ok"))) {
        tags$button(
          class = "h-btn-ok",
          style = paste(
            "padding:5px 14px; background:#1A56C4; color:#fff;",
            "border:none; border-radius:3px; cursor:pointer;",
            "font-family:'Noto Sans',sans-serif; font-size:12.5px;"
          ),
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

  if (n == 0) {
    return(list(list(
      niveau = "warning",
      message = "Le jeu de données ne contient aucune observation.",
      detail = NULL
    )))
  }

  # 1. Doublons de lignes
  n_doublons <- n - nrow(dplyr::distinct(df))
  resultats <- c(resultats, list(list(
    niveau = if (n_doublons == 0) "ok" else "warning",
    message = if (n_doublons == 0) {
      "Aucun doublon détecté."
    } else {
      glue::glue(
        "{n_doublons} ligne(s) dupliquée(s) ",
        "({round(n_doublons / n * 100, 1)} % des observations)"
      )
    },
    detail = if (n_doublons > 0) {
      "Menu Nettoyer > Supprimer les doublons"
    } else NULL
  )))

  # 2. Valeurs manquantes
  na_par_col <- sapply(df, function(x) sum(is.na(x)))
  cols_na <- names(na_par_col[na_par_col > 0])

  if (length(cols_na) == 0) {
    resultats <- c(resultats, list(list(
      niveau = "ok",
      message = "Aucune valeur manquante.",
      detail = NULL
    )))
  } else {
    pct_total <- round(sum(na_par_col) / (n * ncol(df)) * 100, 1)
    detail_cols <- paste(
      sapply(cols_na, function(col) {
        glue::glue(
          "{col} : {na_par_col[col]} ",
          "({round(na_par_col[col] / n * 100, 1)} %)"
        )
      }),
      collapse = " | "
    )

    resultats <- c(resultats, list(list(
      niveau = if (pct_total < 5) "warning" else "error",
      message = glue::glue(
        "{length(cols_na)} colonne(s) avec des valeurs manquantes ",
        "({pct_total} % du total)"
      ),
      detail = detail_cols
    )))
  }

  # 3. Colonnes vides
  cols_vides <- names(df)[sapply(df, function(x) all(is.na(x)))]
  resultats <- c(resultats, list(list(
    niveau = if (length(cols_vides) == 0) "ok" else "error",
    message = if (length(cols_vides) == 0) {
      "Aucune colonne entièrement vide."
    } else {
      glue::glue(
        "{length(cols_vides)} colonne(s) entièrement vide(s) : ",
        "{paste(cols_vides, collapse = ', ')}"
      )
    },
    detail = if (length(cols_vides) > 0) {
      "Menu Nettoyer > Supprimer les colonnes vides"
    } else NULL
  )))

  # 4. Lignes vides
  n_lignes_vides <- sum(apply(df, 1, function(r) all(is.na(r))))
  resultats <- c(resultats, list(list(
    niveau = if (n_lignes_vides == 0) "ok" else "warning",
    message = if (n_lignes_vides == 0) {
      "Aucune ligne entièrement vide."
    } else {
      glue::glue("{n_lignes_vides} ligne(s) entièrement vide(s)")
    },
    detail = if (n_lignes_vides > 0) {
      "Menu Nettoyer > Supprimer les lignes vides"
    } else NULL
  )))

  # 5. Variables constantes
  cols_const <- names(df)[sapply(df, function(x) {
    vals <- x[!is.na(x)]
    length(vals) > 0 && length(unique(vals)) == 1
  })]

  resultats <- c(resultats, list(list(
    niveau = if (length(cols_const) == 0) "ok" else "warning",
    message = if (length(cols_const) == 0) {
      "Aucune variable constante."
    } else {
      glue::glue(
        "{length(cols_const)} variable(s) constante(s) : ",
        "{paste(cols_const, collapse = ', ')}"
      )
    },
    detail = if (length(cols_const) > 0) {
      "Ces variables n'apportent aucune information."
    } else NULL
  )))

  # 6. Incohérences de modalités
  cols_cat <- names(df)[sapply(
    df,
    function(x) is.character(x) || is.factor(x)
  )]

  problemes_cat <- list()

  for (col in cols_cat) {
    vals <- unique(as.character(df[[col]][!is.na(df[[col]])]))
    vals_l <- tolower(trimws(vals))

    doublons_casse <- vals[
      duplicated(vals_l) | duplicated(vals_l, fromLast = TRUE)
    ]

    if (length(doublons_casse) >= 2) {
      problemes_cat <- c(
        problemes_cat,
        glue::glue(
          "{col} : {paste(head(doublons_casse, 4), collapse = ' / ')}"
        )
      )
    }
  }

  if (length(problemes_cat) == 0) {
    resultats <- c(resultats, list(list(
      niveau = "ok",
      message = "Aucune incohérence de modalités détectée.",
      detail = NULL
    )))
  } else {
    resultats <- c(resultats, list(list(
      niveau = "warning",
      message = glue::glue(
        "{length(problemes_cat)} variable(s) avec des modalités incohérentes"
      ),
      detail = paste(head(problemes_cat, 5), collapse = " | ")
    )))
  }

  # 7. Outliers extrêmes
  cols_n <- names(df)[sapply(df, is.numeric)]
  n_cols_out <- 0
  detail_out <- character(0)

  for (col in cols_n) {
    x <- df[[col]][!is.na(df[[col]])]
    if (length(x) < 10) next

    q1 <- as.numeric(quantile(x, .25, names = FALSE))
    q3 <- as.numeric(quantile(x, .75, names = FALSE))
    iqr <- q3 - q1

    n_out <- sum(
      x < q1 - 3 * iqr |
        x > q3 + 3 * iqr
    )

    if (n_out > 0) {
      n_cols_out <- n_cols_out + 1
      detail_out <- c(
        detail_out,
        glue::glue("{col} : {n_out} valeur(s)")
      )
    }
  }

  resultats <- c(resultats, list(list(
    niveau = if (n_cols_out == 0) "ok" else "warning",
    message = if (n_cols_out == 0) {
      "Aucun outlier extrême détecté (IQR × 3)."
    } else {
      glue::glue(
        "{n_cols_out} variable(s) numérique(s) avec ",
        "des valeurs potentiellement aberrantes"
      )
    },
    detail = if (n_cols_out > 0) {
      paste(detail_out, collapse = " | ")
    } else NULL
  )))

  # 8. Types suspects
  cols_txt <- names(df)[sapply(df, is.character)]
  suspects <- character(0)

  for (col in cols_txt) {
    vals <- df[[col]][!is.na(df[[col]])]
    if (length(vals) < 2) next

    pct_num <- mean(!is.na(suppressWarnings(as.numeric(vals))))

    if (pct_num > 0.9) {
      suspects <- c(suspects, col)
    }
  }

  resultats <- c(resultats, list(list(
    niveau = if (length(suspects) == 0) "ok" else "warning",
    message = if (length(suspects) == 0) {
      "Aucun type de variable suspect détecté."
    } else {
      glue::glue(
        "{length(suspects)} variable(s) de type texte ",
        "contiennent principalement des nombres"
      )
    },
    detail = if (length(suspects) > 0) {
      paste0(
        paste(suspects, collapse = ", "),
        " — Corriger via Nettoyer > Corriger le type"
      )
    } else NULL
  )))

  resultats
}

# ------------------------------------------------------------
# Nouvelle couche : problèmes inspectables par variable
# ------------------------------------------------------------

# Retourne les indices de lignes concernées par les problèmes.
# Les doublons correspondent aux lignes appartenant à une répétition
# exacte de ligne dans le jeu de données.
analyser_problemes_apercu <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list())
  }

  lignes_doublons <- tryCatch(
    which(duplicated(df) | duplicated(df, fromLast = TRUE)),
    error = function(e) integer(0)
  )

  resultats <- setNames(vector("list", ncol(df)), names(df))

  for (col in names(df)) {
    x <- df[[col]]
    lignes_manquantes <- which(is.na(x))
    lignes_outliers <- integer(0)

    if (is.numeric(x)) {
      idx <- which(!is.na(x))
      valeurs <- x[idx]

      if (length(valeurs) >= 5) {
        q1 <- as.numeric(
          quantile(valeurs, .25, names = FALSE, na.rm = TRUE)
        )
        q3 <- as.numeric(
          quantile(valeurs, .75, names = FALSE, na.rm = TRUE)
        )
        iqr <- q3 - q1

        if (is.finite(iqr)) {
          bas <- q1 - 1.5 * iqr
          haut <- q3 + 1.5 * iqr
          lignes_outliers <- idx[
            valeurs < bas | valeurs > haut
          ]
        }
      }
    }

    resultats[[col]] <- list(
      manquants = lignes_manquantes,
      outliers = lignes_outliers,
      doublons = lignes_doublons
    )
  }

  attr(resultats, "lignes_doublons") <- lignes_doublons
  resultats
}

# Construire le bouton d'un badge.
qualite_badge <- function(
  type,
  n,
  variable,
  label = NULL,
  action = "filter"
) {
  if (is.null(label)) {
    label <- switch(
      type,
      missing = "manquants",
      outlier = "aberrantes",
      duplicate = "doublons",
      type
    )
  }

  if (n <= 0) return(NULL)

  var_json <- jsonlite::toJSON(
    variable,
    auto_unbox = TRUE,
    ensure_ascii = FALSE
  )

  if (action == "boxplot") {
    onclick <- sprintf(
      "Shiny.setInputValue('hygie_boxplot', {variable:%s}, {priority:'event'})",
      var_json
    )
  } else {
    type_json <- jsonlite::toJSON(
      type,
      auto_unbox = TRUE,
      ensure_ascii = FALSE
    )

    onclick <- sprintf(
      "Shiny.setInputValue('hygie_probleme', {type:%s, variable:%s}, {priority:'event'})",
      type_json,
      var_json
    )
  }

  couleur <- switch(
    type,
    missing = "#B45309",
    outlier = "#B91C1C",
    duplicate = "#6D28D9",
    "#4B5563"
  )

  fond <- switch(
    type,
    missing = "#FEF3C7",
    outlier = "#FEE2E2",
    duplicate = "#EDE9FE",
    "#F3F4F6"
  )

  as.character(tags$button(
    type = "button",
    class = paste0("h-qualite-badge h-qualite-", type),
    title = if (action == "boxplot") {
      "Afficher le boxplot et les valeurs aberrantes"
    } else {
      "Afficher uniquement les lignes concernées"
    },
    onclick = onclick,
    style = paste0(
      "background:", fond, ";",
      "color:", couleur, ";",
      "border:1px solid ", couleur, "33;",
      "font-size:10px; font-weight:600;",
      "padding:2px 5px; border-radius:10px;",
      "cursor:pointer; margin-left:3px;"
    ),
    n,
    " ",
    label
  ))
}

# En-tête d'une variable avec les badges.
qualite_header <- function(value, name, problemes) {
  p <- problemes[[name]]

  if (is.null(p)) {
    return(value)
  }

  badges <- tagList(
    qualite_badge("missing", length(p$manquants), name),
    qualite_badge(
      "outlier",
      length(p$outliers),
      name,
      action = "boxplot"
    ),
    qualite_badge("duplicate", length(p$doublons), name)
  )

  tagList(
    tags$div(
      class = "h-qualite-header",
      tags$span(class = "h-qualite-header-name", value),
      tags$span(
        class = "h-qualite-header-badges",
        badges
      )
    )
  )
}

# Données de base de l'aperçu courant, sans le filtre d'inspection.
qualite_donnees_base <- reactive({
  id <- input$apercu_etape_id

  if (is.null(id) || id == "final") {
    rv$donnees_courantes
  } else {
    tryCatch(
      apercu_etape(rv$pipeline, id),
      error = function(e) NULL
    )
  }
})

# Applique uniquement un filtre d'inspection temporaire.
qualite_donnees_filtrees <- reactive({
  df <- qualite_donnees_base()

  if (is.null(df)) {
    return(NULL)
  }

  filtre <- rv$qualite_filtre

  if (is.null(filtre)) {
    return(df)
  }

  problemes <- analyser_problemes_apercu(df)
  variable <- filtre$variable
  type <- filtre$type

  if (type == "duplicate" && is.null(variable)) {
    idx <- attr(problemes, "lignes_doublons")
  } else if (!is.null(variable) && variable %in% names(problemes)) {
    idx <- problemes[[variable]][[
      switch(
        type,
        missing = "manquants",
        outlier = "outliers",
        duplicate = "doublons",
        "manquants"
      )
    ]]
  } else {
    idx <- integer(0)
  }

  if (length(idx) == 0) {
    return(df[FALSE, , drop = FALSE])
  }

  df[idx, , drop = FALSE]
})

# Initialiser l'état sans toucher au pipeline.
if (!"qualite_filtre" %in% names(rv)) {
  rv$qualite_filtre <- NULL
}

if (!"qualite_boxplot" %in% names(rv)) {
  rv$qualite_boxplot <- NULL
}

# Clic sur un badge : filtre temporaire d'inspection.
observeEvent(input$hygie_probleme, {
  req(input$hygie_probleme)

  type <- input$hygie_probleme$type %||% NULL
  variable <- input$hygie_probleme$variable %||% NULL
  req(type)

  nouveau <- list(type = type, variable = variable)
  ancien <- rv$qualite_filtre

  if (
    !is.null(ancien) &&
    identical(ancien$type, nouveau$type) &&
    identical(ancien$variable, nouveau$variable)
  ) {
    rv$qualite_filtre <- NULL
    showNotification(
      "Filtre d'inspection retiré.",
      type = "message",
      duration = 3
    )
  } else {
    rv$qualite_filtre <- nouveau

    lib <- switch(
      type,
      missing = "valeurs manquantes",
      outlier = "valeurs aberrantes",
      duplicate = "doublons",
      type
    )

    showNotification(
      glue::glue(
        "Aperçu filtré : {lib}",
        if (!is.null(variable)) glue::glue(" — {variable}") else ""
      ),
      type = "message",
      duration = 4
    )
  }
}, ignoreInit = TRUE)

# Réinitialiser l'inspection lorsqu'on change d'étape ou de données.
observe({
  qualite_donnees_base()
  rv$qualite_filtre <- NULL
})

# ------------------------------------------------------------
# Boxplot des valeurs aberrantes
# ------------------------------------------------------------

observeEvent(input$hygie_boxplot, {
  variable <- input$hygie_boxplot$variable %||% NULL
  req(variable)

  rv$qualite_boxplot <- variable

  showModal(modalDialog(
    title = paste("Valeurs aberrantes —", variable),
    size = "l",

    tags$div(
      style = "font-size:12px; color:#4B5563; margin-bottom:8px;",
      "Les points affichés au-delà des moustaches sont détectés ",
      "selon la règle IQR × 1,5. Ils sont potentiellement aberrants ",
      "et ne sont pas automatiquement considérés comme des erreurs."
    ),

    plotOutput("hygie_boxplot", height = "360px"),

    footer = tagList(
      modalButton("Fermer"),
      tags$button(
        class = "h-btn-ok",
        style = paste(
          "padding:6px 14px; background:#1A56C4; color:#fff;",
          "border:none; border-radius:3px; cursor:pointer;",
          "font-family:'Noto Sans',sans-serif; font-size:12px;"
        ),
        onclick = sprintf(
          "Shiny.setInputValue('hygie_probleme', {type:'outlier', variable:%s}, {priority:'event'})",
          jsonlite::toJSON(
            variable,
            auto_unbox = TRUE,
            ensure_ascii = FALSE
          )
        ),
        "Voir les lignes concernées"
      )
    )
  ))
}, ignoreInit = TRUE)

output$hygie_boxplot <- renderPlot({
  variable <- rv$qualite_boxplot
  req(variable)

  df <- qualite_donnees_base()
  req(!is.null(df), variable %in% names(df))

  x <- df[[variable]]
  req(is.numeric(x))

  idx <- which(!is.na(x))
  valeurs <- x[idx]
  req(length(valeurs) >= 5)

  q1 <- as.numeric(quantile(
    valeurs, .25, names = FALSE, na.rm = TRUE
  ))
  q3 <- as.numeric(quantile(
    valeurs, .75, names = FALSE, na.rm = TRUE
  ))
  iqr <- q3 - q1

  bas <- q1 - 1.5 * iqr
  haut <- q3 + 1.5 * iqr

  idx_out <- idx[
    valeurs < bas | valeurs > haut
  ]

  boxplot(
    valeurs,
    horizontal = TRUE,
    outline = FALSE,
    main = paste("Boxplot :", variable),
    xlab = variable
  )

  if (length(idx_out) > 0) {
    y <- rep(1, length(idx_out))

    points(
      df[[variable]][idx_out],
      y,
      pch = 19,
      cex = 1.1
    )

    text(
      df[[variable]][idx_out],
      y,
      labels = paste0("  L", idx_out),
      pos = 4,
      cex = 0.75
    )
  }

  mtext(
    paste0(
      length(idx_out),
      " valeur(s) potentiellement aberrante(s) — règle IQR × 1,5"
    ),
    side = 3,
    line = 0.2,
    cex = 0.85
  )
})

# ------------------------------------------------------------
# Remplacement contrôlé du rendu du tableau
# ------------------------------------------------------------
#
# Le dépôt définit actuellement tableau_donnees directement dans
# server.R. Nous remplaçons ce rendu après l'initialisation du serveur,
# sans modifier le pipeline ni l'interface générale.
#
# Le filtre reste une vue temporaire : aucune étape n'est ajoutée
# à rv$pipeline et aucune donnée source n'est supprimée.

observeEvent(
  rv$donnees_courantes,
  {
    output$tableau_donnees <- renderReactable({
      df <- qualite_donnees_filtrees()
      req(!is.null(df))

      df_propre <- nettoyer_df_pour_dt(df)
      problemes <- analyser_problemes_apercu(
        qualite_donnees_base()
      )

      colonnes <- lapply(
        names(df_propre),
        function(nm) {
          reactable::colDef(
            header = function(value, name) {
              qualite_header(value, name, problemes)
            },
            html = TRUE
          )
        }
      )

      names(colonnes) <- names(df_propre)

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
  },
  ignoreInit = FALSE
)

# ------------------------------------------------------------
# Lancement du contrôle qualité global
# ------------------------------------------------------------

observeEvent(input$qc_analyser, {
  modal_qc_analyser()
}, ignoreInit = TRUE)
