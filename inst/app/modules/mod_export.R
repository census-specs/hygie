# ============================================================
# MODULE EXPORT / PROJETS v5
# ============================================================

# Charger les fonctions de projet une seule fois.
ch_projet <- file.path("..", "..", "R", "projets.R")
if (file.exists(ch_projet)) source(ch_projet, local = FALSE)

modal_export <- function() {
  df  <- isolate(rv$donnees_courantes)
  nom_defaut <- if (!is.null(rv$pipeline$nom_source))
    tools::file_path_sans_ext(rv$pipeline$nom_source)
  else "donnees_traitees"

  modalDialog(
    title = "Exporter les données",
    size  = "m",
    if (!is.null(df)) {
      n_etapes <- max(0L, length(isolate(rv$pipeline$etapes)) - 1L)
      tags$div(class = "h-alerte h-alerte-info",
        glue::glue("{nrow(df)} lignes × {ncol(df)} colonnes — {n_etapes} transformation(s) appliquée(s)"))
    },
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Nom du fichier"),
        tags$input(id = "exp_nom", type = "text", class = "h-input",
          value = nom_defaut,
          oninput = "Shiny.setInputValue('exp_nom', this.value)")
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Format"),
        tags$select(id = "exp_format",
          onchange = "Shiny.setInputValue('exp_format', this.value)",
          class = "h-select",
          tags$option(value = "csv",  "CSV  (.csv)"),
          tags$option(value = "xlsx", "Excel  (.xlsx)"),
          tags$option(value = "tsv",  "TSV  (.tsv)"),
          tags$option(value = "sav",  "SPSS  (.sav)"),
          tags$option(value = "dta",  "Stata  (.dta)"),
          tags$option(value = "rds",  "R natif  (.rds)")
        )
      )
    ),
    uiOutput("exp_options_format"),
    footer = tagList(
      modalButton("Annuler"),
      downloadButton("exp_telecharger", "Télécharger",
        style = "padding:5px 16px; background:#1A56C4; color:#fff; border:none; border-radius:3px; cursor:pointer; font-weight:700; font-family:'Noto Sans',sans-serif; font-size:12.5px;")
    )
  )
}

output$exp_options_format <- renderUI({
  req(input$exp_format)
  switch(input$exp_format,
    "csv" = tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Séparateur"),
      tags$select(id = "exp_sep",
        onchange = "Shiny.setInputValue('exp_sep', this.value)",
        class = "h-select",
        tags$option(value = ",",  "Virgule  ( , )"),
        tags$option(value = ";",  "Point-virgule  ( ; )"),
        tags$option(value = "\t", "Tabulation")
      )
    ),
    "xlsx" = tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nom de la feuille Excel"),
      tags$input(id = "exp_feuille", type = "text", class = "h-input",
        value = "Données",
        oninput = "Shiny.setInputValue('exp_feuille', this.value)")
    ),
    NULL
  )
})

output$exp_telecharger <- downloadHandler(
  filename = function() {
    nom <- trimws(input$exp_nom %||% "donnees_traitees")
    if (nchar(nom) == 0) nom <- "donnees_traitees"
    paste0(nom, ".", input$exp_format %||% "csv")
  },
  content = function(file) {
    df <- rv$donnees_courantes
    req(!is.null(df))
    tryCatch(
      exporter_fichier(df, file,
        list(separateur = input$exp_sep, feuille = input$exp_feuille)),
      error = function(e)
        showNotification(paste0("Erreur export : ", e$message), type = "error")
    )
  }
)

# -----------------------------------------------------------------------------
# PROJETS HYGIE
# -----------------------------------------------------------------------------

projet_nom_defaut <- function() {
  nom <- rv$pipeline$nom_source %||% "mon_projet"
  nom <- tools::file_path_sans_ext(basename(nom))
  if (!nzchar(nom)) nom <- "mon_projet"
  paste0(nom, ".hygie")
}

modal_projet_enregistrer <- function() {
  req(!is.null(rv$pipeline$donnees_brutes))
  modalDialog(
    title = "Enregistrer le projet Hygie",
    size = "m",
    tags$p(
      "Le projet contient les données brutes, le pipeline complet et l'état actuel du travail."
    ),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nom du projet"),
      tags$input(
        id = "projet_nom",
        type = "text",
        class = "h-input",
        value = projet_nom_defaut(),
        oninput = "Shiny.setInputValue('projet_nom', this.value)"
      )
    ),
    tags$div(class = "h-alerte h-alerte-info",
      "Le fichier .hygie est autonome : aucune copie séparée du fichier source n'est nécessaire pour reprendre ce travail."
    ),
    footer = tagList(
      modalButton("Annuler"),
      downloadButton(
        "projet_telecharger",
        "Enregistrer le projet",
        style = "padding:6px 16px; background:#1A56C4; color:#fff; border:none; border-radius:3px; cursor:pointer; font-weight:700; font-family:'Noto Sans',sans-serif; font-size:12.5px;"
      )
    )
  )
}

modal_projet_ouvrir <- function() {
  modalDialog(
    title = "Ouvrir un projet Hygie",
    size = "m",
    tags$p("Sélectionnez un fichier .hygie enregistré précédemment."),
    fileInput(
      "projet_fichier",
      "Fichier projet",
      accept = ".hygie",
      buttonLabel = "Parcourir...",
      placeholder = "Aucun projet sélectionné"
    ),
    tags$div(class = "h-alerte h-alerte-info",
      "Le projet restaurera les données brutes et rejouera automatiquement toutes les étapes actives du pipeline."
    ),
    footer = modalButton("Fermer")
  )
}

# UI compacte injectée dans la barre de menus existante.
insertUI(
  selector = ".h-menubar",
  where = "afterBegin",
  ui = tags$div(
    class = "h-projet-actions",
    style = paste(
      "display:flex; align-items:center; gap:4px; margin-right:8px;",
      "padding-right:8px; border-right:1px solid #E5E7EB;"
    ),
    tags$button(
      class = "h-menu-trigger",
      title = "Ouvrir un projet Hygie",
      onclick = "Shiny.setInputValue('projet_ouvrir', Math.random(), {priority:'event'})",
      icon("folder-open", style = "margin-right:5px;"),
      "Ouvrir"
    ),
    tags$button(
      class = "h-menu-trigger",
      title = "Enregistrer le projet Hygie",
      onclick = "Shiny.setInputValue('projet_enregistrer', Math.random(), {priority:'event'})",
      icon("save", style = "margin-right:5px;"),
      "Enregistrer"
    )
  )
)

observeEvent(input$projet_ouvrir, {
  showModal(modal_projet_ouvrir())
}, ignoreInit = TRUE)

observeEvent(input$projet_enregistrer, {
  if (is.null(rv$pipeline$donnees_brutes)) {
    showNotification("Importez d'abord des données avant d'enregistrer un projet.", type = "warning")
  } else {
    showModal(modal_projet_enregistrer())
  }
}, ignoreInit = TRUE)

output$projet_telecharger <- downloadHandler(
  filename = function() {
    nom <- trimws(input$projet_nom %||% projet_nom_defaut())
    if (!grepl("\\.hygie$", nom, ignore.case = TRUE)) nom <- paste0(nom, ".hygie")
    nom
  },
  content = function(file) {
    req(!is.null(rv$pipeline$donnees_brutes))
    tryCatch({
      sauvegarder_projet(rv$pipeline, rv$donnees_courantes, file)
      showNotification("Projet Hygie enregistré.", type = "message")
    }, error = function(e) {
      showNotification(paste0("Erreur lors de l'enregistrement : ", e$message), type = "error", duration = 8)
    })
  }
)

observeEvent(input$projet_fichier, {
  req(input$projet_fichier$datapath)
  tryCatch({
    projet <- charger_projet(input$projet_fichier$datapath)
    pl <- projet$pipeline
    req(is.list(pl), pl$donnees_brutes)

    rv$pipeline <- pl
    rv$donnees_courantes <- if (!is.null(projet$donnees_courantes)) {
      projet$donnees_courantes
    } else {
      rejouer_pipeline(pl)
    }

    removeModal()
    showNotification(
      glue::glue(
        "Projet ouvert : {pl$nom_source %||% 'sans nom'} — {length(pl$etapes)} étape(s)."
      ),
      type = "message",
      duration = 6
    )
  }, error = function(e) {
    showNotification(paste0("Impossible d'ouvrir le projet : ", e$message), type = "error", duration = 10)
  })
}, ignoreInit = TRUE)
