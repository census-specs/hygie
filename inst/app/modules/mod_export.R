# ============================================================
# MODULE EXPORT v4
# ============================================================

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
