modal_import <- function() {
  modalDialog(
    title = "Importer un fichier",
    size  = "l",
    tags$div(class = "h-form-group",
             tags$label(class = "h-label", "Fichier à importer"),
             fileInput("imp_fichier", NULL,
                       accept      = c(".csv",".tsv",".txt",".xlsx",".xls",
                                       ".sav",".sas7bdat",".dta",".json",
                                       ".rds",".rdata",".rda"),
                       buttonLabel = "Parcourir...",
                       placeholder = "Aucun fichier sélectionné"
             )
    ),
    uiOutput("imp_options_ui"),
    uiOutput("imp_statut_ui"),
    
    # Aperçu dans la modale contrôlé par conditionalPanel (Reactable)
    conditionalPanel(
      condition = "output.imp_a_des_donnees == true",
      tags$div(
        id = "imp_tableau_zone",
        style = "margin-top:6px;",
        tags$div(
          style = "font-size:11.5px; color:#6B7280; margin-bottom:4px;",
          "Aperçu des 5 premières lignes :"
        ),
        reactableOutput("imp_apercu_reactable")
      )
    ),
    
    footer = tagList(
      modalButton("Annuler"),
      tags$button(
        class   = "h-btn-ok",
        onclick = "Shiny.setInputValue('imp_ok', Math.random(), {priority:'event'})",
        "Importer"
      )
    )
  )
}

output$imp_options_ui <- renderUI({
  req(input$imp_fichier)
  ext <- tolower(tools::file_ext(input$imp_fichier$name))
  
  if (ext %in% c("csv", "txt", "tsv")) {
    sep_det <- tryCatch(
      detecter_separateur(input$imp_fichier$datapath),
      error = function(e) ","
    )
    tags$div(class = "h-form-group",
             tags$label(class = "h-label", "Séparateur de colonnes"),
             tags$select(
               id       = "imp_sep",
               class    = "h-select",
               onchange = "Shiny.setInputValue('imp_sep', this.value, {priority:'event'})",
               tags$option(value = ",",  "Virgule  ( , )",       selected = if (sep_det == ",")  "selected"),
               tags$option(value = ";",  "Point-virgule  ( ; )", selected = if (sep_det == ";")  "selected"),
               tags$option(value = "\t", "Tabulation",           selected = if (sep_det == "\t") "selected"),
               tags$option(value = "|",  "Pipe  ( | )",          selected = if (sep_det == "|")  "selected")
             )
    )
  } else if (ext %in% c("xlsx", "xls")) {
    feuilles <- tryCatch(
      lister_feuilles(input$imp_fichier$datapath),
      error = function(e) "Feuille1"
    )
    tags$div(class = "h-form-group",
             tags$label(class = "h-label", "Feuille Excel"),
             tags$select(
               id       = "imp_feuille",
               class    = "h-select",
               onchange = "Shiny.setInputValue('imp_feuille', this.value, {priority:'event'})",
               lapply(feuilles, function(f) tags$option(value = f, f))
             )
    )
  }
})

lire_fichier_import <- function(utiliser_options_courantes = TRUE) {
  req(input$imp_fichier)
  
  ext <- tolower(tools::file_ext(input$imp_fichier$name))
  
  if (ext %in% c("csv", "txt", "tsv")) {
    sep <- if (utiliser_options_courantes &&
               !is.null(input$imp_sep) &&
               nzchar(input$imp_sep)) {
      input$imp_sep
    } else {
      tryCatch(
        detecter_separateur(input$imp_fichier$datapath),
        error = function(e) ","
      )
    }
    
    return(importer_fichier(
      input$imp_fichier$datapath,
      options = list(separateur = sep)
    ))
  }
  
  if (ext %in% c("xlsx", "xls")) {
    feuille <- if (utiliser_options_courantes &&
                   !is.null(input$imp_feuille) &&
                   nzchar(input$imp_feuille)) {
      input$imp_feuille
    } else {
      1
    }
    
    return(importer_fichier(
      input$imp_fichier$datapath,
      options = list(feuille = feuille)
    ))
  }
  
  importer_fichier(input$imp_fichier$datapath, options = list())
}

observeEvent(input$imp_fichier, {
  rv$imp_df_temp <- NULL
  rv$imp_erreur <- NULL
  
  df <- tryCatch(
    lire_fichier_import(utiliser_options_courantes = FALSE),
    error = function(e) {
      rv$imp_erreur <- conditionMessage(e)
      NULL
    }
  )
  
  if (!is.null(df)) {
    rv$imp_df_temp <- as.data.frame(df, stringsAsFactors = FALSE)
  }
}, ignoreInit = TRUE)

observeEvent(input$imp_sep, {
  req(input$imp_fichier)
  
  df <- tryCatch(
    importer_fichier(
      input$imp_fichier$datapath,
      options = list(separateur = input$imp_sep)
    ),
    error = function(e) {
      rv$imp_erreur <- conditionMessage(e)
      NULL
    }
  )
  
  if (!is.null(df)) {
    rv$imp_erreur <- NULL
    rv$imp_df_temp <- as.data.frame(df, stringsAsFactors = FALSE)
  }
}, ignoreInit = TRUE)

observeEvent(input$imp_feuille, {
  req(input$imp_fichier)
  
  df <- tryCatch(
    importer_fichier(
      input$imp_fichier$datapath,
      options = list(feuille = input$imp_feuille)
    ),
    error = function(e) {
      rv$imp_erreur <- conditionMessage(e)
      NULL
    }
  )
  
  if (!is.null(df)) {
    rv$imp_erreur <- NULL
    rv$imp_df_temp <- as.data.frame(df, stringsAsFactors = FALSE)
  }
}, ignoreInit = TRUE)

output$imp_statut_ui <- renderUI({
  req(input$imp_fichier)
  
  df <- rv$imp_df_temp
  err <- rv$imp_erreur
  
  if (is.null(df)) {
    return(tags$div(class = "h-alerte h-alerte-danger",
                    if (!is.null(err) && nzchar(err)) {
                      paste0("Impossible de lire ce fichier : ", err)
                    } else {
                      "Impossible de lire ce fichier. Vérifiez le format ou les options d'import."
                    }
    ))
  }
  
  tags$div(class = "h-alerte h-alerte-succes",
           glue::glue(
             "{format(nrow(df), big.mark = ' ')} lignes",
             "  x  {ncol(df)} colonnes"
           )
  )
})

# Rendu de l'aperçu modale via Reactable
output$imp_apercu_reactable <- renderReactable({
  df <- rv$imp_df_temp
  req(!is.null(df))
  
  df_propre <- nettoyer_df_pour_dt(df)
  req(nrow(df_propre) > 0)
  
  reactable(
    head(df_propre, 5),
    filterable          = FALSE,
    searchable          = FALSE,
    striped             = TRUE,
    compact             = TRUE,
    bordered            = TRUE,
    resizable           = TRUE,
    pagination          = FALSE,
    highlight           = TRUE
  )
})

observeEvent(input$imp_ok, {
  df <- rv$imp_df_temp
  if (is.null(df)) {
    showNotification("Aucune donnée à importer.", type = "warning")
    return()
  }
  nom <- input$imp_fichier$name %||% "fichier"
  
  pl                <- nouveau_pipeline()
  pl$donnees_brutes <- df
  pl$nom_source     <- nom
  pl <- ajouter_etape(pl, list(
    type    = "import",
    libelle = glue::glue("Import : {nom}")
  ))
  rv$pipeline    <- pl
  rv$imp_df_temp <- NULL
  removeModal()
  showNotification(
    glue::glue("{nom}  —  {format(nrow(df), big.mark=' ')} lignes x {ncol(df)} colonnes"),
    type = "message", duration = 5
  )
}, ignoreInit = TRUE)

appliquer_actualiser <- function() {
  showNotification(
    "Pour actualiser : ré-importez le fichier. Les transformations seront rejouées.",
    type = "message", duration = 6
  )
}

modal_ajouter_fichier <- function() {
  modalDialog(
    title = "Ajouter un fichier (même structure)",
    size  = "m",
    tags$div(class = "h-alerte h-alerte-info",
             "Ce fichier doit avoir les mêmes colonnes que le tableau actuel.",
             " Ses lignes seront ajoutées en dessous."
    ),
    tags$div(class = "h-form-group",
             tags$label(class = "h-label", "Fichier à ajouter"),
             fileInput("af_fichier", NULL,
                       accept      = c(".csv",".tsv",".xlsx",".xls",".sav",".dta",".rds"),
                       buttonLabel = "Parcourir...",
                       placeholder = "Aucun fichier sélectionné"
             )
    ),
    uiOutput("af_statut_ui"),
    footer = tagList(
      modalButton("Annuler"),
      tags$button(
        class   = "h-btn-ok",
        onclick = "Shiny.setInputValue('af_ok', Math.random(), {priority:'event'})",
        "Ajouter les lignes"
      )
    )
  )
}

output$af_statut_ui <- renderUI({
  req(input$af_fichier)
  df2 <- tryCatch(
    as.data.frame(importer_fichier(input$af_fichier$datapath)),
    error = function(e) NULL
  )
  if (is.null(df2)) {
    return(tags$div(class = "h-alerte h-alerte-danger",
                    "Impossible de lire ce fichier."))
  }
  rv$af_df_temp <- df2
  df1       <- rv$donnees_courantes
  cols_comm <- if (!is.null(df1)) intersect(names(df1), names(df2)) else names(df2)
  cols_manq <- if (!is.null(df1)) setdiff(names(df1), names(df2))  else character(0)
  tagList(
    tags$div(class = "h-alerte h-alerte-succes",
             glue::glue("{format(nrow(df2), big.mark=' ')} lignes  |  {length(cols_comm)} colonne(s) en commun")
    ),
    if (length(cols_manq) > 0)
      tags$div(class = "h-alerte h-alerte-warning",
               glue::glue("Colonnes absentes (seront NA) : {paste(head(cols_manq, 5), collapse = ', ')}")
      )
  )
})

observeEvent(input$af_ok, {
  df2 <- rv$af_df_temp
  if (is.null(df2)) {
    showNotification("Aucune donnée à ajouter.", type = "warning"); return()
  }
  nom2 <- input$af_fichier$name %||% "fichier"
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type    = "append_custom",
    table2  = df2,
    libelle = glue::glue("Append : {nom2}")
  ))
  rv$af_df_temp <- NULL
  removeModal()
  showNotification(
    glue::glue("{format(nrow(df2), big.mark=' ')} lignes ajoutées depuis '{nom2}'."),
    type = "message"
  )
}, ignoreInit = TRUE)