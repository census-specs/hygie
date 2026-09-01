library(shiny)
library(reactable)

if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

nettoyer_df_pour_dt <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(df) == 0 && ncol(df) == 0) return(df)
  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled") || inherits(df[[col]], "labelled")) df[[col]] <- as.vector(df[[col]])
    if (inherits(df[[col]], "POSIXlt")) df[[col]] <- as.POSIXct(df[[col]])
    if (is.list(df[[col]]) && !is.data.frame(df[[col]])) df[[col]] <- sapply(df[[col]], function(x) paste(as.character(x), collapse = ", "))
  }
  df
}

for (f in c("pipeline.R", "transformations.R")) {
  ch <- file.path("..", "..", "R", f)
  if (file.exists(ch)) source(ch, local = FALSE)
}

server <- function(input, output, session) {
  rv <- reactiveValues(
    pipeline = nouveau_pipeline(), donnees_courantes = NULL, cond_nb = 2L, plg_nb = 3L,
    imp_df_temp = NULL, imp_erreur = NULL, af_df_temp = NULL
  )

  output$a_des_donnees <- reactive({
    df <- rv$donnees_courantes
    !is.null(df) && nrow(df) > 0
  })
  outputOptions(output, "a_des_donnees", suspendWhenHidden = FALSE)

  output$imp_a_des_donnees <- reactive({ !is.null(rv$imp_df_temp) && nrow(rv$imp_df_temp) > 0 })
  outputOptions(output, "imp_a_des_donnees", suspendWhenHidden = FALSE)

  cols_disp <- reactive({ df <- rv$donnees_courantes; if (is.null(df)) character(0) else names(df) })
  cols_num <- reactive({ df <- rv$donnees_courantes; if (is.null(df)) character(0) else names(df)[sapply(df, is.numeric)] })
  cols_texte_fct <- reactive({ df <- rv$donnees_courantes; if (is.null(df)) character(0) else names(df)[sapply(df, function(x) is.character(x) || is.factor(x))] })

  source("modules/mod_import.R", local = TRUE)
  source("modules/mod_nettoyer.R", local = TRUE)
  source("modules/mod_transformer.R", local = TRUE)
  source("modules/mod_combiner.R", local = TRUE)
  source("modules/mod_qualite.R", local = TRUE)
  source("modules/mod_boxplot_qualite.R", local = TRUE)
  source("modules/mod_dates.R", local = TRUE)
  source("modules/mod_export.R", local = TRUE)

  observe({
    pl <- rv$pipeline
    if (!is.null(pl$donnees_brutes)) {
      res <- tryCatch(rejouer_pipeline(pl), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      if (!is.null(res)) rv$donnees_courantes <- res
    }
  })

  output$apercu_donnees <- reactable::renderReactable({
    req(rv$donnees_courantes)
    reactable::reactable(nettoyer_df_pour_dt(rv$donnees_courantes), searchable = TRUE, filterable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 20)
  })

  output$nb_lignes <- renderText({ if (is.null(rv$donnees_courantes)) "0" else format(nrow(rv$donnees_courantes), big.mark = " ") })
  output$nb_colonnes <- renderText({ if (is.null(rv$donnees_courantes)) "0" else format(ncol(rv$donnees_courantes), big.mark = " ") })
}
