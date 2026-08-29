#' Lancer l'application Hygie
#'
#' @param port Port sur lequel lancer l'application (par défaut 3838)
#' @param launch.browser Ouvrir automatiquement le navigateur (TRUE par défaut)
#' @export
run_hygie <- function(port = 3838, launch.browser = TRUE) {
  app_dir <- system.file("app", package = "hygie")
  if (app_dir == "") {
    stop("Impossible de trouver le dossier de l'application Shiny.", call. = FALSE)
  }
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser)
}