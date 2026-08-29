#' Lancer l'interface Hygie
#'
#' Lance l'application Shiny Hygie pour le traitement interactif des données.
#' Aucune connaissance de R n'est requise.
#'
#' @param port Entier. Port à utiliser (défaut : 3838)
#' @param browser Logical. Ouvrir dans le navigateur ? (défaut : TRUE)
#'
#' @return Lance l'application Shiny (invisiblement)
#' @export
#'
#' @examples
#' \dontrun{
#'   hygie()
#' }
hygie <- function(port = 3838, browser = TRUE) {
  app_dir <- system.file("app", package = "hygie")

  # Mode développement : chercher dans le répertoire courant
  if (app_dir == "" || !dir.exists(app_dir)) {
    app_dir <- file.path(getwd(), "inst", "app")
  }

  if (!dir.exists(app_dir)) {
    stop("Impossible de trouver le répertoire de l'application Hygie.\n",
         "Assurez-vous d'être dans le dossier racine du package.", call. = FALSE)
  }

  opts <- list(launch.browser = browser)
  if (!is.null(port)) opts$port <- as.integer(port)

  do.call(shiny::runApp, c(list(appDir = app_dir), opts))
}
