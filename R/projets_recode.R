# =============================================================================
# PROJETS HYGIE — Correction de génération du recodage
# =============================================================================
# Ce fichier est chargé après projets.R (ordre alphabétique des fichiers R).
# Il conserve le générateur existant et remplace uniquement la génération
# du recodage par une forme explicite, lisible et directement réutilisable.

.etape_vers_code_base <- etape_vers_code

.r_recode_code <- function(etape) {
  variable <- as.character(etape$variable %||% "")
  corr <- etape$correspondance %||% character(0)

  if (length(corr) == 0L) {
    return(sprintf(
      "df[[%s]] <- as.character(df[[%s]])",
      .r_string(variable),
      .r_string(variable)
    ))
  }

  # dput() conserve correctement les accents, espaces, nombres et NA.
  # Chaque paire est ensuite écrite explicitement afin que le script soit
  # facile à lire et à modifier par l'utilisateur.
  noms <- names(corr)
  if (is.null(noms) || any(!nzchar(noms))) {
    stop("Impossible de générer le recodage : les modalités sources sont absentes.", call. = FALSE)
  }

  lignes <- vapply(seq_along(corr), function(i) {
    source <- .r_string(noms[[i]])
    cible <- .r_literal(unname(corr[[i]]))
    sprintf("  %s = %s", source, cible)
  }, character(1))

  sprintf(
    "df[[%s]] <- dplyr::recode(as.character(df[[%s]]),\n%s\n)",
    .r_string(variable),
    .r_string(variable),
    paste(lignes, collapse = ",\n")
  )
}

#' Générer du code R pour une étape du pipeline, avec recodage explicite.
#' @export
etape_vers_code <- function(etape) {
  if ((etape$type %||% "") == "recoder") {
    return(.r_recode_code(etape))
  }
  .etape_vers_code_base(etape)
}
