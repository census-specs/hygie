# =============================================================================
# TRANSFORMATIONS — Fonctions utilitaires pour les modules Shiny
# Le moteur principal est dans pipeline.R (appliquer_etape)
# Ce fichier contient les helpers d'analyse et de diagnostic
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# DATES — conversion robuste multi-format
# ---------------------------------------------------------------------------

#' Convertir une colonne hétérogène en Date
#'
#' Essaie plusieurs formats valeur par valeur. Les formats jour/mois sont
#' prioritaires pour éviter les interprétations anglo-saxonnes ambiguës.
#' Les mois français et les sériaux Excel sont également pris en charge.
#' @export
hygie_parse_date_auto <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "-", ".")] <- NA_character_

  out <- as.Date(rep(NA_character_, length(x)))

  # Sériaux Excel : 45292, par exemple. Les YYYYMMDD à 8 chiffres
  # ne sont volontairement pas considérés comme des sériaux Excel.
  num <- suppressWarnings(as.numeric(x))
  idx <- !is.na(num) & grepl("^[0-9]{5}$", x)
  out[idx] <- as.Date(num[idx], origin = "1899-12-30")

  z <- tolower(x)
  z <- chartr("éèêëàâäîïôöùûüçÿ", "eeeeaaaiioouucy", z)

  mois <- c(
    janvier="01", fevrier="02", mars="03", avril="04", mai="05", juin="06",
    juillet="07", aout="08", septembre="09", octobre="10", novembre="11", decembre="12"
  )
  for (m in names(mois)) {
    z <- gsub(
      paste0("([0-9]{1,2})\\s+", m, "\\s+([0-9]{2,4})"),
      paste0("\\1/", mois[[m]], "/\\2"), z
    )
  }

  # Une conversion en Date ne conserve pas l'heure.
  z <- sub("\\s+[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?.*$", "", z)

  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d",
    "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
    "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y",
    "%Y%m%d", "%d%m%Y",
    "%d/%m/%y", "%d-%m-%y", "%d.%m.%y"
  )

  for (f in formats) {
    miss <- is.na(out) & !is.na(z)
    if (!any(miss)) break
    parsed <- suppressWarnings(as.Date(z[miss], format = f))
    out[which(miss)] <- parsed
  }

  miss <- is.na(out) & !is.na(x)
  if (any(miss)) {
    parsed <- suppressWarnings(as.Date(x[miss], format = "%Y-%m-%d %H:%M:%S"))
    out[which(miss)] <- parsed
  }

  out
}

#' Générer une expression R autonome pour une conversion multi-format
#' @param col Nom de colonne R déjà valide dans le contexte d'évaluation
#' @return chaîne contenant une expression R autonome
hygie_date_auto_formula <- function(col) {
  sprintf("(function(x) {\n  x <- trimws(as.character(x))\n  x[x %%in%% c(\"\", \"NA\", \"N/A\", \"NULL\", \"-\", \".\")] <- NA_character_\n  out <- as.Date(rep(NA_character_, length(x)))\n  num <- suppressWarnings(as.numeric(x))\n  idx <- !is.na(num) & grepl(\"^[0-9]{5}$\", x)\n  out[idx] <- as.Date(num[idx], origin = \"1899-12-30\")\n  z <- tolower(x)\n  z <- chartr(\"éèêëàâäîïôöùûüçÿ\", \"eeeeaaaiioouucy\", z)\n  mois <- c(janvier=\"01\", fevrier=\"02\", mars=\"03\", avril=\"04\", mai=\"05\", juin=\"06\", juillet=\"07\", aout=\"08\", septembre=\"09\", octobre=\"10\", novembre=\"11\", decembre=\"12\")\n  for (m in names(mois)) z <- gsub(paste0(\"([0-9]{1,2})\\\\s+\", m, \"\\\\s+([0-9]{2,4})\"), paste0(\"\\\\1/\", mois[[m]], \"/\\\\2\"), z)\n  z <- sub(\"\\\\s+[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?.*$\", \"\", z)\n  formats <- c(\"%%Y-%%m-%%d\", \"%%Y/%%m/%%d\", \"%%Y.%%m.%%d\", \"%%d/%%m/%%Y\", \"%%d-%%m-%%Y\", \"%%d.%%m.%%Y\", \"%%m/%%d/%%Y\", \"%%m-%%d-%%Y\", \"%%m.%%d.%%Y\", \"%%Y%%m%%d\", \"%%d%%m%%Y\", \"%%d/%%m/%%y\", \"%%d-%%m-%%y\", \"%%d.%%m.%%y\")\n  for (f in formats) { miss <- is.na(out) & !is.na(z); if (!any(miss)) break; parsed <- suppressWarnings(as.Date(z[miss], format=f)); out[which(miss)] <- parsed }\n  miss <- is.na(out) & !is.na(x)\n  if (any(miss)) out[which(miss)] <- suppressWarnings(as.Date(x[miss], format=\"%%Y-%%m-%%d %%H:%%M:%%S\"))\n  out\n})(%s)", col)
}

# ---------------------------------------------------------------------------
# DIAGNOSTIC DES DONNÉES
# ---------------------------------------------------------------------------

#' Résumé des valeurs manquantes par colonne
#' @export
resumer_manquants <- function(df) {
  n <- nrow(df)
  data.frame(
    colonne       = names(df),
    type          = sapply(df, function(x) class(x)[1]),
    nb_manquants  = sapply(df, function(x) sum(is.na(x))),
    pct_manquants = round(sapply(df, function(x) mean(is.na(x))) * 100, 1),
    row.names     = NULL,
    stringsAsFactors = FALSE
  )
}

#' Détecter les outliers (IQR ou Z-score)
#' @export
detecter_outliers <- function(df, colonne, methode = "iqr", seuil = NULL) {
  x <- df[[colonne]]
  if (!is.numeric(x)) stop("Colonne numérique requise.")

  if (methode == "iqr") {
    seuil <- seuil %||% 1.5
    q1  <- quantile(x, 0.25, na.rm = TRUE)
    q3  <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    (x < q1 - seuil * iqr) | (x > q3 + seuil * iqr)
  } else {
    seuil <- seuil %||% 3
    z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    abs(z) > seuil
  }
}

#' Résumé statistique rapide d'une colonne
#' @export
resumer_colonne <- function(df, colonne) {
  x <- df[[colonne]]
  if (is.numeric(x)) {
    list(
      type    = "numérique",
      n       = length(x),
      na      = sum(is.na(x)),
      min     = min(x, na.rm = TRUE),
      max     = max(x, na.rm = TRUE),
      moyenne = mean(x, na.rm = TRUE),
      mediane = median(x, na.rm = TRUE),
      ecart_type = sd(x, na.rm = TRUE)
    )
  } else {
    vals <- table(x, useNA = "no")
    list(
      type      = class(x)[1],
      n         = length(x),
      na        = sum(is.na(x)),
      n_distincts = length(vals),
      modalite_freq = if (length(vals) > 0) names(vals)[which.max(vals)] else NA
    )
  }
}

# ---------------------------------------------------------------------------
# IMPORT / EXPORT
# ---------------------------------------------------------------------------

#' Importer un fichier selon son extension
#' @export
importer_fichier <- function(chemin, options = list()) {
  ext <- tolower(tools::file_ext(chemin))

  df <- switch(ext,
    "csv"  = {
      sep <- options$separateur %||% ","
      enc <- options$encodage   %||% "UTF-8"
      readr::read_delim(chemin, delim = sep,
                        locale = readr::locale(encoding = enc),
                        show_col_types = FALSE)
    },
    "tsv"  = readr::read_tsv(chemin, show_col_types = FALSE),
    "txt"  = {
      sep <- options$separateur %||% "\t"
      readr::read_delim(chemin, delim = sep, show_col_types = FALSE)
    },
    "xlsx" = {
      feuille <- options$feuille %||% 1
      readxl::read_excel(chemin, sheet = feuille, col_names = TRUE)
    },
    "xls" = {
      feuille <- options$feuille %||% 1
      readxl::read_excel(chemin, sheet = feuille, col_names = TRUE)
    },
    "sav"      = haven::read_sav(chemin),
    "sas7bdat" = haven::read_sas(chemin),
    "dta"     = haven::read_dta(chemin),
    "json"     = {
      obj <- jsonlite::fromJSON(chemin, flatten = TRUE)
      if (is.data.frame(obj)) obj else as.data.frame(obj)
    },
    "rds"  = {
      obj <- readRDS(chemin)
      if (!is.data.frame(obj)) stop("Le fichier RDS ne contient pas un data.frame.")
      obj
    },
    "rdata" = {
      env  <- new.env()
      load(chemin, envir = env)
      dfs  <- Filter(function(n) is.data.frame(env[[n]]), ls(env))
      if (length(dfs) == 0) stop("Aucun data.frame dans ce fichier RData.")
      env[[dfs[[1]]]]
    },
    "rda" = {
      env  <- new.env()
      load(chemin, envir = env)
      dfs  <- Filter(function(n) is.data.frame(env[[n]]), ls(env))
      if (length(dfs) == 0) stop("Aucun data.frame dans ce fichier RData.")
      env[[dfs[[1]]]]
    },
    stop(glue::glue("Format non supporté : .{ext}"))
  )

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x) || length(x) == 0) NA_character_
        else as.character(x[[1]])
      })
    }
  }

  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled")) {
      df[[col]] <- haven::zap_labels(df[[col]])
    }
  }

  df
}

#' Exporter un data.frame
#' @export
exporter_fichier <- function(donnees, chemin, options = list()) {
  ext <- tolower(tools::file_ext(chemin))
  switch(ext,
    "csv"  = readr::write_delim(donnees, chemin, delim = options$separateur %||% ","),
    "xlsx" = writexl::write_xlsx(setNames(list(donnees), options$feuille %||% "Données"), chemin),
    "sav"  = haven::write_sav(donnees, chemin),
    "dta"  = haven::write_dta(donnees, chemin),
    "rds"  = saveRDS(donnees, chemin),
    "tsv"  = readr::write_tsv(donnees, chemin),
    stop(glue::glue("Format non supporté : .{ext}"))
  )
  invisible(chemin)
}

#' Détecter le séparateur probable d'un CSV
#' @export
detecter_separateur <- function(chemin) {
  lignes <- readLines(chemin, n=5, warn=FALSE)
  premiere <- lignes[1]
  candidats <- c(",", ";", "\t", "|")
  comptes <- sapply(candidats, function(s)
    sum(nchar(premiere) - nchar(gsub(s, "", premiere, fixed=TRUE))))
  candidats[which.max(comptes)]
}

#' Lister les feuilles d'un fichier Excel
#' @export
lister_feuilles <- function(chemin) readxl::excel_sheets(chemin)
