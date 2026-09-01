# =============================================================================
# MOTEUR DE PIPELINE — Hygie v3
# Architecture : données brutes + liste d'étapes paramétrées
# Moteur pur (sans Shiny), testable en console
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# APPLIQUER UNE ÉTAPE
# ---------------------------------------------------------------------------

#' Applique une seule étape de transformation à un data.frame
#'
#' @param df data.frame en entrée
#' @param etape liste décrivant l'opération (type + paramètres)
#' @export
appliquer_etape <- function(df, etape) {
  v <- etape$variable

  switch(etape$type,

    "renommer" = {
      names(df)[names(df) == v] <- etape$nouveau_nom
      df
    },

    "type" = {
      df[[v]] <- switch(etape$nouveau_type,
        "numerique"    = suppressWarnings(as.numeric(df[[v]])),
        "texte"        = as.character(df[[v]]),
        "categorielle" = as.factor(df[[v]]),
        "logique"      = as.logical(df[[v]]),
        "date"         = if (isTRUE(etape$date_auto)) {
          hygie_parse_date_auto(df[[v]])
        } else {
          suppressWarnings(as.Date(df[[v]], format = etape$format_date %||% "%Y-%m-%d"))
        }
      )
      df
    },

    "recoder" = {
      corr <- etape$correspondance
      df[[v]] <- dplyr::recode(as.character(df[[v]]), !!!corr)
      df
    },

    "manquants" = {
      cols <- etape$variables %||% v
      methode <- etape$methode
      valeur  <- etape$valeur

      for (col in cols) {
        df <- switch(methode,
          "supprimer"   = df[!is.na(df[[col]]), ],
          "valeur_fixe" = { df[[col]][is.na(df[[col]])] <- valeur; df },
          "moyenne"     = { df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE); df },
          "mediane"     = { df[[col]][is.na(df[[col]])] <- stats::median(df[[col]], na.rm = TRUE); df },
          "mode"        = {
            tab <- table(df[[col]])
            df[[col]][is.na(df[[col]])] <- names(tab)[which.max(tab)]
            df
          },
          df
        )
      }
      df
    },

    "filtrer" = {
      op  <- etape$operateur
      val <- etape$valeur
      condition <- switch(op,
        ">"  = df[[v]] >  suppressWarnings(as.numeric(val)),
        ">=" = df[[v]] >= suppressWarnings(as.numeric(val)),
        "<"  = df[[v]] <  suppressWarnings(as.numeric(val)),
        "<=" = df[[v]] <= suppressWarnings(as.numeric(val)),
        "="  = df[[v]] == val,
        "!=" = df[[v]] != val,
        {
          expr <- rlang::parse_expr(etape$expression)
          eval(expr, envir = df)
        }
      )
      df[which(condition), ]
    },

    "filtrer_expr" = {
      expr <- rlang::parse_expr(etape$expression)
      masque <- eval(expr, envir = df)
      df[which(masque), ]
    },

    "calculee" = {
      df[[etape$nom]] <- eval(parse(text = etape$formule), envir = df)
      df
    },

    "espaces" = {
      df[[v]] <- switch(etape$mode,
        "bords"    = trimws(df[[v]]),
        "internes" = gsub("\\s+", " ", trimws(df[[v]]))
      )
      df
    },

    "casse" = {
      df[[v]] <- switch(etape$mode,
        "majuscules" = toupper(df[[v]]),
        "minuscules" = tolower(df[[v]]),
        "premiere"   = tools::toTitleCase(tolower(df[[v]]))
      )
      df
    },

    "remplacer_texte" = {
      df[[v]] <- gsub(etape$motif, etape$remplacement, df[[v]], fixed = !isTRUE(etape$regex))
      df
    },

    "remplacer_valeurs" = {
      df[[v]][df[[v]] %in% etape$anciennes_valeurs] <- etape$nouvelle_valeur
      df
    },

    "supprimer_colonnes" = {
      cols <- etape$variables %||% v
      df[, setdiff(names(df), cols), drop = FALSE]
    },

    "scinder" = {
      parties <- if (etape$mode == "separateur") {
        do.call(rbind, strsplit(as.character(df[[v]]), etape$separateur, fixed = TRUE))
      } else {
        pos <- etape$position
        cbind(substr(df[[v]], 1, pos), substr(df[[v]], pos + 1, nchar(df[[v]])))
      }
      n <- ncol(parties)
      noms <- if (!is.null(etape$noms_nouveaux) && length(etape$noms_nouveaux) >= n) etape$noms_nouveaux[seq_len(n)] else paste0(v, "_", seq_len(n))
      for (i in seq_len(n)) df[[noms[i]]] <- parties[, i]
      df
    },

    "fusionner_colonnes" = {
      df[[etape$nouveau_nom]] <- do.call(paste, c(df[etape$variables], sep = etape$separateur %||% " "))
      df
    },

    "extraire" = {
      nom_sortie <- etape$nom_sortie %||% paste0(v, "_extrait")
      df[[nom_sortie]] <- switch(etape$mode,
        "debut" = substr(df[[v]], 1, etape$n),
        "fin"   = substr(df[[v]], nchar(df[[v]]) - etape$n + 1, nchar(df[[v]])),
        "entre" = sub(paste0(".*", etape$avant, "(.*)", etape$apres, ".*"), "\\1", df[[v]])
      )
      df
    },

    "conditionnelle" = {
      resultat <- rep(etape$sinon %||% NA, nrow(df))
      for (cond in rev(etape$conditions)) {
        idx <- which(eval(parse(text = cond$test), envir = df))
        resultat[idx] <- cond$alors
      }
      df[[etape$nom]] <- resultat
      df
    },

    "arrondir" = {
      x <- df[[v]]
      df[[v]] <- switch(etape$mode,
        "arrondir"  = round(x, etape$decimales %||% 0),
        "plafonner" = pmin(x, etape$valeur),
        "seuiller"  = pmax(x, etape$valeur),
        "ecreter"   = pmin(pmax(x, etape$borne_basse), etape$borne_haute),
        x
      )
      df
    },

    "grouper" = {
      df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(etape$variables_groupe))) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(etape$variable_agregee), list(res = match.fun(etape$fonction)), .names = etape$nom_sortie %||% "{.col}_{.fn}"),
          .groups = "drop"
        ) |>
        as.data.frame()
    },

    "trier" = {
      if (etape$ordre == "croissant") dplyr::arrange(df, dplyr::across(dplyr::all_of(v))) else dplyr::arrange(df, dplyr::desc(dplyr::across(dplyr::all_of(v))))
    },

    "doublons" = {
      cols <- etape$variables
      if (is.null(cols) || length(cols) == 0) dplyr::distinct(df) else dplyr::distinct(df, dplyr::across(dplyr::all_of(cols)), .keep_all = TRUE)
    },

    "pivoter_long" = {
      tidyr::pivot_longer(df, cols = dplyr::all_of(etape$variables), names_to = etape$nom_cle %||% "variable", values_to = etape$nom_valeur %||% "valeur") |> as.data.frame()
    },

    "pivoter_large" = {
      tidyr::pivot_wider(df, names_from = dplyr::all_of(etape$variable_cle), values_from = dplyr::all_of(etape$variable_valeur)) |> as.data.frame()
    },

    "outliers_custom" = {
      x <- df[[v]]
      seuil <- etape$seuil %||% 1.5
      masque <- if (etape$methode_detect == "iqr") {
        q1 <- quantile(x, 0.25, na.rm = TRUE); q3 <- quantile(x, 0.75, na.rm = TRUE); iqr <- q3 - q1
        (!is.na(x)) & ((x < q1 - seuil * iqr) | (x > q3 + seuil * iqr))
      } else {
        z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
        (!is.na(x)) & (abs(z) > seuil)
      }
      switch(etape$traitement,
        "remplacer_na" = { df[[v]][masque] <- NA; df },
        "supprimer" = df[!masque, ],
        "winsoriser" = {
          q1 <- quantile(x, 0.25, na.rm = TRUE); q3 <- quantile(x, 0.75, na.rm = TRUE); iqr <- q3 - q1
          df[[v]] <- pmin(pmax(x, q1 - 1.5 * iqr), q3 + 1.5 * iqr); df
        },
        df
      )
    },

    "join_custom" = {
      df2 <- etape$table2; cle1 <- etape$cle1; cle2 <- etape$cle2
      fn <- switch(etape$type_join %||% "left", left=dplyr::left_join, inner=dplyr::inner_join, right=dplyr::right_join, full=dplyr::full_join)
      as.data.frame(fn(df, df2, by=setNames(cle2, cle1)))
    },

    "append_custom" = { as.data.frame(dplyr::bind_rows(df, etape$table2)) },

    "top_n_custom" = { n <- etape$n %||% 10L; if ((etape$depuis %||% "debut") == "debut") head(df, n) else tail(df, n) },

    "reorg_cols" = {
      ordre <- etape$ordre; ordre <- ordre[ordre %in% names(df)]; autres <- setdiff(names(df), ordre); df[, c(ordre, autres), drop=FALSE]
    },

    df
  )
}

# ---------------------------------------------------------------------------
# REJOUER LE PIPELINE
# ---------------------------------------------------------------------------
rejouer_etapes <- function(donnees_originales, etapes) {
  df <- donnees_originales
  for (etape in etapes) {
    if (isFALSE(etape$active)) next
    df <- tryCatch(appliquer_etape(df, etape), error=function(e) stop(sprintf("Étape '%s' impossible : %s", etape$libelle %||% etape$type, conditionMessage(e))))
  }
  df
}

nouveau_pipeline <- function() list(donnees_brutes=NULL, nom_source=NULL, etapes=list())

ajouter_etape <- function(pipeline, etape) {
  if (is.null(etape$id)) etape$id <- paste0("e_", format(Sys.time(), "%H%M%S"), "_", sample(1000:9999,1))
  if (is.null(etape$active)) etape$active <- TRUE
  pipeline$etapes <- c(pipeline$etapes, list(etape)); pipeline
}

supprimer_etape <- function(pipeline, id) { pipeline$etapes <- Filter(function(e) e$id != id, pipeline$etapes); pipeline }

rejouer_pipeline <- function(pipeline) { if (is.null(pipeline$donnees_brutes)) return(NULL); rejouer_etapes(pipeline$donnees_brutes, pipeline$etapes) }

apercu_etape <- function(pipeline, jusqu_a_id) {
  if (is.null(pipeline$donnees_brutes)) return(NULL)
  etapes_tronquees <- list()
  for (e in pipeline$etapes) { etapes_tronquees <- c(etapes_tronquees, list(e)); if (e$id == jusqu_a_id) break }
  rejouer_etapes(pipeline$donnees_brutes, etapes_tronquees)
}

# ---------------------------------------------------------------------------
# GÉNÉRATION DU CODE R
# ---------------------------------------------------------------------------
etape_vers_code <- function(etape) {
  v <- etape$variable %||% ""
  type <- etape$type %||% ""

  if (type == "renommer") return(sprintf('df <- dplyr::rename(df, `%s` = `%s`)', etape$nouveau_nom, v))
  if (type == "type") {
    if (identical(etape$nouveau_type, "date") && isTRUE(etape$date_auto)) {
      return(sprintf('df$`%s` <- hygie_parse_date_auto(df$`%s`)', v, v))
    }
    return(sprintf('df$`%s` <- as.%s(df$`%s`)', v, etape$nouveau_type %||% "character", v))
  }
  if (type == "manquants") return(sprintf('# Traitement manquants : `%s` (%s)', v, etape$methode %||% ""))
  if (type %in% c("filtrer_expr", "filtrer")) return(sprintf('df <- df[%s, ]', etape$expression %||% ""))
  if (type == "calculee") return(sprintf('df$`%s` <- with(df, %s)', etape$nom %||% "col", etape$formule %||% ""))
  if (type == "espaces") return(sprintf('df$`%s` <- trimws(df$`%s`)', v, v))
  if (type == "casse") return(sprintf('df$`%s` <- tolower(df$`%s`)', v, v))
  if (type == "remplacer_texte") return(sprintf('df$`%s` <- gsub("%s", "%s", df$`%s`)', v, etape$motif %||% "", etape$remplacement %||% "", v))
  if (type == "supprimer_colonnes") return(sprintf('df <- df[, !names(df) %%in%% c(%s)]', paste(shQuote(etape$variables %||% v), collapse=", ")))
  if (type == "recoder") return(sprintf('# Recodage de `%s`', v))
  if (type == "scinder") return(sprintf('# Scission de `%s` par "%s"', v, etape$separateur %||% ""))
  if (type == "fusionner_colonnes") return(sprintf('df$`%s` <- paste(%s, sep="%s")', etape$nouveau_nom %||% "col", paste(sprintf('df$`%s`', etape$variables %||% v), collapse=", "), etape$separateur %||% " "))
  if (type == "conditionnelle") return(sprintf('# Variable conditionnelle `%s`', etape$nom %||% "col"))
  if (type == "arrondir") {
    if (identical(etape$mode, "ecreter")) {
      return(sprintf('df$`%s` <- pmin(pmax(df$`%s`, %s), %s)', v, v, format(etape$borne_basse, scientific=FALSE, trim=TRUE), format(etape$borne_haute, scientific=FALSE, trim=TRUE)))
    }
    if (identical(etape$mode, "plafonner")) return(sprintf('df$`%s` <- pmin(df$`%s`, %s)', v, v, format(etape$valeur, scientific=FALSE, trim=TRUE)))
    if (identical(etape$mode, "seuiller")) return(sprintf('df$`%s` <- pmax(df$`%s`, %s)', v, v, format(etape$valeur, scientific=FALSE, trim=TRUE)))
    return(sprintf('df$`%s` <- round(df$`%s`, %d)', v, v, as.integer(etape$decimales %||% 0L)))
  }
  if (type == "grouper") return(sprintf('df <- dplyr::summarise(dplyr::group_by(df, %s), ...)', paste(etape$variables_groupe %||% "", collapse=", ")))
  if (type == "trier") return(sprintf('df <- dplyr::arrange(df, %s)', if (isTRUE(etape$ordre == "decroissant")) paste0("dplyr::desc(`",v,"`)") else paste0("`",v,"`")))
  if (type == "doublons") return('df <- dplyr::distinct(df)')
  if (type == "pivoter_long") return(sprintf('df <- tidyr::pivot_longer(df, cols=c(%s), names_to="%s", values_to="%s")', paste(shQuote(etape$variables %||% ""),collapse=", "), etape$nom_cle%||%"variable", etape$nom_valeur%||%"valeur"))
  if (type == "pivoter_large") return(sprintf('df <- tidyr::pivot_wider(df, names_from="%s", values_from="%s")', etape$variable_cle%||%"", etape$variable_valeur%||%""))
  if (type == "join_custom") return(sprintf('df <- dplyr::left_join(df, table2, by=c("%s"="%s"))', etape$cle1%||%"", etape$cle2%||%""))
  if (type == "append_custom") return('df <- dplyr::bind_rows(df, table2)')
  if (type == "top_n_custom") return(sprintf('df <- head(df, %d)', as.integer(etape$n%||%10L)))
  if (type == "outliers_custom") return(sprintf('# Outliers : `%s` (%s)', v, etape$traitement%||%""))
  sprintf('# Etape : %s', type)
}

# ---------------------------------------------------------------------------
# GÉNÉRATION DU SCRIPT COMPLET
# ---------------------------------------------------------------------------
generer_code <- function(pipeline) {
  if (is.null(pipeline$donnees_brutes)) return("# Aucune donnée chargée")
  entete <- c(
    "# ============================================================",
    "# Script généré par Hygie",
    paste0("# Source : ", pipeline$nom_source %||% "inconnue"),
    paste0("# Date   : ", format(Sys.time(), "%d/%m/%Y %H:%M")),
    "# ============================================================",
    "",
    "library(dplyr)", "library(tidyr)", "library(stringr)", "",
    {
      nom <- pipeline$nom_source %||% "votre_fichier.csv"; ext <- tolower(tools::file_ext(nom))
      switch(ext,
        csv=sprintf('df <- readr::read_csv("%s")',nom), tsv=sprintf('df <- readr::read_tsv("%s")',nom), txt=sprintf('df <- readr::read_delim("%s", delim = "\\t")',nom),
        xlsx=sprintf('df <- readxl::read_excel("%s")',nom), xls=sprintf('df <- readxl::read_excel("%s")',nom), sav=sprintf('df <- haven::read_sav("%s")',nom),
        sas7bdat=sprintf('df <- haven::read_sas("%s")',nom), dta=sprintf('df <- haven::read_dta("%s")',nom), json=sprintf('df <- jsonlite::fromJSON("%s", flatten = TRUE)',nom),
        rds=sprintf('df <- readRDS("%s")',nom), sprintf('df <- readr::read_csv("%s")',nom)
      )
    }, ""
  )
  lignes_etapes <- character(0)
  for (etape in pipeline$etapes) {
    if (isTRUE(etape$type == "import")) next
    if (!isFALSE(etape$active)) {
      lignes_etapes <- c(lignes_etapes, sprintf("# %s", etape$libelle %||% etape$type), etape_vers_code(etape), "")
    }
  }
  paste(c(entete,lignes_etapes),collapse="\n")
}
