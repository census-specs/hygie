# =============================================================================
# PROJETS HYGIE — Reproductibilité et sauvegarde
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------------------------------------------------------
# Utilitaires de génération de code
# -----------------------------------------------------------------------------

.r_literal <- function(x) {
  paste(capture.output(dput(x)), collapse = " ")
}

.r_name <- function(x) {
  paste0("`", gsub("`", "``", as.character(x), fixed = TRUE), "`")
}

.r_string <- function(x) {
  .r_literal(as.character(x))
}

.r_vec_names <- function(x) {
  paste(vapply(x, .r_name, character(1)), collapse = ", ")
}

.r_string_vec <- function(x) {
  paste(vapply(x, .r_string, character(1)), collapse = ", ")
}

# -----------------------------------------------------------------------------
# Génération fiable d'une étape
# -----------------------------------------------------------------------------

#' Générer du code R exécutable pour une étape du pipeline.
#' @export
etape_vers_code <- function(etape) {
  type <- etape$type %||% ""
  v <- etape$variable %||% ""
  vn <- .r_name(v)

  if (type == "import") return(character(0))

  if (type == "renommer") {
    return(sprintf("df <- dplyr::rename(df, %s = %s)",
                   .r_name(etape$nouveau_nom), vn))
  }

  if (type == "type") {
    code <- switch(etape$nouveau_type %||% "texte",
      numerique = sprintf("suppressWarnings(as.numeric(df[[%s]]))", .r_string(v)),
      texte = sprintf("as.character(df[[%s]])", .r_string(v)),
      categorielle = sprintf("as.factor(df[[%s]])", .r_string(v)),
      logique = sprintf("as.logical(df[[%s]])", .r_string(v)),
      date = sprintf("as.Date(df[[%s]], format = %s)", .r_string(v),
                     .r_string(etape$format_date %||% "%Y-%m-%d")),
      sprintf("as.character(df[[%s]])", .r_string(v))
    )
    return(sprintf("df[[%s]] <- %s", .r_string(v), code))
  }

  if (type == "recoder") {
    corr <- etape$correspondance %||% character(0)
    return(sprintf("df[[%s]] <- dplyr::recode(as.character(df[[%s]]), !!!%s)",
                   .r_string(v), .r_string(v), .r_literal(corr)))
  }

  if (type == "manquants") {
    cols <- etape$variables %||% v
    cols <- as.character(cols)
    methode <- etape$methode %||% "supprimer"
    if (methode == "supprimer") {
      return(sprintf("df <- df[stats::complete.cases(df[, c(%s), drop = FALSE]), , drop = FALSE]",
                     .r_string_vec(cols)))
    }
    lignes <- character(0)
    for (col in cols) {
      cc <- .r_string(col)
      expr <- switch(methode,
        valeur_fixe = sprintf("df[[%s]][is.na(df[[%s]])] <- %s", cc, cc, .r_literal(etape$valeur)),
        moyenne = sprintf("df[[%s]][is.na(df[[%s]])] <- mean(df[[%s]], na.rm = TRUE)", cc, cc, cc),
        mediane = sprintf("df[[%s]][is.na(df[[%s]])] <- stats::median(df[[%s]], na.rm = TRUE)", cc, cc, cc),
        mode = sprintf("{ z <- table(df[[%s]]); df[[%s]][is.na(df[[%s]])] <- names(z)[which.max(z)] }", cc, cc, cc),
        sprintf("# Méthode de traitement des manquants non reconnue : %s", .r_string(methode))
      )
      lignes <- c(lignes, expr)
    }
    return(lignes)
  }

  if (type == "filtrer") {
    op <- etape$operateur %||% "="
    val <- etape$valeur
    lhs <- sprintf("df[[%s]]", .r_string(v))
    rhs <- if (op %in% c(">", ">=", "<", "<=")) .r_literal(suppressWarnings(as.numeric(val))) else .r_literal(val)
    return(sprintf("df <- df[%s %s %s, , drop = FALSE]", lhs, op, rhs))
  }

  if (type == "filtrer_expr") {
    return(sprintf("df <- df[which(%s), , drop = FALSE]", etape$expression %||% "TRUE"))
  }

  if (type == "calculee") {
    return(sprintf("df[[%s]] <- with(df, %s)", .r_string(etape$nom %||% "colonne"), etape$formule %||% "NA"))
  }

  if (type == "espaces") {
    expr <- if ((etape$mode %||% "bords") == "internes")
      sprintf("gsub('\\\\s+', ' ', trimws(df[[%s]]))", .r_string(v))
    else sprintf("trimws(df[[%s]])", .r_string(v))
    return(sprintf("df[[%s]] <- %s", .r_string(v), expr))
  }

  if (type == "casse") {
    expr <- switch(etape$mode %||% "minuscules",
      majuscules = sprintf("toupper(df[[%s]])", .r_string(v)),
      premiere = sprintf("tools::toTitleCase(tolower(df[[%s]]))", .r_string(v)),
      sprintf("tolower(df[[%s]])", .r_string(v))
    )
    return(sprintf("df[[%s]] <- %s", .r_string(v), expr))
  }

  if (type == "remplacer_texte") {
    return(sprintf("df[[%s]] <- gsub(%s, %s, df[[%s]], fixed = %s)",
      .r_string(v), .r_string(etape$motif %||% ""), .r_string(etape$remplacement %||% ""),
      .r_string(v), if (isTRUE(etape$regex)) "FALSE" else "TRUE"))
  }

  if (type == "remplacer_valeurs") {
    return(sprintf("df[[%s]][df[[%s]] %%in%% %s] <- %s",
      .r_string(v), .r_string(v), .r_literal(etape$anciennes_valeurs %||% character(0)),
      .r_literal(etape$nouvelle_valeur)))
  }

  if (type == "supprimer_colonnes") {
    cols <- as.character(etape$variables %||% v)
    return(sprintf("df <- df[, setdiff(names(df), c(%s)), drop = FALSE]", .r_string_vec(cols)))
  }

  if (type == "scinder") {
    cols <- etape$noms_nouveaux %||% character(0)
    if ((etape$mode %||% "separateur") == "separateur") {
      if (length(cols) > 0) {
        return(sprintf("df <- tidyr::separate(df, %s, into = %s, sep = %s, remove = TRUE, fill = 'right')",
          vn, .r_literal(cols), .r_string(etape$separateur %||% " ")))
      }
      return(sprintf("df <- tidyr::separate(df, %s, into = c(%s), sep = %s, remove = TRUE, fill = 'right')",
        vn, .r_string_vec(paste0(v, "_", seq_len(2))), .r_string(etape$separateur %||% " ")))
    }
    return(sprintf("df <- tidyr::separate(df, %s, into = %s, sep = %d, remove = TRUE)",
      vn, .r_literal(if (length(cols)) cols else paste0(v, "_", 1:2)), as.integer(etape$position %||% 1L)))
  }

  if (type == "fusionner_colonnes") {
    cols <- as.character(etape$variables %||% character(0))
    refs <- paste(sprintf("df[[%s]]", vapply(cols, .r_string, character(1))), collapse = ", ")
    return(sprintf("df[[%s]] <- paste(%s, sep = %s)", .r_string(etape$nouveau_nom %||% "colonne"), refs, .r_string(etape$separateur %||% " ")))
  }

  if (type == "extraire") {
    out <- etape$nom_sortie %||% paste0(v, "_extrait")
    expr <- switch(etape$mode %||% "debut",
      debut = sprintf("substr(df[[%s]], 1, %d)", .r_string(v), as.integer(etape$n %||% 1L)),
      fin = sprintf("substr(df[[%s]], pmax(1, nchar(df[[%s]]) - %d + 1), nchar(df[[%s]]))", .r_string(v), .r_string(v), as.integer(etape$n %||% 1L), .r_string(v)),
      entre = sprintf("sub(paste0('.*', %s, '(.*)', %s, '.*'), '\\\\1', df[[%s]])", .r_string(etape$avant %||% ""), .r_string(etape$apres %||% ""), .r_string(v)),
      sprintf("as.character(df[[%s]])", .r_string(v))
    )
    return(sprintf("df[[%s]] <- %s", .r_string(out), expr))
  }

  if (type == "conditionnelle") {
    code <- sprintf("df[[%s]] <- %s", .r_string(etape$nom %||% "colonne"), .r_literal(etape$sinon %||% NA))
    for (cond in rev(etape$conditions %||% list())) {
      code <- c(code, sprintf("df[[%s]][which(%s)] <- %s", .r_string(etape$nom %||% "colonne"), cond$test %||% "FALSE", .r_literal(cond$alors)))
    }
    return(code)
  }

  if (type == "arrondir") {
    mode <- etape$mode %||% "arrondir"
    expr <- switch(mode,
      arrondir = sprintf("round(df[[%s]], %d)", .r_string(v), as.integer(etape$decimales %||% 0L)),
      plafonner = sprintf("pmin(df[[%s]], %s)", .r_string(v), .r_literal(etape$valeur)),
      seuiler = sprintf("pmax(df[[%s]], %s)", .r_string(v), .r_literal(etape$valeur)),
      sprintf("df[[%s]]", .r_string(v))
    )
    return(sprintf("df[[%s]] <- %s", .r_string(v), expr))
  }

  if (type == "grouper") {
    groupes <- etape$variables_groupe %||% character(0)
    ref_groupes <- paste(vapply(groupes, .r_name, character(1)), collapse = ", ")
    return(sprintf("df <- df |> dplyr::group_by(dplyr::across(dplyr::all_of(c(%s)))) |> dplyr::summarise(%s = %s(dplyr::across(dplyr::all_of(%s))), .groups = 'drop') |> as.data.frame()",
      .r_string_vec(groupes), .r_name(etape$nom_sortie %||% "resultat"), etape$fonction %||% "mean", .r_string_vec(etape$variable_agregee %||% character(0))))
  }

  if (type == "trier") {
    expr <- if ((etape$ordre %||% "croissant") == "decroissant")
      sprintf("dplyr::desc(df[[%s]])", .r_string(v))
    else sprintf("df[[%s]]", .r_string(v))
    return(sprintf("df <- df[order(%s, na.last = TRUE), , drop = FALSE]", expr))
  }

  if (type == "doublons") {
    cols <- etape$variables %||% character(0)
    if (length(cols) == 0) return("df <- df[!duplicated(df), , drop = FALSE]")
    return(sprintf("df <- df[!duplicated(df[, c(%s), drop = FALSE]), , drop = FALSE]", .r_string_vec(cols)))
  }

  if (type == "pivoter_long") {
    return(sprintf("df <- tidyr::pivot_longer(df, cols = dplyr::all_of(c(%s)), names_to = %s, values_to = %s)",
      .r_string_vec(etape$variables %||% character(0)), .r_string(etape$nom_cle %||% "variable"), .r_string(etape$nom_valeur %||% "valeur")))
  }

  if (type == "pivoter_large") {
    return(sprintf("df <- tidyr::pivot_wider(df, names_from = dplyr::all_of(%s), values_from = dplyr::all_of(%s))",
      .r_string(etape$variable_cle %||% ""), .r_string(etape$variable_valeur %||% "")))
  }

  if (type == "outliers_custom") {
    seuil <- etape$seuil %||% 1.5
    vq <- .r_string(v)
    base <- c(
      sprintf("x <- df[[%s]]", vq),
      sprintf("q1 <- stats::quantile(x, 0.25, na.rm = TRUE)"),
      sprintf("q3 <- stats::quantile(x, 0.75, na.rm = TRUE)"),
      sprintf("iqr <- q3 - q1"),
      sprintf("out <- !is.na(x) & (x < q1 - %s * iqr | x > q3 + %s * iqr)", .r_literal(seuil), .r_literal(seuil))
    )
    traitement <- etape$traitement %||% ""
    action <- switch(traitement,
      remplacer_na = sprintf("df[[%s]][out] <- NA", vq),
      supprimer = "df <- df[!out, , drop = FALSE]",
      winsoriser = sprintf("df[[%s]] <- pmin(pmax(x, q1 - %s * iqr), q3 + %s * iqr)", vq, .r_literal(seuil), .r_literal(seuil)),
      "# Aucune action appliquée aux outliers"
    )
    return(c(base, action))
  }

  if (type == "join_custom") {
    join_fun <- switch(etape$type_join %||% "left", left = "left_join", inner = "inner_join", right = "right_join", full = "full_join")
    return(sprintf("df <- dplyr::%s(df, table2, by = c(%s = %s))", join_fun, .r_string(etape$cle1 %||% ""), .r_string(etape$cle2 %||% "")))
  }

  if (type == "append_custom") return("df <- dplyr::bind_rows(df, table2)")

  if (type == "top_n_custom") {
    n <- as.integer(etape$n %||% 10L)
    return(if ((etape$depuis %||% "debut") == "fin") sprintf("df <- utils::tail(df, %d)", n) else sprintf("df <- utils::head(df, %d)", n))
  }

  if (type == "reorg_cols") {
    cols <- etape$ordre %||% character(0)
    return(sprintf("df <- df[, c(c(%s), setdiff(names(df), c(%s))), drop = FALSE]", .r_string_vec(cols), .r_string_vec(cols)))
  }

  # Une étape inconnue ne doit pas être présentée comme reproductible.
  stop(sprintf("Génération de code non implémentée pour l'étape '%s'.", type))
}

# -----------------------------------------------------------------------------
# Génération du script complet
# -----------------------------------------------------------------------------

#' Générer un script R autonome à partir d'un pipeline Hygie.
#' @export
generer_code <- function(pipeline) {
  if (is.null(pipeline$donnees_brutes)) return("# Aucune donnée chargée dans Hygie.\n")

  nom <- pipeline$nom_source %||% "donnees.csv"
  ext <- tolower(tools::file_ext(nom))
  import <- switch(ext,
    csv = sprintf("df <- readr::read_csv(%s, show_col_types = FALSE)", .r_string(nom)),
    tsv = sprintf("df <- readr::read_tsv(%s, show_col_types = FALSE)", .r_string(nom)),
    txt = sprintf("df <- readr::read_delim(%s, delim = '\\t', show_col_types = FALSE)", .r_string(nom)),
    xlsx = sprintf("df <- readxl::read_excel(%s)", .r_string(nom)),
    xls = sprintf("df <- readxl::read_excel(%s)", .r_string(nom)),
    sav = sprintf("df <- haven::read_sav(%s)", .r_string(nom)),
    sas7bdat = sprintf("df <- haven::read_sas(%s)", .r_string(nom)),
    dta = sprintf("df <- haven::read_dta(%s)", .r_string(nom)),
    json = sprintf("df <- jsonlite::fromJSON(%s, flatten = TRUE)", .r_string(nom)),
    rds = sprintf("df <- readRDS(%s)", .r_string(nom)),
    sprintf("df <- readr::read_csv(%s, show_col_types = FALSE)", .r_string(nom))
  )

  besoins <- unique(c("dplyr", "tidyr", "readr"))
  types <- vapply(pipeline$etapes, function(e) e$type %||% "", character(1))
  if (any(types %in% c("type"))) besoins <- unique(c(besoins, "readxl"))
  if (any(types %in% c("join_custom", "append_custom", "grouper"))) besoins <- unique(c(besoins, "dplyr"))
  if (any(types %in% c("pivoter_long", "pivoter_large", "scinder"))) besoins <- unique(c(besoins, "tidyr"))
  if (ext %in% c("xlsx", "xls")) besoins <- unique(c(besoins, "readxl"))
  if (ext %in% c("sav", "sas7bdat", "dta")) besoins <- unique(c(besoins, "haven"))
  if (ext == "json") besoins <- unique(c(besoins, "jsonlite"))

  lignes <- c(
    "# ============================================================",
    "# Script généré par Hygie",
    paste0("# Source : ", nom),
    paste0("# Généré le : ", format(Sys.time(), "%d/%m/%Y %H:%M")),
    "# ============================================================",
    "",
    paste0("library(", besoins, ")"),
    "",
    import,
    ""
  )

  for (etape in pipeline$etapes) {
    if (isTRUE(etape$type == "import") || isFALSE(etape$active)) next
    libelle <- etape$libelle %||% etape$type
    code <- tryCatch(etape_vers_code(etape), error = function(e) {
      stop(sprintf("Impossible de générer le code pour '%s' : %s", libelle, e$message), call. = FALSE)
    })
    lignes <- c(lignes, paste0("# --- ", libelle, " ---"), code, "")
  }

  lignes <- c(lignes,
    "# Objet final : df",
    "# Pour exporter : readr::write_csv(df, 'donnees_traitees.csv')"
  )
  paste(lignes, collapse = "\n")
}

# -----------------------------------------------------------------------------
# Format de projet .hygie
# -----------------------------------------------------------------------------

#' Sauvegarder un projet Hygie dans un fichier .hygie.
#' Le fichier est autonome : données brutes + pipeline + état courant.
#' @export
sauvegarder_projet <- function(pipeline, donnees_courantes = NULL, chemin) {
  stopifnot(is.list(pipeline), length(chemin) == 1)
  objet <- list(
    format = "hygie-project",
    version = 1L,
    saved_at = Sys.time(),
    pipeline = pipeline,
    donnees_courantes = donnees_courantes
  )
  saveRDS(objet, chemin, version = 3)
  invisible(chemin)
}

#' Charger et valider un projet .hygie.
#' @export
charger_projet <- function(chemin) {
  objet <- readRDS(chemin)
  if (!is.list(objet) || !identical(objet$format, "hygie-project"))
    stop("Ce fichier n'est pas un projet Hygie valide.")
  if (is.null(objet$pipeline) || !is.list(objet$pipeline))
    stop("Le projet Hygie ne contient pas de pipeline valide.")
  objet
}

#' Vérifier qu'un pipeline peut être entièrement traduit en code.
#' @export
verifier_reproductibilite <- function(pipeline) {
  erreurs <- character(0)
  for (i in seq_along(pipeline$etapes)) {
    e <- pipeline$etapes[[i]]
    if (isTRUE(e$type == "import") || isFALSE(e$active)) next
    tryCatch(etape_vers_code(e), error = function(err) {
      erreurs <<- c(erreurs, sprintf("Étape %d (%s) : %s", i, e$type %||% "?", err$message))
    })
  }
  list(ok = length(erreurs) == 0, erreurs = erreurs)
}
