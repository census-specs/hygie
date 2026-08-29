# ============================================================
# MODULE TRANSFORMER v4
# Filtrer, Top N, Sélection colonnes, Trier, Calculée,
# Conditionnelle, Recoder, Recoder plages, Texte, Scinder,
# Fusionner, Extraire, Arrondir, Valeur absolue, Standardiser
# ============================================================

# ── FILTRER ─────────────────────────────────────────────────
modal_filtrer <- function() {
  df <- isolate(rv$donnees_courantes)
  modalDialog(
    title = "Filtrer les lignes",
    size  = "l",
    tags$div(class = "h-alerte h-alerte-info",
      "Syntaxe R : age > 18  |  sexe == 'M'  |  region != 'Douala' & salaire >= 500"
    ),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Expression de filtre"),
      tags$input(id = "filt_expr", type = "text", class = "h-input",
        placeholder = "Ex : age > 18 & statut == 'Actif'",
        oninput = "Shiny.setInputValue('filt_expr', this.value)")
    ),
    uiOutput("filt_apercu"),
    if (!is.null(df)) {
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonnes disponibles"),
        tags$div(class = "h-col-tags",
          lapply(names(df), function(nm) {
            tc <- class(df[[nm]])[1]
            cls <- switch(tc, numeric="num", integer="num", character="texte",
                          factor="fact", Date="date", "")
            tags$span(class = paste("h-col-tag", cls),
              nm, tags$span(style="opacity:.6;", glue::glue(" {tc}")))
          })
        )
      )
    },
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('filt_ok', Math.random(), {priority:'event'})",
          "Appliquer le filtre"
        ))
  )
}

output$filt_apercu <- renderUI({
  req(input$filt_expr, nchar(trimws(input$filt_expr)) > 0)
  df <- rv$donnees_courantes; req(!is.null(df))
  n_tot <- nrow(df)
  n_ok  <- tryCatch({
    masque <- eval(rlang::parse_expr(input$filt_expr), envir = df)
    sum(masque, na.rm = TRUE)
  }, error = function(e) NA)
  if (is.na(n_ok))
    tags$div(class = "h-alerte h-alerte-danger", "Expression invalide — vérifiez la syntaxe.")
  else {
    pct <- round(n_ok / n_tot * 100, 1)
    tags$div(class = "h-alerte h-alerte-succes",
      glue::glue("{n_ok} lignes conservées sur {n_tot} ({pct} %)"))
  }
})

observeEvent(input$filt_ok, {
  req(input$filt_expr); expr <- trimws(input$filt_expr); req(nchar(expr) > 0)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type = "filtrer_expr", expression = expr,
    libelle = glue::glue("Filtre : {expr}")
  ))
  removeModal()
  showNotification("Filtre appliqué.", type = "message")
}, ignoreInit = TRUE)

# ── TOP N ───────────────────────────────────────────────────
modal_top_n <- function() {
  modalDialog(
    title = "Garder les N premières lignes",
    size  = "s",
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nombre de lignes à conserver"),
      tags$input(id = "topn_n", type = "number", class = "h-input",
        value = "10", min = "1",
        oninput = "Shiny.setInputValue('topn_n', parseInt(this.value))")
    ),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Depuis"),
      tags$select(id = "topn_depuis",
        onchange = "Shiny.setInputValue('topn_depuis', this.value)",
        class = "h-select",
        tags$option(value = "debut", "Le début (premières lignes)"),
        tags$option(value = "fin",   "La fin (dernières lignes)")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('topn_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

observeEvent(input$topn_ok, {
  n      <- input$topn_n %||% 10L
  depuis <- input$topn_depuis %||% "debut"
  expr   <- if (depuis == "debut")
    glue::glue("head(., {n})")
  else
    glue::glue("tail(., {n})")
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type     = "top_n_custom",
    n        = n,
    depuis   = depuis,
    libelle  = glue::glue("Garder les {n} {if(depuis=='debut') 'premières' else 'dernières'} lignes")
  ))
  removeModal()
  showNotification(glue::glue("Conservé les {n} lignes ({depuis})."), type = "message")
}, ignoreInit = TRUE)

# ── SÉLECTIONNER COLONNES ───────────────────────────────────
modal_select_cols <- function() {
  cols <- isolate(cols_disp())
  modalDialog(
    title = "Sélectionner des colonnes",
    size  = "m",
    tags$div(class = "h-alerte h-alerte-info",
      "Seules les colonnes cochées seront conservées dans le résultat."),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Colonnes à conserver"),
      checkboxGroupInput("sel_cols", NULL, choices = cols, selected = cols, inline = TRUE)
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('sel_cols_ok', Math.random(), {priority:'event'})",
          "Appliquer la sélection"
        ))
  )
}

observeEvent(input$sel_cols_ok, {
  cols_garder <- input$sel_cols
  req(length(cols_garder) > 0)
  df   <- rv$donnees_courantes; req(!is.null(df))
  cols_suppr <- setdiff(names(df), cols_garder)
  if (length(cols_suppr) == 0) { removeModal(); return() }
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type      = "supprimer_colonnes",
    variables = cols_suppr,
    libelle   = glue::glue("Sélection : {length(cols_garder)} colonne(s) conservée(s)")
  ))
  removeModal()
  showNotification(glue::glue("{length(cols_suppr)} colonne(s) retirée(s)."), type = "message")
}, ignoreInit = TRUE)

# ── TRIER ───────────────────────────────────────────────────
modal_trier <- function() {
  modalDialog(
    title = "Trier les lignes",
    size  = "s",
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne de tri"),
        tags$select(id = "tri_col",
          onchange = "Shiny.setInputValue('tri_col', this.value)",
          class = "h-select",
          lapply(isolate(cols_disp()), function(c) tags$option(value = c, c)))
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Ordre"),
        tags$select(id = "tri_ordre",
          onchange = "Shiny.setInputValue('tri_ordre', this.value)",
          class = "h-select",
          tags$option(value = "croissant",   "Croissant  (A vers Z / 0 vers 9)"),
          tags$option(value = "decroissant", "Décroissant  (Z vers A / 9 vers 0)")
        )
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('tri_ok', Math.random(), {priority:'event'})",
          "Trier"
        ))
  )
}

observeEvent(input$tri_ok, {
  req(input$tri_col)
  col   <- input$tri_col
  ordre <- input$tri_ordre %||% "croissant"
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type = "trier", variable = col, ordre = ordre,
    libelle = glue::glue("Trier : {col} ({ordre})")
  ))
  removeModal()
  showNotification(glue::glue("Trié par '{col}' ({ordre})."), type = "message")
}, ignoreInit = TRUE)

# ── COLONNE CALCULÉE ────────────────────────────────────────
modal_calculee <- function() {
  df <- isolate(rv$donnees_courantes)
  modalDialog(
    title = "Colonne calculée",
    size  = "m",
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Nom de la nouvelle colonne"),
        tags$input(id = "calc_nom", type = "text", class = "h-input",
          placeholder = "Ex : imc, revenu_annuel",
          oninput = "Shiny.setInputValue('calc_nom', this.value)")
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Formule R"),
        tags$input(id = "calc_formule", type = "text", class = "h-input",
          placeholder = "Ex : poids / taille^2",
          oninput = "Shiny.setInputValue('calc_formule', this.value)")
      )
    ),
    tags$div(class = "h-alerte h-alerte-info",
      "Utilisez les noms de colonnes directement. Ex : salaire * 12 / 100  |  paste(prenom, nom)  |  sqrt(surface)"),
    if (!is.null(df)) {
      tags$div(class = "h-col-tags",
        lapply(names(df), function(nm) {
          tc <- class(df[[nm]])[1]
          cls <- switch(tc, numeric="num", integer="num", character="texte", factor="fact", Date="date", "")
          tags$span(class = paste("h-col-tag", cls), nm)
        })
      )
    },
    uiOutput("calc_apercu"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('calc_ok', Math.random(), {priority:'event'})",
          "Créer la colonne"
        ))
  )
}

output$calc_apercu <- renderUI({
  req(input$calc_formule, nchar(trimws(input$calc_formule)) > 0)
  df <- rv$donnees_courantes; req(!is.null(df))
  res <- tryCatch(
    head(eval(parse(text = input$calc_formule), envir = df), 5),
    error = function(e) NULL
  )
  if (is.null(res))
    tags$div(class = "h-alerte h-alerte-danger", "Formule invalide.")
  else {
    preview <- tryCatch(paste(round(as.numeric(res[!is.na(res)]), 4), collapse = "  |  "),
                        error = function(e) paste(as.character(res), collapse = "  |  "))
    tags$div(class = "h-alerte h-alerte-succes",
      glue::glue("Aperçu : {preview}"))
  }
})

observeEvent(input$calc_ok, {
  req(input$calc_nom, input$calc_formule)
  nom <- trimws(input$calc_nom); formule <- trimws(input$calc_formule)
  req(nchar(nom) > 0, nchar(formule) > 0)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type = "calculee", nom = nom, formule = formule,
    libelle = glue::glue("Calculée : {nom} = {formule}")
  ))
  removeModal()
  showNotification(glue::glue("Colonne '{nom}' créée."), type = "message")
}, ignoreInit = TRUE)

# ── VARIABLE CONDITIONNELLE ─────────────────────────────────
modal_conditionnel <- function() {
  modalDialog(
    title = "Variable conditionnelle",
    size  = "l",
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Nom de la nouvelle colonne"),
        tags$input(id = "cond_nom", type = "text", class = "h-input",
          placeholder = "Ex : tranche_age",
          oninput = "Shiny.setInputValue('cond_nom', this.value)")
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Valeur par défaut (si aucune condition)"),
        tags$input(id = "cond_sinon", type = "text", class = "h-input",
          placeholder = "Laisser vide pour NA",
          oninput = "Shiny.setInputValue('cond_sinon', this.value)")
      )
    ),
    tags$hr(style = "margin:10px 0;"),
    tags$div(
      style = "display:grid; grid-template-columns:1fr 1fr; gap:8px; font-size:11.5px; font-weight:700; color:#6B7280; margin-bottom:4px;",
      tags$div("Condition R  (ex : age < 18)"),
      tags$div("Valeur si vraie  (ex : Mineur)")
    ),
    uiOutput("cond_lignes"),
    tags$button(
          class   = "h-btn-ok", style="margin-top:6px; padding:4px 12px; background:#EFF6FF; color:#1A56C4; border:1px solid #B3CFFB; border-radius:3px; cursor:pointer; font-size:12px; font-family:'Noto Sans',sans-serif; font-weight:500;",
          onclick = "Shiny.setInputValue('cond_add', Math.random(), {priority:'event'})",
          "Ajouter une condition"
        ),
    tags$div(class = "h-alerte h-alerte-info", style = "margin-top:10px;",
      "Les conditions sont évaluées dans l'ordre — la première vraie l'emporte."),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('cond_ok', Math.random(), {priority:'event'})",
          "Créer la variable"
        ))
  )
}

rv$cond_nb <- 2

output$cond_lignes <- renderUI({
  n <- rv$cond_nb
  tagList(lapply(seq_len(n), function(i) {
    tags$div(
      style = "display:grid; grid-template-columns:1fr 1fr; gap:8px; margin-bottom:5px;",
      tags$input(id = paste0("cond_t_", i), type = "text", class = "h-input",
        placeholder = glue::glue("Condition {i}"),
        oninput = glue::glue("Shiny.setInputValue('cond_t_{i}', this.value)")),
      tags$input(id = paste0("cond_a_", i), type = "text", class = "h-input",
        placeholder = glue::glue("Valeur {i}"),
        oninput = glue::glue("Shiny.setInputValue('cond_a_{i}', this.value)"))
    )
  }))
})

observeEvent(input$cond_add, { rv$cond_nb <- rv$cond_nb + 1 })

observeEvent(input$cond_ok, {
  req(input$cond_nom); nom <- trimws(input$cond_nom); req(nchar(nom) > 0)
  conditions <- Filter(Negate(is.null), lapply(seq_len(rv$cond_nb), function(i) {
    t <- input[[paste0("cond_t_", i)]]; a <- input[[paste0("cond_a_", i)]]
    if (!is.null(t) && nchar(trimws(t)) > 0) list(test = t, alors = a) else NULL
  }))
  req(length(conditions) > 0)
  sinon <- if (!is.null(input$cond_sinon) && nchar(trimws(input$cond_sinon)) > 0) input$cond_sinon else NA
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type = "conditionnelle", nom = nom, conditions = conditions, sinon = sinon,
    libelle = glue::glue("Conditionnelle : {nom}")
  ))
  rv$cond_nb <- 2; removeModal()
  showNotification(glue::glue("Variable '{nom}' créée."), type = "message")
}, ignoreInit = TRUE)

# ── RECODAGE MODALITÉS ──────────────────────────────────────
modal_recoder <- function() {
  cols_cat <- isolate({
    df <- rv$donnees_courantes
    if (is.null(df)) character(0)
    else names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  })
  modalDialog(
    title = "Recodage des modalités",
    size  = "l",
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Colonne"),
      tags$select(id = "rec_col",
        onchange = "Shiny.setInputValue('rec_col', this.value)",
        class = "h-select",
        lapply(cols_cat, function(c) tags$option(value = c, c)))
    ),
    uiOutput("rec_modalites_ui"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('rec_ok', Math.random(), {priority:'event'})",
          "Appliquer le recodage"
        ))
  )
}

output$rec_modalites_ui <- renderUI({
  req(input$rec_col)
  df <- rv$donnees_courantes; req(!is.null(df), input$rec_col %in% names(df))
  vals <- sort(unique(as.character(df[[input$rec_col]])))
  vals <- vals[!is.na(vals)]
  if (length(vals) > 40) {
    return(tags$div(class = "h-alerte h-alerte-warning",
      glue::glue("{length(vals)} modalités — trop nombreuses pour le recodage manuel. Utilisez 'Recodage par plages' pour les variables numériques.")))
  }
  tagList(
    tags$div(style = "font-size:11.5px; color:#6B7280; margin-bottom:8px;",
      glue::glue("{length(vals)} modalité(s) — entrez les nouvelles valeurs (laisser vide = inchangé) :")),
    tags$div(
      style = "display:grid; grid-template-columns:1fr 20px 1fr; gap:5px 8px; align-items:center;",
      lapply(seq_along(vals), function(i) {
        v <- vals[i]
        list(
          tags$div(style = "background:#F2F4F7; padding:3px 8px; border-radius:3px; font-size:12px; font-weight:500;", v),
          tags$div(style = "text-align:center; color:#9CA3AF; font-size:11px;", "->"),
          tags$input(id = paste0("rec_v_", i), type = "text", class = "h-input",
            style = "padding:3px 7px;",
            placeholder = v,
            oninput = glue::glue("Shiny.setInputValue('rec_v_{i}', this.value)"),
            `data-ancien` = v)
        )
      })
    )
  )
})

observeEvent(input$rec_ok, {
  req(input$rec_col)
  df   <- rv$donnees_courantes; req(!is.null(df))
  col  <- input$rec_col
  vals <- sort(unique(as.character(df[[col]])))
  vals <- vals[!is.na(vals)]
  corresp <- character(0)
  for (i in seq_along(vals)) {
    nouveau <- input[[paste0("rec_v_", i)]]
    if (!is.null(nouveau) && nchar(trimws(nouveau)) > 0 && trimws(nouveau) != vals[i])
      corresp[vals[i]] <- trimws(nouveau)
  }
  if (length(corresp) == 0) {
    showNotification("Aucune modification détectée.", type = "warning"); return()
  }
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type = "recoder", variable = col, correspondance = corresp,
    libelle = glue::glue("Recoder : {col} ({length(corresp)} modalité(s))")
  ))
  removeModal()
  showNotification(glue::glue("{length(corresp)} modalité(s) recodée(s)."), type = "message")
}, ignoreInit = TRUE)

# ── RECODAGE PAR PLAGES ─────────────────────────────────────
modal_recoder_plages <- function() {
  modalDialog(
    title = "Recodage par plages (variable numérique)",
    size  = "l",
    tags$div(class = "h-alerte h-alerte-info",
      "Transforme une variable numérique en catégories. Ex : 0-14 = Enfant, 15-24 = Jeune..."),
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne numérique source"),
        tags$select(id = "plg_col",
          onchange = "Shiny.setInputValue('plg_col', this.value)",
          class = "h-select",
          lapply(isolate(cols_num()), function(c) tags$option(value = c, c)))
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Nom de la nouvelle colonne"),
        tags$input(id = "plg_nom", type = "text", class = "h-input",
          placeholder = "Ex : groupe_age",
          oninput = "Shiny.setInputValue('plg_nom', this.value)")
      )
    ),
    tags$hr(style = "margin:8px 0;"),
    tags$div(
      style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:6px 10px; font-size:11.5px; font-weight:700; color:#6B7280; margin-bottom:4px;",
      tags$div("Borne inférieure (incluse)"),
      tags$div("Borne supérieure (exclue)"),
      tags$div("Label")
    ),
    uiOutput("plg_lignes"),
    tags$button(
          class   = "h-btn-ok", style="margin-top:6px; padding:4px 12px; background:#EFF6FF; color:#1A56C4; border:1px solid #B3CFFB; border-radius:3px; cursor:pointer; font-size:12px; font-family:'Noto Sans',sans-serif; font-weight:500;",
          onclick = "Shiny.setInputValue('plg_add', Math.random(), {priority:'event'})",
          "Ajouter une plage"
        ),
    tags$div(class = "h-alerte h-alerte-info", style = "margin-top:10px;",
      "Valeur par défaut pour les lignes hors plage : NA"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('plg_ok', Math.random(), {priority:'event'})",
          "Créer les plages"
        ))
  )
}

rv$plg_nb <- 3

output$plg_lignes <- renderUI({
  n <- rv$plg_nb
  tagList(lapply(seq_len(n), function(i) {
    tags$div(
      style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:6px 10px; margin-bottom:5px;",
      tags$input(id = paste0("plg_inf_", i), type = "number", class = "h-input",
        style = "padding:4px 7px;", placeholder = "Min",
        oninput = glue::glue("Shiny.setInputValue('plg_inf_{i}', parseFloat(this.value))")),
      tags$input(id = paste0("plg_sup_", i), type = "number", class = "h-input",
        style = "padding:4px 7px;", placeholder = "Max (exclu)",
        oninput = glue::glue("Shiny.setInputValue('plg_sup_{i}', parseFloat(this.value))")),
      tags$input(id = paste0("plg_lab_", i), type = "text", class = "h-input",
        style = "padding:4px 7px;", placeholder = glue::glue("Label {i}"),
        oninput = glue::glue("Shiny.setInputValue('plg_lab_{i}', this.value)"))
    )
  }))
})

observeEvent(input$plg_add, { rv$plg_nb <- rv$plg_nb + 1 })

observeEvent(input$plg_ok, {
  req(input$plg_col, input$plg_nom)
  col <- input$plg_col; nom <- trimws(input$plg_nom); req(nchar(nom) > 0)
  n   <- rv$plg_nb
  plages <- Filter(Negate(is.null), lapply(seq_len(n), function(i) {
    inf <- input[[paste0("plg_inf_", i)]]
    sup <- input[[paste0("plg_sup_", i)]]
    lab <- input[[paste0("plg_lab_", i)]]
    if (!is.null(inf) && !is.null(sup) && !is.null(lab) && nchar(trimws(lab)) > 0)
      list(inf = inf, sup = sup, label = trimws(lab))
    else NULL
  }))
  req(length(plages) > 0)

  # Construire la formule dplyr::case_when
  conditions_cw <- paste(sapply(plages, function(p) {
    glue::glue("{col} >= {p$inf} & {col} < {p$sup} ~ '{p$label}'")
  }), collapse = ",\n    ")
  formule <- glue::glue("dplyr::case_when(\n    {conditions_cw},\n    TRUE ~ NA_character_\n  )")

  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type = "calculee", nom = nom, formule = formule,
    libelle = glue::glue("Plages : {col} -> {nom} ({length(plages)} plage(s))")
  ))
  rv$plg_nb <- 3; removeModal()
  showNotification(glue::glue("Variable '{nom}' créée par plages."), type = "message")
}, ignoreInit = TRUE)

# ── TEXTE ───────────────────────────────────────────────────
modal_texte <- function() {
  cols_txt <- isolate(cols_texte_fct())
  if (length(cols_txt) == 0) cols_txt <- isolate(cols_disp())
  modalDialog(
    title = "Opérations sur le texte",
    size  = "m",
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne texte"),
        tags$select(id = "txt_col", onchange = "Shiny.setInputValue('txt_col', this.value)", class = "h-select",
          lapply(cols_txt, function(c) tags$option(value = c, c)))
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Opération"),
        tags$select(id = "txt_op", onchange = "Shiny.setInputValue('txt_op', this.value)", class = "h-select",
          tags$optgroup(label = "Casse",
            tags$option(value = "casse_maj",  "Tout en MAJUSCULES"),
            tags$option(value = "casse_min",  "Tout en minuscules"),
            tags$option(value = "casse_prem", "Première lettre majuscule")
          ),
          tags$optgroup(label = "Espaces",
            tags$option(value = "esp_bords",    "Supprimer espaces bords (trim)"),
            tags$option(value = "esp_internes", "Supprimer espaces multiples")
          ),
          tags$optgroup(label = "Remplacer",
            tags$option(value = "rem_texte",   "Remplacer du texte"),
            tags$option(value = "rem_valeurs", "Remplacer des valeurs entières")
          ),
          tags$optgroup(label = "Ajouter",
            tags$option(value = "prefixe", "Ajouter un préfixe"),
            tags$option(value = "suffixe", "Ajouter un suffixe")
          ),
          tags$optgroup(label = "Supprimer",
            tags$option(value = "suppr_chiffres",    "Supprimer les chiffres"),
            tags$option(value = "suppr_ponctuation", "Supprimer la ponctuation")
          )
        )
      )
    ),
    uiOutput("txt_options_extra"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('txt_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

output$txt_options_extra <- renderUI({
  req(input$txt_op)
  switch(input$txt_op,
    "rem_texte" = tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Chercher"),
        tags$input(id = "txt_motif", type = "text", class = "h-input",
          placeholder = "Texte à chercher",
          oninput = "Shiny.setInputValue('txt_motif', this.value)")),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Remplacer par"),
        tags$input(id = "txt_remplacement", type = "text", class = "h-input",
          placeholder = "Nouveau texte (vide = supprimer)",
          oninput = "Shiny.setInputValue('txt_remplacement', this.value)"))
    ),
    "rem_valeurs" = tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Valeurs à remplacer (séparées par ;)"),
        tags$input(id = "txt_anc", type = "text", class = "h-input",
          placeholder = "Ex : OUI;Oui;oui",
          oninput = "Shiny.setInputValue('txt_anc', this.value)")),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Nouvelle valeur"),
        tags$input(id = "txt_nouv", type = "text", class = "h-input",
          placeholder = "Ex : Oui",
          oninput = "Shiny.setInputValue('txt_nouv', this.value)"))
    ),
    "prefixe" = tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Préfixe"),
      tags$input(id = "txt_prefixe", type = "text", class = "h-input",
        placeholder = "Ex : REG_",
        oninput = "Shiny.setInputValue('txt_prefixe', this.value)")),
    "suffixe" = tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Suffixe"),
      tags$input(id = "txt_suffixe", type = "text", class = "h-input",
        placeholder = "Ex : _2024",
        oninput = "Shiny.setInputValue('txt_suffixe', this.value)")),
    NULL
  )
})

observeEvent(input$txt_ok, {
  req(input$txt_col, input$txt_op)
  col <- input$txt_col; op <- input$txt_op
  etape <- switch(op,
    "casse_maj"  = list(type="casse", variable=col, mode="majuscules", libelle=glue::glue("MAJUSCULES : {col}")),
    "casse_min"  = list(type="casse", variable=col, mode="minuscules", libelle=glue::glue("minuscules : {col}")),
    "casse_prem" = list(type="casse", variable=col, mode="premiere",   libelle=glue::glue("Titre : {col}")),
    "esp_bords"     = list(type="espaces", variable=col, mode="bords",    libelle=glue::glue("Trim : {col}")),
    "esp_internes"  = list(type="espaces", variable=col, mode="internes", libelle=glue::glue("Espaces multiples : {col}")),
    "rem_texte"  = list(type="remplacer_texte", variable=col,
                        motif=input$txt_motif%||%"", remplacement=input$txt_remplacement%||%"",
                        libelle=glue::glue("Remplacer texte : {col}")),
    "rem_valeurs"= list(type="remplacer_valeurs", variable=col,
                        anciennes_valeurs=trimws(strsplit(input$txt_anc%||%"",";")[[1]]),
                        nouvelle_valeur=input$txt_nouv%||%"",
                        libelle=glue::glue("Remplacer valeurs : {col}")),
    "prefixe"    = list(type="calculee", nom=col,
                        formule=glue::glue('paste0("{input$txt_prefixe%||%""}", {col})'),
                        libelle=glue::glue('Préfixe : {col}')),
    "suffixe"    = list(type="calculee", nom=col,
                        formule=glue::glue('paste0({col}, "{input$txt_suffixe%||%""}")'),
                        libelle=glue::glue('Suffixe : {col}')),
    "suppr_chiffres"    = list(type="remplacer_texte", variable=col, motif="[0-9]", remplacement="", regex=TRUE,
                                libelle=glue::glue("Suppr. chiffres : {col}")),
    "suppr_ponctuation" = list(type="remplacer_texte", variable=col, motif="[[:punct:]]", remplacement="", regex=TRUE,
                                libelle=glue::glue("Suppr. ponctuation : {col}")),
    list(type="espaces", variable=col, mode="bords", libelle="Texte")
  )
  rv$pipeline <- ajouter_etape(rv$pipeline, etape)
  removeModal()
  showNotification(glue::glue("Opération texte appliquée sur '{col}'."), type = "message")
}, ignoreInit = TRUE)

# ── SCINDER ─────────────────────────────────────────────────
modal_scinder <- function() {
  modalDialog(
    title = "Scinder une colonne",
    size  = "m",
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Colonne à scinder"),
      tags$select(id = "sci_col", onchange = "Shiny.setInputValue('sci_col', this.value)", class = "h-select",
        lapply(isolate(cols_texte_fct()), function(c) tags$option(value=c, c)))
    ),
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Séparateur"),
        tags$input(id = "sci_sep", type = "text", class = "h-input",
          value = " ", placeholder = "Ex : espace ; , - /",
          oninput = "Shiny.setInputValue('sci_sep', this.value)")
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Noms nouvelles colonnes (séparés par ;)"),
        tags$input(id = "sci_noms", type = "text", class = "h-input",
          placeholder = "Ex : prenom;nom  (vide = auto)",
          oninput = "Shiny.setInputValue('sci_noms', this.value)")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('sci_ok', Math.random(), {priority:'event'})",
          "Scinder"
        ))
  )
}

observeEvent(input$sci_ok, {
  req(input$sci_col, input$sci_sep)
  col  <- input$sci_col; sep <- input$sci_sep
  noms_raw <- input$sci_noms %||% ""
  noms <- if (nchar(trimws(noms_raw)) > 0) trimws(strsplit(noms_raw, ";")[[1]]) else NULL
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="scinder", variable=col, mode="separateur", separateur=sep, noms_nouveaux=noms,
    libelle=glue::glue('Scinder : {col} par "{sep}"')
  ))
  removeModal()
  showNotification(glue::glue("'{col}' scindée."), type="message")
}, ignoreInit = TRUE)

# ── FUSIONNER ───────────────────────────────────────────────
modal_fusionner <- function() {
  modalDialog(
    title = "Fusionner des colonnes",
    size  = "m",
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Colonnes à fusionner (cochez dans l'ordre souhaité)"),
      checkboxGroupInput("fus_cols", NULL, choices = isolate(cols_disp()), inline = TRUE)
    ),
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Séparateur"),
        tags$input(id = "fus_sep", type = "text", class = "h-input",
          value = " ", placeholder = "Espace, _, -, ...",
          oninput = "Shiny.setInputValue('fus_sep', this.value)")
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Nom de la nouvelle colonne"),
        tags$input(id = "fus_nom", type = "text", class = "h-input",
          placeholder = "Ex : nom_complet",
          oninput = "Shiny.setInputValue('fus_nom', this.value)")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('fus_ok', Math.random(), {priority:'event'})",
          "Fusionner"
        ))
  )
}

observeEvent(input$fus_ok, {
  cols <- input$fus_cols; req(length(cols) >= 2)
  nom  <- trimws(input$fus_nom %||% ""); req(nchar(nom) > 0)
  sep  <- input$fus_sep %||% " "
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="fusionner_colonnes", variables=cols, separateur=sep, nouveau_nom=nom,
    libelle=glue::glue("Fusionner : {paste(cols,collapse=', ')} -> {nom}")
  ))
  removeModal()
  showNotification(glue::glue("Colonnes fusionnées -> '{nom}'."), type="message")
}, ignoreInit = TRUE)

# ── EXTRAIRE ────────────────────────────────────────────────
modal_extraire <- function() {
  modalDialog(
    title = "Extraire des caractères",
    size  = "m",
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne source"),
        tags$select(id = "ext_col", onchange = "Shiny.setInputValue('ext_col', this.value)", class = "h-select",
          lapply(isolate(cols_texte_fct()), function(c) tags$option(value=c, c)))
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Type d'extraction"),
        tags$select(id = "ext_mode", onchange = "Shiny.setInputValue('ext_mode', this.value)", class = "h-select",
          tags$option(value = "debut", "N premiers caractères"),
          tags$option(value = "fin",   "N derniers caractères"),
          tags$option(value = "entre", "Entre deux séquences")
        )
      )
    ),
    uiOutput("ext_param_ui"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('ext_ok', Math.random(), {priority:'event'})",
          "Extraire"
        ))
  )
}

output$ext_param_ui <- renderUI({
  req(input$ext_mode)
  switch(input$ext_mode,
    "debut" = tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nombre de caractères (N)"),
      tags$input(id = "ext_n", type = "number", class = "h-input",
        value = "3", min = "1",
        oninput = "Shiny.setInputValue('ext_n', parseInt(this.value))")
    ),
    "fin" = tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nombre de caractères (N)"),
      tags$input(id = "ext_n", type = "number", class = "h-input",
        value = "3", min = "1",
        oninput = "Shiny.setInputValue('ext_n', parseInt(this.value))")
    ),
    "entre" = tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Séquence avant"),
        tags$input(id = "ext_avant", type = "text", class = "h-input",
          placeholder = "Ex : (",
          oninput = "Shiny.setInputValue('ext_avant', this.value)")),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Séquence après"),
        tags$input(id = "ext_apres", type = "text", class = "h-input",
          placeholder = "Ex : )",
          oninput = "Shiny.setInputValue('ext_apres', this.value)"))
    )
  )
})

observeEvent(input$ext_ok, {
  req(input$ext_col, input$ext_mode)
  col  <- input$ext_col; mode <- input$ext_mode
  etape <- list(type="extraire", variable=col, mode=mode,
                nom_sortie=paste0(col, "_extrait"))
  if (mode %in% c("debut","fin")) {
    etape$n       <- input$ext_n %||% 3L
    etape$libelle <- glue::glue("Extraire {mode} {etape$n} car. : {col}")
  } else {
    etape$avant   <- input$ext_avant %||% ""
    etape$apres   <- input$ext_apres %||% ""
    etape$libelle <- glue::glue("Extraire entre '{etape$avant}' et '{etape$apres}' : {col}")
  }
  rv$pipeline <- ajouter_etape(rv$pipeline, etape)
  removeModal()
  showNotification(glue::glue("Extraction appliquée sur '{col}'."), type="message")
}, ignoreInit = TRUE)

# ── ARRONDIR ────────────────────────────────────────────────
modal_arrondir <- function() {
  modalDialog(
    title = "Arrondir / Ecrêter",
    size  = "m",
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne numérique"),
        tags$select(id = "arr_col", onchange = "Shiny.setInputValue('arr_col', this.value)", class = "h-select",
          lapply(isolate(cols_num()), function(c) tags$option(value=c, c)))
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Opération"),
        tags$select(id = "arr_mode", onchange = "Shiny.setInputValue('arr_mode', this.value)", class = "h-select",
          tags$option(value = "arrondir",  "Arrondir à N décimales"),
          tags$option(value = "plafonner", "Plafonner (valeur maximale)"),
          tags$option(value = "seuiller",  "Seuiller (valeur minimale)")
        )
      )
    ),
    uiOutput("arr_param_ui"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('arr_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

output$arr_param_ui <- renderUI({
  req(input$arr_mode)
  if (input$arr_mode == "arrondir") {
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nombre de décimales"),
      tags$input(id = "arr_dec", type = "number", class = "h-input",
        value = "2", min = "0", max = "10",
        oninput = "Shiny.setInputValue('arr_dec', parseInt(this.value))"))
  } else {
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", if(input$arr_mode=="plafonner") "Valeur maximale" else "Valeur minimale"),
      tags$input(id = "arr_val", type = "number", class = "h-input",
        placeholder = "Entrez la borne...",
        oninput = "Shiny.setInputValue('arr_val', parseFloat(this.value))"))
  }
})

observeEvent(input$arr_ok, {
  req(input$arr_col, input$arr_mode)
  col <- input$arr_col; mode <- input$arr_mode
  etape <- list(type="arrondir", variable=col, mode=mode)
  if (mode == "arrondir") {
    etape$decimales <- input$arr_dec %||% 2L
    etape$libelle   <- glue::glue("Arrondir {col} ({etape$decimales} déc.)")
  } else {
    req(!is.null(input$arr_val))
    etape$valeur  <- input$arr_val
    etape$libelle <- glue::glue("{if(mode=='plafonner') 'Plafonner' else 'Seuiller'} {col} à {etape$valeur}")
  }
  rv$pipeline <- ajouter_etape(rv$pipeline, etape)
  removeModal()
  showNotification(glue::glue("Opération '{mode}' sur '{col}'."), type="message")
}, ignoreInit = TRUE)

# ── VALEUR ABSOLUE ──────────────────────────────────────────
appliquer_valeur_absolue <- function() {
  showModal(modalDialog(
    title = "Valeur absolue",
    size  = "s",
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Colonne numérique"),
      tags$select(id = "abs_col", onchange = "Shiny.setInputValue('abs_col', this.value)", class = "h-select",
        lapply(isolate(cols_num()), function(c) tags$option(value=c, c)))
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('abs_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  ))
}

observeEvent(input$abs_ok, {
  req(input$abs_col); col <- input$abs_col
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="calculee", nom=col, formule=glue::glue("abs({col})"),
    libelle=glue::glue("Valeur absolue : {col}")
  ))
  removeModal()
  showNotification(glue::glue("Valeur absolue appliquée sur '{col}'."), type="message")
}, ignoreInit = TRUE)

# ── STANDARDISER ────────────────────────────────────────────
modal_standardiser <- function() {
  modalDialog(
    title = "Standardiser une variable (Z-score)",
    size  = "s",
    tags$div(class = "h-alerte h-alerte-info", "Z-score = (x - moyenne) / écart-type"),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Colonne numérique"),
      tags$select(id = "std_col", onchange = "Shiny.setInputValue('std_col', this.value)", class = "h-select",
        lapply(isolate(cols_num()), function(c) tags$option(value=c, c)))
    ),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Nom de la colonne résultat"),
      tags$input(id = "std_nom", type = "text", class = "h-input",
        placeholder = "Ex : age_zscore",
        oninput = "Shiny.setInputValue('std_nom', this.value)")
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('std_ok', Math.random(), {priority:'event'})",
          "Standardiser"
        ))
  )
}

observeEvent(input$std_ok, {
  req(input$std_col); col <- input$std_col
  nom <- trimws(input$std_nom %||% "")
  if (nchar(nom) == 0) nom <- paste0(col, "_zscore")
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="calculee", nom=nom,
    formule=glue::glue("({col} - mean({col}, na.rm=TRUE)) / sd({col}, na.rm=TRUE)"),
    libelle=glue::glue("Z-score : {col} -> {nom}")
  ))
  removeModal()
  showNotification(glue::glue("Z-score calculé -> '{nom}'."), type="message")
}, ignoreInit = TRUE)
