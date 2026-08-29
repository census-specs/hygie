# ============================================================
# MODULE DATES — Conversion, extraction, durées
# ============================================================

# ── CONVERTIR EN DATE ───────────────────────────────────────
modal_conv_date <- function() {
  cols_txt <- isolate(cols_texte_fct())
  modalDialog(
    title = "Convertir une colonne en date",
    size  = "m",
    tags$div(class="h-grid-2",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Colonne a convertir"),
        tags$select(id="cd_col", onchange="Shiny.setInputValue('cd_col',this.value)", class="h-select",
          lapply(isolate(cols_disp()), function(c) tags$option(value=c, c)))
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Format de la date dans vos données"),
        tags$select(id="cd_fmt", onchange="Shiny.setInputValue('cd_fmt',this.value)", class="h-select",
          tags$option(value="%Y-%m-%d", "AAAA-MM-JJ      ex : 2024-01-15"),
          tags$option(value="%d/%m/%Y", "JJ/MM/AAAA     ex : 15/01/2024"),
          tags$option(value="%d-%m-%Y", "JJ-MM-AAAA     ex : 15-01-2024"),
          tags$option(value="%m/%d/%Y", "MM/JJ/AAAA     ex : 01/15/2024"),
          tags$option(value="%d %B %Y", "JJ Mois AAAA  ex : 15 janvier 2024"),
          tags$option(value="%Y%m%d",   "AAAAMMJJ       ex : 20240115")
        )
      )
    ),
    uiOutput("cd_apercu"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('cd_ok', Math.random(), {priority:'event'})",
          "Convertir"
        ))
  )
}

output$cd_apercu <- renderUI({
  req(input$cd_col, input$cd_fmt)
  df <- rv$donnees_courantes; req(!is.null(df), input$cd_col %in% names(df))
  echantillon <- head(as.character(df[[input$cd_col]]), 5)
  dates_conv  <- suppressWarnings(as.Date(echantillon, format=input$cd_fmt))
  n_ok <- sum(!is.na(dates_conv))
  tags$div(
    tags$div(class = if(n_ok==length(echantillon)) "h-alerte h-alerte-succes" else "h-alerte h-alerte-warning",
      glue::glue("{n_ok}/{length(echantillon)} valeurs converties correctement")),
    tags$div(style="font-size:11.5px; color:#4A5568;",
      "Apercu de conversion :",
      tags$ul(style="margin:4px 0 0 0;",
        lapply(seq_along(echantillon), function(i) {
          tags$li(glue::glue(
            '"{echantillon[i]}" -> {if(is.na(dates_conv[i])) "ERREUR" else as.character(dates_conv[i])}'))
        })
      )
    )
  )
})

observeEvent(input$cd_ok, {
  req(input$cd_col, input$cd_fmt)
  col <- input$cd_col; fmt <- input$cd_fmt
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type        = "type",
    variable    = col,
    nouveau_type= "date",
    format_date = fmt,
    libelle     = glue::glue("Convertir date : {col} ({fmt})")
  ))
  removeModal()
  showNotification(glue::glue("'{col}' converti en date."), type="message")
}, ignoreInit = TRUE)

# ── EXTRAIRE COMPOSANTES DE DATE ────────────────────────────
modal_extraire_date <- function() {
  cols_dt <- isolate({
    df <- rv$donnees_courantes
    if (is.null(df)) character(0)
    else names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))]
  })
  if (length(cols_dt) == 0) cols_dt <- isolate(cols_disp())

  modalDialog(
    title = "Extraire des composantes de date",
    size  = "m",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonne de date"),
      tags$select(id="ed_col", onchange="Shiny.setInputValue('ed_col',this.value)", class="h-select",
        lapply(cols_dt, function(c) tags$option(value=c, c)))
    ),
    tags$div(class="h-label", "Composantes a extraire"),
    checkboxGroupInput("ed_composantes", NULL, inline=TRUE, choices=c(
      "Annee"="annee", "Mois (numero)"="mois_num", "Mois (nom)"="mois_nom",
      "Jour"="jour", "Trimestre"="trimestre", "Semaine"="semaine",
      "Jour de la semaine"="jour_semaine"
    )),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('ed_ok', Math.random(), {priority:'event'})",
          "Extraire les composantes"
        ))
  )
}

observeEvent(input$ed_ok, {
  req(input$ed_col, length(input$ed_composantes) > 0)
  col  <- input$ed_col
  comp <- input$ed_composantes

  for (c in comp) {
    nom_col <- paste0(col, "_", c)
    formule <- switch(c,
      annee        = glue::glue("as.integer(format(as.Date({col}), '%Y'))"),
      mois_num     = glue::glue("as.integer(format(as.Date({col}), '%m'))"),
      mois_nom     = glue::glue("format(as.Date({col}), '%B')"),
      jour         = glue::glue("as.integer(format(as.Date({col}), '%d'))"),
      trimestre    = glue::glue("paste0('T', ceiling(as.integer(format(as.Date({col}), '%m')) / 3))"),
      semaine      = glue::glue("as.integer(format(as.Date({col}), '%V'))"),
      jour_semaine = glue::glue("weekdays(as.Date({col}))")
    )
    rv$pipeline <- ajouter_etape(rv$pipeline, list(
      type    = "calculee",
      nom     = nom_col,
      formule = formule,
      libelle = glue::glue("Extraire {c} de {col} -> {nom_col}")
    ))
  }
  removeModal()
  showNotification(glue::glue("{length(comp)} composante(s) extraite(s) de '{col}'."), type="message")
}, ignoreInit = TRUE)

# ── DUREE ENTRE DEUX DATES ──────────────────────────────────
modal_duree_date <- function() {
  cols_dt <- isolate({
    df <- rv$donnees_courantes
    if (is.null(df)) character(0)
    else names(df)[sapply(df, function(x) inherits(x,"Date")||inherits(x,"POSIXct")||is.character(x))]
  })
  modalDialog(
    title = "Calculer une durée entre deux dates",
    size  = "m",
    tags$div(class="h-grid-2",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Date de début"),
        tags$select(id="dur_debut", onchange="Shiny.setInputValue('dur_debut',this.value)", class="h-select",
          lapply(cols_dt, function(c) tags$option(value=c, c)))
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Date de fin (ou 'aujourd'hui')"),
        tags$select(id="dur_fin", onchange="Shiny.setInputValue('dur_fin',this.value)", class="h-select",
          c(lapply(cols_dt, function(c) tags$option(value=c, c)),
            list(tags$option(value="__aujourd_hui__", "Aujourd'hui (date du jour)")))
        )
      )
    ),
    tags$div(class="h-grid-2",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Unite"),
        tags$select(id="dur_unite", onchange="Shiny.setInputValue('dur_unite',this.value)", class="h-select",
          tags$option(value="jours",  "Jours"),
          tags$option(value="mois",   "Mois (approx. 30.44 jours)"),
          tags$option(value="annees", "Annees (approx. 365.25 jours)")
        )
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Nom de la nouvelle colonne"),
        tags$input(id="dur_nom", type="text", class="h-input",
          placeholder="Ex : age_jours, duree_mois",
          oninput="Shiny.setInputValue('dur_nom',this.value)")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('dur_ok', Math.random(), {priority:'event'})",
          "Calculer la durée"
        ))
  )
}

observeEvent(input$dur_ok, {
  req(input$dur_debut, input$dur_fin, input$dur_unite)
  debut <- input$dur_debut; fin <- input$dur_fin
  unite <- input$dur_unite; nom <- trimws(input$dur_nom %||% "")
  if (nchar(nom) == 0) nom <- paste0("duree_", unite)

  div_par <- switch(unite, jours=1, mois=30.44, annees=365.25)
  fin_expr <- if (fin == "__aujourd_hui__") "Sys.Date()" else paste0("as.Date(", fin, ")")
  formule  <- glue::glue("as.numeric(as.Date({fin_expr}) - as.Date({debut})) / {div_par}")

  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type    = "calculee",
    nom     = nom,
    formule = formule,
    libelle = glue::glue("Durée ({unite}) : {debut} -> {if(fin=='__aujourd_hui__') 'aujourd_hui' else fin} = {nom}")
  ))
  removeModal()
  showNotification(glue::glue("Durée calculée en {unite} -> colonne '{nom}'."), type="message")
}, ignoreInit = TRUE)
