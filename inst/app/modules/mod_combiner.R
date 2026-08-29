# ============================================================
# MODULE COMBINER — Jointures, Append, Grouper, Pivoter
# ============================================================

# ── JOINTURE ────────────────────────────────────────────────
modal_join <- function(type_join = "left") {
  labels <- c(left="Jointure gauche (Left join)", inner="Jointure interne (Inner join)",
               right="Jointure droite (Right join)", full="Jointure complète (Full join)")
  descriptions <- c(
    left  = "Toutes les lignes de la table principale, avec les colonnes de la table secondaire quand la clé correspond.",
    inner = "Seulement les lignes dont la clé existe dans les deux tables.",
    right = "Toutes les lignes de la table secondaire, avec les colonnes de la table principale.",
    full  = "Toutes les lignes des deux tables, valeurs manquantes là où il n'y a pas de correspondance."
  )
  modalDialog(
    title = labels[type_join],
    size  = "l",
    tags$div(class = "h-alerte h-alerte-info", descriptions[type_join]),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Fichier de la table secondaire"),
      fileInput("join_fichier2", NULL,
        accept = c(".csv",".tsv",".xlsx",".xls",".sav",".dta",".rds"),
        buttonLabel = "Parcourir...",
        placeholder = "Aucun fichier sélectionné"
      )
    ),
    uiOutput("join_apercu_t2"),
    tags$div(class = "h-grid-2",
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne clé — Table principale"),
        tags$select(id = "join_cle1",
          onchange = "Shiny.setInputValue('join_cle1', this.value)",
          class = "h-select",
          lapply(isolate(cols_disp()), function(c) tags$option(value=c, c))
        )
      ),
      tags$div(class = "h-form-group",
        tags$label(class = "h-label", "Colonne clé — Table secondaire"),
        uiOutput("join_cle2_ui")
      )
    ),
    uiOutput("join_preview_result"),
    tags$input(type="hidden", id="join_type", value=type_join),
    footer = tagList(modalButton("Annuler"),
      tags$button(
        class   = "h-btn-ok",
        onclick = "Shiny.setInputValue('join_ok', Math.random(), {priority:'event'})",
        paste("Appliquer la", labels[type_join])
      ))
  )
}

# Table secondaire chargée en mémoire temporaire
join_table2 <- reactive({
  req(input$join_fichier2)
  tryCatch(
    importer_fichier(input$join_fichier2$datapath,
      options = list(separateur = ",")),
    error = function(e) NULL
  )
})

output$join_apercu_t2 <- renderUI({
  req(input$join_fichier2)
  df2 <- join_table2()
  if (is.null(df2)) return(tags$div(class="h-alerte h-alerte-danger", "Impossible de lire ce fichier."))
  tags$div(class = "h-alerte h-alerte-succes",
    glue::glue("Table secondaire chargée : {nrow(df2)} lignes x {ncol(df2)} colonnes"))
})

output$join_cle2_ui <- renderUI({
  df2 <- join_table2()
  if (is.null(df2)) return(tags$div(class="h-alerte h-alerte-warning", "Chargez d'abord la table secondaire."))
  tags$select(id = "join_cle2",
    onchange = "Shiny.setInputValue('join_cle2', this.value)",
    class = "h-select",
    lapply(names(df2), function(c) tags$option(value=c, c))
  )
})

output$join_preview_result <- renderUI({
  df1 <- rv$donnees_courantes; df2 <- join_table2()
  req(!is.null(df1), !is.null(df2), input$join_cle1, input$join_cle2)
  type_join <- input$join_type %||% "left"
  cle1 <- input$join_cle1; cle2 <- input$join_cle2
  n_result <- tryCatch({
    fn <- switch(type_join, left=dplyr::left_join, inner=dplyr::inner_join,
                 right=dplyr::right_join, full=dplyr::full_join)
    res <- fn(df1, df2, by=setNames(cle2, cle1))
    nrow(res)
  }, error=function(e) NA)
  if (is.na(n_result))
    tags$div(class="h-alerte h-alerte-danger", "Erreur — vérifiez les colonnes clés.")
  else
    tags$div(class="h-alerte h-alerte-succes",
      glue::glue("Résultat attendu : {n_result} lignes x {ncol(df1) + ncol(df2) - 1} colonnes"))
})

observeEvent(input$join_ok, {
  df2 <- join_table2()
  req(!is.null(df2), input$join_cle1, input$join_cle2)
  type_join <- input$join_type %||% "left"
  cle1 <- input$join_cle1; cle2 <- input$join_cle2
  nom2 <- input$join_fichier2$name

  labels_fn <- c(left="Jointure gauche", inner="Jointure interne",
                  right="Jointure droite", full="Jointure complète")

  df2_capture <- as.data.frame(df2)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type     = "join_custom",
    type_join= type_join,
    table2   = df2_capture,
    cle1     = cle1,
    cle2     = cle2,
    libelle  = glue::glue("{labels_fn[type_join]} avec {nom2} (clé : {cle1})")
  ))
  removeModal()
  showNotification(glue::glue("Jointure appliquée."), type = "message")
}, ignoreInit = TRUE)

# ── APPEND ──────────────────────────────────────────────────
modal_append <- function() {
  modalDialog(
    title = "Ajouter des lignes (Append)",
    size  = "m",
    tags$div(class = "h-alerte h-alerte-info",
      "Charge un fichier ayant la même structure que le tableau actuel et ajoute ses lignes en dessous."),
    tags$div(class = "h-form-group",
      tags$label(class = "h-label", "Fichier à ajouter"),
      fileInput("app_fichier", NULL,
        accept = c(".csv",".tsv",".xlsx",".xls",".sav",".dta",".rds"),
        buttonLabel = "Parcourir...", placeholder = "Aucun fichier sélectionné"
      )
    ),
    uiOutput("app_apercu"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('app_ok', Math.random(), {priority:'event'})",
          "Ajouter les lignes"
        ))
  )
}

app_table <- reactive({
  req(input$app_fichier)
  tryCatch(importer_fichier(input$app_fichier$datapath), error=function(e) NULL)
})

output$app_apercu <- renderUI({
  req(input$app_fichier); df2 <- app_table()
  if (is.null(df2)) return(tags$div(class="h-alerte h-alerte-danger", "Impossible de lire ce fichier."))
  df1 <- rv$donnees_courantes
  cols_comm <- intersect(names(df1), names(df2))
  cols_manq  <- setdiff(names(df1), names(df2))
  tagList(
    tags$div(class="h-alerte h-alerte-succes",
      glue::glue("{nrow(df2)} lignes à ajouter | {length(cols_comm)} colonne(s) en commun")),
    if (length(cols_manq) > 0)
      tags$div(class="h-alerte h-alerte-warning",
        glue::glue("Colonnes manquantes dans le fichier ajouté (seront NA) : {paste(cols_manq, collapse=', ')}"))
  )
})

observeEvent(input$app_ok, {
  df2 <- app_table(); req(!is.null(df2))
  nom2 <- input$app_fichier$name
  df2_capture <- as.data.frame(df2)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type    = "append_custom",
    table2  = df2_capture,
    libelle = glue::glue("Append : {nom2}")
  ))
  removeModal()
  showNotification(glue::glue("{nrow(df2)} lignes ajoutées depuis {nom2}."), type="message")
}, ignoreInit = TRUE)

# ── GROUPER ─────────────────────────────────────────────────
modal_grouper <- function() {
  modalDialog(
    title = "Grouper et agréger",
    size  = "l",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonnes de regroupement"),
      checkboxGroupInput("grp_cols", NULL,
        choices = isolate(cols_disp()), inline = TRUE)
    ),
    tags$div(class="h-grid-3",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Variable à agréger"),
        tags$select(id="grp_var", onchange="Shiny.setInputValue('grp_var',this.value)", class="h-select",
          lapply(isolate(cols_num()), function(c) tags$option(value=c, c)))
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Fonction"),
        tags$select(id="grp_fn", onchange="Shiny.setInputValue('grp_fn',this.value)", class="h-select",
          tags$option(value="mean",   "Moyenne"),
          tags$option(value="sum",    "Somme"),
          tags$option(value="median", "Médiane"),
          tags$option(value="sd",     "Ecart-type"),
          tags$option(value="min",    "Minimum"),
          tags$option(value="max",    "Maximum"),
          tags$option(value="length", "Effectif (n)"),
          tags$option(value="n_distinct", "Nb. valeurs distinctes")
        )
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Nom du résultat"),
        tags$input(id="grp_nom", type="text", class="h-input",
          placeholder="Ex : total_ventes",
          oninput="Shiny.setInputValue('grp_nom',this.value)")
      )
    ),
    tags$div(class="h-alerte h-alerte-warning",
      "Cette étape remplace le tableau par un résumé agrégé."),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('grp_ok', Math.random(), {priority:'event'})",
          "Grouper et agréger"
        ))
  )
}

observeEvent(input$grp_ok, {
  cols_grp <- input$grp_cols; req(length(cols_grp) > 0, input$grp_var, input$grp_fn)
  nom <- trimws(input$grp_nom %||% "")
  if (nchar(nom) == 0) nom <- paste0(input$grp_var, "_", input$grp_fn)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type             = "grouper",
    variables_groupe = cols_grp,
    variable_agregee = input$grp_var,
    fonction         = input$grp_fn,
    nom_sortie       = nom,
    libelle          = glue::glue("Grouper par {paste(cols_grp,collapse=', ')} -> {nom}")
  ))
  removeModal()
  showNotification("Données agrégées.", type="message")
}, ignoreInit = TRUE)

# ── PIVOTER LONG ────────────────────────────────────────────
modal_pivoter_long <- function() {
  modalDialog(
    title = "Pivoter : Large vers Long",
    size  = "m",
    tags$div(class="h-alerte h-alerte-info",
      "Transforme plusieurs colonnes en deux colonnes : une pour le nom de la variable, une pour sa valeur."),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonnes a pivoter (leurs valeurs deviennent des lignes)"),
      checkboxGroupInput("pvl_cols", NULL, choices=isolate(cols_disp()), inline=TRUE)
    ),
    tags$div(class="h-grid-2",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Nom de la colonne des variables"),
        tags$input(id="pvl_cle", type="text", class="h-input", value="variable",
          oninput="Shiny.setInputValue('pvl_cle',this.value)")
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Nom de la colonne des valeurs"),
        tags$input(id="pvl_val", type="text", class="h-input", value="valeur",
          oninput="Shiny.setInputValue('pvl_val',this.value)")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('pvl_ok', Math.random(), {priority:'event'})",
          "Pivoter"
        ))
  )
}

observeEvent(input$pvl_ok, {
  cols <- input$pvl_cols; req(length(cols) >= 2)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type      = "pivoter_long",
    variables = cols,
    nom_cle   = input$pvl_cle %||% "variable",
    nom_valeur= input$pvl_val %||% "valeur",
    libelle   = glue::glue("Pivoter long ({length(cols)} colonnes)")
  ))
  removeModal()
  showNotification("Tableau pivoté en format long.", type="message")
}, ignoreInit = TRUE)

# ── PIVOTER LARGE ───────────────────────────────────────────
modal_pivoter_large <- function() {
  modalDialog(
    title = "Pivoter : Long vers Large",
    size  = "m",
    tags$div(class="h-alerte h-alerte-info",
      "Transforme une colonne de noms en plusieurs colonnes (opération inverse du pivot long)."),
    tags$div(class="h-grid-2",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Colonne des noms (sera transformée en colonnes)"),
        tags$select(id="pvla_cle", onchange="Shiny.setInputValue('pvla_cle',this.value)", class="h-select",
          lapply(isolate(cols_disp()), function(c) tags$option(value=c, c)))
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Colonne des valeurs"),
        tags$select(id="pvla_val", onchange="Shiny.setInputValue('pvla_val',this.value)", class="h-select",
          lapply(isolate(cols_disp()), function(c) tags$option(value=c, c)))
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('pvla_ok', Math.random(), {priority:'event'})",
          "Pivoter"
        ))
  )
}

observeEvent(input$pvla_ok, {
  req(input$pvla_cle, input$pvla_val)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type          = "pivoter_large",
    variable_cle  = input$pvla_cle,
    variable_valeur= input$pvla_val,
    libelle       = glue::glue("Pivoter large ({input$pvla_cle} -> colonnes)")
  ))
  removeModal()
  showNotification("Tableau pivoté en format large.", type="message")
}, ignoreInit = TRUE)
