library(shiny)
library(reactable)

# Sécurité pour l'opérateur %||%
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

# ── Fonction d'assainissement pour reactable ──────────────
nettoyer_df_pour_dt <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(df) == 0 && ncol(df) == 0) return(df)
  
  for (col in names(df)) {
    if (inherits(df[[col]], "haven_labelled") || inherits(df[[col]], "labelled")) {
      df[[col]] <- as.vector(df[[col]])
    }
    if (inherits(df[[col]], "POSIXlt")) {
      df[[col]] <- as.POSIXct(df[[col]])
    }
    if (is.list(df[[col]]) && !is.data.frame(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) paste(as.character(x), collapse = ", "))
    }
  }
  return(df)
}

# Charger les fonctions R pures
for (f in c("pipeline.R", "transformations.R")) {
  ch <- file.path("..", "..", "R", f)
  if (file.exists(ch)) source(ch, local = FALSE)
}

server <- function(input, output, session) {
  
  # ── Réactifs globaux ──────────────────────────────────────
  rv <- reactiveValues(
    pipeline          = nouveau_pipeline(),
    donnees_courantes = NULL,
    cond_nb           = 2L,
    plg_nb            = 3L,
    imp_df_temp       = NULL,
    imp_erreur        = NULL,
    af_df_temp        = NULL
  )
  
  # ── Indicateurs pour conditionalPanel ─────────────────────
  output$a_des_donnees <- reactive({
    df <- donnees_affichees()
    !is.null(df) && nrow(df) > 0
  })
  outputOptions(output, "a_des_donnees", suspendWhenHidden = FALSE)
  
  output$imp_a_des_donnees <- reactive({
    !is.null(rv$imp_df_temp) && nrow(rv$imp_df_temp) > 0
  })
  outputOptions(output, "imp_a_des_donnees", suspendWhenHidden = FALSE)
  
  # ── Réactifs colonnes ─────────────────────────────────────
  cols_disp <- reactive({
    df <- rv$donnees_courantes
    if (is.null(df)) character(0) else names(df)
  })
  cols_num <- reactive({
    df <- rv$donnees_courantes
    if (is.null(df)) character(0)
    else names(df)[sapply(df, is.numeric)]
  })
  cols_texte_fct <- reactive({
    df <- rv$donnees_courantes
    if (is.null(df)) character(0)
    else names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  })
  
  # ── Charger les modules ───────────────────────────────────
  source("modules/mod_import.R",      local = TRUE)
  source("modules/mod_nettoyer.R",    local = TRUE)
  source("modules/mod_transformer.R", local = TRUE)
  source("modules/mod_combiner.R",    local = TRUE)
  source("modules/mod_qualite.R",     local = TRUE)
  source("modules/mod_boxplot_qualite.R", local = TRUE)
  source("modules/mod_dates.R",       local = TRUE)
  source("modules/mod_export.R",      local = TRUE)
  
  # ── Rejouer le pipeline ───────────────────────────────────
  observe({
    pl <- rv$pipeline
    if (!is.null(pl$donnees_brutes)) {
      res <- tryCatch(
        rejouer_pipeline(pl),
        error = function(e) {
          showNotification(
            paste0("Erreur pipeline : ", e$message),
            type = "error", duration = 12
          )
          pl$donnees_brutes
        }
      )
      rv$donnees_courantes <- if (is.null(res)) pl$donnees_brutes else res
    } else {
      rv$donnees_courantes <- NULL
    }
  })
  
  # ── Activer/désactiver les menus ──────────────────────────
  observe({
    session$sendCustomMessage("majMenus",
                              list(donnees_chargees = !is.null(rv$donnees_courantes))
    )
  })
  
  # ── Barre titre ───────────────────────────────────────────
  output$titlebar_info <- renderText({
    rv$pipeline$nom_source %||% ""
  })
  
  # ── Barre de statut ───────────────────────────────────────
  output$statusbar_contenu <- renderUI({
    df <- rv$donnees_courantes
    if (is.null(df)) {
      return(tags$span(style = "color:#9CA3AF;",
                       "Aucun fichier chargé  —  Importer > Importer un fichier"))
    }
    n_etapes <- max(0L, length(rv$pipeline$etapes) - 1L)
    tagList(
      tags$span(HTML(glue::glue(
        "<b>{format(nrow(df), big.mark=' ')}</b> lignes  ×  <b>{ncol(df)}</b> colonnes"))),
      tags$span(class = "h-statusbar-sep", "  |  "),
      tags$span(HTML(glue::glue("<b>{n_etapes}</b> transformation(s)"))),
      tags$span(class = "h-statusbar-sep", "  |  "),
      tags$span(rv$pipeline$nom_source %||% "")
    )
  })
  
  # ── Données à afficher ────────────────────────────────────
  donnees_affichees <- reactive({
    id <- input$apercu_etape_id
    if (is.null(id) || id == "final") {
      rv$donnees_courantes
    } else {
      tryCatch(
        apercu_etape(rv$pipeline, id),
        error = function(e) {
          showNotification(
            paste0("Impossible d'afficher cette étape : ", e$message),
            type = "error", duration = 8
          )
          NULL
        }
      )
    }
  })
  
  # ── Sélecteur d'aperçu par étape ─────────────────────────
  output$select_apercu_etape <- renderUI({
    etapes <- rv$pipeline$etapes
    if (length(etapes) == 0) return(NULL)
    choix <- c(
      "Résultat final" = "final",
      setNames(
        sapply(etapes, `[[`, "id"),
        paste0(seq_along(etapes), ". ",
               sapply(etapes, function(e) e$libelle %||% e$type))
      )
    )
    tags$select(
      id = "apercu_etape_id",
      onchange = "Shiny.setInputValue('apercu_etape_id', this.value)",
      style = paste(
        "font-size:11px; padding:2px 5px;",
        "border:1px solid #CDD2DB; border-radius:2px;",
        "font-family:'Noto Sans',sans-serif; max-width:300px;"
      ),
      lapply(names(choix),
             function(nm) tags$option(value = choix[[nm]], nm))
    )
  })
  
  # ── Tableau principal Reactable ──────────────────────────
  output$tableau_donnees <- renderReactable({
    df <- donnees_affichees()
    req(!is.null(df))
    
    df_propre <- nettoyer_df_pour_dt(df)
    req(nrow(df_propre) > 0)
    
    reactable(
      df_propre,
      filterable          = TRUE,
      searchable          = TRUE,
      striped             = TRUE,
      highlight           = TRUE,
      compact             = TRUE,
      bordered            = TRUE,
      resizable           = TRUE,
      defaultPageSize     = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions     = c(10, 25, 50, 100),
      language            = reactableLang(
        searchPlaceholder = "Rechercher...",
        noData            = "Aucune donnée disponible",
        pageNext           = "Suivant",
        pagePrevious      = "Précédent",
        pageInfo           = "{rowStart} à {rowEnd} sur {rows} lignes",
        pageSizeOptions   = "Afficher {rows} lignes"
      )
    )
  })
  
  # ── Liste des étapes ──────────────────────────────────────
  output$liste_etapes <- renderUI({
    etapes <- rv$pipeline$etapes
    if (length(etapes) == 0) {
      return(tags$div(
        style = paste(
          "padding:14px; text-align:center; color:#9CA3AF;",
          "font-size:11.5px; font-style:italic;",
          "font-family:'Noto Sans',sans-serif;"
        ),
        "Les étapes apparaîtront ici."
      ))
    }
    
    couleur_type <- function(type) switch(type,
                                          import              = "#1A56C4",
                                          renommer            = "#0369A1", type = "#0369A1",
                                          supprimer_colonnes  = "#DC2626", recoder = "#7C3AED",
                                          arrondir            = "#0891B2",
                                          filtrer_expr = "#D97706", filtrer = "#D97706",
                                          doublons = "#9333EA", trier = "#0369A1",
                                          espaces = "#0369A1", casse = "#0369A1",
                                          remplacer_texte = "#0369A1", remplacer_valeurs = "#0369A1",
                                          scinder = "#0891B2", fusionner_colonnes = "#0891B2",
                                          calculee = "#16A34A", conditionnelle = "#16A34A",
                                          manquants = "#D97706", outliers_custom = "#DC2626",
                                          grouper = "#1A56C4",
                                          pivoter_long = "#0891B2", pivoter_large = "#0891B2",
                                          join_custom = "#7C3AED", append_custom = "#7C3AED",
                                          top_n_custom = "#D97706", reorg_cols = "#0369A1",
                                          "#6B7280"
    )
    
    tags$div(class = "h-etapes-liste",
             tagList(lapply(seq_along(etapes), function(i) {
               e       <- etapes[[i]]
               libelle <- e$libelle %||% e$type
               couleur <- couleur_type(e$type)
               
               tags$div(class = "h-etape",
                        tags$span(class = "h-etape-num", i),
                        tags$span(
                          style = glue::glue(
                            "display:inline-block; width:3px; height:24px;",
                            "background:{couleur}; border-radius:2px;",
                            "flex-shrink:0;"
                          )
                        ),
                        tags$div(class = "h-etape-info",
                                 tags$div(class = "h-etape-label", title = libelle, libelle),
                                 tags$div(class = "h-etape-type", e$type)
                        ),
                        if (e$type != "import") {
                          tags$button(
                            class   = "h-etape-del",
                            title   = "Supprimer cette étape",
                            onclick = glue::glue("Shiny.setInputValue('etape_a_supprimer', '{e$id}', {{priority:'event'}})" ),
                            "x"
                          )
                        }
               )
             }))
    )
  })
  
  # ── Supprimer une étape ───────────────────────────────────
  observeEvent(input$etape_a_supprimer, {
    id_e <- input$etape_a_supprimer
    req(id_e)
    
    etapes <- rv$pipeline$etapes
    idx <- which(sapply(etapes, function(e) e$id == id_e))
    if (length(idx) > 0) {
      lib <- etapes[[idx]]$libelle %||% etapes[[idx]]$type
      rv$pipeline <- supprimer_etape(rv$pipeline, id_e)
      showNotification(glue::glue("Etape supprimée : {lib}"), type = "message")
    }
  })
  
  # ── Dispatcher des actions de menu ────────────────────────
  observeEvent(input$menu_action, {
    a    <- input$menu_action
    vide <- is.null(rv$donnees_courantes)
    nv   <- function() showNotification(
      "Importez d'abord des données.", type = "warning")
    
    switch(a,
           "importer"         = showModal(modal_import()),
           "actualiser"       = { if (vide) nv() else appliquer_actualiser() },
           "ajouter_fichier"  = showModal(modal_ajouter_fichier()),
           "doublons"         = { if (vide) nv() else showModal(modal_doublons()) },
           "lignes_vides"     = { if (vide) nv() else appliquer_lignes_vides() },
           "supprimer_col"    = { if (vide) nv() else showModal(modal_supprimer_col()) },
           "colonnes_vides"   = { if (vide) nv() else appliquer_colonnes_vides() },
           "reorg_cols"       = { if (vide) nv() else showModal(modal_reorg_cols()) },
           "manquants"        = { if (vide) nv() else showModal(modal_manquants()) },
           "rechercher_remplacer" = { if (vide) nv() else showModal(modal_rechercher_remplacer()) },
           "espaces"          = { if (vide) nv() else showModal(modal_espaces()) },
           "casse"            = { if (vide) nv() else showModal(modal_casse()) },
           "type"             = { if (vide) nv() else showModal(modal_type()) },
           "renommer"         = { if (vide) nv() else showModal(modal_renommer()) },
           "filtrer"          = { if (vide) nv() else showModal(modal_filtrer()) },
           "top_n"            = { if (vide) nv() else showModal(modal_top_n()) },
           "select_cols"      = { if (vide) nv() else showModal(modal_select_cols()) },
           "trier"            = { if (vide) nv() else showModal(modal_trier()) },
           "calculee"         = { if (vide) nv() else showModal(modal_calculee()) },
           "conditionnel"     = { if (vide) nv() else showModal(modal_conditionnel()) },
           "recoder"          = { if (vide) nv() else showModal(modal_recoder()) },
           "recoder_plages"   = { if (vide) nv() else showModal(modal_recoder_plages()) },
           "texte"            = { if (vide) nv() else showModal(modal_texte()) },
           "scinder"          = { if (vide) nv() else showModal(modal_scinder()) },
           "fusionner"        = { if (vide) nv() else showModal(modal_fusionner()) },
           "extraire"         = { if (vide) nv() else showModal(modal_extraire()) },
           "arrondir"         = { if (vide) nv() else showModal(modal_arrondir()) },
           "valeur_absolue"   = { if (vide) nv() else appliquer_valeur_absolue() },
           "standardiser"     = { if (vide) nv() else showModal(modal_standardiser()) },
           "conv_date"        = { if (vide) nv() else showModal(modal_conv_date()) },
           "extraire_date"    = { if (vide) nv() else showModal(modal_extraire_date()) },
           "duree_date"       = { if (vide) nv() else showModal(modal_duree_date()) },
           "join_left"        = { if (vide) nv() else showModal(modal_join("left")) },
           "join_inner"       = { if (vide) nv() else showModal(modal_join("inner")) },
           "join_right"       = { if (vide) nv() else showModal(modal_join("right")) },
           "join_full"        = { if (vide) nv() else showModal(modal_join("full")) },
           "append"           = { if (vide) nv() else showModal(modal_append()) },
           "grouper"          = { if (vide) nv() else showModal(modal_grouper()) },
           "pivoter_long"     = { if (vide) nv() else showModal(modal_pivoter_long()) },
           "pivoter_large"    = { if (vide) nv() else showModal(modal_pivoter_large()) },
           "qc_analyser"      = { if (vide) nv() else modal_qc_analyser() },
           "outliers"         = { if (vide) nv() else showModal(modal_outliers()) },
           "code_r"           = afficher_code_r(),
           "reinitialiser"    = confirmer_reinit(),
           "exporter"         = { if (vide) nv() else showModal(modal_export()) },
           "a_propos"         = showModal(modal_a_propos())
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reinit2, confirmer_reinit(), ignoreInit = TRUE)
  observeEvent(input$btn_code2,   afficher_code_r(),  ignoreInit = TRUE)
  
  confirmer_reinit <- function() {
    showModal(modalDialog(
      title = "Réinitialiser le pipeline",
      tags$p("Toutes les transformations seront supprimées. Les données originales sont conservées."),
      footer = tagList(
        modalButton("Annuler"),
        tags$button(
          class   = "h-btn-ok-danger",
          onclick = "Shiny.setInputValue('reinit_ok', Math.random(), {priority:'event'})",
          "Réinitialiser"
        )
      )
    ))
  }
  
  observeEvent(input$reinit_ok, {
    brutes <- rv$pipeline$donnees_brutes
    nom    <- rv$pipeline$nom_source
    pl     <- nouveau_pipeline()
    pl$donnees_brutes <- brutes
    pl$nom_source     <- nom
    if (!is.null(brutes)) {
      pl <- ajouter_etape(pl, list(
        type    = "import",
        libelle = glue::glue("Import : {nom}")
      ))
    }
    rv$pipeline <- pl
    removeModal()
    showNotification("Pipeline réinitialisé.", type = "message")
  }, ignoreInit = TRUE)
  
  afficher_code_r <- function() {
    code <- generer_code(rv$pipeline)
    showModal(modalDialog(
      title = "Code R généré par Hygie",
      size  = "l",
      tags$pre(class = "h-code-block", code),
      footer = modalButton("Fermer")
    ))
  }
  
  modal_a_propos <- function() {
    modalDialog(
      title = "A propos de Hygie",
      tags$p(tags$b("Hygie"), " — Traitement interactif des données"),
      tags$p("Version : 1.0.0"),
      tags$p("Auteur : Pierre Valdeze MBOM MBOM"),
      footer = modalButton("Fermer")
    )
  }
}