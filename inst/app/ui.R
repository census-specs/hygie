library(shiny)
library(reactable)

# ── Helper pour les éléments du menu avec icônes ─────────────
mi <- function(label, action, necessite_donnees = TRUE, icone = NULL) {
  tags$button(
    class                  = "h-dropdown-item",
    `data-action`          = action,
    `data-necessite-donnees` = if (isTRUE(necessite_donnees)) "true" else NULL,
    if (!is.null(icone)) icon(icone, style = "margin-right: 8px; width: 16px; text-align: center;"),
    label
  )
}

msep <- function() tags$div(class = "h-dropdown-separateur")
msec <- function(l) tags$div(class = "h-dropdown-section", l)

menu <- function(label, ...) {
  tags$div(class = "h-menu-item",
           tags$button(class = "h-menu-trigger", label, tags$span(class = "fleche", HTML("&#9660;"))),
           tags$div(class = "h-dropdown", ...)
  )
}

menu_droite <- function(label, ...) {
  tags$div(class = "h-menu-item",
           tags$button(class = "h-menu-trigger", label, tags$span(class = "fleche", HTML("&#9660;"))),
           tags$div(class = "h-dropdown", style = "right:0; left:auto;", ...)
  )
}

ui <- bootstrapPage(
  title = "Hygie",
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel  = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Noto+Sans:wght@300;400;500;600;700&family=Noto+Sans+Mono:wght@400;500&display=swap"),
    tags$link(rel = "stylesheet", href = "hygie.css"),
    tags$script(src = "hygie.js")
  ),
  
  tags$div(class = "h-app",
           
           # ── Barre de titre ────────────────────────────────────────
           tags$div(class = "h-titlebar",
                    tags$div(class = "h-titlebar-logo", "H"),
                    tags$span(class = "h-titlebar-name", "Hygie  —  Traitement des données"),
                    textOutput("titlebar_info", inline = TRUE) |> tagAppendAttributes(class = "h-titlebar-info")
           ),
           
           # ── Barre de menus avec icônes ─────────────────────────────
           tags$div(class = "h-menubar",
                    
                    menu("Importer",
                         msec("Fichier unique"),
                         mi("Importer un fichier...",                "importer",       FALSE, "file-import"),
                         mi("Actualiser depuis la source",           "actualiser",      TRUE,  "sync-alt"),
                         msep(),
                         msec("Plusieurs fichiers"),
                         mi("Ajouter un fichier (même structure)...", "ajouter_fichier", FALSE, "file-medical")
                    ),
                    
                    menu("Nettoyer",
                         msec("Doublons et lignes"),
                         mi("Supprimer les doublons...",             "doublons",        TRUE, "copy"),
                         mi("Supprimer les lignes vides",            "lignes_vides",    TRUE, "eraser"),
                         msep(),
                         msec("Colonnes"),
                         mi("Supprimer des colonnes...",             "supprimer_col",   TRUE, "trash-alt"),
                         mi("Supprimer les colonnes vides",          "colonnes_vides",  TRUE, "ban"),
                         mi("Réorganiser les colonnes...",           "reorg_cols",      TRUE, "sort"),
                         msep(),
                         msec("Valeurs"),
                         mi("Gérer les valeurs manquantes...",       "manquants",       TRUE, "question-circle"),
                         mi("Rechercher et remplacer...",            "rechercher_remplacer", TRUE, "search"),
                         msep(),
                         msec("Texte"),
                         mi("Supprimer les espaces inutiles...",     "espaces",         TRUE, "broom"),
                         mi("Changer la casse...",                   "casse",           TRUE, "font"),
                         msep(),
                         msec("Types et noms"),
                         mi("Corriger le type d'une variable...",    "type",            TRUE, "wrench"),
                         mi("Renommer une colonne...",               "renommer",        TRUE, "edit")
                    ),
                    
                    menu("Transformer",
                         msec("Filtrer et sélectionner"),
                         mi("Filtrer les lignes...",                 "filtrer",         TRUE, "filter"),
                         mi("Garder les N premières lignes...",      "top_n",           TRUE, "list-ol"),
                         mi("Sélectionner des colonnes...",          "select_cols",     TRUE, "tasks"),
                         msep(),
                         msec("Trier"),
                         mi("Trier les lignes...",                   "trier",           TRUE, "sort-amount-down"),
                         msep(),
                         msec("Nouvelle variable"),
                         mi("Colonne calculée...",                   "calculee",        TRUE, "calculator"),
                         mi("Variable conditionnelle...",            "conditionnel",    TRUE, "code-branch"),
                         mi("Recodage des modalités...",             "recoder",         TRUE, "exchange-alt"),
                         mi("Recodage par plages...",                "recoder_plages",  TRUE, "sliders-h"),
                         msep(),
                         msec("Texte"),
                         mi("Opérations sur le texte...",            "texte",           TRUE, "quote-right"),
                         mi("Scinder une colonne...",                "scinder",         TRUE, "columns"),
                         mi("Fusionner des colonnes...",             "fusionner",       TRUE, "object-group"),
                         mi("Extraire des caractères...",            "extraire",        TRUE, "cut"),
                         msep(),
                         msec("Numérique"),
                         mi("Arrondir / Écréter...",                 "arrondir",        TRUE, "ruler"),
                         mi("Valeur absolue",                        "valeur_absolue",  TRUE, "hashtag"),
                         mi("Standardiser (z-score)...",             "standardiser",    TRUE, "chart-line"),
                         msep(),
                         msec("Dates"),
                         mi("Convertir en date...",                  "conv_date",       TRUE, "calendar-alt"),
                         mi("Extraire année / mois / jour...",       "extraire_date",   TRUE, "calendar-day"),
                         mi("Calculer une durée...",                 "duree_date",      TRUE, "stopwatch")
                    ),
                    
                    menu("Combiner",
                         msec("Fusionner des tables"),
                         mi("Jointure gauche (Left join)...",        "join_left",       TRUE, "link"),
                         mi("Jointure interne (Inner join)...",      "join_inner",      TRUE, "link"),
                         mi("Jointure droite (Right join)...",       "join_right",      TRUE, "link"),
                         mi("Jointure complète (Full join)...",      "join_full",       TRUE, "link"),
                         msep(),
                         msec("Ajouter des tables"),
                         mi("Ajouter des lignes (Append)...",        "append",          TRUE, "layer-group"),
                         msep(),
                         msec("Restructurer"),
                         mi("Grouper et agréger...",                 "grouper",         TRUE, "cubes"),
                         mi("Pivoter : Large vers Long...",          "pivoter_long",    TRUE, "table"),
                         mi("Pivoter : Long vers Large...",          "pivoter_large",   TRUE, "table")
                    ),
                    
                    menu("Controle qualite",
                         mi("Analyser le jeu de données",            "qc_analyser",     TRUE, "chart-bar"),
                         msep(),
                         mi("Détecter et traiter les outliers...",   "outliers",        TRUE, "exclamation-triangle")
                    ),
                    
                    tags$div(style = "flex:1;"),
                    
                    menu_droite("Exporter",
                                mi("Exporter les données...",               "exporter",        TRUE, "file-export")
                    ),
                    
                    menu_droite("Code R",
                                mi("Afficher le code R généré",             "code_r",         FALSE, "code"),
                                msep(),
                                mi("Réinitialiser le pipeline...",          "reinitialiser",   FALSE, "undo")
                    ),
                    
                    menu_droite("?",
                                mi("A propos de Hygie",                     "a_propos",       FALSE, "info-circle")
                    )
           ),
           
           # ── Barre de statut ───────────────────────────────────────
           tags$div(class = "h-statusbar",
                    uiOutput("statusbar_contenu", inline = TRUE)
           ),
           
           # ── Zone principale d'affichage ───────────────────────────
           tags$div(class = "h-main",
                    tags$div(class = "h-zone-data",
                             tags$div(class = "h-zone-data-header",
                                      tags$span("Aperçu des données"),
                                      uiOutput("select_apercu_etape", inline = TRUE)
                             ),
                             tags$div(class = "h-zone-data-body", style = "height: calc(100vh - 140px); overflow: auto; padding: 10px;",
                                      
                                      conditionalPanel(
                                        condition = "output.a_des_donnees == false",
                                        tags$div(class = "h-vide",
                                                 tags$div(style = "font-size:48px; opacity:.15; margin-bottom:8px;", "="),
                                                 tags$div(class = "h-vide-titre", "Aucune donnée chargée"),
                                                 tags$div(class = "h-vide-desc",
                                                          "Utilisez le menu ", tags$b("Importer > Importer un fichier"),
                                                          " pour charger vos données.", tags$br(), tags$br(),
                                                          tags$span(style = "color:#9CA3AF; font-size:11.5px;",
                                                                    "Formats : CSV, Excel, SPSS, SAS, Stata, JSON, RDS")
                                                 )
                                        )
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "output.a_des_donnees == true",
                                        reactableOutput("tableau_donnees")
                                      )
                             )
                    ),
                    
                    # ── Panneau de droite : Étapes ───────────────────
                    tags$div(class = "h-zone-etapes",
                             tags$div(class = "h-etapes-header", "Etapes appliquées"),
                             uiOutput("liste_etapes"),
                             tags$div(class = "h-etapes-footer",
                                      tags$button(
                                        class   = "h-btn-small h-btn-reinit",
                                        onclick = "Shiny.setInputValue('btn_reinit2', Math.random(), {priority:'event'})",
                                        "Réinitialiser"
                                      ),
                                      tags$button(
                                        class   = "h-btn-small h-btn-code",
                                        onclick = "Shiny.setInputValue('btn_code2', Math.random(), {priority:'event'})",
                                        "Code R"
                                      )
                             )
                    )
           )
  )
)