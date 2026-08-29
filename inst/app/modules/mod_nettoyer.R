# ============================================================
# MODULE NETTOYER v4 — Doublons, Vides, Manquants, Espaces,
#                      Casse, Rechercher/Remplacer, Types,
#                      Renommer, Réorganiser colonnes
# ============================================================

# ── DOUBLONS ────────────────────────────────────────────────
modal_doublons <- function() {
  df <- isolate(rv$donnees_courantes)
  n_doublons <- if (!is.null(df)) nrow(df) - nrow(dplyr::distinct(df)) else 0
  modalDialog(
    title = "Supprimer les doublons",
    size  = "m",
    if (n_doublons == 0)
      tags$div(class="h-alerte h-alerte-succes", "Aucun doublon détecté dans le jeu de données actuel.")
    else
      tags$div(class="h-alerte h-alerte-warning",
        glue::glue("{n_doublons} ligne(s) dupliquée(s) détectée(s).")),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonnes a considérer (vide = toutes les colonnes)"),
      checkboxGroupInput("dup_cols", NULL, choices=isolate(cols_disp()), inline=TRUE)
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('dup_ok', Math.random(), {priority:'event'})",
          "Supprimer les doublons"
        ))
  )
}

appliquer_doublons_direct <- function() showModal(modal_doublons())

observeEvent(input$dup_ok, {
  cols <- input$dup_cols
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type      = "doublons",
    variables = if (length(cols) > 0) cols else NULL,
    libelle   = "Supprimer les doublons"
  ))
  removeModal()
  showNotification("Doublons supprimés.", type="message")
}, ignoreInit = TRUE)

# ── LIGNES VIDES ────────────────────────────────────────────
appliquer_lignes_vides <- function() {
  df <- rv$donnees_courantes; req(!is.null(df))
  n_vides <- sum(apply(df, 1, function(r) all(is.na(r))))
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type       = "filtrer_expr",
    expression = "rowSums(!is.na(.)) > 0",
    libelle    = "Supprimer les lignes vides"
  ))
  showNotification(glue::glue("Etape ajoutée : suppression des lignes entièrement vides ({n_vides} ligne(s))."), type="message")
}

# ── COLONNES VIDES ──────────────────────────────────────────
appliquer_colonnes_vides <- function() {
  df <- rv$donnees_courantes; req(!is.null(df))
  cols_vides <- names(df)[sapply(df, function(x) all(is.na(x)))]
  if (length(cols_vides) == 0) {
    showNotification("Aucune colonne entièrement vide.", type="message"); return()
  }
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type      = "supprimer_colonnes",
    variables = cols_vides,
    libelle   = glue::glue("Supprimer colonnes vides : {paste(cols_vides,collapse=', ')}")
  ))
  showNotification(glue::glue("{length(cols_vides)} colonne(s) vide(s) supprimée(s)."), type="message")
}

# ── VALEURS MANQUANTES ──────────────────────────────────────
modal_manquants <- function() {
  df  <- isolate(rv$donnees_courantes)
  res <- if (!is.null(df)) resumer_manquants(df) else NULL

  modalDialog(
    title = "Gérer les valeurs manquantes",
    size  = "l",
    if (!is.null(res)) {
      res_m <- res[res$nb_manquants > 0, ]
      if (nrow(res_m) == 0) {
        tags$div(class="h-alerte h-alerte-succes", "Aucune valeur manquante dans ce jeu de données.")
      } else {
        tagList(
          tags$div(class="h-alerte h-alerte-warning",
            glue::glue("{nrow(res_m)} colonne(s) avec des valeurs manquantes")),
          DT::renderDataTable(DT::datatable(res_m,
            options=list(dom="t", pageLength=20), rownames=FALSE, class="compact"))
        )
      }
    },
    tags$hr(),
    tags$div(class="h-grid-3",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Colonne(s)"),
        tags$select(id="mq_cols", multiple=NA,
          onchange="Shiny.setInputValue('mq_cols', Array.from(this.selectedOptions).map(o=>o.value))",
          class="h-select-multi",
          lapply(if(!is.null(df)) names(df) else character(0),
            function(c) tags$option(value=c, c))
        )
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Méthode"),
        tags$select(id="mq_methode", onchange="Shiny.setInputValue('mq_methode',this.value)", class="h-select",
          tags$option(value="supprimer",   "Supprimer les lignes"),
          tags$option(value="valeur_fixe", "Remplacer par une valeur"),
          tags$option(value="moyenne",     "Remplacer par la moyenne"),
          tags$option(value="mediane",     "Remplacer par la médiane"),
          tags$option(value="mode",        "Remplacer par le mode")
        )
      ),
      tags$div(class="h-form-group", uiOutput("mq_valeur_ui"))
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('mq_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

output$mq_valeur_ui <- renderUI({
  req(input$mq_methode == "valeur_fixe")
  tagList(
    tags$label(class="h-label", "Valeur de remplacement"),
    tags$input(id="mq_valeur", type="text", class="h-input",
      placeholder="Ex : 0 ou Inconnu",
      oninput="Shiny.setInputValue('mq_valeur',this.value)")
  )
})

observeEvent(input$mq_ok, {
  cols <- input$mq_cols; methode <- input$mq_methode %||% "supprimer"
  req(length(cols) > 0)
  valeur <- if (methode=="valeur_fixe") input$mq_valeur else NULL
  labels_m <- c(supprimer="Supprimer lignes", valeur_fixe=glue::glue("-> {valeur}"),
                moyenne="-> Moyenne", mediane="-> Mediane", mode="-> Mode")
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type=    "manquants", variable=cols[1], variables=cols,
    methode= methode, valeur=valeur,
    libelle= glue::glue("Manquants : {paste(cols,collapse=', ')} ({labels_m[methode]})")
  ))
  removeModal()
  showNotification("Valeurs manquantes traitées.", type="message")
}, ignoreInit = TRUE)

# ── RECHERCHER / REMPLACER ───────────────────────────────────
modal_rechercher_remplacer <- function() {
  modalDialog(
    title = "Rechercher et remplacer",
    size  = "m",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonne"),
      tags$select(id="rr_col", onchange="Shiny.setInputValue('rr_col',this.value)", class="h-select",
        c(list(tags$option(value="__toutes__", "-- Toutes les colonnes texte --")),
          lapply(isolate(cols_disp()), function(c) tags$option(value=c, c)))
      )
    ),
    tags$div(class="h-grid-2",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Rechercher"),
        tags$input(id="rr_chercher", type="text", class="h-input",
          placeholder="Texte a rechercher...",
          oninput="Shiny.setInputValue('rr_chercher',this.value)")
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Remplacer par"),
        tags$input(id="rr_remplacer", type="text", class="h-input",
          placeholder="Nouveau texte (vide = supprimer)...",
          oninput="Shiny.setInputValue('rr_remplacer',this.value)")
      )
    ),
    tags$div(class="h-form-group",
      checkboxInput("rr_regex", "Expression régulière (regex)", value=FALSE)
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('rr_ok', Math.random(), {priority:'event'})",
          "Remplacer"
        ))
  )
}

observeEvent(input$rr_ok, {
  req(input$rr_chercher)
  col     <- input$rr_col %||% "__toutes__"
  chercher  <- input$rr_chercher
  remplacer <- input$rr_remplacer %||% ""
  regex   <- isTRUE(input$rr_regex)

  if (col == "__toutes__") {
    df <- rv$donnees_courantes; req(!is.null(df))
    cols_txt <- names(df)[sapply(df, function(x) is.character(x)||is.factor(x))]
    for (c in cols_txt) {
      rv$pipeline <- ajouter_etape(rv$pipeline, list(
        type="remplacer_texte", variable=c,
        motif=chercher, remplacement=remplacer, regex=regex,
        libelle=glue::glue('Remplacer "{chercher}" dans {c}')
      ))
    }
  } else {
    rv$pipeline <- ajouter_etape(rv$pipeline, list(
      type="remplacer_texte", variable=col,
      motif=chercher, remplacement=remplacer, regex=regex,
      libelle=glue::glue('Remplacer "{chercher}" dans {col}')
    ))
  }
  removeModal()
  showNotification("Remplacement appliqué.", type="message")
}, ignoreInit = TRUE)

# ── ESPACES ─────────────────────────────────────────────────
modal_espaces <- function() {
  modalDialog(
    title = "Supprimer les espaces inutiles",
    size  = "s",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonne"),
      tags$select(id="esp_col", onchange="Shiny.setInputValue('esp_col',this.value)", class="h-select",
        c(list(tags$option(value="__toutes__", "-- Toutes les colonnes texte --")),
          lapply(isolate(cols_texte_fct()), function(c) tags$option(value=c, c))))
    ),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Type de nettoyage"),
      tags$select(id="esp_mode", onchange="Shiny.setInputValue('esp_mode',this.value)", class="h-select",
        tags$option(value="bords",    "Espaces en debut et fin (trim)"),
        tags$option(value="internes", "Espaces multiples entre les mots aussi")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('esp_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

observeEvent(input$esp_ok, {
  col  <- input$esp_col %||% "__toutes__"
  mode <- input$esp_mode %||% "bords"
  df   <- rv$donnees_courantes; req(!is.null(df))
  cols_a_traiter <- if (col == "__toutes__")
    names(df)[sapply(df, function(x) is.character(x)||is.factor(x))]
  else col

  for (c in cols_a_traiter) {
    rv$pipeline <- ajouter_etape(rv$pipeline, list(
      type="espaces", variable=c, mode=mode,
      libelle=glue::glue("Espaces ({mode}) : {c}")
    ))
  }
  removeModal()
  showNotification(glue::glue("Espaces nettoyés ({length(cols_a_traiter)} colonne(s))."), type="message")
}, ignoreInit = TRUE)

# ── CASSE ───────────────────────────────────────────────────
modal_casse <- function() {
  modalDialog(
    title = "Changer la casse",
    size  = "s",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonne"),
      tags$select(id="cas_col", onchange="Shiny.setInputValue('cas_col',this.value)", class="h-select",
        lapply(isolate(cols_texte_fct()), function(c) tags$option(value=c, c)))
    ),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Casse"),
      tags$select(id="cas_mode", onchange="Shiny.setInputValue('cas_mode',this.value)", class="h-select",
        tags$option(value="majuscules", "MAJUSCULES"),
        tags$option(value="minuscules", "minuscules"),
        tags$option(value="premiere",   "Première lettre de chaque mot")
      )
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('cas_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

observeEvent(input$cas_ok, {
  req(input$cas_col, input$cas_mode)
  col <- input$cas_col; mode <- input$cas_mode
  labels_m <- c(majuscules="MAJUSCULES", minuscules="minuscules", premiere="Première lettre")
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="casse", variable=col, mode=mode,
    libelle=glue::glue("Casse {labels_m[mode]} : {col}")
  ))
  removeModal()
  showNotification(glue::glue("Casse modifiée pour '{col}'."), type="message")
}, ignoreInit = TRUE)

# ── TYPE ────────────────────────────────────────────────────
modal_type <- function() {
  modalDialog(
    title = "Corriger le type d'une variable",
    size  = "s",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonne"),
      tags$select(id="type_col", onchange="Shiny.setInputValue('type_col',this.value)", class="h-select",
        lapply(isolate(cols_disp()), function(c) tags$option(value=c, c)))
    ),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Nouveau type"),
      tags$select(id="type_nouveau", onchange="Shiny.setInputValue('type_nouveau',this.value)", class="h-select",
        tags$option(value="numerique",    "Numérique (numeric)"),
        tags$option(value="texte",        "Texte (character)"),
        tags$option(value="categorielle", "Facteur (catégorielle)"),
        tags$option(value="logique",      "Logique (TRUE/FALSE)"),
        tags$option(value="date",         "Date")
      )
    ),
    uiOutput("type_fmt_date_ui"),
    uiOutput("type_apercu_ui"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('type_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

output$type_fmt_date_ui <- renderUI({
  req(input$type_nouveau == "date")
  tags$div(class="h-form-group",
    tags$label(class="h-label", "Format de date"),
    tags$select(id="type_fmt", onchange="Shiny.setInputValue('type_fmt',this.value)", class="h-select",
      tags$option(value="%Y-%m-%d", "AAAA-MM-JJ"),
      tags$option(value="%d/%m/%Y", "JJ/MM/AAAA"),
      tags$option(value="%d-%m-%Y", "JJ-MM-AAAA"),
      tags$option(value="%m/%d/%Y", "MM/JJ/AAAA")
    )
  )
})

output$type_apercu_ui <- renderUI({
  req(input$type_col)
  df <- rv$donnees_courantes; req(!is.null(df), input$type_col %in% names(df))
  type_actuel <- class(df[[input$type_col]])[1]
  n_na <- sum(is.na(df[[input$type_col]]))
  tags$div(class="h-alerte h-alerte-info",
    glue::glue("Type actuel : {type_actuel}",
      if (n_na > 0) glue::glue(" — {n_na} valeur(s) manquante(s)") else ""))
})

observeEvent(input$type_ok, {
  req(input$type_col, input$type_nouveau)
  col <- input$type_col; type <- input$type_nouveau
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="type", variable=col, nouveau_type=type,
    format_date=input$type_fmt %||% "%Y-%m-%d",
    libelle=glue::glue("Type : {col} -> {type}")
  ))
  removeModal()
  showNotification(glue::glue("Type de '{col}' changé en {type}."), type="message")
}, ignoreInit = TRUE)

# ── RENOMMER ────────────────────────────────────────────────
modal_renommer <- function() {
  modalDialog(
    title = "Renommer une colonne",
    size  = "s",
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonne a renommer"),
      tags$select(id="ren_col", onchange="Shiny.setInputValue('ren_col',this.value)", class="h-select",
        lapply(isolate(cols_disp()), function(c) tags$option(value=c, c)))
    ),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Nouveau nom"),
      tags$input(id="ren_nouveau", type="text", class="h-input",
        placeholder="Entrez le nouveau nom...",
        oninput="Shiny.setInputValue('ren_nouveau',this.value)")
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('ren_ok', Math.random(), {priority:'event'})",
          "Renommer"
        ))
  )
}

observeEvent(input$ren_ok, {
  req(input$ren_col, input$ren_nouveau)
  ancien <- input$ren_col; nouveau <- trimws(input$ren_nouveau)
  req(nchar(nouveau) > 0)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="renommer", variable=ancien, nouveau_nom=nouveau,
    libelle=glue::glue("Renommer : {ancien} -> {nouveau}")
  ))
  removeModal()
  showNotification(glue::glue("'{ancien}' renommé en '{nouveau}'."), type="message")
}, ignoreInit = TRUE)

# ── SUPPRIMER COLONNES ──────────────────────────────────────
modal_supprimer_col <- function() {
  modalDialog(
    title = "Supprimer des colonnes",
    size  = "m",
    tags$div(class="h-alerte h-alerte-warning",
      "La suppression est ajoutée comme étape — elle peut être annulée depuis le panneau droit."),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Colonnes a supprimer"),
      checkboxGroupInput("sup_cols", NULL, choices=isolate(cols_disp()), inline=TRUE)
    ),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok-danger",
          onclick = "Shiny.setInputValue('sup_cols_ok', Math.random(), {priority:'event'})",
          "Supprimer les colonnes sélectionnées"
        ))
  )
}

observeEvent(input$sup_cols_ok, {
  cols <- input$sup_cols; req(length(cols) > 0)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="supprimer_colonnes", variables=cols,
    libelle=glue::glue("Supprimer : {paste(cols,collapse=', ')}")
  ))
  removeModal()
  showNotification(glue::glue("{length(cols)} colonne(s) supprimée(s)."), type="message")
}, ignoreInit = TRUE)

# ── RÉORGANISER COLONNES ────────────────────────────────────
modal_reorg_cols <- function() {
  cols <- isolate(cols_disp())
  modalDialog(
    title = "Réorganiser les colonnes",
    size  = "m",
    tags$div(class="h-alerte h-alerte-info",
      "Sélectionnez les colonnes dans l'ordre souhaité. Les colonnes non sélectionnées seront placées après."),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Ordre des colonnes"),
      tags$select(id="reorg_sel", multiple=NA,
        size=min(length(cols), 12),
        onchange="Shiny.setInputValue('reorg_sel', Array.from(this.selectedOptions).map(o=>o.value))",
        style="width:100%; border:1px solid #CDD2DB; border-radius:3px; padding:3px; font-family:'Noto Sans',sans-serif; font-size:12.5px;",
        lapply(cols, function(c) tags$option(value=c, c))
      )
    ),
    tags$div(style="font-size:11.5px; color:#6B7280;",
      "Ctrl+Clic (ou Cmd+Clic sur Mac) pour sélectionner plusieurs colonnes."),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok",
          onclick = "Shiny.setInputValue('reorg_ok', Math.random(), {priority:'event'})",
          "Appliquer l'ordre"
        ))
  )
}

observeEvent(input$reorg_ok, {
  cols_sel <- input$reorg_sel; req(length(cols_sel) > 0)
  df <- rv$donnees_courantes; req(!is.null(df))
  autres <- setdiff(names(df), cols_sel)
  nouvel_ordre <- c(cols_sel, autres)
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type    = "calculee_multi",
    type_reel = "reorg_cols",
    ordre   = nouvel_ordre,
    libelle = "Réorganiser les colonnes"
  ))
  removeModal()
  showNotification("Colonnes réorganisées.", type="message")
}, ignoreInit = TRUE)

# ── OUTLIERS ────────────────────────────────────────────────
modal_outliers <- function() {
  modalDialog(
    title = "Détecter et traiter les outliers",
    size  = "l",
    tags$div(class="h-grid-3",
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Colonne numérique"),
        tags$select(id="out_col", onchange="Shiny.setInputValue('out_col',this.value)", class="h-select",
          lapply(isolate(cols_num()), function(c) tags$option(value=c, c)))
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Méthode de détection"),
        tags$select(id="out_mdet", onchange="Shiny.setInputValue('out_mdet',this.value)", class="h-select",
          tags$option(value="iqr",    "IQR — Boite a moustaches"),
          tags$option(value="zscore", "Z-score — Distribution normale")
        )
      ),
      tags$div(class="h-form-group",
        tags$label(class="h-label", "Seuil"),
        tags$input(id="out_seuil", type="number", class="h-input",
          value="1.5", min="0.5", max="10", step="0.5",
          oninput="Shiny.setInputValue('out_seuil',parseFloat(this.value))")
      )
    ),
    tags$div(class="h-form-group",
      tags$label(class="h-label", "Traitement"),
      tags$select(id="out_trait", onchange="Shiny.setInputValue('out_trait',this.value)", class="h-select",
        tags$option(value="remplacer_na", "Remplacer par NA"),
        tags$option(value="supprimer",    "Supprimer les lignes"),
        tags$option(value="winsoriser",   "Winsoriser (écréter aux bornes)")
      )
    ),
    uiOutput("out_apercu"),
    footer = tagList(modalButton("Annuler"),
      tags$button(
          class   = "h-btn-ok-danger",
          onclick = "Shiny.setInputValue('out_ok', Math.random(), {priority:'event'})",
          "Appliquer"
        ))
  )
}

output$out_apercu <- renderUI({
  req(input$out_col, input$out_mdet)
  df <- rv$donnees_courantes; req(!is.null(df), input$out_col %in% names(df))
  seuil <- input$out_seuil %||% 1.5
  masque <- tryCatch(detecter_outliers(df, input$out_col, input$out_mdet, seuil), error=function(e) NULL)
  req(!is.null(masque))
  n_out <- sum(masque, na.rm=TRUE); pct <- round(n_out/nrow(df)*100,1)
  if (n_out == 0)
    tags$div(class="h-alerte h-alerte-succes",
      glue::glue("Aucun outlier détecté dans '{input$out_col}' avec ce seuil."))
  else
    tags$div(class="h-alerte h-alerte-danger",
      glue::glue("{n_out} outlier(s) détecté(s) dans '{input$out_col}' ({pct} % des lignes)"))
})

observeEvent(input$out_ok, {
  req(input$out_col, input$out_mdet, input$out_trait)
  col <- input$out_col; mdet <- input$out_mdet; trait <- input$out_trait
  seuil <- input$out_seuil %||% 1.5
  rv$pipeline <- ajouter_etape(rv$pipeline, list(
    type="outliers_custom", variable=col,
    methode_detect=mdet, traitement=trait, seuil=seuil,
    libelle=glue::glue("Outliers : {col} ({mdet}, {trait})")
  ))
  removeModal()
  showNotification(glue::glue("Outliers de '{col}' traités."), type="message")
}, ignoreInit = TRUE)
