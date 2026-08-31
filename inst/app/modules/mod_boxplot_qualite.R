# ============================================================
# RENDU DU BOXPLOT QUALITE — ggplot2
# ============================================================
# Graphique dédié à l'inspection d'une variable numérique.
# Le boxplot et les observations sont calculés à partir des données réelles.
# ============================================================

# Styles dédiés à l'inspection qualité. Ils restent isolés du reste de Hygie.
insertUI(
  selector = "head",
  where = "beforeEnd",
  ui = tags$style(HTML("\n    .h-boxplot-modal-head { padding: 2px 2px 4px; }\n    .h-boxplot-kicker { font-size: 10px; letter-spacing: .08em; font-weight: 700; color: #667085; margin-bottom: 3px; }\n    .h-boxplot-title { font-size: 19px; line-height: 1.25; font-weight: 700; color: #1A1F2E; }\n    .h-boxplot-subtitle { margin-top: 4px; font-size: 11.5px; color: #667085; }\n    .h-boxplot-intro { display:flex; align-items:flex-start; gap:8px; margin: 10px 0 8px; padding: 9px 11px; background:#F8FAFC; border:1px solid #E5E8EE; border-radius:4px; color:#475467; font-size:11.5px; line-height:1.45; }\n    .h-boxplot-dot { width:7px; height:7px; min-width:7px; margin-top:4px; border-radius:50%; background:#D92D20; }\n    .h-boxplot-note { margin-top:5px; color:#667085; font-size:10.5px; }\n    .h-boxplot-modal-head + .h-boxplot-intro { border-left:3px solid #D92D20; }\n    .h-qualite-header { display:flex; flex-direction:column; gap:4px; min-width:110px; }\n    .h-qualite-header-name { font-weight:600; color:#1A1F2E; line-height:1.2; }\n    .h-qualite-header-badges { display:flex; flex-wrap:wrap; gap:3px; }\n    .h-qualite-badge { box-shadow:none; transition:filter .12s, transform .05s; }\n    .h-qualite-badge:hover { filter:brightness(.96); }\n    .h-qualite-badge:active { transform:translateY(1px); }\n    .h-qualite-outlier { font-weight:700 !important; }\n    .h-btn-ok { padding:6px 13px !important; background:#1A56C4 !important; color:#fff !important; border:1px solid #1A56C4 !important; border-radius:3px !important; cursor:pointer; font-family:'Noto Sans',sans-serif; font-size:12px !important; font-weight:600; }\n    .h-btn-ok:hover { background:#0F3A8C !important; }\n  "))
)

output$hygie_boxplot_plot <- renderPlot({
  req(requireNamespace("ggplot2", quietly = TRUE))

  variable <- rv$hygie_boxplot_variable
  req(is.character(variable), length(variable) == 1, nzchar(variable))

  df <- hygie_donnees_inspection_base()
  req(is.data.frame(df), variable %in% names(df))

  original <- df[[variable]]
  req(is.numeric(original))

  idx <- which(is.finite(original))
  vals <- original[idx]
  req(length(vals) >= 5)

  q <- stats::quantile(vals, probs=c(.25,.50,.75), names=FALSE, na.rm=TRUE, type=7)
  q1 <- unname(q[1]); med <- unname(q[2]); q3 <- unname(q[3])
  iqr <- q3 - q1
  limite_basse <- q1 - 1.5 * iqr
  limite_haute <- q3 + 1.5 * iqr

  out_mask <- if (is.finite(iqr) && iqr > 0) vals < limite_basse | vals > limite_haute else rep(FALSE, length(vals))

  plot_df <- data.frame(
    valeur=vals,
    ligne=idx,
    est_outlier=out_mask,
    groupe="Distribution",
    stringsAsFactors=FALSE
  )
  out_df <- plot_df[plot_df$est_outlier,,drop=FALSE]
  normal_df <- plot_df[!plot_df$est_outlier,,drop=FALSE]

  fmt <- function(x) format(x, digits=6, trim=TRUE, scientific=FALSE)
  n_out <- nrow(out_df)
  n_missing <- sum(is.na(original))

  # Un boxplot ggplot2 authentique : les statistiques sont celles du jeu de
  # données sélectionné, tandis que les outliers sont redessinés pour pouvoir
  # leur associer un numéro de ligne.
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x=valeur, y=groupe)) +
    ggplot2::geom_boxplot(
      orientation="y", outlier.shape=NA, width=.30,
      fill="#F8FAFC", colour="#344054", linewidth=.8, fatten=2
    ) +
    ggplot2::geom_point(
      data=normal_df,
      ggplot2::aes(x=valeur, y=groupe), inherit.aes=FALSE,
      position=ggplot2::position_jitter(height=.045, seed=42),
      shape=16, size=1.65, alpha=.38, colour="#667085"
    ) +
    ggplot2::geom_point(
      data=out_df,
      ggplot2::aes(x=valeur, y=groupe), inherit.aes=FALSE,
      position=ggplot2::position_jitter(height=.045, seed=42),
      shape=21, size=3.7, stroke=.85, fill="#D92D20", colour="#991B1B"
    ) +
    ggplot2::geom_text(
      data=out_df,
      ggplot2::aes(x=valeur, y=groupe, label=paste0("L",ligne)),
      inherit.aes=FALSE, nudge_y=.11, size=3.0,
      fontface="bold", colour="#991B1B", check_overlap=FALSE
    ) +
    ggplot2::geom_vline(
      xintercept=c(limite_basse,limite_haute),
      linetype="dashed", linewidth=.45, colour="#B42318"
    ) +
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(.06,.10))) +
    ggplot2::scale_y_discrete(drop=FALSE) +
    ggplot2::labs(
      title=paste0("Distribution de « ",variable," »"),
      subtitle=paste0(
        "n = ",length(vals),"  ·  ",n_out," valeur(s) aberrante(s)",
        if (n_missing > 0) paste0("  ·  ",n_missing," manquante(s)") else ""
      ),
      x="Valeur", y=NULL,
      caption=paste0(
        "Moustaches = 1,5 × IQR   ·   Q1 = ",fmt(q1),
        "   ·   Médiane = ",fmt(med),"   ·   Q3 = ",fmt(q3)
      )
    ) +
    ggplot2::theme_minimal(base_size=11) +
    ggplot2::theme(
      panel.grid.major.y=ggplot2::element_blank(),
      panel.grid.minor=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank(),
      plot.title=ggplot2::element_text(face="bold",size=13,colour="#1F2937"),
      plot.subtitle=ggplot2::element_text(size=9.5,colour="#667085"),
      plot.caption=ggplot2::element_text(size=8.5,colour="#667085",hjust=0),
      axis.title.x=ggplot2::element_text(size=9.5,colour="#475467"),
      panel.border=ggplot2::element_rect(colour="#E5E7EB",fill=NA,linewidth=.5),
      plot.margin=ggplot2::margin(12,22,10,22)
    )

  p
}, res=120)
