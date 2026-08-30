# ============================================================
# RENDU DU BOXPLOT QUALITE — ggplot2
# ============================================================
# Graphique dédié à l'inspection d'une variable numérique.
# Le boxplot et les observations sont calculés à partir des données réelles.
# ============================================================

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

  q <- stats::quantile(
    vals,
    probs = c(.25, .50, .75),
    names = FALSE,
    na.rm = TRUE,
    type = 7
  )

  q1 <- unname(q[1])
  med <- unname(q[2])
  q3 <- unname(q[3])
  iqr <- q3 - q1
  limite_basse <- q1 - 1.5 * iqr
  limite_haute <- q3 + 1.5 * iqr

  out_mask <- vals < limite_basse | vals > limite_haute

  plot_df <- data.frame(
    valeur = vals,
    ligne = idx,
    est_outlier = out_mask,
    groupe = "Distribution",
    stringsAsFactors = FALSE
  )

  out_df <- plot_df[plot_df$est_outlier, , drop = FALSE]
  normal_df <- plot_df[!plot_df$est_outlier, , drop = FALSE]

  # Pour les annotations, un léger décalage vertical évite que plusieurs
  # étiquettes exactement alignées deviennent illisibles.
  if (nrow(out_df) > 0) {
    out_df$y_label <- seq(-0.10, 0.10, length.out = nrow(out_df))
  }

  n_out <- nrow(out_df)
  n_missing <- sum(is.na(original))

  fmt <- function(x) {
    format(x, digits = 6, trim = TRUE, scientific = FALSE)
  }

  # Le graphique montre d'abord la distribution réelle et utilise le boxplot
  # ggplot2 comme référence statistique. Les outliers sont redessinés au-dessus
  # afin d'être toujours visibles et identifiables.
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = valeur, y = groupe)) +
    ggplot2::geom_boxplot(
      orientation = "y",
      outlier.shape = NA,
      width = 0.34,
      fill = "#F8FAFC",
      colour = "#344054",
      linewidth = 0.8,
      fatten = 2
    ) +
    ggplot2::geom_point(
      data = normal_df,
      ggplot2::aes(x = valeur, y = groupe),
      inherit.aes = FALSE,
      position = ggplot2::position_jitter(height = 0.055, seed = 42),
      shape = 16,
      size = 1.7,
      alpha = 0.42,
      colour = "#667085"
    ) +
    ggplot2::geom_point(
      data = out_df,
      ggplot2::aes(x = valeur, y = groupe),
      inherit.aes = FALSE,
      position = ggplot2::position_jitter(height = 0.055, seed = 42),
      shape = 21,
      size = 3.4,
      stroke = 0.8,
      fill = "#D92D20",
      colour = "#991B1B"
    ) +
    ggplot2::geom_vline(
      xintercept = c(limite_basse, limite_haute),
      linetype = "dashed",
      linewidth = 0.45,
      colour = "#B42318"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.06, 0.10))
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      title = paste0("Distribution de « ", variable, " »"),
      subtitle = paste0(
        "n = ", length(vals),
        "  ·  ", n_out, " valeur(s) aberrante(s)",
        if (n_missing > 0) paste0("  ·  ", n_missing, " manquante(s)") else ""
      ),
      x = "Valeur",
      y = NULL,
      caption = paste0(
        "Moustaches = règle 1,5 × IQR   ·   Q1 = ", fmt(q1),
        "   ·   Médiane = ", fmt(med),
        "   ·   Q3 = ", fmt(q3)
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 13, colour = "#1F2937"),
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "#667085"),
      plot.caption = ggplot2::element_text(size = 8.5, colour = "#667085", hjust = 0),
      axis.title.x = ggplot2::element_text(size = 9.5, colour = "#475467"),
      panel.border = ggplot2::element_rect(colour = "#E5E7EB", fill = NA, linewidth = 0.5),
      plot.margin = ggplot2::margin(14, 22, 12, 22)
    )

  p
}, res = 120)
