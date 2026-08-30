# ============================================================
# RENDU DU BOXPLOT QUALITE — ggplot2
# ============================================================
# Renderer dédié au panneau d'inspection des valeurs aberrantes.
# Le graphique est construit à partir des données réelles de la variable.
# ============================================================

output$hygie_boxplot_plot <- renderPlot({
  req(requireNamespace("ggplot2", quietly = TRUE))

  variable <- rv$hygie_boxplot_variable
  req(is.character(variable), length(variable) == 1, nzchar(variable))

  df <- hygie_donnees_inspection_base()
  req(is.data.frame(df), variable %in% names(df))

  x <- suppressWarnings(as.numeric(df[[variable]]))
  idx <- which(is.finite(x))
  vals <- x[idx]
  req(length(vals) >= 5)

  q <- stats::quantile(vals, probs = c(.25, .50, .75), names = FALSE, na.rm = TRUE, type = 7)
  q1 <- unname(q[1])
  med <- unname(q[2])
  q3 <- unname(q[3])
  iqr <- q3 - q1
  req(is.finite(iqr), iqr > 0)

  limite_basse <- q1 - 1.5 * iqr
  limite_haute <- q3 + 1.5 * iqr
  out_mask <- vals < limite_basse | vals > limite_haute

  plot_df <- data.frame(
    valeur = vals,
    ligne = idx,
    est_outlier = out_mask,
    stringsAsFactors = FALSE
  )
  out_df <- plot_df[plot_df$est_outlier, , drop = FALSE]

  set.seed(42)
  plot_df$y <- runif(nrow(plot_df), 0.91, 1.09)
  out_df$y <- plot_df$y[plot_df$est_outlier]

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = valeur, y = 1)) +
    ggplot2::geom_boxplot(
      orientation = "y",
      outlier.shape = NA,
      width = 0.38,
      fill = "#EEF2F6",
      colour = "#475467",
      linewidth = 0.7,
      fatten = 1.5
    ) +
    ggplot2::geom_point(
      data = plot_df[!plot_df$est_outlier, , drop = FALSE],
      ggplot2::aes(x = valeur, y = y),
      inherit.aes = FALSE,
      shape = 16,
      size = 1.8,
      alpha = 0.48,
      colour = "#667085"
    ) +
    ggplot2::geom_point(
      data = out_df,
      ggplot2::aes(x = valeur, y = y),
      inherit.aes = FALSE,
      shape = 21,
      size = 3.3,
      stroke = 0.7,
      fill = "#D92D20",
      colour = "#991B1B"
    ) +
    ggplot2::geom_text(
      data = out_df,
      ggplot2::aes(x = valeur, y = y, label = paste0("L", ligne)),
      inherit.aes = FALSE,
      nudge_y = 0.12,
      size = 3.0,
      fontface = "bold",
      colour = "#991B1B",
      check_overlap = FALSE
    ) +
    ggplot2::geom_vline(
      xintercept = c(limite_basse, limite_haute),
      linetype = "dashed",
      linewidth = 0.45,
      colour = "#B42318"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0.62, 1.38),
      breaks = 1,
      labels = NULL,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = 0.06)
    ) +
    ggplot2::labs(
      title = paste0("Distribution de « ", variable, " »"),
      subtitle = paste0(
        "n = ", length(vals),
        "  •  Q1 = ", format(q1, digits = 6, trim = TRUE),
        "  •  Médiane = ", format(med, digits = 6, trim = TRUE),
        "  •  Q3 = ", format(q3, digits = 6, trim = TRUE),
        "  •  ", length(out_df), " valeur(s) aberrante(s)"
      ),
      x = "Valeur",
      y = NULL,
      caption = if (nrow(out_df) > 0)
        "Rouge = observation au-delà de 1,5 × IQR • Lxx = numéro de ligne"
      else
        "Aucune observation au-delà de 1,5 × IQR"
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
      plot.margin = ggplot2::margin(10, 18, 10, 18)
    )

  p
}, res = 120)
