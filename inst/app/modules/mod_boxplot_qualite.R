# ============================================================
# RENDU DU BOXPLOT QUALITE — VERSION DATA-DRIVEN
# ============================================================
# Charge apres mod_qualite.R et remplace son renderer.
# Le graphique est dessine explicitement a partir des donnees.
# ============================================================

output$hygie_boxplot_plot <- renderPlot({
  variable <- rv$hygie_boxplot_variable
  req(is.character(variable), length(variable) == 1, nzchar(variable))

  df <- hygie_donnees_inspection_base()
  req(is.data.frame(df), variable %in% names(df))

  x <- suppressWarnings(as.numeric(df[[variable]]))
  idx <- which(is.finite(x))
  vals <- x[idx]
  req(length(vals) >= 5)

  q <- quantile(vals, probs = c(.25, .5, .75), names = FALSE, na.rm = TRUE, type = 7)
  q1 <- unname(q[1])
  med <- unname(q[2])
  q3 <- unname(q[3])
  iqr <- q3 - q1
  req(is.finite(iqr), iqr > 0)

  limite_basse <- q1 - 1.5 * iqr
  limite_haute <- q3 + 1.5 * iqr

  # Moustaches: dernier point reel dans les limites de Tukey.
  normales <- vals[vals >= limite_basse & vals <= limite_haute]
  moustache_basse <- min(normales, na.rm = TRUE)
  moustache_haute <- max(normales, na.rm = TRUE)

  out_mask <- vals < limite_basse | vals > limite_haute
  idx_out <- idx[out_mask]
  vals_out <- vals[out_mask]

  etendue <- range(vals, na.rm = TRUE, finite = TRUE)
  amplitude <- diff(etendue)
  marge <- if (is.finite(amplitude) && amplitude > 0) amplitude * .08 else max(abs(etendue), 1) * .08
  if (!is.finite(marge) || marge <= 0) marge <- 1
  xlim <- c(etendue[1] - marge, etendue[2] + marge)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(4.7, 1.5, 4.6, 1.5), mgp = c(2.2, .7, 0), xpd = NA)

  # Aucun appel a boxplot(): la geometrie est dessinee explicitement.
  # Cela evite tout comportement generique ou recouvrement de couches.
  plot(
    NA, NA,
    xlim = xlim, ylim = c(.25, 1.75),
    xaxt = "n", yaxt = "n", bty = "n",
    xlab = "Valeur", ylab = "",
    main = paste0("Distribution de « ", variable, " »")
  )

  ticks <- pretty(xlim, n = 7)
  ticks <- ticks[ticks >= xlim[1] & ticks <= xlim[2]]
  axis(1, at = ticks, labels = format(ticks, trim = TRUE, scientific = FALSE))
  abline(v = ticks, col = "#EEF0F3", lty = 1)

  # Moustache et agrafes.
  segments(moustache_basse, 1, q1, 1, lwd = 2, col = "#667085")
  segments(q3, 1, moustache_haute, 1, lwd = 2, col = "#667085")
  segments(moustache_basse, .78, moustache_basse, 1.22, lwd = 2, col = "#667085")
  segments(moustache_haute, .78, moustache_haute, 1.22, lwd = 2, col = "#667085")

  # Boite Q1-Q3.
  rect(q1, .68, q3, 1.32, col = "#E8EEF7", border = "#475467", lwd = 2)

  # Mediane.
  segments(med, .68, med, 1.32, lwd = 3.5, col = "#1A56C4")

  # Limites theoriques IQR.
  abline(v = c(limite_basse, limite_haute), lty = 3, lwd = 1.2, col = "#B42318")

  # Observations reelles sous la boite.
  set.seed(1234)
  y_obs <- .48 + runif(length(vals), -.045, .045)
  points(vals, y_obs, pch = 16, cex = .52, col = "#98A2B3")

  # Outliers reelles: gros points rouges + lignes d'origine.
  if (length(vals_out) > 0) {
    y_out <- .47 + seq(-.12, .12, length.out = length(vals_out))
    points(vals_out, y_out, pch = 21, cex = 1.45,
           bg = "#D92D20", col = "#991B1B", lwd = 1.2)
    text(vals_out, y_out, labels = paste0("L", idx_out),
         pos = ifelse(seq_along(vals_out) %% 2 == 0, 1, 3),
         offset = .45, cex = .72, font = 2, col = "#991B1B")
  }

  # Legende courte, placee sous le graphique pour ne pas recouvrir la boite.
  legend(
    "bottom", inset = c(0, -.18), horiz = TRUE, xpd = TRUE,
    bty = "n",
    legend = c("Boite Q1-Q3", "Mediane", "Observations", "Outliers", "Limites 1,5 × IQR"),
    pch = c(22, NA, 16, 21, NA),
    pt.bg = c("#E8EEF7", NA, "#98A2B3", "#D92D20", NA),
    col = c("#475467", "#1A56C4", "#98A2B3", "#991B1B", "#B42318"),
    lty = c(NA, 1, NA, NA, 3),
    lwd = c(1, 3.5, 1, 1, 1.2), cex = .68
  )

  mtext(
    paste0("n = ", length(vals),
           "  |  Q1 = ", format(q1, digits = 6, trim = TRUE),
           "  |  Mediane = ", format(med, digits = 6, trim = TRUE),
           "  |  Q3 = ", format(q3, digits = 6, trim = TRUE),
           "  |  Outliers = ", length(vals_out)),
    side = 3, line = 1.05, cex = .74, col = "#475467"
  )

  mtext(
    if (length(vals_out) == 0)
      "Aucune observation au-dela des limites de 1,5 × IQR."
    else
      paste0(length(vals_out), " valeur(s) potentiellement aberrante(s) — Lxx = numero de ligne"),
    side = 3, line = -.05, cex = .78,
    col = if (length(vals_out) == 0) "#166534" else "#991B1B"
  )
}, res = 120)
