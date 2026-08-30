# ============================================================
# INSPECTION DES OUTLIERS — Rendu du boxplot
# ============================================================
# Ce module remplace uniquement le renderer du graphique défini
# dans mod_qualite.R. Il est chargé après mod_qualite.R.
# ============================================================

output$hygie_boxplot_plot <- renderPlot({
  variable <- rv$hygie_boxplot_variable
  req(is.character(variable), length(variable) == 1, nzchar(variable))

  df <- hygie_donnees_inspection_base()
  req(is.data.frame(df), variable %in% names(df))

  x <- df[[variable]]
  req(is.numeric(x))

  # Conserver les indices d'origine pour pouvoir relier les points
  # du graphique aux lignes réelles du jeu de données.
  idx <- which(is.finite(x))
  vals <- x[idx]
  req(length(vals) >= 2)

  q <- as.numeric(quantile(vals, probs = c(.25, .5, .75),
                           names = FALSE, na.rm = TRUE, type = 7))
  q1 <- q[1]
  med <- q[2]
  q3 <- q[3]
  iqr <- q3 - q1

  # Règle Tukey / boxplot : 1,5 × IQR.
  # Même lorsque IQR = 0, on conserve une règle exploitable :
  # toute valeur strictement différente de la valeur centrale est
  # considérée comme potentiellement aberrante.
  if (is.finite(iqr) && iqr > 0) {
    borne_basse <- q1 - 1.5 * iqr
    borne_haute <- q3 + 1.5 * iqr
  } else {
    borne_basse <- q1
    borne_haute <- q3
  }

  idx_out <- idx[x[idx] < borne_basse | x[idx] > borne_haute]
  vals_out <- x[idx_out]

  # Marges robustes : le graphique reste lisible même avec une grande
  # amplitude entre les valeurs normales et les valeurs aberrantes.
  plage <- range(vals, finite = TRUE, na.rm = TRUE)
  amplitude <- diff(plage)
  marge <- if (is.finite(amplitude) && amplitude > 0) amplitude * 0.08 else max(abs(plage), 1) * 0.08
  if (!is.finite(marge) || marge <= 0) marge <- 1

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(4.5, 1.5, 4.2, 1.5), xpd = NA)

  # Le boxplot est construit directement à partir des valeurs de la
  # variable sélectionnée : aucun jeu de données générique.
  boxplot(
    vals,
    horizontal = TRUE,
    outline = FALSE,
    xaxt = "n",
    main = paste0("Distribution de « ", variable, " »"),
    xlab = "Valeur",
    border = "#64748B",
    col = "#E8EEF7",
    whiskcol = "#64748B",
    staplecol = "#64748B",
    medcol = "#1A56C4",
    boxlwd = 1.4,
    whisklty = 1,
    whisklwd = 1.2,
    staplelwd = 1.2
  )

  # Ajouter les valeurs aberrantes par-dessus le boxplot, avec une
  # légère dispersion verticale pour qu'elles restent toutes visibles.
  if (length(idx_out) > 0) {
    y <- rep(1, length(idx_out))
    if (length(idx_out) > 1) {
      y <- y + seq(-0.055, 0.055, length.out = length(idx_out))
    }

    points(
      vals_out, y,
      pch = 21,
      bg = "#C53030",
      col = "#991B1B",
      cex = 1.15
    )

    text(
      vals_out, y,
      labels = paste0("L", idx_out),
      pos = ifelse(seq_along(idx_out) %% 2 == 0, 1, 3),
      offset = 0.45,
      cex = 0.72,
      col = "#7F1D1D"
    )
  }

  # Repères statistiques réellement calculés sur la variable.
  axis(1, at = pretty(c(plage, q1, med, q3, borne_basse, borne_haute), n = 7))
  abline(v = med, lty = 2, lwd = 1, col = "#1A56C4")

  title(
    sub = paste0(
      "n = ", length(vals),
      "   |   Q1 = ", format(q1, trim = TRUE, digits = 7),
      "   |   Médiane = ", format(med, trim = TRUE, digits = 7),
      "   |   Q3 = ", format(q3, trim = TRUE, digits = 7),
      "   |   Outliers = ", length(idx_out)
    ),
    cex.sub = 0.78,
    col.sub = "#4A5568"
  )

  if (length(idx_out) == 0) {
    mtext(
      "Aucune valeur ne dépasse les limites de 1,5 × IQR.",
      side = 3, line = 0.7, cex = 0.82, col = "#166534"
    )
  } else {
    mtext(
      paste0(length(idx_out), " valeur(s) potentiellement aberrante(s) — étiquettes = numéros de lignes"),
      side = 3, line = 0.7, cex = 0.82, col = "#991B1B"
    )
  }
}, res = 110)
