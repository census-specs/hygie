# ============================================================
# HYGIE v1.0 — Lancement en développement
# Exécuter depuis le dossier racine : source("dev_lancer.R")
# ============================================================

pkgs <- c("shiny","DT","dplyr","tidyr","stringr","forcats",
          "readr","readxl","haven","writexl","jsonlite",
          "lubridate","rlang","glue","purrr")

manquants <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(manquants) > 0) {
  message("Installation : ", paste(manquants, collapse = ", "))
  install.packages(manquants)
}

shiny::runApp("inst/app", port = 3838)
