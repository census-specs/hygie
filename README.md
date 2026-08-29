# 🩺 Hygie — Traitement Interactif des Données sous R

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://img.shields.io/badge/R%20>=%204.1.0-blue.svg)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-1.0.0-green.svg)](https://github.com/census-specs/hygie)

**Hygie** est un outil de préparation et de nettoyage de données interactif en R, inspiré du fonctionnement de **PowerQuery**. Il propose une interface graphique complète sous forme de logiciel statistique moderne (menus déroulants, fenêtres de dialogue, historique dynamique).

Il permet d'importer, filtrer, manipuler et exporter vos jeux de données **sans rédiger une seule ligne de code**, tout en générant automatiquement le code R `tidyverse` équivalent.

---

## 🚀 Fonctionnalités principales

- 📥 **Importation multi-formats** : CSV, Excel (`.xlsx`, `.xls`), SPSS (`.sav`), SAS, Stata, JSON, RDS.
- 🧹 **Nettoyage avancé** : Gestion des doublons, suppression des valeurs manquantes, gestion de la casse, suppression des espaces superflus.
- ⚡ **Transformations graphiques** :
  - Création de variables calculées et conditionnelles.
  - Recodage de modalités ou par plages de valeurs.
  - Fusion et scission de colonnes.
  - Conversions et opérations sur les dates.
  - Tri et réorganisation intuitive des colonnes.
- 🔗 **Combinaison de tables** : Jointures (`left`, `inner`, `right`, `full`), empilement de lignes (`append`) et restructuration (`pivot_longer` / `pivot_wider`).
- 📊 **Contrôle Qualité & Outliers** : Analyse globale de la qualité des données et détection/traitement des valeurs aberrantes.
- 📜 **Historique rejouable** : Visualisez chaque étape appliquée, annulez ou supprimez une transformation à tout moment.
- 💻 **Génération de Code R** : Exportez le script R complet produit par vos actions pour garantir la reproductibilité.
- 📤 **Export multi-formats** : Sauvegardez le résultat propre sous formats CSV, Excel, RDS, etc.

---

## 📦 Installation

Vous pouvez installer **Hygie** directement depuis GitHub à l'aide du paquet `remotes` :

```R
# 1. Installer remotes si nécessaire
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
```
# 2. Installer Hygie et l'ensemble de ses dépendances
```R
remotes::install_github("census-specs/hygie")
```
🎯 Prise en main rapide
Une fois le paquet installé, vous pouvez lancer l'application d'une simple commande dans votre console R :

```R
library(hygie)

# Lancer Hygie
run_hygie()
```
Options de lancement :
```R
# Lancer sur un port spécifique sans ouvrir le navigateur externe (affichage Viewer RStudio)
run_hygie(port = 3838, launch.browser = rstudioapi::viewer)
```
🛠️ Stack technique & Dépendances
L'application repose sur l'écosystème R moderne et performant :

Interface : shiny, reactable, DT

Manipulation de données : dplyr, tidyr, stringr, forcats, purrr, rlang

Import / Export : readr, readxl, haven, writexl, jsonlite

Gestion du temps : lubridate

👨‍💻 Auteur & Contact
Pierre Valdeze MBOM MBOM

Email : pierrembom@outlook.com

GitHub : @census-specs

Téléphone : +237 698389030 / 650989019

📄 Licence
Ce projet est sous licence MIT. Consulter le fichier LICENSE pour plus de détails.
