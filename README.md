# Ramses <img src="man/figures/logo.png" align="right" height="138" alt="Ramses logo" />

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R%20CMD%20check-passing-brightgreen.svg)](https://github.com/astral-r/Ramses/actions)
[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/astral-r/Ramses)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-Universe](https://img.shields.io/badge/R--Universe-astral--r-orange.svg)](https://astral-r.r-universe.dev)
<!-- badges: end -->

> **Modern Graphical User Interface for Statistical Analysis in R**  
> Une interface graphique contemporaine, fluide et réactive conçue comme une alternative moderne à **Rcmdr** (R Commander).

---

## 🌟 Présentation

**Ramses** est un package R complet fournissant une interface utilisateur graphique (GUI) moderne développée avec **Shiny** et **bslib** (Bootstrap 5). Conçu pour les chercheurs, data analysts, biostatisticiens, enseignants et étudiants, Ramses simplifie l'exploration et l'analyse statistique tout en garantissant une **reproductibilité scientifique absolue**.

Contrairement aux interfaces classiques reposant sur Tcl/Tk, Ramses offre une expérience web interactive, dynamique et élégante, s'exécutant directement dans votre navigateur web ou au sein de RStudio.

---

## ✨ Fonctionnalités Clés

### 1. Importation & Gestion Multi-formats
- **Formats supportés** : Fichiers délimités (`.csv`, `.tsv`, `.txt`), feuilles de calcul Excel (`.xlsx`, `.xls`), fichiers SPSS (`.sav`), Stata (`.dta`) et objets sérialisés R (`.rds`, `.RData`).
- **Détection automatique du format** basée sur l'extension et fenêtre modale de paramétrage fin (séparateurs, décimales, encodage, noms de variables, typage).
- **Explorateur interactif** avec recherche globale, pagination et filtres par colonne via `DT::datatable`.

### 2. Statistiques Descriptives Complètes
- **Variables Quantitatives** : Moyenne, écart-type, médiane, IQR, min/max, skewness, kurtosis, intervalles de confiance et graphiques univariés (histogrammes avec courbe de densité, boxplots interactifs).
- **Variables Qualitatives** : Tables d'effectifs, pourcentages simples et cumulés, diagrammes en barres et camemberts interactifs via Plotly.
- **Analyses Bivariées & Contingence** : Tableaux croisés avec pourcentages ligne / colonne / total, et matrices de corrélation numériques (Pearson, Spearman).

### 3. Visualisation Dynamique ("Tableau-Style" Chart Builder)
- Système de glisser-déposer intuitif pour assigner les axes X, Y, la couleur/remplissage, la taille et les facettes (`facet_wrap`).
- Plus de 10 géométries ggplot2 intégrées : Nuages de points, régressions linéaires et LOESS, boîtes à moustaches, diagrammes en violon, histogrammes, barres, séries temporelles et cartes de chaleur.
- Double affichage synchronisé : Rendu interactif WebGL/SVG via `plotly` et code R source `ggplot2` généré en temps réel.

### 4. Tests Statistiques & Modélisation Exhaustifs
Six catégories méthodologiques prêtes à l'emploi :
1. **Tests paramétriques univariés & bivariés** : Test de Student pour échantillon unique, échantillons indépendants (Welch ou Student classique), et séries appariées.
2. **Tests non-paramétriques** : Test de Wilcoxon-Mann-Whitney (2 groupes) et test de Wilcoxon pour séries appariées.
3. **Analyse de variance (ANOVA)** : ANOVA à un facteur, ANOVA factorielle et test non-paramétrique de Kruskal-Wallis avec tests post-hoc (Tukey HSD).
4. **Tests d'association & proportions** : Test du Chi-2 d'indépendance de Pearson, test exact de Fisher, et tests de proportions à 1 ou 2 échantillons.
5. **Régression & Modélisation** : Régression linéaire simple et multiple (OLS), régression logistique binaire avec calcul automatique des Odds Ratios (OR) et diagnostics des résidus.
6. **Tests de normalité & homoscédasticité** : Shapiro-Wilk, Kolmogorov-Smirnov, test de Levene et test de Bartlett.

### 5. Journal R Markdown & Reproductibilité
- **Journalisation automatique** : Chaque filtre, transformation de données, graphique et test statistique exécuté dans l'interface est fidèlement consigné sous forme de script R Markdown (`.Rmd`).
- **Console d'édition en direct** : Permet d'insérer des notes textuelles, interprétations cliniques ou remarques d'analyse.
- **Exportation multi-formats** :
  - Script R exécutable pur (`.R`) pour exécution en ligne de commande ou `source()`.
  - Fichier source R Markdown complet (`.Rmd`).
  - Rapport autonome compilé en HTML interactif (`.html`) avec table des matières et thèmes graphiques.

---

## 🚀 Installation

### Prérequis
- R version **4.1.0** ou supérieure installée sur votre système.

### Installation depuis GitHub
Vous pouvez installer la version de développement de **Ramses** directement depuis GitHub à l'aide du package `remotes` :

```r
# Installer le package remotes si nécessaire
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Installer Ramses depuis le dépôt GitHub
remotes::install_github("astral-r/Ramses")
```

### Installation locale (depuis les sources du package)
Si vous avez cloné ou téléchargé le code source :

```r
# Depuis le répertoire racine du package
devtools::install()

# Ou en exécutant le script automatisé d'assemblage
source("dev/build_package.R")
```

---

## 💻 Utilisation Rapide

Le démarrage de l'application nécessite une seule ligne de code :

```r
# Charger le package
library(Ramses)

# Lancer l'interface utilisateur dans votre navigateur par défaut
run_app()
```

### Options de Lancement Avancées

La fonction `run_app()` offre des paramètres de configuration adaptés aux environnements locaux, serveurs distants ou conteneurs Docker :

```r
# Lancer sur un port TCP spécifique (ex: 3838)
run_app(port = 3838)

# Lancer sans ouvrir automatiquement le navigateur (mode serveur)
run_app(port = 8080, launch.browser = FALSE)

# Écouter sur toutes les interfaces réseau (ex: conteneur Docker / Cloud Run)
run_app(host = "0.0.0.0", port = 3000)
```

---

## 📁 Architecture du Package

```text
Ramses/
├── DESCRIPTION             # Métadonnées officielles et dépendances
├── NAMESPACE               # Fonctions exportées et imports roxygen2
├── LICENSE                 # Licence MIT
├── .Rbuildignore           # Fichiers exclus de la compilation binaire
├── README.md               # Documentation de présentation du projet
├── dev/
│   └── build_package.R     # Script d'audit, documentation et installation
├── R/
│   ├── run_app.R           # Point d'entrée public exporté : run_app()
│   ├── app_ui.R            # Interface globale bslib (navbar, modales)
│   ├── app_server.R        # Logique réactive centrale et journal Rmd
│   ├── mod_descriptives.R  # Module de statistiques descriptives univariées/bivariées
│   ├── mod_chart_builder.R # Module créateur de graphiques (ggplot2 / Plotly)
│   └── mod_tests.R         # Module de tests statistiques et modélisation
└── man/                    # Fiches d'aide générées automatiquement par roxygen2
```

---

## 🤝 Contribution & Signalement de Bugs

Les contributions sont les bienvenues ! Pour signaler un problème ou proposer une nouvelle fonctionnalité :
1. Ouvrez une *issue* sur GitHub : [https://github.com/astral-r/Ramses/issues](https://github.com/astral-r/Ramses/issues)
2. Soumettez une *Pull Request* avec les tests associés.

---

## 📄 Licence

Ce projet est distribué sous licence libre **MIT**. Consultez le fichier [LICENSE](LICENSE) pour plus de détails.
