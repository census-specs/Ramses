# Ramses <img src="man/figures/logo.png" align="right" height="138" alt="Ramses logo" />

[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)](https://github.com/census-specs/Ramses)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

> **Ramses** est une interface graphique moderne pour réaliser des analyses statistiques avec R, sans devoir commencer par écrire du code.

---

## 📌 Qu'est-ce que Ramses ?

**Ramses** est un package R qui propose une interface graphique interactive construite avec **Shiny** et **bslib**.

L'objectif est de rendre l'analyse statistique plus simple et plus accessible, notamment pour les étudiants, enseignants, chercheurs et professionnels qui souhaitent travailler avec R à travers une interface graphique.

Ramses permet de charger un jeu de données, l'explorer, réaliser des analyses statistiques, créer des graphiques et conserver les commandes utilisées dans un journal **R Markdown**.

Ramses est développé comme une alternative moderne à **Rcmdr (R Commander)**, avec une interface accessible depuis un navigateur web.

> **État du projet :** Ramses est actuellement en développement. Le package n'a pas encore fait l'objet d'une vérification complète avec `R CMD check`.

---

## ✨ Ce que Ramses permet de faire

### 📂 Importer des données

Ramses peut importer plusieurs formats courants :

- CSV, TXT et TSV ;
- Excel (`.xlsx`, `.xls`) ;
- SPSS (`.sav`) ;
- Stata (`.dta`) ;
- RDS (`.rds`) ;
- objets R présents dans la session (`.GlobalEnv`).

Une fenêtre d'importation permet de configurer les principaux paramètres du fichier avant son chargement.

### 📊 Explorer les données

Le jeu de données actif peut être consulté dans une table interactive avec recherche, pagination et défilement horizontal.

Ramses affiche également un résumé du jeu de données : nombre de lignes, nombre de colonnes et types généraux de variables.

### 📈 Faire des statistiques descriptives

Le module descriptif permet notamment de travailler avec :

- les variables quantitatives ;
- les variables qualitatives ;
- les distributions et statistiques usuelles ;
- les tableaux d'effectifs et de pourcentages ;
- les analyses par groupes ;
- les corrélations entre variables numériques.

### 📉 Créer des graphiques

Le **Chart Builder** permet de construire graphiquement différents types de visualisations à partir de `ggplot2`, avec une restitution interactive via `plotly`.

Il propose notamment des graphiques tels que :

- nuages de points ;
- histogrammes ;
- diagrammes en barres ;
- boxplots ;
- violons ;
- courbes ;
- régressions ;
- cartes de chaleur.

Le code R correspondant au graphique peut également être consulté.

### 🧪 Réaliser des tests statistiques et des modèles

Ramses regroupe plusieurs méthodes statistiques dans une interface guidée, notamment :

- tests t ;
- tests de Wilcoxon ;
- ANOVA ;
- Kruskal-Wallis ;
- Chi-deux et test exact de Fisher ;
- tests de proportions ;
- tests de normalité ;
- tests d'homoscédasticité ;
- régression linéaire ;
- régression logistique.

Les choix proposés dépendent du type de données et de l'analyse sélectionnée.

### 📝 Conserver les étapes de l'analyse

Ramses possède un **journal R Markdown** qui enregistre les principales étapes réalisées dans l'interface.

Il est possible d'y ajouter des notes et commentaires, puis d'exporter le travail sous différents formats, notamment :

- script R (`.R`) ;
- fichier R Markdown (`.Rmd`) ;
- rapport HTML.

L'objectif est de faciliter la compréhension, la reproduction et la poursuite d'une analyse.

### 🎓 Formation statistique intégrée

Le dépôt contient également une **formation statistique complète composée de 15 modules**, avec des contenus pédagogiques, un glossaire et des jeux de données d'exemple.

---

## 🚀 Installation

### Prérequis

Il faut disposer de :

- **R ≥ 4.1.0** ;
- une connexion Internet pour installer Ramses et ses dépendances ;
- un navigateur web récent pour utiliser l'interface.

### Installer Ramses depuis GitHub

La méthode la plus simple consiste à installer `remotes`, puis Ramses directement depuis le dépôt GitHub :

```r
# 1. Installer remotes si nécessaire
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# 2. Installer Ramses
remotes::install_github("census-specs/Ramses")
```

Cette commande installe également les dépendances déclarées par Ramses.

### Installer une copie locale du dépôt

Si vous avez téléchargé ou cloné le dépôt sur votre ordinateur, placez-vous dans le dossier racine de **Ramses**, puis utilisez :

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_local(".")
```

Vous pouvez également utiliser `devtools::install()` si `devtools` est déjà installé :

```r
devtools::install()
```

---

## ▶️ Lancer Ramses

Après l'installation :

```r
library(Ramses)
run_app()
```

Par défaut, Ramses cherche un navigateur compatible avec le mode fenêtre dédiée. Si aucun navigateur compatible n'est trouvé, il utilise le navigateur par défaut du système.

### Quelques options utiles

Ouvrir Ramses dans le navigateur standard :

```r
run_app(standalone = FALSE)
```

Utiliser un port précis :

```r
run_app(standalone = FALSE, port = 3838)
```

Démarrer le serveur sans ouvrir automatiquement le navigateur :

```r
run_app(launch.browser = FALSE)
```

Pour un environnement distant ou conteneurisé :

```r
run_app(host = "0.0.0.0", port = 3000)
```

---

## 📁 Structure du dépôt

```text
Ramses/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── LICENSE
├── R/
│   ├── app_ui.R
│   ├── app_server.R
│   ├── run_app.R
│   ├── mod_data_prep.R
│   ├── mod_descriptives.R
│   ├── mod_chart_builder.R
│   ├── mod_tests.R
│   ├── mod_regression.R
│   └── utilitaires...
├── dev/
├── docs/
└── inst/
```

Le code principal de l'application se trouve dans le dossier `R/`. La documentation et les ressources pédagogiques sont regroupées notamment dans `docs/` et `inst/`.

---

## 🐛 Signaler un problème

Si vous rencontrez un problème ou souhaitez proposer une amélioration, vous pouvez ouvrir une **issue** sur le dépôt GitHub :

https://github.com/census-specs/Ramses/issues

---

## 📄 Licence

Ramses est distribué sous licence **MIT**. Voir le fichier [LICENSE](LICENSE).

---

## 👤 Auteur et parcours

### Pierre Valdeze MBOM MBOM

**Ingénieur agroéconomiste et statisticien**  
**Secrétaire Technique (Statisticien)** — Service des Techniques Agricoles, SOCAPALM, Kienké  
**Travailleur indépendant / Freelance**

#### Coordonnées

- **Téléphone :** +237 698 389 030 / +237 650 989 019
- **E-mail :** pierrembom@outlook.com
- **E-mail professionnel :** pmbom@socapalm.org

#### Formation

- **Master en Statistiques Agricoles — 2025** — ISSEA-CEMAC, Institut Sous-régional de Statistique et d'Économie Appliquée
- **Diplôme d'Ingénieur Agronome, option Économie et Sociologie Rurales** — Université de Dschang, Cameroun

#### Parcours professionnel et entrepreneurial

**Secrétaire Technique (Statisticien) — Service des Techniques Agricoles, SOCAPALM, Kienké**  
Fonction actuelle dans un environnement professionnel lié aux techniques agricoles et à l'analyse statistique.

**Promoteur — MEMOSTAT COMPANY**  
Création et développement d'une start-up spécialisée notamment dans le suivi et l'accompagnement des mémoires d'étudiants ainsi que dans les analyses statistiques pour les professionnels.

**Responsable commercial — CABI (Center for Agric-Business Innovations)**  
Développement de produits rentables à base de champignons et signature de contrats de vente.

**Promoteur et Président — Association ASTRAL**  
Ancien promoteur et président de l'association ASTRAL.

#### Activités actuelles et développement de solutions numériques

En parallèle de son activité professionnelle, Pierre Valdeze MBOM MBOM exerce également comme **freelance** et développe activement des solutions simples et rapides d'utilisation destinées notamment aux utilisateurs novices.

Il s'appuie sur **l'intelligence artificielle** pour concevoir des outils facilitant le traitement et l'analyse des données, notamment :

- **Ramses** : solution orientée vers l'analyse statistique et l'accompagnement des utilisateurs dans l'exploitation des données ;
- **Hygie** : solution destinée notamment au traitement et à la préparation des données.

Son parcours se situe ainsi à l'intersection de **l'agriculture, de l'agroéconomie, des statistiques, de l'analyse des données, de l'entrepreneuriat et du développement de solutions numériques**.

Dépôt du projet : https://github.com/census-specs/Ramses
