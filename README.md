# Ramses

> **Interface graphique moderne pour l'analyse statistique avec R**
>
> Une GUI Shiny/bslib pensée pour rendre l'analyse statistique plus accessible, tout en conservant le code R et une démarche reproductible.

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/census-specs/Ramses)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Présentation

**Ramses** est un package R qui transforme une session R en une interface graphique interactive accessible depuis un navigateur. Le projet vise particulièrement les personnes qui souhaitent réaliser des analyses statistiques sans devoir écrire immédiatement tout le code à la main, tout en gardant une sortie R exploitable et reproductible.

Le projet est construit avec **Shiny**, **bslib**, **DT**, **ggplot2**, **Plotly**, **rmarkdown** et **knitr**. L'architecture actuelle est volontairement compacte : une application principale orchestre trois modules fonctionnels — descriptives, visualisation et tests/modélisation.

Ramses est un projet en **version 0.1.0**. Il s'agit d'une base fonctionnelle destinée à évoluer, et non encore d'un équivalent complet de R Commander, jamovi ou SPSS.

## Ce que Ramses fait actuellement

### 1. Importation et exploration

Formats actuellement pris en charge par l'interface :

- fichiers texte délimités : `.csv`, `.txt`, `.tsv` ;
- Excel : `.xlsx`, `.xls` ;
- SPSS : `.sav` ;
- Stata : `.dta` ;
- objets R sérialisés : `.rds`.

L'importation est guidée par une fenêtre modale. Pour les fichiers texte, le séparateur, le séparateur décimal et la présence d'un en-tête peuvent être configurés. Les fichiers Excel permettent de choisir une feuille.

Une table interactive `DT` permet ensuite de rechercher, trier et parcourir les observations.

> **Limite actuelle :** `.RData` apparaît dans certaines anciennes interfaces/documentations du projet, mais n'est pas géré par la logique d'importation actuelle. Il n'est donc pas présenté comme format supporté.

### 2. Statistiques descriptives

Le module **Analyses Descriptives** comprend quatre volets :

- **Variables quantitatives** : N, valeurs manquantes, moyenne, médiane, variance, écart-type, coefficient de variation, minimum, maximum, IQR, asymétrie et aplatissement ; analyse globale ou par groupe ; boxplot et histogramme.
- **Variables qualitatives** : effectifs, pourcentages, pourcentages cumulés, gestion des valeurs manquantes et tri des modalités.
- **Tableaux croisés** : effectifs, pourcentages en ligne, en colonne ou sur le total, avec visualisation bivariée.
- **Matrice de corrélation** : Pearson ou Spearman, avec gestion configurable des valeurs manquantes et heatmap.

### 3. Créateur de graphiques

Le **Chart Builder** propose actuellement **8 types de graphiques** :

1. nuage de points ;
2. diagramme en barres ;
3. boîte à moustaches ;
4. diagramme en violon ;
5. histogramme ;
6. courbe / ligne ;
7. densité ;
8. carte thermique.

Les variables peuvent être affectées aux axes X/Y, à la couleur ou au remplissage, à la taille des points et aux facettes. Le graphique est construit avec `ggplot2`, puis rendu de manière interactive avec Plotly.

Le module peut également produire le **code ggplot2 correspondant** et l'injecter dans le journal R Markdown.

> **Limite actuelle :** le Chart Builder utilise des sélecteurs Shiny ; il ne s'agit pas encore d'un véritable système drag-and-drop.

### 4. Tests statistiques et modélisation

Le module **Inférence Statistique & Modélisation** couvre actuellement :

- **Normalité et variances** : Shapiro-Wilk, Kolmogorov-Smirnov, Bartlett et Fligner-Killeen ;
- **Comparaison à 1 ou 2 échantillons** : test t à un échantillon, test t indépendant (Welch ou variances égales), test t apparié, Wilcoxon à un échantillon, Wilcoxon indépendant et Wilcoxon apparié ;
- **3 groupes ou plus** : ANOVA à un facteur avec Tukey HSD, ou Kruskal-Wallis avec comparaisons par paires de Wilcoxon et correction de Bonferroni ;
- **Variables qualitatives** : Chi-deux, Fisher exact et McNemar ;
- **Corrélations** : Pearson, Spearman et Kendall avec test d'hypothèse ;
- **Modélisation** : régression linéaire `lm()` et régression logistique binaire `glm(..., family = binomial())`.

Les résultats affichent notamment statistique de test, degrés de liberté lorsque disponibles, p-value, décision selon alpha, intervalles de confiance pour plusieurs tests et interprétation textuelle.

> **Limites actuelles :** l'ANOVA est à un facteur ; les tests de proportions ne sont pas encore présents ; le calcul d'Odds Ratios n'est pas encore exposé séparément dans l'interface ; la régression logistique nécessite une variable réponse binaire valide.

## Reproductibilité : le journal R Markdown

L'une des idées centrales de Ramses est de ne pas enfermer l'utilisateur dans une interface graphique.

Chaque analyse déclenchée dans les modules peut ajouter un bloc de code au **Journal R Markdown**. L'utilisateur peut également insérer des notes en Markdown.

Le journal peut être exporté sous trois formes :

- `.R` : extraction des blocs R ;
- `.Rmd` : document source complet ;
- `.html` : rapport compilé avec `rmarkdown`/`knitr`.

Cette approche correspond au positionnement du projet : **faciliter l'analyse graphique sans abandonner la reproductibilité ni le code R**.

> **Point important :** le journal actuel est un mécanisme de génération de code, pas encore un système de provenance complet. Certaines opérations affichées par l'interface ne sont pas encore représentées par un objet de résultats standardisé. Une architecture de résultats commune sera importante pour les prochaines versions.

## Utilisation

### Installation depuis GitHub

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("census-specs/Ramses")
```

### Lancer Ramses

```r
library(Ramses)
run_app()
```

Le fonctionnement repose sur un objet Shiny. Depuis une session R interactive, l'objet retourné par `shinyApp()` peut être lancé automatiquement lorsqu'il est imprimé ; c'est le comportement utilisé par `run_app()`. citeturn4search0turn4search5

### Options

```r
# Navigateur standard
run_app(standalone = FALSE)

# Port précis
run_app(port = 3838)

# Mode serveur / conteneur
run_app(host = "0.0.0.0", port = 3000, launch.browser = FALSE)
```

## Architecture actuelle

```text
Ramses/
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── README.md
├── R/
│   ├── run_app.R              # Point d'entrée public
│   ├── app_ui.R               # UI principale et modale d'importation
│   ├── app_server.R            # état des données, journal et exports
│   ├── globals.R               # globalVariables() pour R CMD check
│   ├── mod_descriptives.R      # descriptives, fréquences, contingence, corrélations
│   ├── mod_chart_builder.R     # génération ggplot2 + rendu Plotly
│   └── mod_tests.R              # tests d'hypothèses et régressions
├── man/                         # documentation roxygen2
├── inst/app/www/                # ressources statiques
├── docs/                        # site documentaire statique
└── dev/
    └── build_package.R          # documentation, check et installation locale
```

## État du projet

**Version : 0.1.0 — développement actif.**

### Points forts actuels

- interface web moderne plutôt qu'une GUI Tcl/Tk ;
- séparation en modules Shiny ;
- calculs réalisés par les fonctions statistiques de R ;
- visualisations interactives ;
- génération de code R ;
- export R / R Markdown / HTML ;
- architecture suffisamment simple pour être étendue progressivement.

### Priorités techniques recommandées

1. Ajouter une véritable suite de tests `testthat` et l'intégrer au CI.
2. Centraliser les résultats dans une structure commune séparant **valeurs scientifiques, présentation, interprétation, contrôles et code reproductible**.
3. Sécuriser la génération des formules et du code pour les noms de variables contenant espaces, accents ou caractères spéciaux.
4. Renforcer les contrôles méthodologiques avant chaque test : taille d'échantillon, données manquantes, nombre de modalités, conditions d'application et structure de la réponse.
5. Corriger les graphiques actuellement simplifiés, notamment la carte thermique et les graphiques nécessitant des variables quantitatives.
6. Ajouter progressivement les analyses manquantes plutôt que d'augmenter artificiellement la liste des fonctionnalités annoncées.

Pour un package R maintenable, `R CMD check` doit rester une étape régulière du développement, et une suite `testthat` est recommandée pour les nouvelles fonctionnalités. citeturn2search1turn2search2

## Documentation

Le site statique du projet se trouve dans `docs/` :

- `index.html` — présentation du projet ;
- `installation.html` — installation et dépannage ;
- `tutoriel.html` — parcours pratique ;
- `auteur.html` — philosophie et conception du projet.

## Contribution

Les issues et pull requests sont les bienvenues :

https://github.com/census-specs/Ramses/issues

## Licence

Ramses est distribué sous licence **MIT**.
