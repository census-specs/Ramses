# Audit technique — Ramses 0.1.0

Date de l'audit : 4 septembre 2026

## 1. Résumé exécutif

Ramses possède déjà une vraie architecture de package Shiny : une fonction publique `run_app()`, une application principale, trois modules fonctionnels et une couche de documentation Roxygen2. Le cœur est lisible et le découpage est cohérent pour un premier prototype.

Le principal problème n'est pas l'absence de fonctionnalités, mais le décalage entre ce que le projet annonce et ce que le code implémente réellement. La documentation initiale annonçait plusieurs fonctionnalités absentes (drag-and-drop, >10 graphiques, ANOVA factorielle, Levene, tests de proportions, Odds Ratios, etc.) et contenait une fausse identité académique ainsi que des liens vers un autre dépôt.

Le code montre également plusieurs points méthodologiques à sécuriser avant de présenter Ramses comme un outil statistique généraliste : génération de formules non protégée contre les noms de variables complexes, contrôles de conditions d'application encore limités, carte thermique simplifiée, validation incomplète des types de variables et journal R Markdown qui ne représente pas encore toute la provenance analytique.

## 2. Architecture observée

Le package est organisé autour de :

- `R/run_app.R` : point d'entrée et logique de lancement navigateur/standalone ;
- `R/app_ui.R` : interface globale et fenêtre d'importation ;
- `R/app_server.R` : état réactif du jeu de données, journal R Markdown, importations et exports ;
- `R/mod_descriptives.R` : descriptives quantitatives/qualitatives, contingence et corrélations ;
- `R/mod_chart_builder.R` : construction d'un objet ggplot2 puis conversion Plotly ;
- `R/mod_tests.R` : tests d'hypothèses et régressions ;
- `R/globals.R` : déclarations pour `R CMD check` ;
- `man/` : documentation générée par roxygen2 ;
- `inst/app/www/` : ressources statiques ;
- `docs/` : site documentaire statique.

Cette architecture est adaptée à une première version. Elle peut évoluer vers des modules plus spécialisés sans réécrire toute l'application.

## 3. Fonctionnalités réellement implémentées

### Données

- CSV/TXT/TSV via lecteur texte configurable ;
- XLSX/XLS via `readxl` ;
- SAV via `haven` ;
- DTA via `haven` ;
- RDS via `readRDS` ;
- table interactive DT ;
- détection de types simplifiée.

### Descriptives

- quantitatif : N, NA, moyenne, médiane, variance, SD, CV, min, max, IQR, asymétrie, aplatissement ;
- qualitatif : effectifs, pourcentages, cumulés, NA, tri ;
- contingence : effectifs et pourcentages ligne/colonne/total ;
- matrice de corrélation : Pearson/Spearman et gestion des NA.

### Visualisation

8 géométries sont effectivement proposées : scatter, bar, boxplot, violin, histogram, line, density et heatmap.

### Inférence/modélisation

- Shapiro-Wilk ;
- Kolmogorov-Smirnov ;
- Bartlett ;
- Fligner-Killeen ;
- t-test un échantillon, indépendant et apparié ;
- Wilcoxon un échantillon, indépendant et apparié ;
- ANOVA à un facteur + Tukey ;
- Kruskal-Wallis + comparaisons par paires avec Bonferroni ;
- Chi-deux ;
- Fisher exact ;
- McNemar ;
- Pearson/Spearman/Kendall ;
- régression linéaire ;
- régression logistique binaire.

### Reproductibilité

Le journal R Markdown enregistre des blocs de code produits par les modules et permet l'export `.R`, `.Rmd` et `.html`.

## 4. Incohérences corrigées dans cette mise à jour

1. Les liens `astral-r/Ramses` ont été remplacés par `census-specs/Ramses`.
2. Le badge R-CMD-check « passing » non démontré a été supprimé du README.
3. Le badge R-Universe non démontré a été supprimé.
4. `.RData` n'est plus présenté comme format supporté.
5. Les annonces de plus de 10 graphiques ont été corrigées en 8.
6. Les affirmations sur le drag-and-drop ont été retirées : l'interface utilise actuellement des sélecteurs Shiny.
7. Les fonctionnalités absentes (ANOVA factorielle, Levene, tests de proportions, Odds Ratios dédiés) ne sont plus annoncées comme disponibles.
8. Le tutoriel clinique fictif et ses résultats numériques non garantis ont été remplacés par un parcours basé sur `iris`.
9. La page auteur ne présente plus de doctorat, affiliation, ORCID, expérience ou association non vérifiés.
10. `DESCRIPTION` a été réaligné sur le dépôt actuel et `testthat` édition 3 a été déclaré.

## 5. Problèmes techniques prioritaires

### P0 — Reproductibilité du code généré

Plusieurs morceaux de code généré construisent des formules à partir de chaînes de caractères sans protéger les noms de variables avec des backticks. Une colonne appelée par exemple `revenu mensuel`, `âge`, `traitement (A/B)` ou contenant certains caractères spéciaux peut produire du code R invalide.

**Action recommandée :** créer un helper unique du type `quote_var()`/`as_name()` et l'utiliser partout dans la génération des formules et du code R.

### P0 — Conditions méthodologiques

Le choix d'un test ne doit pas seulement dépendre du type apparent de variable. Il faut contrôler les tailles d'échantillon, valeurs manquantes, nombre de groupes, groupes vides, variance nulle, réponse binaire, indépendance/appariement et autres conditions pertinentes.

**Action recommandée :** créer une couche de validation indépendante des modules UI.

### P1 — Architecture des résultats

Les modules calculent directement puis construisent leur HTML dans `renderUI`. Cela mélange valeur scientifique et présentation.

**Architecture cible recommandée :** un objet de résultat commun contenant au minimum :

```text
type
status
title
summary
data
tables
figures
messages
warnings
checks
interpretation
code
parameters
metadata
reproducibility
```

La couche statistique devrait produire cet objet ; la couche Shiny devrait uniquement le présenter. Cela facilitera les exports Word/HTML/PDF, les tests, les futures interfaces et la cohérence entre analyses.

### P1 — Chart Builder

La carte thermique actuelle utilise `geom_tile()` mais ne définit pas de valeur `fill`/`z` calculée : elle ne constitue donc pas encore une heatmap statistique générale. Les validations des types pour histogramme et densité doivent également être renforcées.

### P1 — Journal R Markdown

Le journal est utile, mais il s'agit actuellement d'une génération de code plutôt que d'un système complet de provenance. Certaines actions de l'interface peuvent produire du code qui ne correspond pas exactement à l'état affiché ou aux paramètres réellement utilisés.

**Action recommandée :** chaque analyse doit conserver ses paramètres et son code dans le même objet de résultat avant de l'ajouter au journal.

### P2 — Tests automatisés

Le dépôt ne contient actuellement pas de dossier `tests/`. Le script `dev/build_package.R` mentionne les tests et `testthat`, mais aucune suite automatisée n'est présente dans l'arborescence observée.

**Action recommandée :** mettre en place `testthat` édition 3, puis tester d'abord les fonctions statistiques pures et les helpers avant les tests d'interface Shiny.

### P2 — Script de développement

`dev/build_package.R` utilise `rprojroot` mais ne l'ajoute pas à sa liste `dev_deps`. Sur une machine de développement neuve, le script peut donc échouer avant d'avoir installé toutes ses dépendances.

## 6. Qualité logicielle

### Points forts

- namespace explicite pour la majorité des appels ;
- utilisation de `moduleServer()` ;
- séparation UI/server des modules ;
- gestion d'erreurs par `tryCatch` dans plusieurs chemins ;
- utilisation de `validate()`/`req()` dans plusieurs rendus ;
- export R Markdown intégré au workflow ;
- ressources statiques embarquées dans `inst/app/www`.

### Points à améliorer

- plusieurs blocs de logique sont très longs et gagneraient à être factorisés ;
- certaines fonctions calculent puis reconstruisent plusieurs fois les mêmes résultats ;
- les messages utilisateur utilisent parfois « accepter/conserver H0 », formulation à remplacer par « ne pas rejeter H0 » ;
- la génération de code et le calcul devraient partager une même définition des paramètres ;
- certaines couleurs sont codées directement dans les modules au lieu d'une couche de thème ;
- les fonctionnalités de copie vers le presse-papiers affichent actuellement une notification mais ne réalisent pas partout une copie navigateur effective ;
- la dépendance `fontawesome` est déclarée mais l'interface utilise aussi un CDN Font Awesome directement ;
- la dépendance externe aux Google Fonts/CDN rend l'expérience moins autonome hors connexion.

## 7. Méthodologie : corrections de vocabulaire à prévoir

Ramses doit éviter les formulations statistiques trop catégoriques :

- préférer **« on ne rejette pas H0 »** à « H0 est conservée » ;
- éviter **« les données sont normales »** après un test non significatif ; préférer « aucune évidence statistique suffisante contre l'hypothèse de normalité » ;
- distinguer **significativité statistique** et **importance pratique/clinique** ;
- ne pas déduire une causalité d'une simple corrélation ;
- ne pas présenter automatiquement un test paramétrique comme valide uniquement parce que Shapiro-Wilk n'est pas significatif.

## 8. Roadmap recommandée

### Version 0.1.x — Stabilisation

- corriger les noms de variables complexes ;
- corriger la heatmap ;
- ajouter tests `testthat` ;
- renforcer les validations ;
- harmoniser les interprétations ;
- fiabiliser les exports ;
- mettre en place un premier workflow CI.

### Version 0.2.x — Architecture analytique

- créer les helpers statistiques communs ;
- introduire l'objet de résultat universel ;
- séparer calcul, résultat, interprétation et présentation ;
- ajouter davantage de contrôles et d'effets de taille ;
- améliorer la gestion des facteurs et valeurs manquantes.

### Version 0.3.x — Couverture statistique

- tests de proportions ;
- Levene ;
- OR et IC pour régression logistique ;
- ANOVA factorielle ;
- tailles d'effet ;
- diagnostics de modèles plus complets ;
- davantage de graphiques statistiques spécialisés.

### À plus long terme

Ramses peut devenir le module d'analyse statistique d'une suite GUI R plus large consacrée à la préparation, l'analyse et la visualisation des données. Cette évolution est cohérente avec l'objectif pédagogique du projet : rendre l'analyse accessible sans abandonner R, la transparence et la reproductibilité.

## 9. Références de développement

Les recommandations concernant `R CMD check`, la structure des packages et `testthat` édition 3 suivent les pratiques documentées par *R Packages* et testthat. `R CMD check` est conçu pour détecter de nombreux problèmes de structure, namespace, documentation et tests ; une suite `testthat` est recommandée pour les packages activement développés.
