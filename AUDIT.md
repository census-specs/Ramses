# Audit technique — Ramses 0.1.0

Date de l'audit initial : 4 septembre 2026
Dernière mise à jour : 4 septembre 2026

## 1. Résumé exécutif

Ramses possède déjà une vraie architecture de package Shiny : une fonction publique `run_app()`, une application principale, trois modules fonctionnels et une couche de documentation Roxygen2. Le cœur est lisible et le découpage est cohérent pour un premier prototype.

Le principal problème n'est pas l'absence de fonctionnalités, mais le décalage initial entre ce que le projet annonçait et ce que le code implémentait réellement. Cette documentation a été réalignée sur le code observé.

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
- `docs/` : site documentaire statique ;
- `tests/testthat/` : premiers tests automatisés de fumée ;
- `.github/workflows/` : premier workflow CI pour `R CMD check`.

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

## 4. Incohérences corrigées

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

## 5. Travaux ajoutés après l'audit initial

### Tests

Le dépôt contient maintenant :

- `tests/testthat.R` ;
- `tests/testthat/test-package-smoke.R`.

Ces tests vérifient actuellement les métadonnées du package, le retour de `run_app()` et la construction des principales interfaces publiques.

### Intégration continue

Un workflow `.github/workflows/R-CMD-check.yaml` a été ajouté. Il installe R et les dépendances de développement puis lance `R CMD check` sur les pushs vers `main`/`develop/**` et sur les pull requests vers `main`.

> Le workflow n'a pas été exécuté dans l'environnement d'audit : R n'y était pas disponible. Le premier passage CI sur GitHub devra donc être considéré comme une étape de validation réelle.

## 6. Problèmes techniques prioritaires

### P0 — Reproductibilité du code généré

Plusieurs morceaux de code généré construisent des formules à partir de chaînes de caractères sans protéger les noms de variables avec des backticks. Une colonne appelée par exemple `revenu mensuel`, `âge`, `traitement (A/B)` ou contenant certains caractères spéciaux peut produire du code R invalide.

**Action recommandée :** créer un helper unique de quotation des noms de variables et l'utiliser partout dans la génération des formules et du code R.

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

### P1 — Copie et autonomie de l'interface

Les fonctions de copie vers le presse-papiers et certaines ressources externes doivent être fiabilisées. L'objectif est de rendre l'application utilisable de manière aussi autonome que possible, notamment sur des connexions faibles.

## 7. Méthodologie : corrections de vocabulaire à prévoir

Ramses doit éviter les formulations statistiques trop catégoriques :

- préférer **« on ne rejette pas H0 »** à « H0 est conservée » ;
- éviter **« les données sont normales »** après un test non significatif ; préférer « aucune évidence statistique suffisante contre l'hypothèse de normalité » ;
- distinguer **significativité statistique** et **importance pratique** ;
- ne pas déduire une causalité d'une simple corrélation ;
- ne pas présenter automatiquement un test paramétrique comme valide uniquement parce que Shapiro-Wilk n'est pas significatif.

## 8. Roadmap recommandée

### Version 0.1.x — Stabilisation

- [x] premiers tests `testthat` ;
- [x] premier workflow CI ;
- [ ] corriger les noms de variables complexes ;
- [ ] corriger la heatmap ;
- [ ] renforcer les validations méthodologiques ;
- [ ] harmoniser les interprétations ;
- [ ] fiabiliser les exports.

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
