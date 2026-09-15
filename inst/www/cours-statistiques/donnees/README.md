# Jeux de données — Formation aux méthodes statistiques avec Ramses

Ce dossier contient les jeux de données utilisés dans les modules de la formation.

## 📊 Fichiers disponibles

### 1. `10-parcelles-mais.csv`

Jeu de données principal utilisé dans les **modules 01 à 11**.

**Description :** 10 parcelles de maïs réparties entre deux villages (Bonabéri et Bépanda), avec leur superficie et leur rendement mesurés.

**Colonnes :**
| Nom | Type | Description | Unité |
|---|---|---|---|
| `parcelle` | Qualitatif | Identifiant de la parcelle (P01 à P10) | — |
| `village` | Qualitatif | Village d'implantation | — |
| `superficie` | Quantitatif | Superficie cultivée | hectares (ha) |
| `rendement` | Quantitatif | Rendement en grains | tonnes par hectare (t/ha) |

**Utilisation :** découverte, statistiques descriptives, graphiques, tests t, corrélation, régression.

---

### 2. `40-parcelles-factoriel.csv`

Jeu de données étendu utilisé dans les **modules 09b et 12**.

**Description :** Essai factoriel complet 2×2 : 2 variétés de maïs × 2 niveaux de fertilisation, avec 10 répétitions par combinaison, soit 40 parcelles au total.

**Colonnes :**
| Nom | Type | Description | Unité |
|---|---|---|---|
| `parcelle` | Qualitatif | Identifiant de la parcelle (P001 à P040) | — |
| `variete` | Qualitatif | Variété de maïs (A ou B) | — |
| `fertilisation` | Qualitatif | Niveau de fertilisation (Sans ou Avec) | — |
| `rendement` | Quantitatif | Rendement en grains | tonnes par hectare (t/ha) |

**Utilisation :** ANOVA à 2 facteurs, interactions, puissance statistique, tests post-hoc.

---

## 🔄 Reproduire ces données

Les scripts R de génération sont dans le dossier `scripts-R/`. Pour les régénérer :

```bash
cd donnees/scripts-R
Rscript generer-10-parcelles.R
Rscript generer-40-parcelles.R