# ==============================================================================
# Declarations des variables globales pour R CMD check
# Fichier : R/globals.R
# ==============================================================================

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".",
    "x",
    "y",
    "label",
    "value",
    "variable",
    "count",
    "percentage",
    "term",
    "estimate",
    "std.error",
    "statistic",
    "p.value"
  ))
}
