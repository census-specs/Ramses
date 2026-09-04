# ==============================================================================
# Script de Build, Validation et Installation du Package R "Ramses"
# Fichier : dev/build_package.R
# ==============================================================================
# Ce script automatise la chaîne de compilation et de contrôle qualité
# du package Ramses pour un déploiement local ou une soumission CRAN / GitHub.
#
# Étapes exécutées :
#   1. Vérification et installation des dépendances de développement
#   2. Génération de la documentation Roxygen2 (man/ & NAMESPACE)
#   3. Audit et contrôle qualité complet du package (devtools::check)
#   4. Installation de Ramses dans la bibliothèque R active (devtools::install)
# ==============================================================================

options(warn = 1)

cli_title <- function(txt) {
  cat("\n", paste(rep("=", 75), collapse = ""), "\n", sep = "")
  cat("  >>> ", txt, "\n")
  cat(paste(rep("=", 75), collapse = ""), "\n\n", sep = "")
}

cli_success <- function(txt) cat("[OK] ", txt, "\n", sep = "")
cli_info <- function(txt) cat("[INFO] ", txt, "\n", sep = "")

cli_title("1. VERIFICATION DE L'ENVIRONNEMENT DE DEVELOPPEMENT")

pkg_root <- rprojroot::find_root(rprojroot::is_r_package, path = getwd())
cli_info(paste0("Racine du package detectee : ", pkg_root))
setwd(pkg_root)

dev_deps <- c("devtools", "roxygen2", "rmarkdown", "knitr", "testthat", "rprojroot")
missing_deps <- dev_deps[!vapply(dev_deps, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing_deps) > 0) {
  cli_info(paste("Installation des dependances manquantes :", paste(missing_deps, collapse = ", ")))
  install.packages(missing_deps, repos = "https://cloud.r-project.org")
} else {
  cli_success("Toutes les dependances de developpement sont installees.")
}

cli_title("2. GENERATION DE LA DOCUMENTATION (Roxygen2)")
cli_info("Generation des fiches d'aide (.Rd) et mise a jour de NAMESPACE...")
tryCatch({
  devtools::document(pkg = pkg_root)
  cli_success("Documentation Roxygen2 generee avec succes dans 'man/' et 'NAMESPACE'.")
}, error = function(e) {
  stop("Erreur lors de la documentation Roxygen2 : ", e$message)
})

cli_title("3. AUDIT QUALITE ET CONTROLE DE CONFORMITE (devtools::check)")
cli_info("Execution de R CMD check via devtools::check()...")
cli_info("Recherche d'erreurs (ERRORS), d'avertissements (WARNINGS) et de remarques (NOTES)...")

check_results <- devtools::check(
  pkg = pkg_root,
  document = FALSE,
  cran = FALSE,
  error_on = "warning"
)

cat("\n")
if (length(check_results$errors) == 0 && length(check_results$warnings) == 0) {
  cli_success("Check reussi : 0 Erreur, 0 Avertissement !")
  if (length(check_results$notes) > 0) {
    cli_info(paste0(length(check_results$notes), " note(s) mineure(s) signalee(s)."))
  }
} else {
  warning("Le check a releve des avertissements ou erreurs. Veuillez inspecter les logs ci-dessus.")
}

cli_title("4. INSTALLATION LOCALE DU PACKAGE (devtools::install)")
cli_info("Installation de la version locale de Ramses dans la bibliotheque R active...")
tryCatch({
  devtools::install(
    pkg = pkg_root,
    dependencies = TRUE,
    upgrade = "never",
    build_vignettes = FALSE
  )
  cli_success("Package 'Ramses' installe avec succes dans votre environnement R !")
}, error = function(e) {
  stop("Erreur lors de l'installation du package : ", e$message)
})

cli_title("5. TEST RAPIDE DE LANCEMENT")
cli_info("Pour tester l'interface graphique de Ramses des maintenant, executez :")
cat("\n    library(Ramses)\n    run_app()\n\n")
cat(paste(rep("=", 75), collapse = ""), "\n")
cat("Processus de build termine avec succes.\n")
