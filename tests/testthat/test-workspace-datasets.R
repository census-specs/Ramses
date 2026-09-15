library(testthat)

# ==============================================================================
# TESTS UNITAIRES ET D'INTÉGRATION — DÉTECTION DU WORKSPACE R
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. DÉTECTION DANS L'ENVIRONNEMENT
# ------------------------------------------------------------------------------

test_that("1. Détection avec un environnement vide", {
  test_env <- new.env(parent = emptyenv())
  res <- ramses_list_workspace_datasets(test_env)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_equal(names(res), c("name", "nrow", "ncol", "class", "size", "valid"))
})

test_that("2. Détection d'un seul data.frame", {
  test_env <- new.env(parent = emptyenv())
  test_env$df1 <- data.frame(a = 1:5, b = letters[1:5])
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "df1")
  expect_equal(res$nrow, 5)
  expect_equal(res$ncol, 2)
  expect_equal(res$class, "data.frame")
  expect_true(res$valid)
})

test_that("3. Détection de plusieurs data.frames triés par nom", {
  test_env <- new.env(parent = emptyenv())
  test_env$z_table <- data.frame(x = 1:10)
  test_env$a_table <- data.frame(y = 1:20, z = 1:20)
  test_env$m_table <- data.frame(w = 1:30)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 3)
  expect_equal(res$name, c("a_table", "m_table", "z_table"))
  expect_equal(res$nrow, c(20, 30, 10))
  expect_equal(res$ncol, c(2, 1, 1))
})

test_that("4. Détection d'un tibble (tbl_df)", {
  test_env <- new.env(parent = emptyenv())
  test_env$tib <- structure(
    data.frame(x = 1:10, y = 11:20),
    class = c("tbl_df", "tbl", "data.frame")
  )
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "tib")
  expect_equal(res$class, "tbl_df")
  expect_true(res$valid)
})

test_that("5. Détection d'un data.table", {
  test_env <- new.env(parent = emptyenv())
  test_env$dt <- structure(
    data.frame(x = 1:10, y = 11:20),
    class = c("data.table", "data.frame")
  )
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "dt")
  expect_equal(res$class, "data.table")
  expect_true(res$valid)
})

test_that("6. Exclusion d'un vecteur", {
  test_env <- new.env(parent = emptyenv())
  test_env$vec <- 1:100
  test_env$df <- data.frame(a = 1:10)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "df")
})

test_that("7. Exclusion d'une liste générique", {
  test_env <- new.env(parent = emptyenv())
  test_env$lst <- list(a = 1:10, b = letters)
  test_env$df <- data.frame(a = 1:10)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "df")
})

test_that("8. Exclusion d'une matrice", {
  test_env <- new.env(parent = emptyenv())
  test_env$mat <- matrix(1:9, nrow = 3)
  test_env$df <- data.frame(a = 1:10)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "df")
})

test_that("9. Exclusion d'une fonction", {
  test_env <- new.env(parent = emptyenv())
  test_env$ma_fonction <- function(x) x + 1
  test_env$df <- data.frame(a = 1:10)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "df")
})

test_that("10. Exclusion d'un modèle statistique (lm)", {
  test_env <- new.env(parent = emptyenv())
  test_env$mon_modele <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  test_env$df <- data.frame(a = 1:10)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "df")
})

# ------------------------------------------------------------------------------
# 2. VALIDITÉ ET CAS LIMITES
# ------------------------------------------------------------------------------

test_that("11. Dataset avec 0 ligne marqué comme non valide", {
  test_env <- new.env(parent = emptyenv())
  test_env$empty_rows <- data.frame(a = numeric(0), b = character(0))
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "empty_rows")
  expect_equal(res$nrow, 0)
  expect_false(res$valid)
})

test_that("12. Dataset avec 0 colonne marqué comme non valide", {
  test_env <- new.env(parent = emptyenv())
  test_env$empty_cols <- data.frame(row.names = 1:5)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "empty_cols")
  expect_equal(res$ncol, 0)
  expect_false(res$valid)
})

test_that("13. Dimensions exactes retournées", {
  test_env <- new.env(parent = emptyenv())
  test_env$d350x18 <- as.data.frame(matrix(rnorm(350 * 18), nrow = 350, ncol = 18))
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(res$nrow, 350)
  expect_equal(res$ncol, 18)
  expect_true(res$valid)
})

test_that("14. Noms exacts retournés sans altération", {
  test_env <- new.env(parent = emptyenv())
  test_env$donnees_patients_2026 <- data.frame(x = 1:10)
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(res$name, "donnees_patients_2026")
})

test_that("15. Classe correcte identifiée", {
  test_env <- new.env(parent = emptyenv())
  test_env$df_base <- data.frame(x = 1:5)
  test_env$tbl_obj <- structure(data.frame(x = 1:5), class = c("tbl_df", "tbl", "data.frame"))
  
  res <- ramses_list_workspace_datasets(test_env)
  expect_equal(res$class[res$name == "df_base"], "data.frame")
  expect_equal(res$class[res$name == "tbl_obj"], "tbl_df")
})

# ------------------------------------------------------------------------------
# 3. RÉCUPÉRATION ET CHARGEMENT (ramses_get_workspace_dataset)
# ------------------------------------------------------------------------------

test_that("16. Récupération correcte d'un dataset", {
  test_env <- new.env(parent = emptyenv())
  test_env$mes_donnees <- data.frame(
    id = 1:10,
    score = c(12, 14, 15, 11, 16, 18, 19, 13, 15, 17),
    groupe = rep(c("A", "B"), each = 5)
  )
  
  df_loaded <- ramses_get_workspace_dataset("mes_donnees", envir = test_env)
  expect_s3_class(df_loaded, "data.frame")
  expect_equal(nrow(df_loaded), 10)
  expect_equal(ncol(df_loaded), 3)
})

test_that("17. Dimensions conservées lors du chargement", {
  test_env <- new.env(parent = emptyenv())
  test_env$iris_copy <- iris
  
  df_loaded <- ramses_get_workspace_dataset("iris_copy", envir = test_env)
  expect_equal(dim(df_loaded), dim(iris))
})

test_that("18. Valeurs et colonnes conservées fidèlement", {
  test_env <- new.env(parent = emptyenv())
  orig <- data.frame(
    Nom = c("Alice", "Bob", "Charlie"),
    Age = c(25, 30, 35),
    Taille = c(1.65, 1.80, 1.75),
    stringsAsFactors = FALSE
  )
  test_env$personnes <- orig
  
  df_loaded <- ramses_get_workspace_dataset("personnes", envir = test_env)
  expect_equal(df_loaded, orig)
})

test_that("19. L'objet source dans l'environnement n'est jamais modifié", {
  test_env <- new.env(parent = emptyenv())
  orig <- data.frame(x = 1:5, y = 6:10)
  test_env$donnees <- orig
  
  # Chargement dans Ramses
  df_ramses <- ramses_get_workspace_dataset("donnees", envir = test_env)
  
  # Modification interne dans Ramses
  df_ramses$x[1] <- 9999
  df_ramses$nouvelle_col <- "test"
  
  # L'objet original dans test_env doit rester strictement identique
  expect_equal(test_env$donnees, orig)
  expect_equal(test_env$donnees$x[1], 1)
  expect_false("nouvelle_col" %in% names(test_env$donnees))
})

test_that("20. Erreur propre si l'objet n'existe pas", {
  test_env <- new.env(parent = emptyenv())
  expect_error(
    ramses_get_workspace_dataset("inexistant", envir = test_env),
    "L'objet 'inexistant' n'existe pas"
  )
})

test_that("21. Erreur propre si l'objet n'est pas un data.frame", {
  test_env <- new.env(parent = emptyenv())
  test_env$pas_un_df <- list(1, 2, 3)
  expect_error(
    ramses_get_workspace_dataset("pas_un_df", envir = test_env),
    "n'est pas un tableau de données"
  )
})

test_that("21b. Erreur propre si l'objet est vide", {
  test_env <- new.env(parent = emptyenv())
  test_env$vide <- data.frame()
  expect_error(
    ramses_get_workspace_dataset("vide", envir = test_env),
    "est vide"
  )
})

# ------------------------------------------------------------------------------
# 4. INTÉGRATION ET NON-RÉGRESSION STATISTIQUE
# ------------------------------------------------------------------------------

test_that("22-24. Intégration dans le réactif data_holder", {
  test_env <- new.env(parent = emptyenv())
  test_env$mes_data <- data.frame(
    Groupe = rep(c("A", "B"), each = 5),
    Rendement = c(10, 11, 12, 10, 11, 15, 16, 14, 15, 17)
  )
  
  df_loaded <- ramses_get_workspace_dataset("mes_data", envir = test_env)
  
  # Simulation du conteneur réactif
  data_holder <- list(
    name = "mes_data",
    df = df_loaded,
    source_file_name = NULL,
    source_file_datapath = NULL
  )
  
  expect_equal(data_holder$name, "mes_data")
  expect_equal(nrow(data_holder$df), 10)
  expect_null(data_holder$source_file_name)
  expect_null(data_holder$source_file_datapath)
})

test_that("Non-régression : Test t sur dataset issu du workspace", {
  test_env <- new.env(parent = emptyenv())
  test_env$data_t <- data.frame(
    Groupe = rep(c("A", "B"), each = 10),
    Score = c(rnorm(10, mean = 20, sd = 2), rnorm(10, mean = 25, sd = 2))
  )
  
  df <- ramses_get_workspace_dataset("data_t", envir = test_env)
  t_res <- t.test(Score ~ Groupe, data = df)
  expect_s3_class(t_res, "htest")
  expect_true(is.numeric(t_res$statistic))
})

test_that("Non-régression : ANOVA 1 facteur sur dataset issu du workspace", {
  test_env <- new.env(parent = emptyenv())
  test_env$data_aov <- data.frame(
    Traitement = factor(rep(c("T1", "T2", "T3"), each = 10)),
    Reponse = c(rnorm(10, 10), rnorm(10, 15), rnorm(10, 20))
  )
  
  df <- ramses_get_workspace_dataset("data_aov", envir = test_env)
  mod <- aov(Reponse ~ Traitement, data = df)
  s <- summary(mod)
  expect_equal(length(s), 1)
})

test_that("Non-régression : ANOVA à mesures répétées sur dataset issu du workspace", {
  test_env <- new.env(parent = emptyenv())
  test_env$df_ref_rm <- data.frame(
    Sujet = factor(rep(1:10, times = 3)),
    Temps = factor(rep(c("T1", "T2", "T3"), each = 10)),
    Y = c(10, 12, 11, 9, 14, 13, 10, 12, 11, 15,
          13, 15, 14, 12, 18, 15, 14, 16, 13, 17,
          18, 20, 17, 19, 24, 21, 18, 22, 19, 23)
  )
  
  df <- ramses_get_workspace_dataset("df_ref_rm", envir = test_env)
  rm_res <- ramses_anova_rm(df, response = "Y", subject = "Sujet", within_factor = "Temps")
  expect_equal(rm_res$anova_table$F_value[rm_res$anova_table$Source == "Temps"], 298.3171, tolerance = 1e-3)
})

test_that("Non-régression : Kruskal-Wallis sur dataset issu du workspace", {
  test_env <- new.env(parent = emptyenv())
  test_env$data_kw <- data.frame(
    Groupe = factor(rep(c("A", "B", "C"), each = 8)),
    Valeur = c(1:8, 5:12, 10:17)
  )
  
  df <- ramses_get_workspace_dataset("data_kw", envir = test_env)
  kw_res <- kruskal.test(Valeur ~ Groupe, data = df)
  expect_s3_class(kw_res, "htest")
  expect_true(kw_res$p.value < 0.05)
})

test_that("Non-régression : ANCOVA sur dataset issu du workspace", {
  test_env <- new.env(parent = emptyenv())
  test_env$data_anc <- data.frame(
    Groupe = factor(rep(c("Ctrl", "Treat"), each = 15)),
    Covariable = rnorm(30, mean = 50, sd = 5),
    Outcome = rnorm(30, mean = 100, sd = 10)
  )
  
  df <- ramses_get_workspace_dataset("data_anc", envir = test_env)
  anc_mod <- lm(Outcome ~ Covariable + Groupe, data = df)
  expect_s3_class(anc_mod, "lm")
  expect_equal(length(coef(anc_mod)), 3)
})
