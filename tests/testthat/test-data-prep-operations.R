# ==============================================================================
# Tests unitaires : Operations de preparation des donnees
# Fichier : tests/testthat/test-data-prep-operations.R
# ==============================================================================

testthat::context("Operations de preparation des donnees Ramses")

testthat::test_that("ramses_prep_diagnose fonctionne sur un dataframe standard et complexe", {
  df <- data.frame(
    `age du patient` = c(25, 30, NA, 40, 30),
    `groupe traitement` = factor(c("A", "B", "A", "B", "B")),
    `ville resid` = c(" Paris ", "Lyon", "Marseille", NA, "Lyon"),
    `statut` = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  diag <- ramses_prep_diagnose(df)
  testthat::expect_type(diag, "list")
  testthat::expect_equal(diag$overview$n_rows, 5)
  testthat::expect_equal(diag$overview$n_cols, 4)
  testthat::expect_equal(diag$overview$n_duplicates, 1)
  testthat::expect_equal(diag$overview$total_nas, 2)
  testthat::expect_equal(diag$overview$total_outliers, 0)
  testthat::expect_equal(nrow(diag$columns), 4)
  testthat::expect_true("age du patient" %in% diag$columns$column)

  # Avec valeurs aberrantes
  df_outliers <- data.frame(val = c(10, 12, 11, 13, 100, 11, 12))
  diag_out <- ramses_prep_diagnose(df_outliers)
  testthat::expect_equal(diag_out$overview$total_outliers, 1)
})

testthat::test_that("ramses_prep_rename renomme correctement les variables avec caracteres speciaux", {
  df <- data.frame(`col a` = 1:5, `col b` = 6:10, check.names = FALSE)

  res <- ramses_prep_rename(df, "col a", "col_nouvelle (kg)")
  testthat::expect_true("col_nouvelle (kg)" %in% names(res$df))
  testthat::expect_false("col a" %in% names(res$df))
  testthat::expect_match(res$code, "names\\(dataset\\)")

  # Erreur si nom inexistant
  testthat::expect_error(ramses_prep_rename(df, "col_inconnue", "test"))
})

testthat::test_that("ramses_prep_drop supprime les variables correctement", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  res <- ramses_prep_drop(df, c("a", "c"))
  testthat::expect_equal(names(res$df), "b")
  testthat::expect_equal(ncol(res$df), 1)
})

testthat::test_that("ramses_prep_reorder et ramses_prep_select_vars fonctionnent", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  reord <- ramses_prep_reorder(df, c("c", "b"))
  testthat::expect_equal(names(reord$df), c("c", "b", "a"))

  sel <- ramses_prep_select_vars(df, c("c", "a"))
  testthat::expect_equal(names(sel$df), c("c", "a"))
  testthat::expect_error(ramses_prep_select_vars(df, character(0)))
})

testthat::test_that("ramses_prep_cast convertit tous les types de base", {
  df <- data.frame(
    num_str = c("10.5", "20.2", "30.0"),
    factor_col = c("Faible", "Moyen", "Eleve"),
    date_str = c("2024-01-15", "2024-02-20", "2024-03-25"),
    bool_str = c("TRUE", "FALSE", "TRUE"),
    stringsAsFactors = FALSE
  )

  # Cast vers numeric
  c_num <- ramses_prep_cast(df, "num_str", "numeric")
  testthat::expect_true(is.numeric(c_num$df$num_str))
  testthat::expect_equal(c_num$df$num_str[1], 10.5)

  # Cast vers factor
  c_fac <- ramses_prep_cast(df, "factor_col", "factor")
  testthat::expect_true(is.factor(c_fac$df$factor_col))

  # Cast vers Date
  c_date <- ramses_prep_cast(df, "date_str", "Date", date_format = "%Y-%m-%d")
  testthat::expect_true(inherits(c_date$df$date_str, "Date"))

  # Cast vers logical
  c_bool <- ramses_prep_cast(df, "bool_str", "logical")
  testthat::expect_true(is.logical(c_bool$df$bool_str))
})

testthat::test_that("ramses_prep_recode et ramses_prep_replace_value fonctionnent", {
  df <- data.frame(
    statut = c("H", "F", "H", "F", "Inconnu"),
    score = c(10, 20, 999, 40, NA),
    stringsAsFactors = FALSE
  )

  # Recodage
  rec <- ramses_prep_recode(df, "statut", list("H" = "Homme", "F" = "Femme"), default = "keep")
  testthat::expect_equal(rec$df$statut, c("Homme", "Femme", "Homme", "Femme", "Inconnu"))

  # Remplacement d'une valeur sentinelle par NA
  rep_val <- ramses_prep_replace_value(df, "score", old_val = 999, is_new_na = TRUE)
  testthat::expect_true(is.na(rep_val$df$score[3]))
})

testthat::test_that("ramses_prep_impute_na traite les valeurs manquantes selon toutes les methodes", {
  df <- data.frame(
    val1 = c(10, 20, NA, 40, 50),
    val2 = c("A", "B", "A", NA, "A"),
    stringsAsFactors = FALSE
  )

  # Suppression des lignes NA
  drop_res <- ramses_prep_impute_na(df, vars = "val1", method = "drop_rows")
  testthat::expect_equal(nrow(drop_res$df), 4)

  # Imputation fixe
  fix_res <- ramses_prep_impute_na(df, vars = "val1", method = "fixed", fixed_value = 0)
  testthat::expect_equal(fix_res$df$val1[3], 0)

  # Imputation par moyenne
  mean_res <- ramses_prep_impute_na(df, vars = "val1", method = "mean")
  testthat::expect_equal(mean_res$df$val1[3], mean(c(10, 20, 40, 50)))

  # Imputation par mediane
  med_res <- ramses_prep_impute_na(df, vars = "val1", method = "median")
  testthat::expect_equal(med_res$df$val1[3], stats::median(c(10, 20, 40, 50)))

  # Imputation par mode
  mode_res <- ramses_prep_impute_na(df, vars = "val2", method = "mode")
  testthat::expect_equal(mode_res$df$val2[4], "A")
})

testthat::test_that("ramses_prep_deduplicate supprime les doublons", {
  df <- data.frame(
    id = c(1, 2, 2, 3, 3),
    val = c("a", "b", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  # Dedup global
  dedup_all <- ramses_prep_deduplicate(df)
  testthat::expect_equal(nrow(dedup_all$df), 4)

  # Dedup par id
  dedup_id <- ramses_prep_deduplicate(df, vars = "id", keep = "first")
  testthat::expect_equal(nrow(dedup_id$df), 3)
})

testthat::test_that("ramses_prep_trim_ws et ramses_prep_change_case nettoient les chaines", {
  df <- data.frame(
    nom = c("  jean dupont  ", "MARIE   CURIE ", "  albert einstein"),
    stringsAsFactors = FALSE
  )

  # Trim
  trimmed <- ramses_prep_trim_ws(df, "nom", mode = "both")
  testthat::expect_equal(trimmed$df$nom[1], "jean dupont")

  # Squish
  squished <- ramses_prep_trim_ws(df, "nom", mode = "squish")
  testthat::expect_equal(squished$df$nom[2], "MARIE CURIE")

  # Casse
  lower_res <- ramses_prep_change_case(df, "nom", target_case = "lower")
  testthat::expect_true(grepl("marie", lower_res$df$nom[2]))

  upper_res <- ramses_prep_change_case(df, "nom", target_case = "upper")
  testthat::expect_true(grepl("JEAN", upper_res$df$nom[1]))

  title_res <- ramses_prep_change_case(df, "nom", target_case = "title")
  testthat::expect_true(grepl("Jean Dupont", title_res$df$nom[1]))
})

testthat::test_that("ramses_prep_filter et ramses_prep_sort fonctionnent", {
  df <- data.frame(
    age = c(25, 45, 18, 60, 30),
    ville = c("Paris", "Lyon", "Marseille", "Paris", "Bordeaux"),
    stringsAsFactors = FALSE
  )

  # Filtre simple
  filt_1 <- ramses_prep_filter(df, list(list(var = "age", op = "gte", val = 30)))
  testthat::expect_equal(nrow(filt_1$df), 3)

  # Filtre texte
  filt_txt <- ramses_prep_filter(df, list(list(var = "ville", op = "eq", val = "Paris")))
  testthat::expect_equal(nrow(filt_txt$df), 2)

  # Filtre multiple avec AND
  filt_and <- ramses_prep_filter(
    df,
    list(
      list(var = "age", op = "gt", val = 20),
      list(var = "ville", op = "eq", val = "Paris")
    ),
    combine_op = "AND"
  )
  testthat::expect_equal(nrow(filt_and$df), 2)

  # Tri
  sorted <- ramses_prep_sort(df, vars = "age", orders = "desc")
  testthat::expect_equal(sorted$df$age, c(60, 45, 30, 25, 18))
})
