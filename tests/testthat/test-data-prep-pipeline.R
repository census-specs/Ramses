# ==============================================================================
# Tests unitaires : Chaining et moteur de pipeline de preparation
# Fichier : tests/testthat/test-data-prep-pipeline.R
# ==============================================================================

testthat::context("Pipeline et reproductibilite des transformations Ramses")

testthat::test_that("ramses_prep_apply_pipeline enchaine correctement plusieurs transformations", {
  raw_df <- data.frame(
    `Nom Client` = c("  Alice  ", "  Bob  ", "Charlie", "Bob"),
    `Age` = c(25, 30, NA, 30),
    `Montant Achat` = c(100, 200, 150, 200),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Definition d'un pipeline complet
  pipeline <- list(
    list(type = "trim_ws", params = list(vars = "Nom Client", mode = "both")),
    list(type = "deduplicate", params = list(vars = NULL, keep = "first")),
    list(type = "impute_na", params = list(vars = "Age", method = "mean")),
    list(type = "rename", params = list(old_name = "Nom Client", new_name = "nom_propre")),
    list(type = "compute", params = list(new_var = "tva", expr_str = "`Montant Achat` * 0.2"))
  )

  pipe_res <- ramses_prep_apply_pipeline(raw_df, pipeline)

  testthat::expect_type(pipe_res, "list")
  testthat::expect_equal(pipe_res$n_steps, 5)

  final_df <- pipe_res$df
  testthat::expect_equal(nrow(final_df), 3) # 1 doublon retire
  testthat::expect_true("nom_propre" %in% names(final_df))
  testthat::expect_true("tva" %in% names(final_df))
  testthat::expect_equal(final_df$nom_propre[1], "Alice")
  testthat::expect_false(any(is.na(final_df$Age)))
  testthat::expect_equal(final_df$tva, c(20, 40, 30))

  # Verification que le code genere est non-vide et contient les etapes
  testthat::expect_true(nzchar(pipe_res$code))
  testthat::expect_match(pipe_res$code, "trimws")
  testthat::expect_match(pipe_res$code, "duplicated")
  testthat::expect_match(pipe_res$code, "nom_propre")
})
