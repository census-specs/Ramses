test_that("safe R symbols handle non-syntactic names", {
  expect_equal(ramses_code_symbol("age"), "age")
  expect_equal(ramses_code_symbol("revenu mensuel"), "`revenu mensuel`")
  expect_equal(ramses_code_symbol("rendement_kg/ha"), "`rendement_kg/ha`")
  expect_equal(ramses_code_symbol("traitement (A/B)"), "`traitement (A/B)`")
  expect_equal(ramses_code_symbol("âge"), "`âge`")
})

test_that("safe dataframe column code preserves the original column name", {
  expect_equal(
    ramses_code_column("donnees", "revenu mensuel"),
    "donnees[[\"revenu mensuel\"]]"
  )
  expect_equal(
    ramses_code_column("mes donnees", "rendement_kg/ha"),
    "`mes donnees`[[\"rendement_kg/ha\"]]"
  )
})

test_that("safe formulas do not interpret punctuation as operators", {
  fml <- ramses_formula("revenu mensuel", "traitement (A/B)")
  expect_equal(
    rlang::expr_text(fml, width = Inf),
    "`revenu mensuel` ~ `traitement (A/B)`"
  )

  fml_multi <- ramses_formula(
    "rendement_kg/ha",
    c("revenu mensuel", "âge", "traitement (A/B)")
  )
  expect_equal(
    rlang::expr_text(fml_multi, width = Inf),
    "`rendement_kg/ha` ~ `revenu mensuel` + âge + `traitement (A/B)`"
  )
})
