test_that("safe R symbols handle non-syntactic names", {
  expect_equal(ramses_code_symbol("age"), "age")
  expect_equal(ramses_code_symbol("revenu mensuel"), "`revenu mensuel`")
  expect_equal(ramses_code_symbol("rendement_kg/ha"), "`rendement_kg/ha`")
  expect_equal(ramses_code_symbol("traitement (A/B)"), "`traitement (A/B)`")
  expect_equal(ramses_code_symbol("âge"), "âge")
})

test_that("safe R strings escape special characters", {
  expect_equal(ramses_code_string("A"), '"A"')
  expect_equal(ramses_code_string('modalité "A/B"'), '"modalité \\\"A/B\\\""')
  expect_equal(ramses_code_string("ligne\n2"), '"ligne\\n2"')
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

test_that("generated formulas and expressions are parseable", {
  formula_code <- ramses_formula_code(
    "rendement_kg/ha",
    c("revenu mensuel", "traitement (A/B)")
  )
  expect_silent(parse(text = formula_code))

  column_code <- ramses_code_column("mes donnees", "rendement_kg/ha")
  expect_silent(parse(text = column_code))

  string_code <- ramses_code_string('A "special" value')
  expect_silent(parse(text = string_code))
})
