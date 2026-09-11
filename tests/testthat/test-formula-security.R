test_that("ramses_code_symbol properly escapes syntactic and non-syntactic names", {
  expect_equal(ramses_code_symbol("age"), "age")
  expect_equal(ramses_code_symbol("revenu mensuel"), "`revenu mensuel`")
  expect_equal(ramses_code_symbol("rendement_kg/ha"), "`rendement_kg/ha`")
  expect_equal(ramses_code_symbol("traitement (A/B)"), "`traitement (A/B)`")
  expect_equal(ramses_code_symbol("parcelle : type"), "`parcelle : type`")
  expect_equal(ramses_code_symbol("revenu + charges"), "`revenu + charges`")
  expect_equal(ramses_code_symbol("variable-test"), "`variable-test`")
  expect_equal(ramses_code_symbol("x:y"), "`x:y`")
  expect_equal(ramses_code_symbol('nom avec "guillemets"'), '`nom avec "guillemets"`')
  expect_equal(ramses_code_symbol("l'eau"), "`l'eau`")
  expect_equal(ramses_code_symbol("a`b"), "`a\\`b`")
  expect_equal(ramses_code_symbol("ligne1\nligne2"), "`ligne1\\nligne2`")

  # Verification que tous les symboles generes sont du code R valide et parseable
  test_names <- c(
    "age",
    "revenu mensuel",
    "vari\u00e9t\u00e9",
    "l'eau",
    'nom avec "guillemets"',
    "a`b",
    "revenu + charges",
    "rendement_kg/ha",
    "traitement (A/B)",
    "ligne1\nligne2"
  )
  for (nm in test_names) {
    sym_code <- ramses_code_symbol(nm)
    parsed <- parse(text = sym_code)
    expect_equal(length(parsed), 1)
    expect_true(is.symbol(parsed[[1]]) || is.name(parsed[[1]]))
    expect_equal(as.character(parsed[[1]]), nm)
  }
})

test_that("ramses_code_column safely formats dataset column references", {
  expect_equal(ramses_code_column("df", "age"), 'df[["age"]]')
  expect_equal(ramses_code_column("df", "revenu mensuel"), 'df[["revenu mensuel"]]')
  expect_equal(ramses_code_column("data (1)", 'nom avec "guillemets"'), '`data (1)`[["nom avec \\"guillemets\\""]]')
})

test_that("ramses_formula builds valid formulas evaluated on datasets with special names", {
  # Create sample dataframe with complex non-syntactic column names
  df_test <- data.frame(
    `rendement_kg/ha` = c(10.5, 12.1, 14.3, 11.0, 15.2, 13.8),
    `traitement (A/B)` = factor(c("A", "A", "A", "B", "B", "B")),
    `revenu + charges` = c(100, 120, 140, 110, 150, 130),
    `parcelle : type` = factor(c("T1", "T2", "T1", "T2", "T1", "T2")),
    check.names = FALSE
  )

  # Two-group comparison formula
  fml_t <- ramses_formula(response = "rendement_kg/ha", terms = "traitement (A/B)")
  res_t <- stats::t.test(fml_t, data = df_test)
  expect_s3_class(res_t, "htest")

  # Regression formula with multiple non-syntactic predictors
  fml_lm <- ramses_formula(
    response = "rendement_kg/ha",
    terms = c("traitement (A/B)", "revenu + charges", "parcelle : type")
  )
  res_lm <- stats::lm(fml_lm, data = df_test)
  expect_s3_class(res_lm, "lm")

  # One-sided formula for Plotly/lattice
  fml_one <- ramses_formula(response = NULL, terms = "parcelle : type")
  expect_equal(length(fml_one), 2)
})

test_that("ramses_formula_code produces parseable and executable formula code strings", {
  code_str <- ramses_formula_code(
    response = "rendement_kg/ha",
    terms = c("traitement (A/B)", "revenu + charges")
  )
  expect_equal(code_str, "`rendement_kg/ha` ~ `traitement (A/B)` + `revenu + charges`")

  parsed_fml <- eval(parse(text = code_str))
  expect_s3_class(parsed_fml, "formula")
})
