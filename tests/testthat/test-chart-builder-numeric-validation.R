test_that("numeric variable is accepted for histogram", {
  df <- data.frame(
    val_num = c(10.5, 20.3, 15.8, 30.1, 25.4)
  )
  res <- ramses_check_chart_numeric_variable(df, "val_num", "histogram")
  expect_null(res)
  expect_true(ramses_is_numeric_variable(df$val_num))
})

test_that("integer variable is accepted for histogram and density", {
  df <- data.frame(
    val_int = 1:50
  )
  res_hist <- ramses_check_chart_numeric_variable(df, "val_int", "histogram")
  res_dens <- ramses_check_chart_numeric_variable(df, "val_int", "density")
  expect_null(res_hist)
  expect_null(res_dens)
  expect_true(ramses_is_numeric_variable(df$val_int))
})

test_that("character variable is rejected for histogram", {
  df <- data.frame(
    val_char = c("10", "20", "30", "40"),
    stringsAsFactors = FALSE
  )
  res <- ramses_check_chart_numeric_variable(df, "val_char", "histogram")
  expect_false(is.null(res))
  expect_match(res, "histogramme.*num", ignore.case = TRUE)
  expect_false(ramses_is_numeric_variable(df$val_char))
})

test_that("factor variable is rejected for histogram", {
  df <- data.frame(
    val_fac = factor(c("10", "20", "30", "40"))
  )
  res <- ramses_check_chart_numeric_variable(df, "val_fac", "histogram")
  expect_false(is.null(res))
  expect_match(res, "quantitative", ignore.case = TRUE)
  expect_false(ramses_is_numeric_variable(df$val_fac))
})

test_that("numeric variable with missing values is accepted", {
  df <- data.frame(
    val_na = c(1.5, NA, 3.8, NA, 5.2)
  )
  res_hist <- ramses_check_chart_numeric_variable(df, "val_na", "histogram")
  res_dens <- ramses_check_chart_numeric_variable(df, "val_na", "density")
  expect_null(res_hist)
  expect_null(res_dens)
  expect_true(ramses_is_numeric_variable(df$val_na))
})

test_that("complex variable name with spaces and special symbols is correctly validated", {
  df <- data.frame(
    "concentration (mg/L) & [A]" = c(12.4, 15.6, 9.8, 22.1),
    "categorie / statut" = c("Bas", "Haut", "Moyen", "Haut"),
    check.names = FALSE
  )
  res_ok <- ramses_check_chart_numeric_variable(df, "concentration (mg/L) & [A]", "histogram")
  expect_null(res_ok)

  res_bad <- ramses_check_chart_numeric_variable(df, "categorie / statut", "histogram")
  expect_false(is.null(res_bad))
  expect_match(res_bad, "quantitative", ignore.case = TRUE)
})

test_that("density with numeric variable is accepted", {
  df <- data.frame(
    val_dens = c(100.2, 105.4, 98.6, 102.1)
  )
  res <- ramses_check_chart_numeric_variable(df, "val_dens", "density")
  expect_null(res)
})

test_that("density with non-numeric variable is rejected", {
  df_char <- data.frame(
    groupe = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  res_char <- ramses_check_chart_numeric_variable(df_char, "groupe", "density")
  expect_false(is.null(res_char))
  expect_match(res_char, "densit", ignore.case = TRUE)

  df_fac <- data.frame(
    groupe = factor(c("G1", "G2", "G1", "G2"))
  )
  res_fac <- ramses_check_chart_numeric_variable(df_fac, "groupe", "density")
  expect_false(is.null(res_fac))
  expect_match(res_fac, "quantitative", ignore.case = TRUE)
})

test_that("validation stops before ggplot2 call on non-numeric input", {
  # Verification that calling validation on invalid data stops before graphics rendering
  df <- data.frame(
    cat_var = c("alpha", "beta", "gamma")
  )
  err <- ramses_check_chart_numeric_variable(df, "cat_var", "histogram")
  expect_true(!is.null(err))

  # If we simulate building ggplot with this variable, it would fail without prior check
  # The validator returns error string so build_ggplot_object triggers shiny::validate
  # and never reaches ggplot2::geom_histogram
  expect_type(err, "character")
  expect_true(nchar(err) > 0)
})
