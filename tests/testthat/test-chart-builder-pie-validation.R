test_that("categorical variable is accepted for pie chart", {
  df <- data.frame(
    val_cat = factor(c("A", "B", "A", "C", "B"))
  )
  res <- ramses_check_chart_pie_variable(df, "val_cat")
  expect_null(res)
})

test_that("character variable is accepted for pie chart", {
  df <- data.frame(
    val_char = c("Homme", "Femme", "Homme"),
    stringsAsFactors = FALSE
  )
  res <- ramses_check_chart_pie_variable(df, "val_char")
  expect_null(res)
})

test_that("numeric variable with too many unique values is rejected for pie chart", {
  df <- data.frame(
    val_num = 1:50
  )
  res <- ramses_check_chart_pie_variable(df, "val_num")
  expect_false(is.null(res))
  expect_match(res, "trop de modalit\u00e9s")
})

test_that("character variable with too many unique values is rejected for pie chart", {
  df <- data.frame(
    val_char = paste0("ID_", 1:50)
  )
  res <- ramses_check_chart_pie_variable(df, "val_char")
  expect_false(is.null(res))
  expect_match(res, "trop de modalit\u00e9s")
})

test_that("numeric variable with few unique values is accepted for pie chart", {
  df <- data.frame(
    val_num = c(1, 2, 1, 3, 2, 1, 1, 2, 3)
  )
  res <- ramses_check_chart_pie_variable(df, "val_num")
  expect_null(res)
})

test_that("pie chart variable with missing values is handled correctly", {
  df <- data.frame(
    val_na = factor(c("A", NA, "B", NA, "C"))
  )
  res <- ramses_check_chart_pie_variable(df, "val_na")
  expect_null(res)
})

test_that("pie chart rejects 1 single non-NA category", {
  df <- data.frame(
    val_single = c("Oui", "Oui", NA, "Oui")
  )
  res <- ramses_check_chart_pie_variable(df, "val_single")
  expect_false(is.null(res))
  expect_match(res, "une seule modalit\u00e9")
})
