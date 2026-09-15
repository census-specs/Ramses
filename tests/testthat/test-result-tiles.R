test_that("ramses_result_tiles returns NULL on empty or NULL input", {
  expect_null(ramses_result_tiles(NULL))
  expect_null(ramses_result_tiles(list()))
  expect_null(ramses_result_tiles(list(NULL)))
})

test_that("ramses_result_tiles renders valid Bootstrap structure and cards", {
  tiles_data <- list(
    list(label = "Moyenne", value = "12,4", subtext = "Unit\u00e9 : kg"),
    list(label = "\u00c9cart-type", value = "2,31"),
    list(label = "t", value = "4,21", status = "primary"),
    list(label = "p-value", value = "0,002", status = "success")
  )

  res <- ramses_result_tiles(tiles_data, title = "Indicateurs cl\u00e9s")
  res_str <- as.character(res)

  expect_true(grepl("Moyenne", res_str))
  expect_true(grepl("12,4", res_str))
  expect_true(grepl("Unit\u00e9 : kg", res_str))
  expect_true(grepl("\u00c9cart-type", res_str))
  expect_true(grepl("2,31", res_str))
  expect_true(grepl("4,21", res_str))
  expect_true(grepl("0,002", res_str))
  expect_true(grepl("Indicateurs cl\u00e9s", res_str))
  expect_true(grepl("col-", res_str))
  expect_true(grepl("p-2 bg-white rounded border shadow-sm", res_str))
})

test_that("ramses_result_tiles handles NA and missing values gracefully", {
  tiles_data <- list(
    list(label = "Test NA", value = NA, subtext = NA),
    list(label = "Test vide", value = NULL)
  )

  res <- ramses_result_tiles(tiles_data)
  res_str <- as.character(res)

  expect_true(grepl("Test NA", res_str))
  expect_true(grepl("\u2014", res_str)) # em-dash for NA
})
