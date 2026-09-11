test_that("R directory is 100% pure ASCII with unicode escapes", {
  r_files <- list.files(
    path = system.file("R", package = "Ramses"),
    pattern = "\\.[Rr]$",
    full.names = TRUE
  )

  if (length(r_files) == 0) {
    r_files <- list.files("../../R", pattern = "\\.[Rr]$", full.names = TRUE)
  }
  if (length(r_files) == 0) {
    r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
  }

  expect_gt(length(r_files), 0)

  for (f in r_files) {
    raw_bytes <- readBin(f, "raw", file.info(f)$size)
    non_ascii <- raw_bytes[raw_bytes > as.raw(127)]
    expect_equal(
      length(non_ascii), 0,
      info = paste("Non-ASCII bytes found in", basename(f))
    )
  }
})

test_that("Unicode escapes evaluate to exact French text", {
  expect_equal(eval(parse(text = '"Statistiques descriptives"')), "Statistiques descriptives")
  expect_equal(eval(parse(text = '"M\u00e9diane"')), "Médiane")
  expect_equal(eval(parse(text = '"Donn\u00e9es"')), "Données")
  expect_equal(eval(parse(text = '"R\u00b2"')), "R²")
  expect_equal(eval(parse(text = '"Qualit\u00e9 globale"')), "Qualité globale")
})
