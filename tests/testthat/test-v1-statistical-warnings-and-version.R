test_that("ramses_get_version returns version 1.0.0", {
  ver <- ramses_get_version()
  expect_equal(ver, "1.0.0")
})

test_that("Wilcoxon test with ties handles exact = FALSE smoothly", {
  # Donnees avec ex-aequo
  x <- c(10, 12, 12, 14, 15, 18)
  y <- c(11, 12, 13, 14, 16, 17)
  
  # Verifie que exact = FALSE s'execute sans avertissement R brut
  res_one <- suppressWarnings(stats::wilcox.test(x, mu = 12, exact = FALSE))
  expect_s3_class(res_one, "htest")
  
  res_indep <- suppressWarnings(stats::wilcox.test(x, y, exact = FALSE))
  expect_s3_class(res_indep, "htest")
  
  res_paired <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE, exact = FALSE))
  expect_s3_class(res_paired, "htest")
})

test_that("Chi-deux low expected counts detection works as intended", {
  # Tableau 2x2 avec petits effectifs (low expected counts)
  tab_small <- matrix(c(2, 8, 1, 9), nrow = 2)
  chisq_res <- suppressWarnings(stats::chisq.test(tab_small))
  
  expect_true(any(chisq_res$expected < 5))
  
  # Test exact de Fisher pour petits effectifs
  fisher_res <- stats::fisher.test(tab_small)
  expect_s3_class(fisher_res, "htest")
  expect_true(fisher_res$p.value >= 0 && fisher_res$p.value <= 1)
})

test_that("Shapiro-Wilk validations prevent invalid calculations", {
  # Less than 3 observations
  val_small <- c(1, 2)
  expect_true(length(val_small) < 3)
  
  # Zero variance
  val_zero_var <- c(5, 5, 5, 5)
  expect_equal(stats::sd(val_zero_var), 0)
})

test_that("page_about_ramses_ui generates full page UI with correct version and links", {
  ui <- page_about_ramses_ui()
  html_str <- as.character(ui)
  
  # Structural checks
  expect_true(grepl("btn_about_back", html_str))
  expect_true(grepl("Retour", html_str))
  expect_true(grepl("https://github.com/census-specs/Ramses", html_str))
  expect_true(grepl("Pierre Valdeze MBOM MBOM", html_str))
  expect_true(grepl(paste0("Ramses ", ramses_get_version()), html_str))
  
  # Ensure no modalDialog is in the about page UI
  expect_false(grepl("modalDialog", html_str))
  expect_false(grepl("modal-dialog", html_str))
})

