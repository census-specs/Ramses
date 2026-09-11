test_that("ramses_compute_effect_size computes Cohen's d correctly for t_indep", {
  set.seed(42)
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 12, sd = 2)
  
  res_eq <- ramses_compute_effect_size("t_indep", y1 = x, y2 = y, var_equal = TRUE)
  expect_false(is.null(res_eq))
  expect_equal(res_eq$name, "d de Cohen")
  expect_true(is.numeric(res_eq$value))
  expect_true(nzchar(res_eq$magnitude))
  
  res_welch <- ramses_compute_effect_size("t_indep", y1 = x, y2 = y, var_equal = FALSE)
  expect_false(is.null(res_welch))
  expect_equal(res_welch$name, "d de Cohen")
  expect_true(is.numeric(res_welch$value))
  expect_true(nzchar(res_welch$magnitude))

  # Cas avec valeurs manquantes
  x_na <- c(x, NA, NA)
  y_na <- c(NA, y, NA)
  res_na_eq <- ramses_compute_effect_size("t_indep", y1 = x_na, y2 = y_na, var_equal = TRUE)
  expect_false(is.null(res_na_eq))
  expect_true(is.numeric(res_na_eq$value))
  expect_equal(res_na_eq$value, res_eq$value)

  res_na_welch <- ramses_compute_effect_size("t_indep", y1 = x_na, y2 = y_na, var_equal = FALSE)
  expect_false(is.null(res_na_welch))
  expect_true(is.numeric(res_na_welch$value))
  expect_equal(res_na_welch$value, res_welch$value)
})

test_that("ramses_compute_effect_size computes effect sizes for one-sample and paired t-tests", {
  set.seed(123)
  x <- rnorm(25, mean = 5.5, sd = 1)
  y <- rnorm(25, mean = 6.0, sd = 1)
  
  res_one <- ramses_compute_effect_size("t_one_sample", y1 = x, mu = 5.0)
  expect_false(is.null(res_one))
  expect_true(is.numeric(res_one$value))
  
  res_paired <- ramses_compute_effect_size("t_paired", y1 = x, y2 = y)
  expect_false(is.null(res_paired))
  expect_true(is.numeric(res_paired$value))
})

test_that("ramses_compute_effect_size computes eta-squared for ANOVA and Kruskal", {
  df <- data.frame(
    val = c(rnorm(10, 5), rnorm(10, 8), rnorm(10, 11)),
    grp = factor(rep(c("A", "B", "C"), each = 10))
  )
  aov_fit <- stats::aov(val ~ grp, data = df)
  es_aov <- ramses_compute_effect_size("anova", stat_result = aov_fit)
  expect_false(is.null(es_aov))
  expect_true(es_aov$value >= 0 && es_aov$value <= 1)
  
  krusk_res <- stats::kruskal.test(val ~ grp, data = df)
  es_krusk <- ramses_compute_effect_size("kruskal", stat_result = krusk_res, y1 = df$val)
  expect_false(is.null(es_krusk))
  expect_true(is.numeric(es_krusk$value))
})

test_that("ramses_compute_effect_size computes Cramer's V for contingency tables", {
  tab <- matrix(c(20, 5, 10, 25), nrow = 2)
  chisq_res <- stats::chisq.test(tab)
  es_chi <- ramses_compute_effect_size("chisq", stat_result = chisq_res, tab = tab)
  expect_false(is.null(es_chi))
  expect_true(es_chi$value >= 0 && es_chi$value <= 1)
})

test_that("ramses_compute_effect_size computes correlation effect sizes", {
  set.seed(42)
  x <- rnorm(30)
  y <- 0.7 * x + rnorm(30, sd = 0.5)
  
  # Pearson via objet stat_result
  cor_res <- stats::cor.test(x, y, method = "pearson")
  es_cor <- ramses_compute_effect_size("cor_pearson", stat_result = cor_res)
  expect_false(is.null(es_cor))
  expect_true(is.numeric(es_cor$value))
  expect_equal(es_cor$value, unname(cor_res$estimate))
  expect_true(nzchar(es_cor$magnitude))

  # Coherence avec cor() direct
  es_direct <- ramses_compute_effect_size("cor_pearson", y1 = x, y2 = y)
  expect_false(is.null(es_direct))
  expect_true(is.numeric(es_direct$value))
  expect_equal(es_direct$value, stats::cor(x, y, method = "pearson"))

  # Gestion des valeurs manquantes
  x_na <- c(x, NA, NA)
  y_na <- c(NA, y, NA)
  es_na <- ramses_compute_effect_size("cor_pearson", y1 = x_na, y2 = y_na)
  expect_false(is.null(es_na))
  expect_true(is.numeric(es_na$value))
  expect_equal(es_na$value, stats::cor(x_na, y_na, use = "complete.obs", method = "pearson"))

  # Verification pour Spearman et Kendall
  cor_spear <- stats::cor.test(x, y, method = "spearman", exact = FALSE)
  es_spear <- ramses_compute_effect_size("cor_spearman", stat_result = cor_spear)
  expect_false(is.null(es_spear))
  expect_true(is.numeric(es_spear$value))
  expect_equal(es_spear$value, unname(cor_spear$estimate))

  cor_kend <- stats::cor.test(x, y, method = "kendall", exact = FALSE)
  es_kend <- ramses_compute_effect_size("cor_kendall", stat_result = cor_kend)
  expect_false(is.null(es_kend))
  expect_true(is.numeric(es_kend$value))
  expect_equal(es_kend$value, unname(cor_kend$estimate))
})

test_that("ramses_compute_power calculates post-hoc power correctly", {
  set.seed(42)
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 12, sd = 2)
  
  pwr_indep <- ramses_compute_power("t_indep", y1 = x, y2 = y, alpha = 0.05)
  expect_false(is.null(pwr_indep))
  expect_true(pwr_indep$value >= 0 && pwr_indep$value <= 1)
  expect_true(nzchar(pwr_indep$percentage))
  
  pwr_paired <- ramses_compute_power("t_paired", y1 = x, y2 = y, alpha = 0.05)
  expect_false(is.null(pwr_paired))
  expect_true(pwr_paired$value >= 0 && pwr_paired$value <= 1)
  
  pwr_one <- ramses_compute_power("t_one_sample", y1 = x, mu = 9.0, alpha = 0.05)
  expect_false(is.null(pwr_one))
  expect_true(pwr_one$value >= 0 && pwr_one$value <= 1)
  
  # Correlation power
  cor_res <- stats::cor.test(x, y, method = "pearson")
  pwr_cor <- ramses_compute_power("cor_pearson", stat_result = cor_res, y1 = x, y2 = y, alpha = 0.05)
  expect_false(is.null(pwr_cor))
  expect_true(pwr_cor$value >= 0 && pwr_cor$value <= 1)
})
