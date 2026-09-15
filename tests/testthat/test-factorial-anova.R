# Test suite for 2-Way Factorial ANOVA in Ramses

testthat::test_that("ramses_anova_factorial handles balanced 2-way ANOVA without interaction (Type II)", {
  # Dataset: ToothGrowth balanced subset or synthetic balanced design
  set.seed(42)
  df <- data.frame(
    yield = c(20, 22, 19, 21, 28, 30, 27, 29, 25, 26, 24, 27, 35, 36, 33, 38),
    variety = factor(rep(c("V1", "V2"), each = 8)),
    treatment = factor(rep(rep(c("T1", "T2"), each = 4), 2))
  )

  res <- ramses_anova_factorial(df, "yield", "variety", "treatment", interaction = FALSE, alpha = 0.05)

  testthat::expect_s3_class(res, "ramses_anova_factorial")
  testthat::expect_false(res$has_interaction)
  testthat::expect_equal(res$type_ss, "Type II")
  testthat::expect_named(res$terms_info, c("variety", "treatment", "Residuals"))

  # Verification of SS, Df and F against drop1(lm)
  mod_ref <- stats::lm(yield ~ variety + treatment, data = df)
  d1 <- stats::drop1(mod_ref, test = "F")

  testthat::expect_equal(res$terms_info$variety$df, d1["variety", "Df"])
  testthat::expect_equal(res$terms_info$variety$f_value, d1["variety", "F value"], tolerance = 1e-4)
  testthat::expect_equal(res$terms_info$variety$p_value, d1["variety", "Pr(>F)"], tolerance = 1e-4)
  
  # Eta squared partial calculation
  ss_var <- res$terms_info$variety$sum_sq
  ss_res <- res$terms_info$Residuals$sum_sq
  testthat::expect_equal(res$terms_info$variety$eta_p_sq, ss_var / (ss_var + ss_res), tolerance = 1e-4)
})

testthat::test_that("ramses_anova_factorial handles balanced 2-way ANOVA with interaction (Type III)", {
  df <- data.frame(
    yield = c(20, 22, 19, 21, 28, 30, 27, 29, 25, 26, 24, 27, 35, 36, 33, 38),
    variety = factor(rep(c("V1", "V2"), each = 8)),
    treatment = factor(rep(rep(c("T1", "T2"), each = 4), 2))
  )

  res <- ramses_anova_factorial(df, "yield", "variety", "treatment", interaction = TRUE, alpha = 0.05)

  testthat::expect_true(res$has_interaction)
  testthat::expect_equal(res$type_ss, "Type III")
  testthat::expect_named(res$terms_info, c("variety", "treatment", "variety:treatment", "Residuals"))

  # Reference with contr.sum
  df_ref <- df
  stats::contrasts(df_ref$variety) <- stats::contr.sum
  stats::contrasts(df_ref$treatment) <- stats::contr.sum
  mod_ref <- stats::lm(yield ~ variety * treatment, data = df_ref)
  d1 <- stats::drop1(mod_ref, ~ ., test = "F")

  testthat::expect_equal(res$terms_info$variety$f_value, d1["variety", "F value"], tolerance = 1e-4)
  testthat::expect_equal(res$terms_info$treatment$f_value, d1["treatment", "F value"], tolerance = 1e-4)
  testthat::expect_equal(res$terms_info[["variety:treatment"]]$f_value, d1["variety:treatment", "F value"], tolerance = 1e-4)
})

testthat::test_that("ramses_anova_factorial handles unbalanced designs with Type III SS properly", {
  # Unbalanced dataset
  df_unbal <- data.frame(
    len = c(4.2, 11.5, 7.3, 5.8, 6.4, 20.0, 16.5, 14.2, 15.3, 21.0, 25.5, 26.4),
    supp = factor(c("OJ", "OJ", "OJ", "OJ", "VC", "VC", "VC", "VC", "VC", "VC", "VC", "VC")),
    dose = factor(c("D1", "D1", "D2", "D2", "D1", "D1", "D1", "D2", "D2", "D2", "D2", "D2"))
  )

  # Check global options before
  opt_before <- options("contrasts")[[1]]

  res <- ramses_anova_factorial(df_unbal, "len", "supp", "dose", interaction = TRUE, alpha = 0.05)

  # Global options must remain UNCHANGED
  opt_after <- options("contrasts")[[1]]
  testthat::expect_identical(opt_before, opt_after)

  testthat::expect_equal(res$n_obs, nrow(df_unbal))
  testthat::expect_true(res$has_interaction)
})

testthat::test_that("ramses_anova_factorial automatically converts character and numeric inputs to factors", {
  df_raw <- data.frame(
    response = c(10, 12, 11, 14, 20, 22, 21, 23),
    factor_num = c(1, 1, 2, 2, 1, 1, 2, 2),
    factor_chr = c("A", "A", "A", "A", "B", "B", "B", "B")
  )

  res <- ramses_anova_factorial(df_raw, "response", "factor_num", "factor_chr", interaction = TRUE)

  testthat::expect_equal(res$terms_info$factor_num$df, 1)
  testthat::expect_equal(res$terms_info$factor_chr$df, 1)
  testthat::expect_equal(res$terms_info[["factor_num:factor_chr"]]$df, 1)
})

testthat::test_that("ramses_anova_factorial computes diagnostics (normality and homoscedasticity)", {
  set.seed(123)
  df <- data.frame(
    y = rnorm(24, mean = 50, sd = 5),
    f1 = factor(rep(c("A", "B", "C"), each = 8)),
    f2 = factor(rep(rep(c("X", "Y"), each = 4), 3))
  )

  res <- ramses_anova_factorial(df, "y", "f1", "f2", interaction = TRUE)

  testthat::expect_false(is.null(res$diagnostics$shapiro))
  testthat::expect_false(is.null(res$diagnostics$fligner))
  testthat::expect_true(is.numeric(res$diagnostics$shapiro$p_value))
  testthat::expect_true(is.numeric(res$diagnostics$fligner$p_value))
})

testthat::test_that("ramses_anova_factorial validates errors and edge cases", {
  df_empty <- data.frame(y = numeric(0), a = character(0), b = character(0))
  testthat::expect_error(ramses_anova_factorial(df_empty, "y", "a", "b"))

  # Factor with only 1 level
  df_one_level <- data.frame(
    y = c(1, 2, 3, 4),
    a = c("A1", "A1", "A1", "A1"),
    b = c("B1", "B2", "B1", "B2")
  )
  testthat::expect_error(ramses_anova_factorial(df_one_level, "y", "a", "b"))
})

testthat::test_that("ramses_qualify_eta_p_sq correctly qualifies effect sizes", {
  testthat::expect_equal(ramses_qualify_eta_p_sq(0.005), "effet n\u00e9gligeable")
  testthat::expect_equal(ramses_qualify_eta_p_sq(0.03), "effet faible")
  testthat::expect_equal(ramses_qualify_eta_p_sq(0.09), "effet moyen")
  testthat::expect_equal(ramses_qualify_eta_p_sq(0.20), "effet fort")
})

testthat::test_that("1-way ANOVA and Kruskal-Wallis regression safety", {
  # Verify standard 1-way ANOVA still behaves identically
  data(PlantGrowth)
  aov_fit <- stats::aov(weight ~ group, data = PlantGrowth)
  es <- ramses_compute_effect_size("anova", stat_result = aov_fit)
  testthat::expect_equal(es$name, "Eta-carr\u00e9 (\u03b7\u00b2)")

  # Verify Kruskal-Wallis
  kw_fit <- stats::kruskal.test(weight ~ group, data = PlantGrowth)
  es_kw <- ramses_compute_effect_size("kruskal", stat_result = kw_fit, y1 = PlantGrowth$weight)
  testthat::expect_equal(es_kw$name, "Eta-carr\u00e9 H (\u03b7\u00b2_H)")
})
