test_that("ramses_render_lm_results outputs 5 structured pedagogical sections for linear regression", {
  fit <- stats::lm(mpg ~ hp, data = mtcars)
  reg_state <- list(
    model = fit,
    var_y = "mpg",
    vars_x = "hp",
    model_type = "linear",
    alpha = 0.05
  )

  ui <- ramses_render_lm_results(reg_state)
  ui_str <- as.character(ui)

  expect_true(grepl("1. Test global du mod", ui_str))
  expect_true(grepl("2. Qualit", ui_str))
  expect_true(grepl("3. Coefficients du mod", ui_str))
  expect_true(grepl("4. V", ui_str))
  expect_true(grepl("5. Conclusion et synth", ui_str))

  # Numerical indicators present
  smry <- summary(fit)
  fstat <- smry$fstatistic
  f_val <- round(fstat[1], 2)
  expect_true(grepl(as.character(f_val), ui_str))
  expect_true(grepl("R\u00b2 =", ui_str))
  expect_true(grepl("RMSE =", ui_str))
  expect_true(grepl("IC 95 %", ui_str))
  expect_true(grepl("Toutes choses", ui_str))
})

test_that("ramses_render_lm_results correctly identifies qualitative factor reference levels", {
  df_agri <- data.frame(
    rendement = c(2.1, 2.5, 2.8, 3.1, 3.4, 3.8, 4.2, 4.5, 4.9, 5.1),
    engrais = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    variete = factor(rep(c("Locale", "Amelioree_A"), each = 5), levels = c("Locale", "Amelioree_A"))
  )

  fit <- stats::lm(rendement ~ engrais + variete, data = df_agri)
  reg_state <- list(
    model = fit,
    var_y = "rendement",
    vars_x = c("engrais", "variete"),
    model_type = "linear",
    alpha = 0.05
  )

  ui <- ramses_render_lm_results(reg_state)
  ui_str <- as.character(ui)

  expect_true(grepl("variete : Amelioree_A vs Locale", ui_str))
  expect_true(grepl("modalit\u00e9 de r\u00e9f\u00e9rence", ui_str))
})

test_that("ramses_render_lm_results numerical values match R base lm summary and confint", {
  fit <- stats::lm(mpg ~ hp + wt, data = mtcars)
  smry <- summary(fit)
  ci <- stats::confint(fit, level = 0.95)
  rmse <- sqrt(mean(stats::residuals(fit)^2))

  reg_state <- list(
    model = fit,
    var_y = "mpg",
    vars_x = c("hp", "wt"),
    model_type = "linear",
    alpha = 0.05
  )

  ui <- ramses_render_lm_results(reg_state)
  ui_str <- as.character(ui)

  # Validate F statistic
  f_val <- round(smry$fstatistic[1], 2)
  expect_true(grepl(paste0("F(", smry$fstatistic[2], ", ", smry$fstatistic[3], ") = ", f_val), ui_str, fixed = TRUE))

  # Validate R2 and adjusted R2
  r2_val <- round(smry$r.squared, 4)
  r2_adj_val <- round(smry$adj.r.squared, 4)
  expect_true(grepl(paste0("R\u00b2 = ", r2_val), ui_str, fixed = TRUE))
  expect_true(grepl(paste0("round(r2_adj, 4)", r2_adj_val), ui_str) || grepl(as.character(r2_adj_val), ui_str))

  # Validate RMSE and sigma
  rmse_val <- round(rmse, 4)
  sigma_val <- round(smry$sigma, 4)
  expect_true(grepl(as.character(rmse_val), ui_str))
  expect_true(grepl(as.character(sigma_val), ui_str))

  # Validate CI95% bounds
  hp_ci_low <- round(ci["hp", 1], 4)
  hp_ci_high <- round(ci["hp", 2], 4)
  expect_true(grepl(as.character(hp_ci_low), ui_str))
  expect_true(grepl(as.character(hp_ci_high), ui_str))
})

test_that("ramses_render_lm_results handles non-significant global model gracefully", {
  set.seed(42)
  df_dummy <- data.frame(
    y = rnorm(20),
    x = rnorm(20)
  )
  fit_ns <- stats::lm(y ~ x, data = df_dummy)
  reg_state <- list(
    model = fit_ns,
    var_y = "y",
    vars_x = "x",
    model_type = "linear",
    alpha = 0.05
  )

  ui <- ramses_render_lm_results(reg_state)
  ui_str <- as.character(ui)

  expect_true(grepl("Le mod\u00e8le n'est pas statistiquement significatif au seuil de 5 %", ui_str))
})

test_that("ramses_pedagogy_reg handles missing or NULL model_type robustly without error", {
  fit <- stats::lm(mpg ~ hp, data = mtcars)
  reg_state_null_type <- list(
    model = fit,
    var_y = "mpg",
    vars_x = "hp",
    model_type = NULL,
    alpha = 0.05,
    calculated = TRUE
  )

  # Must execute without "l'argument est de longueur nulle" error
  ui <- ramses_pedagogy_reg(reg_state_null_type, mtcars)
  expect_true(inherits(ui, "shiny.tag"))
  ui_str <- as.character(ui)
  expect_true(grepl("Test F", ui_str))
  expect_true(grepl("Normalit", ui_str))
})
