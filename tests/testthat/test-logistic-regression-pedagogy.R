# ==============================================================================
# Tests pour la renovation de la Regression Logistique Binaire et de sa restitution
# Fichier : tests/testthat/test-logistic-regression-pedagogy.R
# ==============================================================================

test_that("ramses_render_logistic_results outputs 7 structured pedagogical sections", {
  df <- data.frame(
    admit = c(0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1),
    gre = c(380, 660, 800, 640, 520, 760, 560, 400, 540, 700, 480, 620),
    gpa = c(3.61, 3.67, 4.0, 3.19, 2.93, 3.0, 2.98, 3.08, 3.39, 3.92, 3.10, 3.55)
  )

  mod <- stats::glm(admit ~ gre + gpa, data = df, family = stats::binomial(link = "logit"))
  reg_state <- list(
    model = mod,
    var_y = "admit",
    vars_x = c("gre", "gpa"),
    alpha = 0.05,
    n_initial = 12,
    n_used = 12,
    n_excluded = 0,
    event_label = "1",
    ref_label = "0"
  )

  ui <- ramses_render_logistic_results(reg_state)
  ui_str <- as.character(ui)

  # Verification de la presence des 7 sections pedagogiques
  expect_true(grepl("1. Test global du mod", ui_str))
  expect_true(grepl("2. Qualit", ui_str))
  expect_true(grepl("3. Coefficients du mod", ui_str))
  expect_true(grepl("4. Odds Ratios", ui_str))
  expect_true(grepl("5. Performance pr", ui_str))
  expect_true(grepl("6. V", ui_str))
  expect_true(grepl("7. Conclusion et synth", ui_str))
})

test_that("ramses_render_logistic_results accurately computes LRT, McFadden R2 and Odds Ratios", {
  df <- data.frame(
    y = c(0, 0, 1, 0, 0, 1, 1, 0, 1, 1),
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )

  mod <- stats::glm(y ~ x, data = df, family = stats::binomial(link = "logit"))
  reg_state <- list(
    model = mod,
    var_y = "y",
    vars_x = "x",
    alpha = 0.05,
    n_initial = 10,
    n_used = 10,
    n_excluded = 0,
    event_label = "1",
    ref_label = "0"
  )

  ui <- ramses_render_logistic_results(reg_state)
  ui_str <- as.character(ui)

  # LRT statistique = Null deviance - Residual deviance
  lrt_val <- round(mod$null.deviance - mod$deviance, 2)
  expect_true(grepl(as.character(lrt_val), ui_str))

  # Pseudo-R2 de McFadden
  mcfadden <- round(1 - (mod$deviance / mod$null.deviance), 4)
  expect_true(grepl(as.character(mcfadden), ui_str))

  # OR = exp(coef)
  or_x <- round(exp(stats::coef(mod)["x"]), 3)
  expect_true(grepl(as.character(or_x), ui_str))
})

test_that("ramses_render_logistic_results identifies factor reference level properly", {
  df <- data.frame(
    disease = factor(c("No", "No", "Yes", "No", "Yes", "Yes", "No", "Yes")),
    treatment = factor(c("Ctrl", "Ctrl", "Ctrl", "Active", "Active", "Active", "Active", "Active"), levels = c("Ctrl", "Active"))
  )

  mod <- stats::glm(disease ~ treatment, data = df, family = stats::binomial(link = "logit"))
  reg_state <- list(
    model = mod,
    var_y = "disease",
    vars_x = "treatment",
    alpha = 0.05,
    n_initial = 8,
    n_used = 8,
    n_excluded = 0,
    event_label = "Yes",
    ref_label = "No"
  )

  ui <- ramses_render_logistic_results(reg_state)
  ui_str <- as.character(ui)

  # Check that factor contrast label shows Active vs Ctrl (reference)
  expect_true(grepl("treatment : Active vs Ctrl", ui_str))
  expect_true(grepl("r\u00e9f\u00e9rence", ui_str))
})

test_that("ramses_pedagogy_reg for logistic regression reinforces odds ratio vs probability distinction", {
  df <- data.frame(
    y = c(0, 0, 1, 1, 1, 0),
    x = c(10, 20, 15, 30, 25, 12)
  )
  mod <- stats::glm(y ~ x, data = df, family = stats::binomial(link = "logit"))
  reg_state <- list(
    model = mod,
    var_y = "y",
    vars_x = "x",
    model_type = "logistic",
    alpha = 0.05,
    calculated = TRUE
  )

  ped_ui <- ramses_pedagogy_reg(reg_state, df)
  ped_str <- as.character(ped_ui)

  # Must distinguish odds from probability / risk
  expect_true(grepl("cotes", ped_str) || grepl("odds", ped_str))
  expect_true(grepl("P / \\(1 - P\\)", ped_str) || grepl("ln\\(p", ped_str))
  expect_false(grepl("augmente le risque / la probabilit\u00e9", ped_str))
})
