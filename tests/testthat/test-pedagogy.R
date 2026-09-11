test_that("ramses_pedagogy_container builds proper HTML structure", {
  h0 <- "H0 : Aucune difference"
  h1 <- "H1 : Difference significative"
  conds <- list(
    shiny::tags$li("Condition 1 verifiee"),
    shiny::tags$li("Condition 2 valide")
  )
  guide <- "Regle de decision selon alpha."

  ui <- ramses_pedagogy_container(h0, h1, conds, guide)
  ui_str <- as.character(ui)

  expect_true(grepl("1. Hypoth", ui_str))
  expect_true(grepl("2. Conditions d'application", ui_str))
  expect_true(grepl("3. Guide d'interpr", ui_str))
  expect_true(grepl("H0 : Aucune difference", ui_str))
  expect_true(grepl("H1 : Difference significative", ui_str))
})

test_that("ramses_pedagogy_norm evaluates normality conditions correctly", {
  df <- data.frame(val = rnorm(50))
  st <- list(
    test = "shapiro",
    var = "val",
    group = NULL,
    alpha = 0.05,
    calculated = TRUE,
    result = list(p.value = 0.42)
  )

  ui <- ramses_pedagogy_norm(st, df)
  ui_str <- as.character(ui)

  expect_true(grepl("distribution normale", ui_str))
  expect_true(grepl("Shapiro-Wilk", ui_str))
  expect_true(grepl("n = 50", ui_str))
})

test_that("ramses_pedagogy_two handles alternative hypotheses and Welch vs Student", {
  df <- data.frame(
    y = c(rnorm(25, mean = 5), rnorm(25, mean = 7)),
    grp = factor(rep(c("A", "B"), each = 25))
  )

  # Bilateral Welch
  st_welch <- list(
    test = "t_indep",
    var_y = "y",
    var_group = "grp",
    selected_modalities = c("A", "B"),
    alternative = "two.sided",
    var_equal = FALSE,
    alpha = 0.05,
    calculated = TRUE,
    result = list(p.value = 0.002)
  )

  ui_w <- ramses_pedagogy_two(st_welch, df)
  str_w <- as.character(ui_w)
  expect_true(grepl("Welch", str_w))
  expect_true(grepl("H0", str_w))
  expect_true(grepl("H1", str_w))
  expect_true(grepl("diff", str_w))

  # Unilateral gauche Student
  st_less <- list(
    test = "t_indep",
    var_y = "y",
    var_group = "grp",
    selected_modalities = c("A", "B"),
    alternative = "less",
    var_equal = TRUE,
    alpha = 0.05,
    calculated = TRUE,
    result = list(p.value = 0.04)
  )

  ui_l <- ramses_pedagogy_two(st_less, df)
  str_l <- as.character(ui_l)
  expect_true(grepl("inf", str_l))
})

test_that("ramses_pedagogy_cont assesses Cochran condition on contingency tables", {
  # Grande table avec effectifs suffisants
  df_big <- data.frame(
    A = factor(rep(c("O1", "O2"), each = 50)),
    B = factor(rep(c("E1", "E2"), times = 50))
  )
  tab_big <- table(df_big$A, df_big$B)
  chisq_res <- stats::chisq.test(tab_big)

  st_cont <- list(
    test = "chisq",
    var_row = "A",
    var_col = "B",
    correct = TRUE,
    alpha = 0.05,
    calculated = TRUE,
    tab = tab_big,
    result = chisq_res
  )

  ui_c <- ramses_pedagogy_cont(st_cont, df_big)
  str_c <- as.character(ui_c)
  expect_true(grepl("Cochran", str_c))
  expect_true(grepl("ind", str_c))
})

test_that("ramses_pedagogy_cor and reg generate custom contextual guidance", {
  df <- data.frame(
    x = 1:30,
    y = 2 * (1:30) + rnorm(30),
    bin = factor(rep(c("Yes", "No"), each = 15))
  )

  st_cor <- list(
    method = "pearson",
    var_x = "x",
    var_y = "y",
    alternative = "greater",
    alpha = 0.05,
    calculated = TRUE,
    result = list(p.value = 0.0001)
  )
  ui_cor <- ramses_pedagogy_cor(st_cor, df)
  str_cor <- as.character(ui_cor)
  expect_true(grepl("positive", str_cor))

  # Regression lm
  lm_fit <- stats::lm(y ~ x, data = df)
  st_reg <- list(
    model_type = "linear",
    var_y = "y",
    vars_x = c("x"),
    alpha = 0.05,
    calculated = TRUE,
    model = lm_fit
  )
  ui_reg <- ramses_pedagogy_reg(st_reg, df)
  str_reg <- as.character(ui_reg)
  expect_true(grepl("Test F", str_reg))
  expect_true(grepl("pr", str_reg))
})
