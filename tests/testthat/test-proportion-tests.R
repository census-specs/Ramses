test_that("ramses_test_one_prop computes single proportion test correctly", {
  df <- data.frame(
    outcome = c(rep("Succes", 60), rep("Echec", 40)),
    stringsAsFactors = FALSE
  )

  # Test asymptotique bilatéral
  res_prop <- ramses_test_one_prop(
    df = df,
    var_outcome = "outcome",
    success_modality = "Succes",
    p0 = 0.5,
    alternative = "two.sided",
    conf_level = 0.95,
    method = "prop",
    correct = TRUE
  )

  expect_false(is.null(res_prop))
  expect_equal(res_prop$n, 100)
  expect_equal(res_prop$x, 60)
  expect_equal(res_prop$p_obs, 0.6)
  expect_equal(res_prop$p0, 0.5)
  expect_true(is.numeric(res_prop$p_value))
  expect_true(length(res_prop$conf_int) == 2)
  expect_true(res_prop$conf_int[1] < res_prop$conf_int[2])
  expect_true(nzchar(res_prop$decision))
  expect_false(is.null(res_prop$effect_size))
  expect_equal(res_prop$effect_size$name, "h de Cohen")
  expect_false(is.null(res_prop$power))

  # Test exact binomial
  res_binom <- ramses_test_one_prop(
    df = df,
    var_outcome = "outcome",
    success_modality = "Succes",
    p0 = 0.5,
    alternative = "two.sided",
    conf_level = 0.95,
    method = "binom"
  )

  expect_false(is.null(res_binom))
  expect_equal(res_binom$method, "binom")
  expect_true(is.numeric(res_binom$p_value))
  expect_equal(res_binom$x, 60)
})

test_that("ramses_test_one_prop handles one-sided alternatives and continuity correction", {
  df <- data.frame(
    outcome = factor(c(rep("Oui", 70), rep("Non", 30)))
  )

  res_greater <- ramses_test_one_prop(
    df = df,
    var_outcome = "outcome",
    success_modality = "Oui",
    p0 = 0.5,
    alternative = "greater",
    method = "prop",
    correct = FALSE
  )
  expect_true(res_greater$p_value < 0.05)

  res_less <- ramses_test_one_prop(
    df = df,
    var_outcome = "outcome",
    success_modality = "Oui",
    p0 = 0.5,
    alternative = "less",
    method = "prop",
    correct = FALSE
  )
  expect_true(res_less$p_value > 0.5)
})

test_that("ramses_test_one_prop flags warnings for small sample sizes", {
  df_small <- data.frame(
    reponse = c("A", "A", "B")
  )

  res_small <- ramses_test_one_prop(
    df = df_small,
    var_outcome = "reponse",
    success_modality = "A",
    p0 = 0.5,
    method = "prop"
  )

  expect_false(is.null(res_small$warning_msg))
  expect_true(grepl("Faibles effectifs", res_small$warning_msg))
})

test_that("ramses_test_one_prop handles missing values and edge cases robustly", {
  df_na <- data.frame(
    statut = c("Positif", "Negatif", NA, "Positif", NA, "Negatif")
  )

  res <- ramses_test_one_prop(
    df = df_na,
    var_outcome = "statut",
    success_modality = "Positif",
    p0 = 0.5
  )

  expect_equal(res$n, 4)
  expect_equal(res$x, 2)
  expect_equal(res$p_obs, 0.5)

  # Column name non-syntactique
  df_special <- data.frame(
    `Taux Succes %` = c("OUI", "NON", "OUI"),
    check.names = FALSE
  )
  res_spec <- ramses_test_one_prop(
    df = df_special,
    var_outcome = "Taux Succes %",
    success_modality = "OUI"
  )
  expect_equal(res_spec$n, 3)
  expect_equal(res_spec$x, 2)

  # Errors
  expect_error(ramses_test_one_prop(NULL, "a", "b"))
  expect_error(ramses_test_one_prop(df_na, "inexistant", "Positif"))
  expect_error(ramses_test_one_prop(df_na, "statut", "Introuvable"))
  expect_error(ramses_test_one_prop(df_na, "statut", "Positif", p0 = 1.5))
})

test_that("ramses_test_one_prop rejects non-existent success modality with clear message", {
  # Character variable
  df_char <- data.frame(reponse = c("Oui", "Non", "Oui"), stringsAsFactors = FALSE)
  expect_error(
    ramses_test_one_prop(df_char, "reponse", "Peut-etre"),
    "n'existe pas dans les observations"
  )

  # Factor variable
  df_fac <- data.frame(reponse = factor(c("A", "B", "A")))
  expect_error(
    ramses_test_one_prop(df_fac, "reponse", "C"),
    "n'existe pas dans les observations"
  )

  # Numeric variable
  df_num <- data.frame(val = c(0, 1, 1, 0))
  expect_error(
    ramses_test_one_prop(df_num, "val", 2),
    "n'existe pas dans les observations"
  )
  # Numeric valid case: passing 1 works
  res_num <- ramses_test_one_prop(df_num, "val", 1)
  expect_equal(res_num$x, 2)
  expect_equal(res_num$n, 4)

  # Missing values: modality only in NA or absent should fail
  df_na <- data.frame(statut = c("Valide", NA, "Valide"))
  expect_error(
    ramses_test_one_prop(df_na, "statut", "Absent"),
    "n'existe pas dans les observations"
  )
})

test_that("ramses_test_two_props computes comparison of two proportions correctly", {
  df <- data.frame(
    traitement = c(rep("Placebo", 50), rep("Actif", 50)),
    guerison = c(rep("Oui", 20), rep("Non", 30), rep("Oui", 35), rep("Non", 15)),
    stringsAsFactors = FALSE
  )

  # Test asymptotique (prop.test)
  res_prop <- ramses_test_two_props(
    df = df,
    var_outcome = "guerison",
    var_group = "traitement",
    success_modality = "Oui",
    group_levels = c("Actif", "Placebo"),
    alternative = "two.sided",
    conf_level = 0.95,
    method = "prop",
    correct = TRUE
  )

  expect_false(is.null(res_prop))
  expect_equal(res_prop$group_levels, c("Actif", "Placebo"))
  expect_equal(res_prop$groups_summary$Total, c(50, 50))
  expect_equal(res_prop$groups_summary$Succes, c(35, 20))
  expect_equal(res_prop$groups_summary$Proportion, c(0.7, 0.4))
  expect_equal(res_prop$diff, 0.3)
  expect_equal(res_prop$relative_risk, 0.7 / 0.4)
  expect_equal(res_prop$odds_ratio, (35 * 30) / (15 * 20))
  expect_true(is.numeric(res_prop$p_value))
  expect_true(res_prop$p_value < 0.05)
  expect_false(is.null(res_prop$effect_size))
  expect_equal(res_prop$effect_size$name, "h de Cohen")
  expect_false(is.null(res_prop$power))

  # Test exact de Fisher
  res_fisher <- ramses_test_two_props(
    df = df,
    var_outcome = "guerison",
    var_group = "traitement",
    success_modality = "Oui",
    group_levels = c("Actif", "Placebo"),
    method = "fisher"
  )

  expect_false(is.null(res_fisher))
  expect_equal(res_fisher$method, "fisher")
  expect_true(is.numeric(res_fisher$p_value))
})

test_that("ramses_test_two_props subsets groups when group variable has >2 levels", {
  df <- data.frame(
    groupe = c(rep("A", 20), rep("B", 20), rep("C", 20)),
    succes = c(rep("Oui", 10), rep("Non", 10), rep("Oui", 16), rep("Non", 4), rep("Oui", 5), rep("Non", 15))
  )

  res <- ramses_test_two_props(
    df = df,
    var_outcome = "succes",
    var_group = "groupe",
    success_modality = "Oui",
    group_levels = c("B", "C")
  )

  expect_equal(res$group_levels, c("B", "C"))
  expect_equal(res$groups_summary$Total, c(20, 20))
  expect_equal(res$groups_summary$Succes, c(16, 5))
})

test_that("ramses_test_two_props flags warnings for low cell counts", {
  df_low <- data.frame(
    groupe = c("A", "A", "B", "B"),
    resultat = c("Succes", "Echec", "Echec", "Echec")
  )

  res <- ramses_test_two_props(
    df = df_low,
    var_outcome = "resultat",
    var_group = "groupe",
    success_modality = "Succes",
    method = "prop"
  )

  expect_false(is.null(res$warning_msg))
  expect_true(grepl("Faibles effectifs", res$warning_msg))
})

test_that("ramses_test_two_props handles errors properly", {
  df <- data.frame(x = c("A", "A"), y = c("B", "B"))
  expect_error(ramses_test_two_props(NULL, "x", "y", "A"))
  expect_error(ramses_test_two_props(df, "inexistant", "y", "A"))
  expect_error(ramses_test_two_props(df, "x", "inexistant", "A"))
  # Only 1 group modality
  expect_error(ramses_test_two_props(df, "x", "y", "A"))
})
