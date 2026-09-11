test_that("ramses_compute_normality_by_group fonctionne pour 2 groupes normaux", {
  set.seed(123)
  df <- data.frame(
    Revenu = c(rnorm(50, mean = 2000, sd = 200), rnorm(50, mean = 2500, sd = 300)),
    Sexe = rep(c("Homme", "Femme"), each = 50),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Revenu", var_group = "Sexe", alpha = 0.05)

  expect_type(res, "list")
  expect_equal(res$var_y, "Revenu")
  expect_equal(res$var_group, "Sexe")
  expect_equal(nrow(res$summary_table), 2)
  expect_true(all(res$summary_table$N == 50))
  expect_true(all(!is.na(res$summary_table$W)))
  expect_true(all(!is.na(res$summary_table$p_value)))
  expect_true(all(res$summary_table$p_value >= 0.01))
  expect_true(all(res$summary_table$Status == "compatible"))
})

test_that("ramses_compute_normality_by_group detecte 2 groupes non-normaux", {
  set.seed(123)
  df <- data.frame(
    Score = c(rgamma(100, shape = 0.5), rgamma(100, shape = 0.2)),
    Groupe = rep(c("G1", "G2"), each = 100),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Score", var_group = "Groupe")

  expect_equal(nrow(res$summary_table), 2)
  expect_true(all(res$summary_table$p_value < 0.05))
  expect_true(all(res$summary_table$Status == "deviation"))
})

test_that("ramses_compute_normality_by_group gere les groupes de tailles differentes", {
  set.seed(456)
  df <- data.frame(
    Val = c(rnorm(20), rnorm(100)),
    Grp = c(rep("Petit", 20), rep("Grand", 100)),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Val", var_group = "Grp")

  st <- res$summary_table
  expect_equal(st$N[st$Group == "Petit"], 20)
  expect_equal(st$N[st$Group == "Grand"], 100)
  expect_true(all(!is.na(st$W)))
})

test_that("ramses_compute_normality_by_group gere la presence de NA dans Y et groupe", {
  set.seed(789)
  df <- data.frame(
    Mesure = c(rnorm(30), NA, rnorm(29), NA),
    Categorie = c(rep("A", 30), "A", rep("B", 29), NA),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Mesure", var_group = "Categorie")

  st <- res$summary_table
  expect_equal(st$N[st$Group == "A"], 30)
  expect_equal(st$N[st$Group == "B"], 29)
})

test_that("ramses_compute_normality_by_group gere un groupe avec N < 3", {
  df <- data.frame(
    Y = c(10, 12, rnorm(20)),
    Group = c("Petit", "Petit", rep("Normal", 20)),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Group")

  st <- res$summary_table
  petit_row <- st[st$Group == "Petit", ]
  expect_equal(petit_row$N, 2)
  expect_true(is.na(petit_row$W))
  expect_true(is.na(petit_row$p_value))
  expect_equal(petit_row$Status, "small_sample")
  expect_true(grepl("insuffisant", petit_row$Conclusion))
})

test_that("ramses_compute_normality_by_group gere un groupe avec toutes les valeurs manquantes", {
  df <- data.frame(
    Y = c(NA, NA, rnorm(10)),
    Group = c("Vide", "Vide", rep("Plein", 10)),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Group")

  st <- res$summary_table
  vide_row <- st[st$Group == "Vide", ]
  expect_equal(vide_row$N, 0)
  expect_true(is.na(vide_row$W))
  expect_equal(vide_row$Status, "small_sample")
})

test_that("ramses_compute_normality_by_group rejette une variable Y non numerique", {
  df <- data.frame(
    Texte = c("a", "b", "c"),
    Group = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    ramses_compute_normality_by_group(df, var_y = "Texte", var_group = "Group"),
    "num\u00e9rique"
  )
})

test_that("ramses_compute_normality_by_group valide les variables de groupe et d'interet", {
  df <- data.frame(
    Y = rnorm(10),
    Group = rep(c("A", "B"), 5),
    stringsAsFactors = FALSE
  )

  expect_error(ramses_compute_normality_by_group(df, var_y = "Inexistante", var_group = "Group"))
  expect_error(ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Inexistante"))
  expect_error(ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Y"))
})

test_that("ramses_compute_normality_by_group gere des noms de variables complexes", {
  set.seed(111)
  df <- data.frame(
    v1 = rnorm(40),
    v2 = rep(c("Jeune", "Senior"), each = 20),
    stringsAsFactors = FALSE
  )
  names(df) <- c("Revenu Mensuel (\u20ac)", "Cat\u00e9gorie d'\u00e2ges")

  res <- ramses_compute_normality_by_group(
    df,
    var_y = "Revenu Mensuel (\u20ac)",
    var_group = "Cat\u00e9gorie d'\u00e2ges"
  )

  expect_equal(nrow(res$summary_table), 2)
  expect_true(all(res$summary_table$N == 20))
})

test_that("ramses_compute_normality_by_group gere les modalites avec espaces", {
  df <- data.frame(
    Y = rnorm(30),
    Group = rep(c("Groupe A", "Groupe B"), each = 15),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Group")
  expect_setequal(res$summary_table$Group, c("Groupe A", "Groupe B"))
})

test_that("ramses_compute_normality_by_group gere les modalites avec accents", {
  df <- data.frame(
    Y = rnorm(30),
    Group = rep(c("\u00c9lev\u00e9", "Mod\u00e9r\u00e9"), each = 15),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Group")
  expect_setequal(res$summary_table$Group, c("\u00c9lev\u00e9", "Mod\u00e9r\u00e9"))
})

test_that("ramses_compute_normality_by_group gere les modalites avec apostrophes et guillemets", {
  df <- data.frame(
    Y = rnorm(30),
    Group = rep(c("Val d'Oise", "Secteur \"Priv\u00e9\""), each = 15),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Y", var_group = "Group")
  expect_setequal(res$summary_table$Group, c("Val d'Oise", "Secteur \"Priv\u00e9\""))
})

test_that("Les donnees QQ-plot et brutes correspondent exactement aux donnees de Shapiro-Wilk", {
  set.seed(999)
  y_a <- rnorm(25, mean = 10, sd = 2)
  y_b <- rnorm(30, mean = 15, sd = 3)
  df <- data.frame(
    Val = c(y_a, y_b),
    Grp = c(rep("G_A", 25), rep("G_B", 30)),
    stringsAsFactors = FALSE
  )

  res <- ramses_compute_normality_by_group(df, var_y = "Val", var_group = "Grp")

  raw_a <- res$raw_data_by_group[["G_A"]]
  raw_b <- res$raw_data_by_group[["G_B"]]

  expect_equal(raw_a, y_a)
  expect_equal(raw_b, y_b)

  qq_a <- res$qq_data[res$qq_data$group == "G_A", ]
  qq_b <- res$qq_data[res$qq_data$group == "G_B", ]

  expect_equal(sort(qq_a$y_observed), sort(y_a))
  expect_equal(sort(qq_b$y_observed), sort(y_b))

  sh_a <- shapiro.test(y_a)
  sh_b <- shapiro.test(y_b)

  st <- res$summary_table
  expect_equal(st$W[st$Group == "G_A"], unname(sh_a$statistic))
  expect_equal(st$p_value[st$Group == "G_A"], sh_a$p.value)
  expect_equal(st$W[st$Group == "G_B"], unname(sh_b$statistic))
  expect_equal(st$p_value[st$Group == "G_B"], sh_b$p.value)
})
