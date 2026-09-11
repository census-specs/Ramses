test_that("1. statistiques quantitatives sans groupe -> comportement inchange", {
  # Cas univarie (1 seule variable) -> format 2 colonnes SPSS
  df_univ <- data.frame(
    age = c(20, 25, 30, 35, 40)
  )
  res_univ <- ramses_compute_quanti_table(df_univ, vars = "age")
  expect_s3_class(res_univ, "data.frame")
  expect_true(nrow(res_univ) > 0)
  expect_equal(names(res_univ), c("Statistique", "Valeur"))
  
  val_n <- res_univ$Valeur[res_univ$Statistique == "Effectif valide (N)"]
  val_mean <- res_univ$Valeur[res_univ$Statistique == "Moyenne"]
  expect_equal(val_n, "5")
  expect_equal(val_mean, "30.000")

  # Cas univarie avec valeurs manquantes
  df_na <- data.frame(
    poids = c(60, NA, 70, 80, NA, 90)
  )
  res_na <- ramses_compute_quanti_table(df_na, vars = "poids")
  expect_s3_class(res_na, "data.frame")
  expect_equal(res_na$Valeur[res_na$Statistique == "Effectif valide (N)"], "4")
  expect_equal(res_na$Valeur[res_na$Statistique == "Valeurs manquantes (NA)"], "2")
  expect_equal(res_na$Valeur[res_na$Statistique == "Moyenne"], "75.000")

  # Cas multivarie sans groupe (2 variables) -> format horizontal classique
  df_multi <- data.frame(
    var1 = c(10, 20, 30),
    var2 = c(100, 200, 300)
  )
  res_multi <- ramses_compute_quanti_table(df_multi, vars = c("var1", "var2"))
  expect_s3_class(res_multi, "data.frame")
  expect_equal(nrow(res_multi), 2)
  expect_equal(res_multi$Variable, c("var1", "var2"))
  expect_true("Moyenne" %in% names(res_multi))
  expect_equal(res_multi$Moyenne, c(20, 200))
})

test_that("2. une variable quantitative avec deux groupes -> resultats verticaux", {
  df <- data.frame(
    sexe = c("Hommes", "Hommes", "Femmes", "Femmes"),
    age = c(25, 35, 28, 32)
  )
  active_stats <- c("n", "mean", "median", "sd")
  res <- ramses_compute_quanti_table(df, vars = "age", group_var = "sexe", active_stats = active_stats)
  
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("Variable", "Groupe", "Statistique", "Valeur"))
  expect_equal(nrow(res), 8) # 2 groupes * 4 statistiques
  expect_true(all(res$Variable == "age"))
  expect_setequal(unique(res$Groupe), c("Femmes", "Hommes"))

  # Verification des statistiques pour Femmes
  val_femmes_n <- res$Valeur[res$Groupe == "Femmes" & res$Statistique == "N"]
  val_femmes_mean <- res$Valeur[res$Groupe == "Femmes" & res$Statistique == "Moyenne"]
  val_femmes_med <- res$Valeur[res$Groupe == "Femmes" & res$Statistique == "M\u00e9diane"]
  expect_equal(val_femmes_n, 2)
  expect_equal(val_femmes_mean, 30)
  expect_equal(val_femmes_med, 30)

  # Verification des statistiques pour Hommes
  val_hommes_n <- res$Valeur[res$Groupe == "Hommes" & res$Statistique == "N"]
  val_hommes_mean <- res$Valeur[res$Groupe == "Hommes" & res$Statistique == "Moyenne"]
  val_hommes_med <- res$Valeur[res$Groupe == "Hommes" & res$Statistique == "M\u00e9diane"]
  expect_equal(val_hommes_n, 2)
  expect_equal(val_hommes_mean, 30)
  expect_equal(val_hommes_med, 30)
})

test_that("3. plusieurs groupes -> toutes les statistiques sont presentes", {
  df <- data.frame(
    region = c("Nord", "Nord", "Sud", "Sud", "Est", "Est"),
    vente = c(100, 150, 200, 250, 300, 350)
  )
  all_stats <- c("n", "na", "mean", "median", "var", "sd", "cv", "min", "max", "iqr", "skewness", "kurtosis")
  res <- ramses_compute_quanti_table(df, vars = "vente", group_var = "region", active_stats = all_stats)

  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("Variable", "Groupe", "Statistique", "Valeur"))
  expect_equal(nrow(res), 3 * length(all_stats))

  expected_stat_labels <- c("N", "NA", "Moyenne", "M\u00e9diane", "Variance", "\u00c9cart-type", "CV (%)", "Min", "Max", "IQR", "Asym\u00e9trie", "Aplatissement")
  for (grp in c("Nord", "Sud", "Est")) {
    res_grp <- res[res$Groupe == grp, ]
    expect_equal(nrow(res_grp), length(expected_stat_labels))
    expect_equal(res_grp$Statistique, expected_stat_labels)
  }
})

test_that("4. groupe entierement NA -> aucune erreur", {
  df <- data.frame(
    grp = c("G1", "G1", "G2", "G2"),
    score = c(10, 20, NA, NA)
  )
  res <- ramses_compute_quanti_table(df, vars = "score", group_var = "grp")
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("Variable", "Groupe", "Statistique", "Valeur"))

  # Groupe G1 a des valeurs valides
  expect_equal(res$Valeur[res$Groupe == "G1" & res$Statistique == "N"], 2)
  expect_equal(res$Valeur[res$Groupe == "G1" & res$Statistique == "NA"], 0)
  expect_equal(res$Valeur[res$Groupe == "G1" & res$Statistique == "Moyenne"], 15)

  # Groupe G2 a uniquement des NA : pas d'erreur, NA_real_ utilise
  expect_equal(res$Valeur[res$Groupe == "G2" & res$Statistique == "N"], 0)
  expect_equal(res$Valeur[res$Groupe == "G2" & res$Statistique == "NA"], 2)
  expect_true(is.na(res$Valeur[res$Groupe == "G2" & res$Statistique == "Moyenne"]))
  expect_true(is.na(res$Valeur[res$Groupe == "G2" & res$Statistique == "\u00c9cart-type"]))
})

test_that("5. plusieurs groupes avec certains entierement NA -> aucune erreur", {
  df <- data.frame(
    secteur = c("A", "A", "B", "B", "C", "C"),
    mesure = c(100, 110, NA, NA, 200, 210)
  )
  res <- ramses_compute_quanti_table(df, vars = "mesure", group_var = "secteur")
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("Variable", "Groupe", "Statistique", "Valeur"))
  expect_setequal(unique(res$Groupe), c("A", "B", "C"))

  # B est entierement NA
  expect_equal(res$Valeur[res$Groupe == "B" & res$Statistique == "N"], 0)
  expect_equal(res$Valeur[res$Groupe == "B" & res$Statistique == "NA"], 2)
  expect_true(is.na(res$Valeur[res$Groupe == "B" & res$Statistique == "Moyenne"]))
  expect_true(is.na(res$Valeur[res$Groupe == "B" & res$Statistique == "\u00c9cart-type"]))

  # A et C sont valides
  expect_equal(res$Valeur[res$Groupe == "A" & res$Statistique == "Moyenne"], 105)
  expect_equal(res$Valeur[res$Groupe == "C" & res$Statistique == "Moyenne"], 205)
})

test_that("6. variable avec nom complexe -> fonctionnement correct", {
  df_complex <- data.frame(
    "concentration [mg/L] / total" = c(5.2, 8.4, NA, 12.1),
    "statut d'activite (A & B)" = c("Actif", "Inactif", "Actif", "Inactif"),
    check.names = FALSE
  )
  v_name <- "concentration [mg/L] / total"
  g_name <- "statut d'activite (A & B)"

  # Univarie complexe
  res_univ <- ramses_compute_quanti_table(df_complex, vars = v_name)
  expect_s3_class(res_univ, "data.frame")
  expect_true(nrow(res_univ) > 0)

  # Groupe complexe
  res_grp <- ramses_compute_quanti_table(df_complex, vars = v_name, group_var = g_name)
  expect_s3_class(res_grp, "data.frame")
  expect_equal(names(res_grp), c("Variable", "Groupe", "Statistique", "Valeur"))
  expect_true(all(res_grp$Variable == v_name))
  expect_setequal(unique(res_grp$Groupe), c("Actif", "Inactif"))
  expect_true(nrow(res_grp) > 0)
})

test_that("7. verification du nombre de lignes = combinaisons groupe x statistiques", {
  # 1 variable, 2 groupes, 8 stats actives -> 1 * 2 * 8 = 16 lignes
  df1 <- data.frame(
    g = c("X", "X", "Y", "Y"),
    v = c(1, 2, 3, 4)
  )
  res1 <- ramses_compute_quanti_table(df1, vars = "v", group_var = "g",
                                     active_stats = c("n", "na", "mean", "median", "sd", "min", "max", "iqr"))
  expect_equal(nrow(res1), 16)

  # 1 variable, 3 groupes, 3 stats actives -> 1 * 3 * 3 = 9 lignes
  df2 <- data.frame(
    g = c("P", "Q", "R"),
    v = c(10, 20, 30)
  )
  res2 <- ramses_compute_quanti_table(df2, vars = "v", group_var = "g",
                                     active_stats = c("n", "mean", "sd"))
  expect_equal(nrow(res2), 9)

  # 2 variables, 2 groupes, 4 stats actives -> 2 * 2 * 4 = 16 lignes
  df3 <- data.frame(
    g = c("A", "B", "A", "B"),
    v1 = c(1, 2, 3, 4),
    v2 = c(10, 20, 30, 40)
  )
  res3 <- ramses_compute_quanti_table(df3, vars = c("v1", "v2"), group_var = "g",
                                     active_stats = c("n", "mean", "min", "max"))
  expect_equal(nrow(res3), 16)
})

test_that("8. gestion des cas limites : groupes NA ou dataframe vide", {
  # Colonne de groupe ne contient que des NA
  df_na_grp <- data.frame(
    grp = c(NA_character_, NA_character_),
    val = c(10, 20)
  )
  res_no_grp <- ramses_compute_quanti_table(df_na_grp, vars = "val", group_var = "grp")
  expect_s3_class(res_no_grp, "data.frame")
  expect_equal(nrow(res_no_grp), 0)
  expect_equal(names(res_no_grp), c("Variable", "Groupe", "Statistique", "Valeur"))

  # Dataframe a 0 ligne
  df_empty <- data.frame(grp = character(0), val = numeric(0))
  res_empty <- ramses_compute_quanti_table(df_empty, vars = "val", group_var = "grp")
  expect_s3_class(res_empty, "data.frame")
  expect_equal(nrow(res_empty), 0)
  expect_equal(names(res_empty), c("Variable", "Groupe", "Statistique", "Valeur"))
})
