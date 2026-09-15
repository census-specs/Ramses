library(testthat)

test_that("1. Calcul ANCOVA 1 facteur + 1 covariable standard fonctionne", {
  set.seed(42)
  df <- data.frame(
    groupe = rep(c("A", "B", "C"), each = 20),
    covar = rnorm(60, mean = 50, sd = 10),
    y = rnorm(60, mean = 100, sd = 15)
  )
  # Ajouter un effet groupe et un effet covariable
  df$y <- df$y + ifelse(df$groupe == "B", 10, ifelse(df$groupe == "C", -10, 0)) + 0.5 * df$covar

  res <- ramses_ancova(df, var_y = "y", var_factor = "groupe", var_covar = "covar", alpha = 0.05)

  expect_true(is.list(res))
  expect_equal(res$var_y, "y")
  expect_equal(res$var_factor, "groupe")
  expect_equal(res$var_covar, "covar")
  expect_true(inherits(res$model_main, "lm"))
  expect_true(is.data.frame(res$anova_table))
  expect_true(is.data.frame(res$adjusted_means))
})

test_that("2. Sommes des carres de Type II exactes via drop1", {
  set.seed(123)
  df <- data.frame(
    groupe = factor(rep(c("T1", "T2", "T3"), times = c(15, 25, 20))), # Desequilibre
    covar = rnorm(60, 20, 5),
    y = rnorm(60, 50, 8)
  )
  df$y <- df$y + as.numeric(df$groupe) * 3 + 0.8 * df$covar

  res <- ramses_ancova(df, var_y = "y", var_factor = "groupe", var_covar = "covar")
  
  # Comparaison directe avec drop1(lm(y ~ groupe + covar), test = 'F')
  mod_ref <- lm(y ~ groupe + covar, data = df)
  d1 <- drop1(mod_ref, test = "F")
  
  tab <- res$anova_table
  row_grp <- tab[tab$Term == "groupe", ]
  row_cov <- tab[tab$Term == "covar", ]
  
  expect_equal(row_grp$Sum_Sq, d1["groupe", "Sum of Sq"], tolerance = 1e-5)
  expect_equal(row_grp$F_value, d1["groupe", "F value"], tolerance = 1e-5)
  expect_equal(row_grp$p_value, d1["groupe", "Pr(>F)"], tolerance = 1e-5)
  
  expect_equal(row_cov$Sum_Sq, d1["covar", "Sum of Sq"], tolerance = 1e-5)
  expect_equal(row_cov$F_value, d1["covar", "F value"], tolerance = 1e-5)
  expect_equal(row_cov$p_value, d1["covar", "Pr(>F)"], tolerance = 1e-5)
})

test_that("3. Modele de controle des pentes (interaction A*C)", {
  df <- data.frame(
    grp = rep(c("A", "B"), each = 15),
    x = rnorm(30, 10, 2),
    y = rnorm(30, 20, 3)
  )
  res <- ramses_ancova(df, var_y = "y", var_factor = "grp", var_covar = "x")
  expect_true(inherits(res$model_slopes, "lm"))
  expect_true(!is.null(res$slopes_test))
  expect_true(!is.na(res$slopes_test$p_value))
})

test_that("4. Detection pente non significative (homogeneite validee)", {
  set.seed(1)
  df <- data.frame(
    grp = rep(c("G1", "G2"), each = 30),
    cov = rnorm(60, 10, 2)
  )
  # Meme pente pour les deux groupes
  df$y <- 5 + 2 * df$cov + ifelse(df$grp == "G2", 3, 0) + rnorm(60, 0, 1)

  res <- ramses_ancova(df, "y", "grp", "cov")
  expect_true(res$slopes_test$pentes_homogenes)
  expect_gt(res$slopes_test$p_value, 0.05)
})

test_that("5. Detection pente significative (alerte d'interaction)", {
  set.seed(2)
  df <- data.frame(
    grp = rep(c("G1", "G2"), each = 30),
    cov = rnorm(60, 10, 2)
  )
  # Pentes differentes (0.5 vs 3.5)
  df$y <- ifelse(df$grp == "G1", 2 + 0.5 * df$cov, 2 + 3.5 * df$cov) + rnorm(60, 0, 1)

  res <- ramses_ancova(df, "y", "grp", "cov")
  expect_false(res$slopes_test$pentes_homogenes)
  expect_lt(res$slopes_test$p_value, 0.05)
})

test_that("6. Calcul des moyennes ajustees manuelles (predict avec se.fit)", {
  df <- data.frame(
    traitement = rep(c("Controle", "Dose1", "Dose2"), each = 10),
    age = rnorm(30, 40, 5),
    score = rnorm(30, 70, 10)
  )
  res <- ramses_ancova(df, "score", "traitement", "age")
  adj <- res$adjusted_means
  
  expect_equal(nrow(adj), 3)
  expect_true(all(c("Groupe", "N", "Moyenne_Brute", "Moyenne_Ajustee", "SE", "CI_lower", "CI_upper") %in% names(adj)))
  expect_true(all(adj$SE > 0))
  expect_true(all(adj$CI_lower < adj$Moyenne_Ajustee))
  expect_true(all(adj$CI_upper > adj$Moyenne_Ajustee))
})

test_that("7. Evaluation des moyennes ajustees a la moyenne globale de la covariable", {
  df <- data.frame(
    groupe = factor(rep(c("A", "B"), each = 15)),
    covar = c(rnorm(15, 20, 2), rnorm(15, 30, 2)), # covariable desequilibree entre groupes
    y = rnorm(30, 50, 5)
  )
  mean_cov_globale <- mean(df$covar)
  res <- ramses_ancova(df, "y", "groupe", "covar")
  
  expect_equal(res$mean_covar, mean_cov_globale, tolerance = 1e-6)
  
  # Verif predict manuel
  mod <- lm(y ~ groupe + covar, data = df)
  pred_manual <- predict(mod, newdata = data.frame(groupe = factor(c("A", "B")), covar = mean_cov_globale))
  expect_equal(res$adjusted_means$Moyenne_Ajustee, unname(pred_manual), tolerance = 1e-6)
})

test_that("8. Intervalles de confiance a 95% des moyennes ajustees sont coherents", {
  df <- data.frame(
    g = rep(c("A", "B", "C"), each = 12),
    x = rnorm(36, 15, 3),
    y = rnorm(36, 100, 10)
  )
  res <- ramses_ancova(df, "y", "g", "x", alpha = 0.05)
  adj <- res$adjusted_means
  
  crit_t <- qt(0.975, df = res$model_main$df.residual)
  for (i in 1:nrow(adj)) {
    expect_equal(adj$CI_lower[i], adj$Moyenne_Ajustee[i] - crit_t * adj$SE[i], tolerance = 1e-6)
    expect_equal(adj$CI_upper[i], adj$Moyenne_Ajustee[i] + crit_t * adj$SE[i], tolerance = 1e-6)
  }
})

test_that("9. Calcul des comparaisons post-hoc deux a deux", {
  df <- data.frame(
    trt = rep(c("Ctrl", "MedA", "MedB", "MedC"), each = 10),
    cov = rnorm(40, 50, 5),
    y = rnorm(40, 100, 8)
  )
  res <- ramses_ancova(df, "y", "trt", "cov")
  ph <- res$post_hoc
  
  # 4 groupes -> choose(4, 2) = 6 comparaisons
  expect_equal(nrow(ph), 6)
  expect_true(all(c("Groupe_1", "Groupe_2", "Difference", "SE", "t_value", "p_value_raw", "p_value_adj") %in% names(ph)))
})

test_that("10. Correction de Holm appliquee correctement sur les post-hoc", {
  df <- data.frame(
    trt = rep(c("Ctrl", "T1", "T2"), each = 15),
    x = rnorm(45, 10, 2),
    y = rnorm(45, 50, 5)
  )
  res <- ramses_ancova(df, "y", "trt", "x")
  ph <- res$post_hoc
  
  p_adj_expected <- p.adjust(ph$p_value_raw, method = "holm")
  expect_equal(ph$p_value_adj, p_adj_expected, tolerance = 1e-6)
  expect_true(all(ph$p_value_adj >= ph$p_value_raw))
})

test_that("11. Calcul exact des erreurs types des differences via vcov", {
  df <- data.frame(
    g = factor(rep(c("A", "B", "C"), each = 10)),
    cov = rnorm(30, 25, 4),
    y = rnorm(30, 60, 6)
  )
  res <- ramses_ancova(df, "y", "g", "cov")
  ph <- res$post_hoc
  
  # Comparaison B vs A
  mod <- res$model_main
  V <- vcov(mod)
  se_B_vs_A <- sqrt(V["gB", "gB"])
  
  row_BA <- ph[ph$Groupe_1 == "A" & ph$Groupe_2 == "B", ]
  expect_equal(row_BA$SE, se_B_vs_A, tolerance = 1e-6)
})

test_that("12. Calcul de la taille d'effet eta_p_sq", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), each = 15),
    cov = rnorm(45, 10, 2),
    y = rnorm(45, 20, 4)
  )
  res <- ramses_ancova(df, "y", "grp", "cov")
  tab <- res$anova_table
  
  ss_res <- tab[tab$Term == "R\u00e9sidus", "Sum_Sq"]
  ss_grp <- tab[tab$Term == "grp", "Sum_Sq"]
  ss_cov <- tab[tab$Term == "cov", "Sum_Sq"]
  
  eta_grp_expected <- ss_grp / (ss_grp + ss_res)
  eta_cov_expected <- ss_cov / (ss_cov + ss_res)
  
  expect_equal(tab[tab$Term == "grp", "eta_p_sq"], eta_grp_expected, tolerance = 1e-5)
  expect_equal(tab[tab$Term == "cov", "eta_p_sq"], eta_cov_expected, tolerance = 1e-5)
})

test_that("13. Diagnostic de normalite des residus (Shapiro-Wilk)", {
  df <- data.frame(
    grp = rep(c("A", "B"), each = 20),
    x = rnorm(40, 5, 1),
    y = rnorm(40, 10, 2)
  )
  res <- ramses_ancova(df, "y", "grp", "x")
  diag <- res$diagnostics
  
  expect_true(!is.null(diag$shapiro))
  sh_expected <- stats::shapiro.test(residuals(res$model_main))
  expect_equal(diag$shapiro$p_value, sh_expected$p.value, tolerance = 1e-6)
})

test_that("14. Diagnostic d'homogeneite des variances (Fligner/Bartlett)", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), each = 15),
    x = rnorm(45, 5, 1),
    y = rnorm(45, 10, 2)
  )
  res <- ramses_ancova(df, "y", "grp", "x")
  diag <- res$diagnostics
  
  expect_true(!is.null(diag$homoscedasticity))
  expect_true(!is.na(diag$homoscedasticity$p_value))
})

test_that("15. Gestion des valeurs manquantes (NA) robuste", {
  df <- data.frame(
    grp = c(NA, rep(c("A", "B"), each = 15)),
    x = c(rnorm(15, 10, 2), NA, rnorm(15, 10, 2)),
    y = c(rnorm(30, 50, 5), NA)
  )
  res <- ramses_ancova(df, "y", "grp", "x")
  expect_equal(res$n_obs, 28)
})

test_that("16. Gestion des facteurs a 2 niveaux et plus", {
  df2 <- data.frame(
    grp = rep(c("A", "B"), each = 10),
    x = rnorm(20, 10, 1),
    y = rnorm(20, 30, 2)
  )
  res2 <- ramses_ancova(df2, "y", "grp", "x")
  expect_equal(nrow(res2$adjusted_means), 2)
  expect_equal(nrow(res2$post_hoc), 1)
})

test_that("17. Preservation totale de l'ANOVA 1 facteur (non-regression)", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), each = 10),
    y = rnorm(30, 50, 5)
  )
  fml <- ramses_formula("y", "grp")
  fit <- stats::aov(fml, data = df)
  expect_true(inherits(fit, "aov"))
  es <- ramses_compute_effect_size("anova", stat_result = fit)
  expect_true(!is.null(es$value))
})

test_that("18. Preservation totale de l'ANOVA 2 facteurs (non-regression)", {
  df <- data.frame(
    f1 = rep(c("A", "B"), each = 20),
    f2 = rep(c("C", "D"), times = 20),
    y = rnorm(40, 50, 5)
  )
  res_fact <- ramses_anova_factorial(df, "y", "f1", "f2", interaction = TRUE)
  expect_true(!is.null(res_fact$anova_table))
  expect_equal(res_fact$type_ss, "Type III")
})

test_that("19. Preservation de Kruskal-Wallis (non-regression)", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), each = 10),
    y = rnorm(30, 50, 5)
  )
  fml <- ramses_formula("y", "grp")
  fit_kw <- stats::kruskal.test(fml, data = df)
  expect_true(inherits(fit_kw, "htest"))
})

test_that("20. Gestion des erreurs et cas limites sans crash", {
  # Donnees insuffisantes
  df_empty <- data.frame(y = numeric(0), grp = character(0), cov = numeric(0))
  expect_error(ramses_ancova(df_empty, "y", "grp", "cov"))

  # Covariable constante
  df_cst <- data.frame(
    grp = rep(c("A", "B"), each = 10),
    cov = rep(5, 20),
    y = rnorm(20, 10, 2)
  )
  expect_error(ramses_ancova(df_cst, "y", "grp", "cov"))
})
