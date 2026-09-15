library(testthat)

# ==============================================================================
# TESTS UNITAIRES — MOTEUR ANOVA A MESURES REPETEES (ramses_anova_rm)
# ==============================================================================

# Jeu de donnees de reference de l'audit (N = 10 sujets, K = 3 niveaux de Temps)
df_ref_rm <- data.frame(
  Sujet = factor(rep(1:10, times = 3)),
  Temps = factor(rep(c("T1", "T2", "T3"), each = 10), levels = c("T1", "T2", "T3")),
  Y = c(10, 12, 11, 9, 14, 13, 10, 12, 11, 15,
        13, 15, 14, 12, 18, 15, 14, 16, 13, 17,
        18, 20, 17, 19, 24, 21, 18, 22, 19, 23)
)

test_that("Test 1 : Jeu de reference audit - Statistique F et p-value brute exactes", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

  tab <- res$anova_table
  row_time <- tab[tab$Source == "Temps", ]
  row_res <- tab[tab$Source == "R\u00e9sidus", ]
  row_sub <- tab[tab$Source == "Sujets", ]

  expect_equal(row_time$Df, 2)
  expect_equal(row_sub$Df, 9)
  expect_equal(row_res$Df, 18)

  expect_equal(row_time$Sum_Sq, 362.4, tolerance = 1e-4)
  expect_equal(row_sub$Sum_Sq, 102.1667, tolerance = 1e-4)
  expect_equal(row_res$Sum_Sq, 10.9333, tolerance = 1e-4)

  expect_equal(row_time$Mean_Sq, 181.2, tolerance = 1e-4)
  expect_equal(row_res$Mean_Sq, 10.9333 / 18, tolerance = 1e-4)

  # F = 181.2 / (10.9333 / 18) = 298.3171
  expect_equal(row_time$F_value, 298.3171, tolerance = 1e-3)
  expect_equal(row_time$p_value, 2.213e-14, tolerance = 1e-12)
})

test_that("Test 2 : Jeu de reference audit - Test de Mauchly (W, Chi2, Df, p)", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

  mauchly <- res$mauchly
  expect_true(mauchly$applicable)
  expect_equal(mauchly$df, 2)
  expect_equal(mauchly$w, 0.7852, tolerance = 1e-3)
  expect_equal(mauchly$statistic, 1.9341, tolerance = 1e-3)
  expect_equal(mauchly$p_value, 0.3802, tolerance = 1e-3)
})

test_that("Test 3 : Jeu de reference audit - Epsilons GG et HF exacts", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

  corr <- res$corrections
  expect_true(corr$applicable)
  expect_equal(corr$eps_gg, 0.8232, tolerance = 1e-3)
  expect_equal(corr$eps_hf, 0.9835, tolerance = 1e-3)
})

test_that("Test 4 : Jeu de reference audit - p-values corrigees GG et HF", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

  corr <- res$corrections
  expect_equal(corr$df1_gg, 2 * 0.8232, tolerance = 1e-3)
  expect_equal(corr$df2_gg, 18 * 0.8232, tolerance = 1e-3)
  expect_equal(corr$p_gg, 2.673e-12, tolerance = 1e-10)

  expect_equal(corr$df1_hf, 2 * 0.9835, tolerance = 1e-3)
  expect_equal(corr$df2_hf, 18 * 0.9835, tolerance = 1e-3)
  expect_equal(corr$p_hf, 2.557e-14, tolerance = 1e-11)
})

test_that("Test 5 : Jeu de reference audit - Tailles d'effet eta_p_sq et eta_g_sq", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

  es <- res$effect_sizes
  expect_equal(es$eta_p_sq, 362.4 / (362.4 + 10.9333), tolerance = 1e-4) # 0.9707
  expect_equal(es$eta_p_sq, 0.9707, tolerance = 1e-3)
  expect_equal(es$eta_g_sq, 362.4 / (362.4 + 102.1667 + 10.9333), tolerance = 1e-4) # 0.7621
  expect_equal(es$eta_g_sq, 0.7621, tolerance = 1e-3)
})

test_that("Test 6 : Cas K = 2 niveaux - Equivalence exacte F = t^2 et meme p-value avec test t apparie", {
  df_k2 <- df_ref_rm[df_ref_rm$Temps %in% c("T1", "T2"), ]
  df_k2$Temps <- droplevels(df_k2$Temps)

  res_rm <- ramses_anova_rm(df_k2, response = "Y", subject = "Sujet", within_factor = "Temps")
  f_val <- res_rm$anova_table[res_rm$anova_table$Source == "Temps", "F_value"]
  p_rm <- res_rm$anova_table[res_rm$anova_table$Source == "Temps", "p_value"]

  v1 <- df_k2$Y[df_k2$Temps == "T1"]
  v2 <- df_k2$Y[df_k2$Temps == "T2"]
  t_res <- stats::t.test(v1, v2, paired = TRUE)
  t_stat <- unname(t_res$statistic)

  expect_equal(f_val, t_stat^2, tolerance = 1e-6)
  expect_equal(p_rm, t_res$p.value, tolerance = 1e-6)
  expect_false(res_rm$mauchly$applicable)
  expect_equal(res_rm$corrections$eps_gg, 1.0)
})

test_that("Test 7 : Sujet avec mesure manquante -> exclusion complete du sujet", {
  df_na <- df_ref_rm
  # Supprimer une mesure pour le sujet 1 a T3
  df_na$Y[df_na$Sujet == 1 & df_na$Temps == "T3"] <- NA

  res <- ramses_anova_rm(df_na, response = "Y", subject = "Sujet", within_factor = "Temps")

  expect_equal(res$data_info$initial_subjects, 10)
  expect_equal(res$data_info$excluded_subjects, 1)
  expect_equal(res$data_info$final_subjects, 9)
  expect_equal(res$data_info$n_obs, 27)

  # Les Df doivent etre : Temps = 2, Sujets = 8, Residus = 16
  tab <- res$anova_table
  expect_equal(tab[tab$Source == "Sujets", "Df"], 8)
  expect_equal(tab[tab$Source == "R\u00e9sidus", "Df"], 16)
})

test_that("Test 8 : Doublon Sujet x Temps -> erreur explicite", {
  df_dup <- rbind(df_ref_rm, data.frame(Sujet = factor(1), Temps = factor("T1"), Y = 99))
  expect_error(
    ramses_anova_rm(df_dup, response = "Y", subject = "Sujet", within_factor = "Temps"),
    "Doublons detectes"
  )
})

test_that("Test 9 : Un seul niveau du facteur -> erreur explicite", {
  df_1lvl <- df_ref_rm[df_ref_rm$Temps == "T1", ]
  expect_error(
    ramses_anova_rm(df_1lvl, response = "Y", subject = "Sujet", within_factor = "Temps"),
    "au moins 2 modalites"
  )
})

test_that("Test 10 : Variable reponse non quantitative -> erreur explicite", {
  df_bad_y <- df_ref_rm
  df_bad_y$Y <- as.character(df_bad_y$Y)
  expect_error(
    ramses_anova_rm(df_bad_y, response = "Y", subject = "Sujet", within_factor = "Temps"),
    "doit etre de type numerique"
  )
})

test_that("Test 11 : Identifiant sujet manquant ou inexistant -> erreur", {
  expect_error(
    ramses_anova_rm(df_ref_rm, response = "Y", subject = "Inconnu", within_factor = "Temps"),
    "introuvable"
  )
})

test_that("Test 12 : Facteur non ordonne -> respect de l'ordre d'apparition sans alteration", {
  df_char_factor <- data.frame(
    Sujet = rep(1:5, times = 3),
    Temps = rep(c("Post", "Pre", "FollowUp"), each = 5),
    Y = rnorm(15, 50, 5)
  )
  res <- ramses_anova_rm(df_char_factor, response = "Y", subject = "Sujet", within_factor = "Temps")
  expect_equal(res$data_info$levels, c("Post", "Pre", "FollowUp"))
})

test_that("Test 13 : Bornes mathematiques des epsilons respectees", {
  # Test sur K = 4 niveaux
  set.seed(42)
  N_sub <- 15
  K_lvl <- 4
  df_k4 <- data.frame(
    Sujet = factor(rep(1:N_sub, times = K_lvl)),
    Temps = factor(rep(paste0("T", 1:K_lvl), each = N_sub)),
    Y = rnorm(N_sub * K_lvl, 20, 5)
  )
  res_k4 <- ramses_anova_rm(df_k4, response = "Y", subject = "Sujet", within_factor = "Temps")
  
  eps_gg <- res_k4$corrections$eps_gg
  eps_hf <- res_k4$corrections$eps_hf

  # Borne inferieure GG : 1 / (K - 1) = 1/3 = 0.3333
  expect_gte(eps_gg, 1 / (K_lvl - 1))
  expect_lte(eps_gg, 1.0)
  expect_lte(eps_hf, 1.0)
})

test_that("Test 14 : Concordance avec stats::aov(Y ~ Temps + Error(Sujet/Temps))", {
  fit_aov <- stats::aov(Y ~ Temps + Error(Sujet / Temps), data = df_ref_rm)
  smry_aov <- summary(fit_aov)

  # Extraction de la strate Error: Sujet:Temps
  strate_within <- smry_aov[["Error: Sujet:Temps"]][[1]]
  f_aov <- strate_within["Temps", "F value"]
  p_aov <- strate_within["Temps", "Pr(>F)"]
  ss_time_aov <- strate_within["Temps", "Sum Sq"]
  ss_res_aov <- strate_within["Residuals", "Sum Sq"]

  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")
  tab <- res$anova_table

  expect_equal(tab[tab$Source == "Temps", "Sum_Sq"], ss_time_aov, tolerance = 1e-6)
  expect_equal(tab[tab$Source == "R\u00e9sidus", "Sum_Sq"], ss_res_aov, tolerance = 1e-6)
  expect_equal(tab[tab$Source == "Temps", "F_value"], f_aov, tolerance = 1e-6)
  expect_equal(tab[tab$Source == "Temps", "p_value"], p_aov, tolerance = 1e-6)
})

test_that("Test 15 : Comparaisons post-hoc (tests t apparies et ajustement de Holm)", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")
  ph <- res$post_hoc

  # 3 niveaux -> 3 comparaisons (T1 vs T2, T1 vs T3, T2 vs T3)
  expect_equal(nrow(ph), 3)
  expect_true(all(c("Niveau_1", "Niveau_2", "Difference", "SE", "t_value", "Df", "p_value_raw", "p_value_adj", "sig") %in% names(ph)))
  
  # Comparaison manuelle T1 vs T2
  v1 <- df_ref_rm$Y[df_ref_rm$Temps == "T1"]
  v2 <- df_ref_rm$Y[df_ref_rm$Temps == "T2"]
  t_manual <- stats::t.test(v1, v2, paired = TRUE)

  row_12 <- ph[ph$Niveau_1 == "T1" & ph$Niveau_2 == "T2", ]
  expect_equal(row_12$t_value, unname(t_manual$statistic), tolerance = 1e-6)
  expect_equal(row_12$Difference, mean(v1 - v2), tolerance = 1e-6)
  expect_equal(row_12$p_value_raw, t_manual$p.value, tolerance = 1e-6)
})

test_that("Test 16 : Non-regression des autres methodes statistiques existantes", {
  # 1. ANOVA 1 facteur
  fit_aov1 <- stats::aov(Y ~ Temps, data = df_ref_rm)
  expect_true(inherits(fit_aov1, "aov"))

  # 2. ANOVA factorielle
  df_fact <- data.frame(
    Y = rnorm(20, 10, 2),
    A = factor(rep(c("A1", "A2"), each = 10)),
    B = factor(rep(c("B1", "B2"), times = 10))
  )
  res_fact <- ramses_anova_factorial(df_fact, "Y", "A", "B", interaction = TRUE)
  expect_true(is.data.frame(res_fact$anova_table))

  # 3. ANCOVA
  df_anc <- data.frame(
    Y = rnorm(20, 50, 5),
    G = factor(rep(c("G1", "G2"), each = 10)),
    X = rnorm(20, 20, 3)
  )
  res_anc <- ramses_ancova(df_anc, "Y", "G", "X")
  expect_true(is.data.frame(res_anc$anova_table))

  # 4. Kruskal-Wallis
  kw_res <- stats::kruskal.test(Y ~ Temps, data = df_ref_rm)
  expect_true(inherits(kw_res, "htest"))
})


# ==============================================================================
# TESTS PHASE 3 — GRAPHIQUES, R MARKDOWN ET VALIDATIONS FINALES
# ==============================================================================

# Sous-jeu K = 2 niveaux (T1 et T2)
df_ref_k2 <- df_ref_rm[df_ref_rm$Temps %in% c("T1", "T2"), ]
df_ref_k2$Temps <- droplevels(df_ref_k2$Temps)
res_k2_obj <- ramses_anova_rm(df_ref_k2, response = "Y", subject = "Sujet", within_factor = "Temps")
res_k3_obj <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

test_that("Phase 3 - Test 1 : Spaghetti plot genere avec succes (K = 2)", {
  p <- ramses_plot_anova_rm(res_k2_obj, type = "spaghetti", interactive = FALSE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("Phase 3 - Test 2 : Spaghetti plot genere avec succes (K >= 3)", {
  p <- ramses_plot_anova_rm(res_k3_obj, type = "spaghetti", interactive = FALSE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("Phase 3 - Test 3 : Graphique des moyennes genere avec succes (K = 2)", {
  p <- ramses_plot_anova_rm(res_k2_obj, type = "means", interactive = FALSE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("Phase 3 - Test 4 : Graphique des moyennes genere avec succes (K >= 3)", {
  p <- ramses_plot_anova_rm(res_k3_obj, type = "means", interactive = FALSE)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg"))
})

test_that("Phase 3 - Test 5 : Presence des elements du spaghetti plot (lignes individuelles, points, ligne moyenne, barres IC95)", {
  p <- ramses_plot_anova_rm(res_k3_obj, type = "spaghetti", interactive = FALSE)
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  # Doit contenir GeomLine (lignes individuelles + moyenne), GeomPoint (points individuels + moyenne), GeomErrorbar (IC 95%)
  expect_true("GeomLine" %in% geom_classes)
  expect_true("GeomPoint" %in% geom_classes)
  expect_true("GeomErrorbar" %in% geom_classes)
  expect_gte(length(p$layers), 4)
})

test_that("Phase 3 - Test 6 : Presence des elements du graphique des moyennes (points, barres IC95)", {
  p <- ramses_plot_anova_rm(res_k3_obj, type = "means", interactive = FALSE)
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomPoint" %in% geom_classes)
  expect_true("GeomErrorbar" %in% geom_classes)
  expect_true("GeomLine" %in% geom_classes)
})

test_that("Phase 3 - Test 7 : Exclusion des sujets incomplets repercutee sur les graphiques", {
  df_incomplete <- df_ref_rm
  df_incomplete$Y[df_incomplete$Sujet == 1 & df_incomplete$Temps == "T3"] <- NA

  res_inc <- ramses_anova_rm(df_incomplete, response = "Y", subject = "Sujet", within_factor = "Temps")
  p_inc <- ramses_plot_anova_rm(res_inc, type = "spaghetti", interactive = FALSE)

  # Sujet 1 doit etre absent des trajectoires graphiques
  subjects_in_plot <- unique(p_inc$layers[[1]]$data$subject)
  expect_equal(length(subjects_in_plot), 9)
  expect_false("1" %in% as.character(subjects_in_plot))
})

test_that("Phase 3 - Test 8 : Generation du R Markdown complet", {
  rmd_text <- ramses_rmd_anova_rm(res_k3_obj, ds_name = "df_ref_rm", include_posthoc = TRUE)

  expect_true(is.character(rmd_text))
  expect_gte(nchar(rmd_text), 200)
  expect_true(grepl("## A\\. Pr\u00e9sentation", rmd_text))
  expect_true(grepl("## B\\. M\u00e9thode", rmd_text))
  expect_true(grepl("## C\\. V\u00e9rifications", rmd_text))
  expect_true(grepl("## D\\. Tableau", rmd_text))
  expect_true(grepl("## E\\. Test de sph\u00e9ricit\u00e9", rmd_text))
  expect_true(grepl("## F\\. D\u00e9cision", rmd_text))
  expect_true(grepl("## G\\. Tailles d'effet", rmd_text))
  expect_true(grepl("## H\\. Comparaisons post-hoc", rmd_text))
  expect_true(grepl("## I\\. Visualisations", rmd_text))
})

test_that("Phase 3 - Test 9 : Presence du modele dans le R Markdown", {
  rmd_text <- ramses_rmd_anova_rm(res_k3_obj, ds_name = "df_ref_rm", include_posthoc = FALSE)
  expect_true(grepl("aov\\(`Y` ~ `Temps` \\+ Error\\(`Sujet`/`Temps`\\)\\)", rmd_text))
})

test_that("Phase 3 - Test 10 : Presence de Mauchly dans le R Markdown pour K >= 3", {
  rmd_text <- ramses_rmd_anova_rm(res_k3_obj, ds_name = "df_ref_rm", include_posthoc = FALSE)
  expect_true(grepl("Test de Mauchly", rmd_text))
  expect_true(grepl("W = 0\\.785", rmd_text))
})

test_that("Phase 3 - Test 11 : Absence de Mauchly pour K = 2", {
  rmd_k2 <- ramses_rmd_anova_rm(res_k2_obj, ds_name = "df_ref_k2", include_posthoc = FALSE)
  expect_false(grepl("Test de Mauchly :", rmd_k2))
  expect_true(grepl("automatiquement satisfaite", rmd_k2))
})

test_that("Phase 3 - Test 12 : Presence GG / HF dans le R Markdown pour K >= 3", {
  rmd_text <- ramses_rmd_anova_rm(res_k3_obj, ds_name = "df_ref_rm", include_posthoc = FALSE)
  expect_true(grepl("Greenhouse-Geisser", rmd_text))
  expect_true(grepl("Huynh-Feldt", rmd_text))
  expect_true(grepl("0\\.823", rmd_text))
  expect_true(grepl("0\\.983", rmd_text))
})

test_that("Phase 3 - Test 13 : Presence des post-hoc dans le R Markdown lorsque actives", {
  rmd_with_ph <- ramses_rmd_anova_rm(res_k3_obj, ds_name = "df_ref_rm", include_posthoc = TRUE)
  expect_true(grepl("## H\\. Comparaisons post-hoc", rmd_with_ph))
  expect_true(grepl("`T1` vs `T2`", rmd_with_ph))
  expect_true(grepl("`T1` vs `T3`", rmd_with_ph))
})

test_that("Phase 3 - Test 14 : Absence des post-hoc dans le R Markdown lorsqu'ils sont desactives", {
  rmd_no_ph <- ramses_rmd_anova_rm(res_k3_obj, ds_name = "df_ref_rm", include_posthoc = FALSE)
  expect_false(grepl("## H\\. Comparaisons post-hoc", rmd_no_ph))
})

test_that("Phase 3 - Test 15 : Non-regression ANOVA 1 facteur", {
  fit_aov1 <- stats::aov(Y ~ Temps, data = df_ref_rm)
  expect_true(inherits(fit_aov1, "aov"))
  expect_equal(length(coef(fit_aov1)), 3)
})

test_that("Phase 3 - Test 16 : Non-regression ANOVA factorielle", {
  df_fact <- data.frame(
    Y = rnorm(20, 10, 2),
    A = factor(rep(c("A1", "A2"), each = 10)),
    B = factor(rep(c("B1", "B2"), times = 10))
  )
  res_fact <- ramses_anova_factorial(df_fact, "Y", "A", "B", interaction = TRUE)
  expect_true(is.data.frame(res_fact$anova_table))
  expect_equal(nrow(res_fact$anova_table), 4)
})

test_that("Phase 3 - Test 17 : Non-regression ANCOVA", {
  df_anc <- data.frame(
    Y = rnorm(20, 50, 5),
    G = factor(rep(c("G1", "G2"), each = 10)),
    X = rnorm(20, 20, 3)
  )
  res_anc <- ramses_ancova(df_anc, "Y", "G", "X")
  expect_true(is.data.frame(res_anc$anova_table))
  expect_equal(nrow(res_anc$anova_table), 3)
})

test_that("Phase 3 - Test 18 : Non-regression Kruskal-Wallis et tests t", {
  kw_res <- stats::kruskal.test(Y ~ Temps, data = df_ref_rm)
  expect_true(inherits(kw_res, "htest"))

  t_ind <- stats::t.test(df_ref_rm$Y[df_ref_rm$Temps == "T1"], df_ref_rm$Y[df_ref_rm$Temps == "T2"])
  expect_true(inherits(t_ind, "htest"))

  t_pair <- stats::t.test(df_ref_rm$Y[df_ref_rm$Temps == "T1"], df_ref_rm$Y[df_ref_rm$Temps == "T2"], paired = TRUE)
  expect_true(inherits(t_pair, "htest"))
})

# ==============================================================================
# TESTS D'INTEGRATION UI — VALIDATION DU SCHEMA CONSOMME PAR MOD_TESTS.R
# ==============================================================================

test_that("Integration UI - Test 19 : Schema complet des champs consommes par mod_tests.R (K >= 3)", {
  res <- ramses_anova_rm(df_ref_rm, response = "Y", subject = "Sujet", within_factor = "Temps")

  # 1. Mauchly
  expect_true(is.numeric(res$mauchly$w))
  expect_true(is.numeric(res$mauchly$df))
  expect_true(is.numeric(res$mauchly$p_value))
  expect_equal(round(res$mauchly$w, 4), 0.7852)

  # 2. Corrections GG et HF
  expect_true(is.numeric(res$corrections$eps_gg))
  expect_true(is.numeric(res$corrections$df1_gg))
  expect_true(is.numeric(res$corrections$df2_gg))
  expect_true(is.numeric(res$corrections$p_gg))
  expect_equal(round(res$corrections$eps_gg, 4), 0.8232)

  expect_true(is.numeric(res$corrections$eps_hf))
  expect_true(is.numeric(res$corrections$df1_hf))
  expect_true(is.numeric(res$corrections$df2_hf))
  expect_true(is.numeric(res$corrections$p_hf))
  expect_equal(round(res$corrections$eps_hf, 4), 0.9835)

  # 3. Data info
  expect_equal(res$data_info$final_subjects, 10)
  expect_equal(res$data_info$k_levels, 3)
  expect_equal(res$data_info$n_obs, 30)

  # 4. Effect sizes
  expect_true(is.numeric(res$effect_sizes$eta_p_sq))
  expect_true(is.character(res$effect_sizes$eta_p_sq_mag))
  expect_true(is.numeric(res$effect_sizes$eta_g_sq))
  expect_equal(round(res$effect_sizes$eta_p_sq, 4), 0.9707)

  # 5. Simulation du formatage UI
  expect_no_error({
    w_str <- paste0("Statistique W = ", round(res$mauchly$w, 4), " ; ddl = ", res$mauchly$df, " ; p-value = ", format.pval(res$mauchly$p_value, digits = 4))
    gg_str <- paste0(round(res$corrections$eps_gg, 4), " (", round(res$corrections$df1_gg, 2), " ; ", round(res$corrections$df2_gg, 2), ")")
    hf_str <- paste0(round(res$corrections$eps_hf, 4), " (", round(res$corrections$df1_hf, 2), " ; ", round(res$corrections$df2_hf, 2), ")")
    es_str <- paste0(round(res$effect_sizes$eta_p_sq, 4), " (", res$effect_sizes$eta_p_sq_mag, ")")
    info_str <- paste0("N = ", res$data_info$final_subjects, " sujets | k = ", res$data_info$k_levels, " mesures")
  })
})

test_that("Integration UI - Test 20 : Schema et consommation sans erreur pour K = 2", {
  df_k2 <- df_ref_rm[df_ref_rm$Temps %in% c("T1", "T2"), ]
  df_k2$Temps <- droplevels(df_k2$Temps)
  res_k2 <- ramses_anova_rm(df_k2, response = "Y", subject = "Sujet", within_factor = "Temps")

  expect_false(res_k2$mauchly$applicable)
  expect_equal(res_k2$data_info$final_subjects, 10)
  expect_equal(res_k2$data_info$k_levels, 2)
  expect_equal(res_k2$data_info$n_obs, 20)

  expect_no_error({
    info_str <- paste0("N = ", res_k2$data_info$final_subjects, " sujets | k = ", res_k2$data_info$k_levels, " mesures")
    es_str <- paste0(round(res_k2$effect_sizes$eta_p_sq, 4), " (", res_k2$effect_sizes$eta_p_sq_mag, ")")
  })
})

# ==============================================================================
# TESTS DE PEDAGOGIE — VALIDATION DE RAMSES_PEDAGOGY_MULTI POUR ANOVA RM
# ==============================================================================

test_that("Pedagogie ANOVA RM - Test 21 : Generation sans erreur pour K = 3", {
  df_ref_ped <- data.frame(
    Sujet = factor(rep(1:10, times = 3)),
    Temps = factor(rep(c("T1", "T2", "T3"), each = 10), levels = c("T1", "T2", "T3")),
    Rendement = c(
      10, 12, 11, 9, 14, 13, 10, 12, 11, 15,
      13, 15, 14, 12, 18, 15, 14, 16, 13, 17,
      18, 20, 17, 19, 24, 21, 18, 22, 19, 23
    )
  )

  rm_res <- ramses_anova_rm(
    data = df_ref_ped,
    response = "Rendement",
    subject = "Sujet",
    within_factor = "Temps",
    alpha = 0.05
  )

  mock_state <- list(
    calculated = TRUE,
    test = "anova_rm",
    alpha = 0.05,
    var_y = "Rendement",
    var_group = "Temps",
    result = rm_res
  )

  ped_ui <- NULL
  expect_no_error({
    ped_ui <- ramses_pedagogy_multi(mock_state, df_ref_ped)
  })

  expect_true(!is.null(ped_ui))
  html_str <- as.character(ped_ui)
  expect_true(grepl("Structure des donn", html_str))
  expect_true(grepl("Mauchly", html_str))
  expect_true(grepl("N = 10 sujets", html_str))
  expect_true(grepl("3 modalit", html_str))
})

test_that("Pedagogie ANOVA RM - Test 22 : Generation sans erreur pour K = 2 (sans Mauchly)", {
  df_k2 <- data.frame(
    Sujet = factor(rep(1:10, times = 2)),
    Temps = factor(rep(c("T1", "T2"), each = 10), levels = c("T1", "T2")),
    Rendement = c(
      10, 12, 11, 9, 14, 13, 10, 12, 11, 15,
      13, 15, 14, 12, 18, 15, 14, 16, 13, 17
    )
  )

  rm_res_k2 <- ramses_anova_rm(
    data = df_k2,
    response = "Rendement",
    subject = "Sujet",
    within_factor = "Temps",
    alpha = 0.05
  )

  mock_state_k2 <- list(
    calculated = TRUE,
    test = "anova_rm",
    alpha = 0.05,
    var_y = "Rendement",
    var_group = "Temps",
    result = rm_res_k2
  )

  ped_ui_k2 <- NULL
  expect_no_error({
    ped_ui_k2 <- ramses_pedagogy_multi(mock_state_k2, df_k2)
  })

  expect_true(!is.null(ped_ui_k2))
  html_str <- as.character(ped_ui_k2)
  expect_true(grepl("Avec 2 modalit", html_str) || grepl("2 niveaux", html_str))
})

test_that("Pedagogie - Test 23 : Non-regression ANOVA 1 facteur, factorielle, ANCOVA, Kruskal-Wallis", {
  df_test <- data.frame(
    Y = c(10, 12, 14, 16, 20, 22, 24, 26, 30, 32, 34, 36),
    G1 = factor(rep(c("A", "B", "C"), each = 4)),
    G2 = factor(rep(c("M", "F"), times = 6)),
    X = c(1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6)
  )

  # 1. ANOVA 1 facteur
  aov1_fit <- stats::aov(Y ~ G1, data = df_test)
  state_aov1 <- list(calculated = TRUE, test = "anova", alpha = 0.05, var_y = "Y", var_group = "G1", result = aov1_fit)
  expect_no_error({
    ui1 <- ramses_pedagogy_multi(state_aov1, df_test)
    expect_true(!is.null(ui1))
  })

  # 2. Kruskal-Wallis
  kw_fit <- stats::kruskal.test(Y ~ G1, data = df_test)
  state_kw <- list(calculated = TRUE, test = "kruskal", alpha = 0.05, var_y = "Y", var_group = "G1", result = kw_fit)
  expect_no_error({
    ui_kw <- ramses_pedagogy_multi(state_kw, df_test)
    expect_true(!is.null(ui_kw))
  })
})


