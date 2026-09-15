# ==============================================================================
# Helpers utilitaires centralises pour la manipulation securisee des noms et valeurs R
# Fichier : R/utils_helpers.R
# ==============================================================================

#' Representation textuelle securisee d'un nom de symbole R pour la generation de code
#'
#' @param name Chaine de caracteres representant le nom d'une variable ou colonne.
#' @return Chaine de caracteres formatee (conservee brute si syntaxique, echappee par des backticks si non syntaxique).
#' @noRd
ramses_code_symbol <- function(name) {
  if (is.null(name) || !is.character(name) || length(name) != 1 || !nzchar(name)) {
    return(as.character(name))
  }
  # Verification si le nom est un identifiant R syntaxique valide et non un mot reserve
  is_syntactic <- (make.names(name) == name) &&
    !(name %in% c(
      "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
      "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
      "NA_complex_", "NA_character_", "..."
    ))

  if (is_syntactic) {
    return(name)
  }

  encodeString(name, quote = "`")
}

#' Representation textuelle securisee d'une valeur chaine ou d'un vecteur sous forme de litteral R
#'
#' @param x Chaine de caracteres ou vecteur de caracteres (ou valeur scalaire/vecteur quelconque).
#' @return Chaine de caracteres representant le litteral R exact (avec guillemets et echappements valides).
#' @noRd
ramses_code_string <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }
  if (length(x) == 0) {
    return("character(0)")
  }
  tryCatch({
    rlang::expr_text(x)
  }, error = function(e) {
    paste(deparse(x, width.cutoff = 500L), collapse = "\n")
  })
}

#' Reference securisee a une colonne de dataset pour la generation de code R (syntaxe [[ ]])
#'
#' @param dataset_name Nom du data.frame / tibble (ex: "dataset", "df").
#' @param col_name Nom de la colonne.
#' @return Chaine de caracteres au format dataset[["nom_colonne"]] avec echappement propre.
#' @noRd
ramses_code_column <- function(dataset_name, col_name) {
  ds_sym <- ramses_code_symbol(dataset_name)
  sprintf('%s[[%s]]', ds_sym, ramses_code_string(col_name))
}

#' Construction securisee d'une formule R (stats::formula) a partir de symboles
#'
#' @param response Nom de la variable reponse (Y, a gauche du ~). Si NULL ou vide, formule unilaterale (~ X).
#' @param terms Vecteur de caracteres contenant le ou les noms de variables explicatives (X, a droite du ~).
#' @param env Environnement associe a la formule (par defaut parent.frame()).
#' @param op Operateur de combinaison pour plusieurs termes ("+" par defaut, ou "*" pour interaction factorielle).
#' @return Un objet de classe \code{formula} interpretable par R.
#' @noRd
ramses_formula <- function(response = NULL, terms, env = parent.frame(), op = "+") {
  if (is.null(terms) || length(terms) == 0) {
    stop("terms doit etre un vecteur de noms de colonnes non vide.")
  }

  rhs_expr <- if (length(terms) == 1) {
    rlang::sym(terms[[1]])
  } else {
    op_fn <- if (identical(op, "*")) "*" else "+"
    Reduce(function(acc, x) rlang::call2(op_fn, acc, rlang::sym(x)), terms[-1], init = rlang::sym(terms[[1]]))
  }

  if (is.null(response) || !nzchar(response)) {
    rlang::new_formula(lhs = NULL, rhs = rhs_expr, env = env)
  } else {
    rlang::new_formula(lhs = rlang::sym(response), rhs = rhs_expr, env = env)
  }
}

#' Generation de code R textuel pour une formule avec protection des noms non syntaxiques
#'
#' @param response Nom de la variable reponse (Y). Si NULL ou vide, formule unilaterale.
#' @param terms Vecteur de caracteres contenant les noms des variables explicatives.
#' @param op Operateur de combinaison ("+" par defaut, ou "*").
#' @return Chaine de caracteres representant la formule en code R.
#' @noRd
ramses_formula_code <- function(response = NULL, terms, op = "+") {
  if (is.null(terms) || length(terms) == 0) {
    stop("terms doit etre un vecteur de noms de colonnes non vide.")
  }
  sep_str <- if (identical(op, "*")) " * " else " + "
  rhs_str <- paste(vapply(terms, ramses_code_symbol, character(1)), collapse = sep_str)
  if (is.null(response) || !nzchar(response)) {
    paste0("~ ", rhs_str)
  } else {
    paste0(ramses_code_symbol(response), " ~ ", rhs_str)
  }
}

#' Qualification qualitative de la taille d'effet Eta-carre partiel (eta_p^2)
#'
#' Seuils de reference de Cohen pour eta_p^2 / eta^2 :
#' - < 0.01 : effet negligeable
#' - 0.01 a 0.06 : effet faible
#' - 0.06 a 0.14 : effet moyen
#' - >= 0.14 : effet fort
#'
#' @param val Valeur numerique de l'eta-carre partiel
#' @return Chaine de caracteres avec la magnitude
#' @noRd
ramses_qualify_eta_p_sq <- function(val) {
  if (is.null(val) || is.na(val)) return("non calculable")
  if (val < 0.01) {
    "effet n\u00e9gligeable"
  } else if (val < 0.06) {
    "effet faible"
  } else if (val < 0.14) {
    "effet moyen"
  } else {
    "effet fort"
  }
}

#' Moteur de calcul d'une ANOVA factorielle a 2 facteurs (Type II ou Type III)
#'
#' Effectue une ANOVA factorielle rigoureuse sans dependance externe lourde :
#' - Sans interaction (A + B) : Sommes des carres de TYPE II (principe marginal)
#' - Avec interaction (A * B) : Sommes des carres de TYPE III avec contrastes orthogonaux sum-to-zero (contr.sum)
#' - Conversion explicite et securisee des variables explicatives en facteurs
#' - Extraction par noms de termes explicites (sans indices de position hardcodes)
#' - Calcul de l'Eta-carre partiel (eta_p^2 = SS_effet / (SS_effet + SS_residus))
#' - Diagnostics des residus (Shapiro-Wilk) et d'homogeneite des variances (Fligner-Killeen)
#'
#' @param df Data frame source
#' @param var_y Nom de la variable dependante quantitative
#' @param var_factor1 Nom du premier facteur
#' @param var_factor2 Nom du second facteur
#' @param interaction Booleen indiquant si l'interaction doit etre incluse (defaut: TRUE)
#' @param alpha Seuil de significativite (defaut: 0.05)
#' @return Liste structuree contenant :
#'   \item{model}{Objet lm ajuste}
#'   \item{anova_table}{Data.frame contenant les colonnes Term, Df, Sum_Sq, Mean_Sq, F_value, p_value, eta_p_sq, eta_p_sq_mag}
#'   \item{type_ss}{Type canonique des sommes des carres ("Type II" ou "Type III")}
#'   \item{type_ss_detail}{Description detaillee de la methode appliquee}
#'   \item{diagnostics}{Liste structuree des tests de diagnostic (shapiro, fligner)}
#'   \item{terms_info}{Liste indexee par nom de terme avec details statistiques}
#'   \item{residuals}{Vecteur des residus du modele}
#'   \item{shapiro}{Resultat de stats::shapiro.test sur les residus}
#'   \item{fligner}{Resultat de stats::fligner.test sur les cellules croisees}
#'   \item{cell_stats}{Tableau recapitulatif des moyennes, ecart-types et IC95 par cellule}
#'   \item{has_interaction}{Booleen}
#'   \item{var_y, var_factor1, var_factor2}{Noms des variables}
#'   \item{n_obs}{Nombre total d'observations valides}
#' @noRd
ramses_anova_factorial <- function(df, var_y, var_factor1, var_factor2, interaction = TRUE, alpha = 0.05) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le parametre 'df' doit etre un data.frame valide.")
  }
  if (is.null(var_y) || !var_y %in% names(df)) {
    stop(paste0("La variable dependante '", var_y, "' est introuvable."))
  }
  if (is.null(var_factor1) || !var_factor1 %in% names(df)) {
    stop(paste0("Le premier facteur '", var_factor1, "' est introuvable."))
  }
  if (is.null(var_factor2) || !var_factor2 %in% names(df)) {
    stop(paste0("Le second facteur '", var_factor2, "' est introuvable."))
  }
  if (identical(var_factor1, var_factor2)) {
    stop("Les deux facteurs explicatifs doivent etre distincts.")
  }

  # 1. Copie de travail securisee et filtrage des donnees completes
  clean_df <- df[!is.na(df[[var_y]]) & !is.na(df[[var_factor1]]) & !is.na(df[[var_factor2]]), , drop = FALSE]
  if (nrow(clean_df) < 4) {
    stop("Effectif insuffisant pour ajuster une ANOVA a 2 facteurs (minimum 4 observations completes requises).")
  }

  if (!is.numeric(clean_df[[var_y]])) {
    stop(paste0("La variable dependante '", var_y, "' doit etre quantitative."))
  }

  # Conversion explicite en facteurs qualitatifs avec nettoyage des niveaux inutilises
  clean_df[[var_factor1]] <- droplevels(as.factor(clean_df[[var_factor1]]))
  clean_df[[var_factor2]] <- droplevels(as.factor(clean_df[[var_factor2]]))

  k1 <- length(levels(clean_df[[var_factor1]]))
  k2 <- length(levels(clean_df[[var_factor2]]))

  if (k1 < 2) {
    stop(paste0("Le facteur '", var_factor1, "' doit comporter au moins 2 modalites distinctes (trouve : ", k1, ")."))
  }
  if (k2 < 2) {
    stop(paste0("Le facteur '", var_factor2, "' doit comporter au moins 2 modalites distinctes (trouve : ", k2, ")."))
  }

  # 2. Statistiques par cellule (A x B)
  cell_split <- split(clean_df[[var_y]], list(clean_df[[var_factor1]], clean_df[[var_factor2]]), drop = FALSE)
  cell_rows <- list()
  for (f1_lvl in levels(clean_df[[var_factor1]])) {
    for (f2_lvl in levels(clean_df[[var_factor2]])) {
      vals <- clean_df[[var_y]][clean_df[[var_factor1]] == f1_lvl & clean_df[[var_factor2]] == f2_lvl]
      n_cell <- length(vals)
      m_cell <- if (n_cell > 0) mean(vals) else NA_real_
      s_cell <- if (n_cell > 1) stats::sd(vals) else 0
      se_cell <- if (n_cell > 1) s_cell / sqrt(n_cell) else 0
      ci_rad <- if (n_cell > 1) stats::qt(0.975, df = n_cell - 1) * se_cell else 0
      cell_rows[[length(cell_rows) + 1]] <- data.frame(
        factor1 = f1_lvl,
        factor2 = f2_lvl,
        n = n_cell,
        mean = m_cell,
        sd = s_cell,
        se = se_cell,
        ci_lower = if (!is.na(m_cell)) m_cell - ci_rad else NA_real_,
        ci_upper = if (!is.na(m_cell)) m_cell + ci_rad else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  cell_stats <- do.call(rbind, cell_rows)
  names(cell_stats)[1:2] <- c(var_factor1, var_factor2)

  # 3. Ajustement selon Type II ou Type III
  term_a <- var_factor1
  term_b <- var_factor2
  term_ab <- paste0(var_factor1, ":", var_factor2)

  terms_info <- list()
  rows_list <- list()

  if (!isTRUE(interaction)) {
    # -------------------------------------------------------------
    # SANS INTERACTION : SOMMES DES CARRES TYPE II
    # -------------------------------------------------------------
    type_ss <- "Type II"
    type_ss_detail <- "Type II (mod\u00e8le additif A + B)"
    fml_full <- ramses_formula(response = var_y, terms = c(var_factor1, var_factor2), op = "+")
    mod_full <- stats::lm(fml_full, data = clean_df)

    rss_full <- sum(stats::residuals(mod_full)^2)
    df_res <- stats::df.residual(mod_full)
    if (df_res <= 0) {
      stop("Degres de liberte residuels nuls ou negatifs. Donnees insuffisantes pour estimer le modele.")
    }
    ms_res <- rss_full / df_res

    # Effet A (ajuste pour B) : RSS(B) - RSS(A + B)
    fml_no_a <- ramses_formula(response = var_y, terms = var_factor2)
    mod_no_a <- stats::lm(fml_no_a, data = clean_df)
    rss_no_a <- sum(stats::residuals(mod_no_a)^2)
    ss_a <- max(0, rss_no_a - rss_full)
    df_a <- k1 - 1
    ms_a <- if (df_a > 0) ss_a / df_a else 0
    f_a <- if (ms_res > 0) ms_a / ms_res else NA_real_
    p_a <- if (!is.na(f_a)) stats::pf(f_a, df_a, df_res, lower.tail = FALSE) else NA_real_
    eta_p_a <- if ((ss_a + rss_full) > 0) ss_a / (ss_a + rss_full) else 0

    terms_info[[term_a]] <- list(
      term = term_a,
      df = df_a,
      sum_sq = ss_a,
      mean_sq = ms_a,
      f_value = f_a,
      p_value = p_a,
      eta_p_sq = eta_p_a,
      eta_p_sq_mag = ramses_qualify_eta_p_sq(eta_p_a),
      sig = (!is.na(p_a) && p_a < alpha)
    )

    # Effet B (ajuste pour A) : RSS(A) - RSS(A + B)
    fml_no_b <- ramses_formula(response = var_y, terms = var_factor1)
    mod_no_b <- stats::lm(fml_no_b, data = clean_df)
    rss_no_b <- sum(stats::residuals(mod_no_b)^2)
    ss_b <- max(0, rss_no_b - rss_full)
    df_b <- k2 - 1
    ms_b <- if (df_b > 0) ss_b / df_b else 0
    f_b <- if (ms_res > 0) ms_b / ms_res else NA_real_
    p_b <- if (!is.na(f_b)) stats::pf(f_b, df_b, df_res, lower.tail = FALSE) else NA_real_
    eta_p_b <- if ((ss_b + rss_full) > 0) ss_b / (ss_b + rss_full) else 0

    terms_info[[term_b]] <- list(
      term = term_b,
      df = df_b,
      sum_sq = ss_b,
      mean_sq = ms_b,
      f_value = f_b,
      p_value = p_b,
      eta_p_sq = eta_p_b,
      eta_p_sq_mag = ramses_qualify_eta_p_sq(eta_p_b),
      sig = (!is.na(p_b) && p_b < alpha)
    )

    terms_info[["Residuals"]] <- list(
      term = "R\u00e9sidus",
      df = df_res,
      sum_sq = rss_full,
      mean_sq = ms_res,
      f_value = NA_real_,
      p_value = NA_real_,
      eta_p_sq = NA_real_,
      eta_p_sq_mag = "\u2014",
      sig = FALSE
    )

    anova_table <- data.frame(
      Term = c(term_a, term_b, "R\u00e9sidus"),
      Df = c(df_a, df_b, df_res),
      Sum_Sq = c(ss_a, ss_b, rss_full),
      Mean_Sq = c(ms_a, ms_b, ms_res),
      F_value = c(f_a, f_b, NA_real_),
      p_value = c(p_a, p_b, NA_real_),
      eta_p_sq = c(eta_p_a, eta_p_b, NA_real_),
      eta_p_sq_mag = c(ramses_qualify_eta_p_sq(eta_p_a), ramses_qualify_eta_p_sq(eta_p_b), "\u2014"),
      stringsAsFactors = FALSE
    )

  } else {
    # -------------------------------------------------------------
    # AVEC INTERACTION : SOMMES DES CARRES TYPE III (contr.sum)
    # -------------------------------------------------------------
    type_ss <- "Type III"
    type_ss_detail <- "Type III (contrastes orthogonaux contr.sum)"
    
    # Definition locale des contrastes de somme nulle pour garantir l'orthogonalite
    stats::contrasts(clean_df[[var_factor1]]) <- stats::contr.sum(levels(clean_df[[var_factor1]]))
    stats::contrasts(clean_df[[var_factor2]]) <- stats::contr.sum(levels(clean_df[[var_factor2]]))

    fml_full <- ramses_formula(response = var_y, terms = c(var_factor1, var_factor2), op = "*")
    mod_full <- stats::lm(fml_full, data = clean_df)

    rss_full <- sum(stats::residuals(mod_full)^2)
    df_res <- stats::df.residual(mod_full)
    if (df_res <= 0) {
      stop("Degres de liberte residuels nuls ou negatifs. Le nombre de cellules croisees egale ou depasse le nombre d'observations.")
    }
    ms_res <- rss_full / df_res

    # Utilisation de stats::drop1(mod_full, ~ ., test = "F") sous codage contr.sum
    d1 <- stats::drop1(mod_full, ~ ., test = "F")
    
    # Identification securisee par nom de terme (sans aucun indice numerique direct)
    # Dans drop1, les lignes correspondent aux noms des termes
    d1_rownames <- rownames(d1)

    # Extraction du terme A
    row_a_idx <- which(d1_rownames == term_a | d1_rownames == paste0("`", term_a, "`"))
    if (length(row_a_idx) == 0) {
      # Fallback modele marginal direct
      fml_no_a_int <- stats::as.formula(paste0("`", var_y, "` ~ `", var_factor2, "` + `", var_factor1, "`:`", var_factor2, "`"))
      mod_no_a_int <- stats::lm(fml_no_a_int, data = clean_df)
      ss_a <- max(0, sum(stats::residuals(mod_no_a_int)^2) - rss_full)
      df_a <- k1 - 1
    } else {
      df_a <- d1[row_a_idx[1], "Df"]
      ss_a <- d1[row_a_idx[1], "Sum of Sq"]
    }
    ms_a <- if (df_a > 0) ss_a / df_a else 0
    f_a <- if (ms_res > 0) ms_a / ms_res else NA_real_
    p_a <- if (!is.na(f_a)) stats::pf(f_a, df_a, df_res, lower.tail = FALSE) else NA_real_
    eta_p_a <- if ((ss_a + rss_full) > 0) ss_a / (ss_a + rss_full) else 0

    terms_info[[term_a]] <- list(
      term = term_a,
      df = df_a,
      sum_sq = ss_a,
      mean_sq = ms_a,
      f_value = f_a,
      p_value = p_a,
      eta_p_sq = eta_p_a,
      eta_p_sq_mag = ramses_qualify_eta_p_sq(eta_p_a),
      sig = (!is.na(p_a) && p_a < alpha)
    )

    # Extraction du terme B
    row_b_idx <- which(d1_rownames == term_b | d1_rownames == paste0("`", term_b, "`"))
    if (length(row_b_idx) == 0) {
      fml_no_b_int <- stats::as.formula(paste0("`", var_y, "` ~ `", var_factor1, "` + `", var_factor1, "`:`", var_factor2, "`"))
      mod_no_b_int <- stats::lm(fml_no_b_int, data = clean_df)
      ss_b <- max(0, sum(stats::residuals(mod_no_b_int)^2) - rss_full)
      df_b <- k2 - 1
    } else {
      df_b <- d1[row_b_idx[1], "Df"]
      ss_b <- d1[row_b_idx[1], "Sum of Sq"]
    }
    ms_b <- if (df_b > 0) ss_b / df_b else 0
    f_b <- if (ms_res > 0) ms_b / ms_res else NA_real_
    p_b <- if (!is.na(f_b)) stats::pf(f_b, df_b, df_res, lower.tail = FALSE) else NA_real_
    eta_p_b <- if ((ss_b + rss_full) > 0) ss_b / (ss_b + rss_full) else 0

    terms_info[[term_b]] <- list(
      term = term_b,
      df = df_b,
      sum_sq = ss_b,
      mean_sq = ms_b,
      f_value = f_b,
      p_value = p_b,
      eta_p_sq = eta_p_b,
      eta_p_sq_mag = ramses_qualify_eta_p_sq(eta_p_b),
      sig = (!is.na(p_b) && p_b < alpha)
    )

    # Extraction du terme d'interaction A:B
    row_ab_idx <- which(
      d1_rownames == term_ab | 
      d1_rownames == paste0(var_factor2, ":", var_factor1) |
      d1_rownames == paste0("`", var_factor1, "`:`", var_factor2, "`") |
      d1_rownames == paste0("`", var_factor2, "`:`", var_factor1, "`")
    )
    if (length(row_ab_idx) == 0) {
      fml_add <- ramses_formula(response = var_y, terms = c(var_factor1, var_factor2), op = "+")
      mod_add <- stats::lm(fml_add, data = clean_df)
      ss_ab <- max(0, sum(stats::residuals(mod_add)^2) - rss_full)
      df_ab <- (k1 - 1) * (k2 - 1)
    } else {
      df_ab <- d1[row_ab_idx[1], "Df"]
      ss_ab <- d1[row_ab_idx[1], "Sum of Sq"]
    }
    ms_ab <- if (df_ab > 0) ss_ab / df_ab else 0
    f_ab <- if (ms_res > 0) ms_ab / ms_res else NA_real_
    p_ab <- if (!is.na(f_ab)) stats::pf(f_ab, df_ab, df_res, lower.tail = FALSE) else NA_real_
    eta_p_ab <- if ((ss_ab + rss_full) > 0) ss_ab / (ss_ab + rss_full) else 0

    terms_info[[term_ab]] <- list(
      term = term_ab,
      df = df_ab,
      sum_sq = ss_ab,
      mean_sq = ms_ab,
      f_value = f_ab,
      p_value = p_ab,
      eta_p_sq = eta_p_ab,
      eta_p_sq_mag = ramses_qualify_eta_p_sq(eta_p_ab),
      sig = (!is.na(p_ab) && p_ab < alpha)
    )

    terms_info[["Residuals"]] <- list(
      term = "R\u00e9sidus",
      df = df_res,
      sum_sq = rss_full,
      mean_sq = ms_res,
      f_value = NA_real_,
      p_value = NA_real_,
      eta_p_sq = NA_real_,
      eta_p_sq_mag = "\u2014",
      sig = FALSE
    )

    anova_table <- data.frame(
      Term = c(term_a, term_b, term_ab, "R\u00e9sidus"),
      Df = c(df_a, df_b, df_ab, df_res),
      Sum_Sq = c(ss_a, ss_b, ss_ab, rss_full),
      Mean_Sq = c(ms_a, ms_b, ms_ab, ms_res),
      F_value = c(f_a, f_b, f_ab, NA_real_),
      p_value = c(p_a, p_b, p_ab, NA_real_),
      eta_p_sq = c(eta_p_a, eta_p_b, eta_p_ab, NA_real_),
      eta_p_sq_mag = c(ramses_qualify_eta_p_sq(eta_p_a), ramses_qualify_eta_p_sq(eta_p_b), ramses_qualify_eta_p_sq(eta_p_ab), "\u2014"),
      stringsAsFactors = FALSE
    )
  }

  # 4. Diagnostics statistiques
  resids <- stats::residuals(mod_full)
  shapiro_res <- if (length(resids) >= 3 && length(resids) <= 5000) {
    tryCatch(stats::shapiro.test(resids), error = function(e) NULL)
  } else NULL

  # Test de Fligner-Killeen sur la combinaison des facteurs
  grp_inter <- interaction(clean_df[[var_factor1]], clean_df[[var_factor2]], drop = TRUE)
  fligner_res <- if (length(unique(grp_inter)) >= 2) {
    tryCatch(stats::fligner.test(clean_df[[var_y]] ~ grp_inter), error = function(e) NULL)
  } else NULL

  diag_shapiro <- if (!is.null(shapiro_res)) {
    list(
      statistic = unname(shapiro_res$statistic),
      p_value = shapiro_res$p.value,
      p.value = shapiro_res$p.value,
      method = shapiro_res$method,
      htest = shapiro_res
    )
  } else {
    list(
      statistic = NA_real_,
      p_value = NA_real_,
      p.value = NA_real_,
      method = "Shapiro-Wilk normality test",
      htest = NULL
    )
  }

  diag_fligner <- if (!is.null(fligner_res)) {
    list(
      statistic = unname(fligner_res$statistic),
      p_value = fligner_res$p.value,
      p.value = fligner_res$p.value,
      method = fligner_res$method,
      test = "fligner",
      htest = fligner_res
    )
  } else {
    list(
      statistic = NA_real_,
      p_value = NA_real_,
      p.value = NA_real_,
      method = "Fligner-Killeen test of homogeneity of variances",
      test = "fligner",
      htest = NULL
    )
  }

  diagnostics_fact <- list(
    shapiro = diag_shapiro,
    homoscedasticity = diag_fligner,
    fligner = diag_fligner
  )

  res_obj <- list(
    model = mod_full,
    anova_table = anova_table,
    type_ss = type_ss,
    type_ss_detail = type_ss_detail,
    contrasts = if (isTRUE(interaction)) "contr.sum" else "default",
    terms_info = terms_info,
    residuals = resids,
    diagnostics = diagnostics_fact,
    shapiro = shapiro_res,
    fligner = fligner_res,
    cell_stats = cell_stats,
    has_interaction = isTRUE(interaction),
    var_y = var_y,
    var_factor1 = var_factor1,
    var_factor2 = var_factor2,
    n_obs = nrow(clean_df),
    alpha = alpha
  )
  class(res_obj) <- c("ramses_anova_factorial", "list")
  res_obj
}


#' Moteur de calcul d'une ANCOVA (1 facteur inter-sujets + 1 covariable quantitative)
#'
#' Effectue une Analyse de Covariance rigoureuse sans dependance externe :
#' - Controle prealable de l'homogeneite des pentes (modele Y ~ Facteur * Covariable)
#' - Ajustement du modele ANCOVA principal (Y ~ Facteur + Covariable)
#' - Sommes des carres de TYPE II via stats::drop1(mod, test = "F")
#' - Calcul de l'Eta-carre partiel (eta_p^2) pour le facteur et la covariable
#' - Calcul des moyennes ajustees evaluees a la moyenne globale de la covariable (+ IC95%)
#' - Comparaisons multiples par paires des moyennes ajustees avec correction de Holm
#' - Diagnostics des residus (Shapiro-Wilk) et d'homogeneite des variances (Fligner-Killeen)
#'
#' @param df Data frame source
#' @param var_y Nom de la variable dependante quantitative
#' @param var_factor Nom du facteur qualitatif inter-sujets
#' @param var_covar Nom de la covariable quantitative
#' @param alpha Seuil de significativite (defaut: 0.05)
#' @return Liste structuree contenant tous les resultats de l'ANCOVA
#' @noRd
ramses_ancova <- function(df, var_y, var_factor, var_covar, alpha = 0.05) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le parametre 'df' doit etre un data.frame valide.")
  }
  if (is.null(var_y) || !var_y %in% names(df)) {
    stop(paste0("La variable dependante '", var_y, "' est introuvable."))
  }
  if (is.null(var_factor) || !var_factor %in% names(df)) {
    stop(paste0("Le facteur '", var_factor, "' est introuvable."))
  }
  if (is.null(var_covar) || !var_covar %in% names(df)) {
    stop(paste0("La covariable '", var_covar, "' est introuvable."))
  }
  if (identical(var_y, var_factor) || identical(var_y, var_covar) || identical(var_factor, var_covar)) {
    stop("La variable dependante, le facteur et la covariable doivent etre trois variables distinctes.")
  }

  alpha_num <- as.numeric(alpha)
  if (is.na(alpha_num) || alpha_num <= 0 || alpha_num >= 1) {
    alpha_num <- 0.05
  }

  # 1. Nettoyage securise des donnees completes
  clean_df <- df[!is.na(df[[var_y]]) & !is.na(df[[var_factor]]) & !is.na(df[[var_covar]]), , drop = FALSE]
  if (nrow(clean_df) < 4) {
    stop("Effectif insuffisant pour ajuster une ANCOVA (minimum 4 observations completes requises).")
  }

  if (!is.numeric(clean_df[[var_y]])) {
    stop(paste0("La variable dependante '", var_y, "' doit etre de type numerique."))
  }
  if (!is.numeric(clean_df[[var_covar]])) {
    stop(paste0("La covariable '", var_covar, "' doit etre de type numerique (quantitative)."))
  }

  # Verification de la variabilite de la covariable
  sd_covar <- stats::sd(clean_df[[var_covar]])
  if (is.na(sd_covar) || sd_covar == 0) {
    stop(paste0("La covariable '", var_covar, "' a une variance nulle (valeur constante). Elle ne peut pas etre utilisee comme covariable."))
  }

  # Conversion explicite du facteur
  clean_df[[var_factor]] <- droplevels(as.factor(clean_df[[var_factor]]))
  k_levels <- levels(clean_df[[var_factor]])
  k <- length(k_levels)
  if (k < 2) {
    stop(paste0("Le facteur '", var_factor, "' doit comporter au moins 2 modalites distinctes (trouve : ", k, ")."))
  }

  # 2. Test d'homogeneite des pentes (modele Y ~ Facteur * Covariable)
  fml_pentes <- ramses_formula(response = var_y, terms = c(var_factor, var_covar), op = "*")
  mod_pentes <- stats::lm(fml_pentes, data = clean_df)
  
  d1_pentes <- stats::drop1(mod_pentes, ~ ., test = "F")
  d1_p_names <- rownames(d1_pentes)
  
  term_int_candidates <- c(
    paste0(var_factor, ":", var_covar),
    paste0(var_covar, ":", var_factor),
    paste0("`", var_factor, "`:`", var_covar, "`"),
    paste0("`", var_covar, "`:`", var_factor, "`")
  )
  row_int_idx <- which(d1_p_names %in% term_int_candidates)
  
  if (length(row_int_idx) > 0) {
    df_int <- d1_pentes[row_int_idx[1], "Df"]
    ss_int <- d1_pentes[row_int_idx[1], "Sum of Sq"]
    f_int <- d1_pentes[row_int_idx[1], "F value"]
    p_int <- d1_pentes[row_int_idx[1], "Pr(>F)"]
  } else {
    # Fallback par comparaison de modeles anova(mod_main, mod_pentes)
    fml_main_tmp <- ramses_formula(response = var_y, terms = c(var_factor, var_covar), op = "+")
    mod_main_tmp <- stats::lm(fml_main_tmp, data = clean_df)
    comp_aov <- stats::anova(mod_main_tmp, mod_pentes)
    df_int <- comp_aov$Df[2]
    ss_int <- comp_aov$`Sum of Sq`[2]
    f_int <- comp_aov$F[2]
    p_int <- comp_aov$`Pr(>F)`[2]
  }

  pentes_homogenes <- (!is.na(p_int) && p_int > alpha_num)

  # 3. Modele ANCOVA principal (additif Y ~ Facteur + Covariable)
  fml_main <- ramses_formula(response = var_y, terms = c(var_factor, var_covar), op = "+")
  mod_main <- stats::lm(fml_main, data = clean_df)

  rss_main <- sum(stats::residuals(mod_main)^2)
  df_res <- stats::df.residual(mod_main)
  if (df_res <= 0) {
    stop("Degres de liberte residuels nuls ou negatifs. Effectif insuffisant pour estimer le modele ANCOVA.")
  }
  ms_res <- rss_main / df_res

  # Sommes des carres Type II via drop1
  d1_main <- stats::drop1(mod_main, test = "F")
  d1_m_names <- rownames(d1_main)

  row_fact_idx <- which(d1_m_names %in% c(var_factor, paste0("`", var_factor, "`")))
  row_cov_idx <- which(d1_m_names %in% c(var_covar, paste0("`", var_covar, "`")))

  # Facteur
  if (length(row_fact_idx) > 0) {
    df_fact <- d1_main[row_fact_idx[1], "Df"]
    ss_fact <- d1_main[row_fact_idx[1], "Sum of Sq"]
    f_fact <- d1_main[row_fact_idx[1], "F value"]
    p_fact <- d1_main[row_fact_idx[1], "Pr(>F)"]
  } else {
    df_fact <- k - 1
    ss_fact <- 0
    f_fact <- NA_real_
    p_fact <- NA_real_
  }
  ms_fact <- if (df_fact > 0) ss_fact / df_fact else 0
  eta_p_fact <- if ((ss_fact + rss_main) > 0) ss_fact / (ss_fact + rss_main) else 0

  # Covariable
  if (length(row_cov_idx) > 0) {
    df_cov <- d1_main[row_cov_idx[1], "Df"]
    ss_cov <- d1_main[row_cov_idx[1], "Sum of Sq"]
    f_cov <- d1_main[row_cov_idx[1], "F value"]
    p_cov <- d1_main[row_cov_idx[1], "Pr(>F)"]
  } else {
    df_cov <- 1
    ss_cov <- 0
    f_cov <- NA_real_
    p_cov <- NA_real_
  }
  ms_cov <- if (df_cov > 0) ss_cov / df_cov else 0
  eta_p_cov <- if ((ss_cov + rss_main) > 0) ss_cov / (ss_cov + rss_main) else 0

  anova_table <- data.frame(
    Term = c(var_factor, var_covar, "R\u00e9sidus"),
    Df = c(df_fact, df_cov, df_res),
    Sum_Sq = c(ss_fact, ss_cov, rss_main),
    Mean_Sq = c(ms_fact, ms_cov, ms_res),
    F_value = c(f_fact, f_cov, NA_real_),
    p_value = c(p_fact, p_cov, NA_real_),
    eta_p_sq = c(eta_p_fact, eta_p_cov, NA_real_),
    eta_p_sq_mag = c(ramses_qualify_eta_p_sq(eta_p_fact), ramses_qualify_eta_p_sq(eta_p_cov), "\u2014"),
    stringsAsFactors = FALSE
  )

  # 4. Moyennes ajustees (evaluees a la moyenne globale de la covariable)
  mean_covar <- mean(clean_df[[var_covar]])
  grid_df <- data.frame(
    tmp_fact = factor(k_levels, levels = k_levels),
    tmp_cov = rep(mean_covar, k),
    stringsAsFactors = FALSE
  )
  names(grid_df) <- c(var_factor, var_covar)

  pred_out <- stats::predict(mod_main, newdata = grid_df, se.fit = TRUE)
  t_crit <- stats::qt(1 - alpha_num / 2, df = df_res)

  raw_means <- vapply(k_levels, function(lvl) {
    vals <- clean_df[[var_y]][clean_df[[var_factor]] == lvl]
    if (length(vals) > 0) mean(vals) else NA_real_
  }, numeric(1))

  counts_by_group <- as.numeric(table(clean_df[[var_factor]]))

  adjusted_means_table <- data.frame(
    Groupe = k_levels,
    N = counts_by_group,
    Moyenne_Brute = unname(raw_means),
    Moyenne_Ajustee = unname(pred_out$fit),
    SE = unname(pred_out$se.fit),
    CI_lower = unname(pred_out$fit - t_crit * pred_out$se.fit),
    CI_upper = unname(pred_out$fit + t_crit * pred_out$se.fit),
    stringsAsFactors = FALSE
  )

  # 5. Comparaisons multiples deux a deux des moyennes ajustees (Correction de Holm)
  pairs_list <- list()
  if (k >= 2) {
    # Construction de la matrice de design pour les moyennes ajustees
    X_grid <- stats::model.matrix(stats::delete.response(stats::terms(mod_main)), data = grid_df)
    V_mat <- stats::vcov(mod_main)

    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        g1 <- k_levels[i]
        g2 <- k_levels[j]
        m1_adj <- pred_out$fit[i]
        m2_adj <- pred_out$fit[j]
        diff_val <- m1_adj - m2_adj

        L_diff <- X_grid[i, ] - X_grid[j, ]
        var_diff <- as.numeric(t(L_diff) %*% V_mat %*% L_diff)
        se_diff <- if (var_diff > 0) sqrt(var_diff) else 0

        t_stat <- if (se_diff > 0) diff_val / se_diff else NA_real_
        p_raw <- if (!is.na(t_stat)) 2 * stats::pt(abs(t_stat), df = df_res, lower.tail = FALSE) else NA_real_

        ci_l <- if (se_diff > 0) diff_val - t_crit * se_diff else NA_real_
        ci_u <- if (se_diff > 0) diff_val + t_crit * se_diff else NA_real_

        pairs_list[[length(pairs_list) + 1]] <- data.frame(
          Groupe_1 = g1,
          Groupe_2 = g2,
          Difference = diff_val,
          SE = se_diff,
          t_value = t_stat,
          p_value_raw = p_raw,
          CI_lower = ci_l,
          CI_upper = ci_u,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  post_hoc_table <- if (length(pairs_list) > 0) {
    ph_df <- do.call(rbind, pairs_list)
    ph_df$p_value_adj <- stats::p.adjust(ph_df$p_value_raw, method = "holm")
    ph_df$sig <- (!is.na(ph_df$p_value_adj) & ph_df$p_value_adj < alpha_num)
    ph_df
  } else {
    data.frame(
      Groupe_1 = character(0),
      Groupe_2 = character(0),
      Difference = numeric(0),
      SE = numeric(0),
      t_value = numeric(0),
      p_value_raw = numeric(0),
      p_value_adj = numeric(0),
      CI_lower = numeric(0),
      CI_upper = numeric(0),
      sig = logical(0),
      stringsAsFactors = FALSE
    )
  }

  # 6. Diagnostics statistiques
  resids_main <- stats::residuals(mod_main)
  shapiro_res <- if (length(resids_main) >= 3 && length(resids_main) <= 5000) {
    tryCatch(stats::shapiro.test(resids_main), error = function(e) NULL)
  } else NULL

  fligner_res <- if (k >= 2) {
    tryCatch(stats::fligner.test(resids_main ~ clean_df[[var_factor]]), error = function(e) NULL)
  } else NULL

  diag_shapiro <- if (!is.null(shapiro_res)) {
    list(
      statistic = unname(shapiro_res$statistic),
      p_value = shapiro_res$p.value,
      p.value = shapiro_res$p.value,
      method = shapiro_res$method,
      htest = shapiro_res
    )
  } else {
    list(
      statistic = NA_real_,
      p_value = NA_real_,
      p.value = NA_real_,
      method = "Shapiro-Wilk normality test",
      htest = NULL
    )
  }

  diag_homo <- if (!is.null(fligner_res)) {
    list(
      statistic = unname(fligner_res$statistic),
      p_value = fligner_res$p.value,
      p.value = fligner_res$p.value,
      method = fligner_res$method,
      test = "fligner",
      htest = fligner_res
    )
  } else {
    list(
      statistic = NA_real_,
      p_value = NA_real_,
      p.value = NA_real_,
      method = "Fligner-Killeen test of homogeneity of variances",
      test = "fligner",
      htest = NULL
    )
  }

  diagnostics_list <- list(
    shapiro = diag_shapiro,
    homoscedasticity = diag_homo,
    fligner = diag_homo
  )

  list(
    model_main = mod_main,
    model_slopes = mod_pentes,
    model_pentes = mod_pentes,
    anova_table = anova_table,
    adjusted_means = adjusted_means_table,
    post_hoc = post_hoc_table,
    mean_covar = mean_covar,
    slopes_test = list(
      df = df_int,
      sum_sq = ss_int,
      f_value = f_int,
      p_value = p_int,
      pentes_homogenes = pentes_homogenes
    ),
    diagnostics = diagnostics_list,
    shapiro = shapiro_res,
    fligner = fligner_res,
    residuals = resids_main,
    var_y = var_y,
    var_factor = var_factor,
    var_covar = var_covar,
    n_obs = nrow(clean_df),
    alpha = alpha_num
  )
}

#' Moteur d'estimation complet pour l'ANOVA a mesures repetees (1 facteur intra-sujets)
#'
#' Ajuste le modele d'ANOVA intra-sujets Y ~ Temps + Error(Sujet) pour un plan complet et equilibre.
#' Inclut la decomposition exacte des carres (SS), les degres de liberte, les carres moyens (MS),
#' la statistique F et sa p-value brute, le test de sphericite de Mauchly (W, chi2, df, p) pour K >= 3,
#' les corrections d'epsilon de Greenhouse-Geisser et Huynh-Feldt (avec degres de liberte et p-values corriges),
#' les tailles d'effet eta_p^2 et eta_g^2, le tableau des moyennes par niveau, ainsi que les comparaisons
#' post-hoc appariees deux a deux avec correction de Holm.
#'
#' @param data Data frame contenant les donnees.
#' @param response Nom de la variable dependante quantitative (Y).
#' @param subject Nom de la variable d'identification des sujets (Sujet).
#' @param within_factor Nom du facteur qualitatif intra-sujets (Temps / Condition).
#' @param alpha Seuil de significativite (defaut: 0.05).
#' @return Liste structuree contenant tous les resultats de l'ANOVA a mesures repetees.
#' @export
ramses_anova_rm <- function(data,
                            response,
                            subject,
                            within_factor,
                            alpha = 0.05) {
  # 1. Validations initiales des parametres
  if (is.null(data) || !is.data.frame(data)) {
    stop("Le parametre 'data' doit etre un data.frame valide.")
  }
  if (is.null(response) || !is.character(response) || length(response) != 1 || !nzchar(response)) {
    stop("La variable reponse 'response' doit etre une chaine de caracteres non vide.")
  }
  if (!response %in% names(data)) {
    stop(paste0("La variable reponse '", response, "' est introuvable dans le jeu de donnees."))
  }
  if (is.null(subject) || !is.character(subject) || length(subject) != 1 || !nzchar(subject)) {
    stop("La variable identifiant 'subject' doit etre une chaine de caracteres non vide.")
  }
  if (!subject %in% names(data)) {
    stop(paste0("La variable sujet '", subject, "' est introuvable dans le jeu de donnees."))
  }
  if (is.null(within_factor) || !is.character(within_factor) || length(within_factor) != 1 || !nzchar(within_factor)) {
    stop("La variable de facteur intra-sujets 'within_factor' doit etre une chaine de caracteres non vide.")
  }
  if (!within_factor %in% names(data)) {
    stop(paste0("Le facteur intra-sujets '", within_factor, "' est introuvable dans le jeu de donnees."))
  }
  if (identical(response, subject) || identical(response, within_factor) || identical(subject, within_factor)) {
    stop("La variable reponse, la variable sujet et le facteur intra-sujets doivent etre trois variables distinctes.")
  }

  alpha_num <- as.numeric(alpha)
  if (is.na(alpha_num) || alpha_num <= 0 || alpha_num >= 1) {
    alpha_num <- 0.05
  }

  # 2. Validation du type quantitatif de la reponse
  if (!is.numeric(data[[response]])) {
    stop(paste0("La variable reponse '", response, "' doit etre de type numerique (quantitative)."))
  }

  # 3. Nettoyage des NA initiaux
  valid_rows <- !is.na(data[[response]]) & !is.na(data[[subject]]) & !is.na(data[[within_factor]])
  df_clean <- data[valid_rows, , drop = FALSE]

  if (nrow(df_clean) == 0) {
    stop("Aucune observation conjointe valide non manquante pour les trois variables selectionnees.")
  }

  # 4. Preparation du facteur intra-sujets
  # Conserver l'ordre des niveaux existants si c'est deja un facteur, sinon ordre d'apparition
  if (is.factor(df_clean[[within_factor]])) {
    df_clean[[within_factor]] <- droplevels(df_clean[[within_factor]])
  } else {
    uniq_levels <- unique(as.character(df_clean[[within_factor]]))
    df_clean[[within_factor]] <- factor(as.character(df_clean[[within_factor]]), levels = uniq_levels)
  }
  k_levels <- levels(df_clean[[within_factor]])
  k <- length(k_levels)
  if (k < 2) {
    stop(paste0("Le facteur intra-sujets '", within_factor, "' doit comporter au moins 2 modalites distinctes (trouve : ", k, ")."))
  }

  # 5. Conversion du sujet en identifiant texte/facteur
  df_clean[[subject]] <- as.character(df_clean[[subject]])
  initial_subjects_vec <- unique(df_clean[[subject]])
  initial_subjects_count <- length(initial_subjects_vec)

  if (initial_subjects_count < 2) {
    stop("Le jeu de donnees doit contenir au moins 2 sujets distincts.")
  }

  # 6. Detection des doublons Sujet x Temps
  counts_table <- table(df_clean[[subject]], df_clean[[within_factor]])
  if (any(counts_table > 1)) {
    stop(paste0(
      "Doublons detectes : certains sujets possedent plusieurs mesures pour une meme modalite du facteur '",
      within_factor, "'. Chaque sujet ne doit avoir qu'une seule observation par niveau."
    ))
  }

  # 7. Filtrage des sujets incomplets (complete cases par sujet sur les K niveaux)
  # Un sujet est complet s'il possede exactement 1 mesure pour chacun des K niveaux
  complete_subjects_mask <- apply(counts_table == 1, 1, all)
  complete_subjects <- names(complete_subjects_mask)[complete_subjects_mask]
  final_subjects_count <- length(complete_subjects)
  excluded_subjects_count <- initial_subjects_count - final_subjects_count

  if (final_subjects_count < 2) {
    stop(paste0(
      "Effectif insuffisant apres exclusion des sujets incomplets : seuls ",
      final_subjects_count, " sujet(s) possede(nt) l'ensemble des ", k,
      " mesures completes. Au moins 2 sujets complets sont requis pour l'ANOVA a mesures repetees."
    ))
  }

  final_df <- df_clean[df_clean[[subject]] %in% complete_subjects, , drop = FALSE]
  final_df[[subject]] <- factor(final_df[[subject]], levels = complete_subjects)

  # 8. Construction de la matrice ordonnee Sujets (lignes) x Temps (colonnes)
  N <- final_subjects_count
  Y_mat <- matrix(NA_real_, nrow = N, ncol = k, dimnames = list(complete_subjects, k_levels))
  for (i_sub in seq_len(N)) {
    s_id <- complete_subjects[i_sub]
    for (j_lvl in seq_len(k)) {
      lvl_name <- k_levels[j_lvl]
      val <- final_df[[response]][final_df[[subject]] == s_id & final_df[[within_factor]] == lvl_name]
      Y_mat[i_sub, j_lvl] <- val[1]
    }
  }

  # 9. Calculs de l'ANOVA a mesures repetees (decomposition exacte des carres)
  mean_grand <- mean(Y_mat)
  means_by_level <- colMeans(Y_mat)
  means_by_subject <- rowMeans(Y_mat)

  # SS Total
  ss_total <- sum((Y_mat - mean_grand)^2)
  df_total <- N * k - 1

  # SS Sujet (entre-sujets)
  ss_subject <- k * sum((means_by_subject - mean_grand)^2)
  df_subject <- N - 1

  # SS Facteur (intra-sujets)
  ss_factor <- N * sum((means_by_level - mean_grand)^2)
  df_factor <- k - 1

  # SS Residus (interaction Sujet x Facteur)
  # ss_resids = sum_{i,j} (Y_{ij} - mean_i - mean_j + mean_grand)^2
  mat_fitted <- matrix(means_by_subject, nrow = N, ncol = k, byrow = FALSE) +
    matrix(means_by_level, nrow = N, ncol = k, byrow = TRUE) -
    mean_grand
  mat_residuals <- Y_mat - mat_fitted

  ss_resids <- sum(mat_residuals^2)
  df_resids <- df_subject * df_factor

  # Robustesse numerique pour ss_resids
  if (ss_resids < 0) ss_resids <- 0

  # Carres moyens (MS)
  ms_factor <- if (df_factor > 0) ss_factor / df_factor else 0
  ms_resids <- if (df_resids > 0) ss_resids / df_resids else 0
  ms_subject <- if (df_subject > 0) ss_subject / df_subject else 0

  # Statistique F et p-value brute
  if (ms_resids > 0) {
    f_stat <- ms_factor / ms_resids
    p_raw <- stats::pf(f_stat, df1 = df_factor, df2 = df_resids, lower.tail = FALSE)
  } else if (ms_factor == 0 && ms_resids == 0) {
    f_stat <- NA_real_
    p_raw <- NA_real_
  } else {
    f_stat <- Inf
    p_raw <- 0
  }

  # 10. Tailles d'effet exactes
  # eta_p_sq (partiel) = SS_factor / (SS_factor + SS_resids)
  eta_p_sq <- if ((ss_factor + ss_resids) > 0) ss_factor / (ss_factor + ss_resids) else 0
  # eta_g_sq (generalise) = SS_factor / SS_total
  eta_g_sq <- if (ss_total > 0) ss_factor / ss_total else 0

  eta_p_sq_mag <- ramses_qualify_eta_p_sq(eta_p_sq)

  # Tableau d'ANOVA structure
  anova_table <- data.frame(
    Source = c(within_factor, "Sujets", "R\u00e9sidus"),
    Df = c(df_factor, df_subject, df_resids),
    Sum_Sq = c(ss_factor, ss_subject, ss_resids),
    Mean_Sq = c(ms_factor, ms_subject, ms_resids),
    F_value = c(f_stat, NA_real_, NA_real_),
    p_value = c(p_raw, NA_real_, NA_real_),
    eta_p_sq = c(eta_p_sq, NA_real_, NA_real_),
    eta_g_sq = c(eta_g_sq, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  # 11. Tableau descriptif des moyennes et ecarts-types par niveau
  sds_by_level <- apply(Y_mat, 2, stats::sd)
  se_by_level <- sds_by_level / sqrt(N)
  t_crit <- stats::qt(1 - alpha_num / 2, df = pmax(1, N - 1))

  means_table <- data.frame(
    Niveau = k_levels,
    N = rep(N, k),
    Moyenne = unname(means_by_level),
    Ecart_Type = unname(sds_by_level),
    SE = unname(se_by_level),
    CI_lower = unname(means_by_level - t_crit * se_by_level),
    CI_upper = unname(means_by_level + t_crit * se_by_level),
    stringsAsFactors = FALSE
  )

  # 12. Test de sphericite de Mauchly & Corrections d'Epsilon
  if (k == 2) {
    # Cas K = 2 : Sphericite triviale et exacte, Mauchly non applicable
    mauchly_res <- list(
      applicable = FALSE,
      w = 1.0,
      statistic = NA_real_,
      df = 0,
      p_value = NA_real_,
      message = "Mauchly non applicable : 2 niveaux. Sph\u00e9ricit\u00e9 automatiquement satisfaite."
    )

    corrections_res <- list(
      applicable = FALSE,
      eps_gg = 1.0,
      df1_gg = df_factor,
      df2_gg = df_resids,
      p_gg = p_raw,
      eps_hf = 1.0,
      df1_hf = df_factor,
      df2_hf = df_resids,
      p_hf = p_raw,
      message = "Corrections non requises : sph\u00e9ricit\u00e9 exacte (K = 2)."
    )
  } else {
    # Cas K >= 3 : Calcul matriciel autonome de Mauchly et des Epsilons
    p_dim <- k - 1
    
    # Matrice de contrastes orthonormes K x (K - 1)
    C_raw <- stats::contr.helmert(k)
    C_ortho <- qr.Q(qr(C_raw))[, 1:p_dim, drop = FALSE]

    # Projection des donnees et matrice de covariance des contrastes S
    X_proj <- Y_mat %*% C_ortho
    S_mat <- stats::cov(X_proj)

    tr_S <- sum(diag(S_mat))
    det_S <- det(S_mat)
    tr_S2 <- sum(S_mat * t(S_mat)) # Egal a Tr(S %*% S)

    # Statistique W de Mauchly
    mean_trace <- tr_S / p_dim
    w_mauchly <- if (mean_trace > 0 && det_S > 0) {
      det_S / (mean_trace^p_dim)
    } else {
      0
    }
    # Bornage de W dans [0, 1]
    w_mauchly <- max(0, min(1, w_mauchly))

    # Facteur correctif de Box d
    d_box <- 1 - ((2 * p_dim^2 + p_dim + 2) / (6 * p_dim * (N - 1)))
    df_mauchly <- (p_dim * (p_dim + 1)) / 2 - 1

    if (w_mauchly > 0 && df_mauchly > 0) {
      chi2_mauchly <- -(N - 1) * d_box * log(w_mauchly)
      p_mauchly <- stats::pchisq(chi2_mauchly, df = df_mauchly, lower.tail = FALSE)
    } else {
      chi2_mauchly <- Inf
      p_mauchly <- 0
    }

    mauchly_res <- list(
      applicable = TRUE,
      w = w_mauchly,
      statistic = chi2_mauchly,
      df = df_mauchly,
      p_value = p_mauchly,
      message = if (!is.na(p_mauchly) && p_mauchly < alpha_num) {
        "Violation de l'hypoth\u00e8se de sph\u00e9ricit\u00e9 (p \u2264 \u03b1). Utiliser les p-values corrig\u00e9es (Greenhouse-Geisser ou Huynh-Feldt)."
      } else {
        "Hypoth\u00e8se de sph\u00e9ricit\u00e9 respect\u00e9e (p > \u03b1)."
      }
    )

    # 13. Epsilon de Greenhouse-Geisser (GG)
    eps_gg_raw <- if (tr_S2 > 0) (tr_S^2) / (p_dim * tr_S2) else (1 / p_dim)
    # Bornes mathematiques GG : [1 / (K - 1), 1]
    eps_gg <- max(1 / p_dim, min(1.0, eps_gg_raw))

    df1_gg <- eps_gg * df_factor
    df2_gg <- eps_gg * df_resids
    p_gg <- if (!is.na(f_stat) && is.finite(f_stat)) {
      stats::pf(f_stat, df1 = df1_gg, df2 = df2_gg, lower.tail = FALSE)
    } else {
      p_raw
    }

    # 14. Epsilon de Huynh-Feldt (HF)
    denom_hf <- p_dim * (N - 1 - p_dim * eps_gg)
    eps_hf_raw <- if (denom_hf > 0) {
      (N * p_dim * eps_gg - 2) / denom_hf
    } else {
      1.0
    }
    # Bornage strict de HF : max(1 / (K - 1), min(1.0, eps_hf_raw))
    eps_hf <- max(1 / p_dim, min(1.0, eps_hf_raw))

    df1_hf <- eps_hf * df_factor
    df2_hf <- eps_hf * df_resids
    p_hf <- if (!is.na(f_stat) && is.finite(f_stat)) {
      stats::pf(f_stat, df1 = df1_hf, df2 = df2_hf, lower.tail = FALSE)
    } else {
      p_raw
    }

    corrections_res <- list(
      applicable = TRUE,
      eps_gg = eps_gg,
      df1_gg = df1_gg,
      df2_gg = df2_gg,
      p_gg = p_gg,
      eps_hf = eps_hf,
      df1_hf = df1_hf,
      df2_hf = df2_hf,
      p_hf = p_hf,
      message = "Corrections de sph\u00e9ricit\u00e9 calcul\u00e9es."
    )
  }

  # 15. Comparaisons post-hoc deux a deux (tests t apparies avec ajustement de Holm)
  pairs_list <- list()
  for (i_lvl in 1:(k - 1)) {
    for (j_lvl in (i_lvl + 1):k) {
      lvl1 <- k_levels[i_lvl]
      lvl2 <- k_levels[j_lvl]
      v1 <- Y_mat[, i_lvl]
      v2 <- Y_mat[, j_lvl]
      diff_vec <- v1 - v2
      mean_diff <- mean(diff_vec)
      sd_diff <- stats::sd(diff_vec)
      se_diff <- if (sd_diff > 0) sd_diff / sqrt(N) else 0

      t_test_res <- stats::t.test(v1, v2, paired = TRUE)
      t_val <- unname(t_test_res$statistic)
      p_raw_pair <- t_test_res$p.value
      d_cohen <- if (sd_diff > 0) abs(mean_diff) / sd_diff else 0

      pairs_list[[length(pairs_list) + 1]] <- data.frame(
        Niveau_1 = lvl1,
        Niveau_2 = lvl2,
        Difference = mean_diff,
        SE = se_diff,
        t_value = t_val,
        Df = N - 1,
        p_value_raw = p_raw_pair,
        d_cohen_paired = d_cohen,
        stringsAsFactors = FALSE
      )
    }
  }

  post_hoc_df <- do.call(rbind, pairs_list)
  post_hoc_df$p_value_adj <- stats::p.adjust(post_hoc_df$p_value_raw, method = "holm")
  post_hoc_df$sig <- (!is.na(post_hoc_df$p_value_adj) & post_hoc_df$p_value_adj < alpha_num)

  # 16. Structure de resultat complete
  list(
    call = match.call(),
    response = response,
    subject = subject,
    within_factor = within_factor,
    alpha = alpha_num,
    data_info = list(
      initial_subjects = initial_subjects_count,
      excluded_subjects = excluded_subjects_count,
      final_subjects = final_subjects_count,
      n_obs = nrow(final_df),
      levels = k_levels,
      k_levels = k
    ),
    anova_table = anova_table,
    means_table = means_table,
    mauchly = mauchly_res,
    corrections = corrections_res,
    effect_sizes = list(
      eta_p_sq = eta_p_sq,
      eta_p_sq_mag = eta_p_sq_mag,
      eta_g_sq = eta_g_sq
    ),
    post_hoc = post_hoc_df,
    residuals = as.vector(mat_residuals),
    y_matrix = Y_mat
  )
}


#' Graphiques adaptes pour l'ANOVA a mesures repetees
#'
#' Genere le graphique de type spaghetti (trajectoires individuelles et moyenne du groupe avec IC 95%)
#' ou le graphique synthetique des moyennes avec barres d'erreur (IC 95%).
#'
#' @param rm_res Resultat retourne par `ramses_anova_rm()`.
#' @param type Type de graphique : "spaghetti" (defaut) ou "means".
#' @param interactive Booleen indiquant si le graphique doit etre converti en objet Plotly interactif (defaut : FALSE).
#' @return Un objet ggplot ou plotly.
#' @export
ramses_plot_anova_rm <- function(rm_res, type = c("spaghetti", "means"), interactive = FALSE) {
  type <- match.arg(type)

  if (is.null(rm_res) || !is.list(rm_res) || is.null(rm_res$anova_table) || is.null(rm_res$means_table) || is.null(rm_res$y_matrix)) {
    p_err <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text", x = 1, y = 1,
        label = "Donn\u00e9es insuffisantes ou invalides pour g\u00e9n\u00e9rer le graphique d'ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es.",
        size = 4, color = "#DC2626"
      ) +
      ggplot2::theme_void()
    if (isTRUE(interactive)) {
      return(plotly::ggplotly(p_err))
    }
    return(p_err)
  }

  y_name <- rm_res$response
  within_name <- rm_res$within_factor
  subj_name <- rm_res$subject
  level_names <- rm_res$data_info$levels
  means_df <- rm_res$means_table
  means_df$Niveau <- factor(means_df$Niveau, levels = level_names)

  if (type == "spaghetti") {
    N <- nrow(rm_res$y_matrix)
    k <- ncol(rm_res$y_matrix)
    subj_names <- rownames(rm_res$y_matrix)

    # Reconstitution des donnees individuelles completes ordonnees
    plot_df <- data.frame(
      subject = rep(subj_names, times = k),
      condition = factor(rep(level_names, each = N), levels = level_names),
      y = as.vector(rm_res$y_matrix),
      stringsAsFactors = FALSE
    )

    p <- ggplot2::ggplot() +
      # 1. Trajectoires individuelles fines par sujet
      ggplot2::geom_line(
        data = plot_df,
        ggplot2::aes(x = condition, y = y, group = subject),
        color = "#9CA3AF",
        alpha = 0.5,
        linewidth = 0.5
      ) +
      # 2. Points individuels par sujet
      ggplot2::geom_point(
        data = plot_df,
        ggplot2::aes(x = condition, y = y, group = subject),
        color = "#6B7280",
        alpha = 0.6,
        size = 2
      ) +
      # 3. Ligne de la moyenne generale du groupe
      ggplot2::geom_line(
        data = means_df,
        ggplot2::aes(x = Niveau, y = Moyenne, group = 1),
        color = "#1D4ED8",
        linewidth = 1.2
      ) +
      # 4. Barres d'erreur IC 95% autour de la moyenne
      ggplot2::geom_errorbar(
        data = means_df,
        ggplot2::aes(x = Niveau, ymin = CI_lower, ymax = CI_upper),
        color = "#1D4ED8",
        width = 0.12,
        linewidth = 0.9
      ) +
      # 5. Points de la moyenne
      ggplot2::geom_point(
        data = means_df,
        ggplot2::aes(x = Niveau, y = Moyenne),
        color = "#1D4ED8",
        size = 4
      ) +
      ggplot2::labs(
        title = paste0("ANOVA mesures r\u00e9p\u00e9t\u00e9es : ", y_name, " par ", within_name),
        subtitle = "Trajectoires individuelles (gris) et \u00e9volution moyenne \u00b1 IC95% (bleu)",
        x = within_name,
        y = y_name
      ) +
      ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
      ggplot2::theme(
        text = ggplot2::element_text(family = "sans"),
        plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
        plot.subtitle = ggplot2::element_text(size = 10, color = "#4B5563"),
        axis.title = ggplot2::element_text(size = 11, color = "#374151"),
        axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
      )

  } else {
    # type == "means"
    p <- ggplot2::ggplot(
      data = means_df,
      ggplot2::aes(x = Niveau, y = Moyenne, group = 1)
    ) +
      ggplot2::geom_line(color = "#1D4ED8", linewidth = 1.1) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), color = "#1D4ED8", width = 0.15, linewidth = 0.9) +
      ggplot2::geom_point(color = "#1D4ED8", size = 4) +
      ggplot2::labs(
        title = paste0("Moyennes et intervalles de confiance \u00e0 95% : ", y_name),
        subtitle = paste0("Facteur intra-sujets : ", within_name, " (N = ", rm_res$data_info$final_subjects, " sujets)"),
        x = within_name,
        y = paste0("Moyenne de ", y_name)
      ) +
      ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
      ggplot2::theme(
        text = ggplot2::element_text(family = "sans"),
        plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
        plot.subtitle = ggplot2::element_text(size = 10, color = "#4B5563"),
        axis.title = ggplot2::element_text(size = 11, color = "#374151"),
        axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
      )
  }

  if (isTRUE(interactive)) {
    tryCatch({
      plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans"))
    }, error = function(e) {
      p
    })
  } else {
    p
  }
}


#' Generation du rapport R Markdown complet pour l'ANOVA a mesures repetees
#'
#' Construit un texte structure au format R Markdown comprenant l'ensemble des
#' sections reglementaires (A a I) et le bloc de code R reproductible.
#'
#' @param rm_res Resultat retourne par `ramses_anova_rm()`.
#' @param ds_name Nom du jeu de donnees dans la session R.
#' @param include_posthoc Booleen indiquant si la section de comparaisons post-hoc doit etre incluse.
#' @return Une chaine de caracteres contenant le rapport R Markdown complet.
#' @export
ramses_rmd_anova_rm <- function(rm_res, ds_name = "data", include_posthoc = FALSE) {
  if (is.null(rm_res) || !is.list(rm_res) || is.null(rm_res$anova_table)) {
    return("# ANOVA a mesures repetees\n\nErreur : Resultat d'analyse non disponible.\n")
  }

  y_var <- rm_res$response
  subj_var <- rm_res$subject
  within_var <- rm_res$within_factor
  alpha_val <- rm_res$alpha
  data_info <- rm_res$data_info
  tab <- rm_res$anova_table
  mauchly <- rm_res$mauchly
  corr <- rm_res$corrections
  es <- rm_res$effect_sizes
  ph <- rm_res$post_hoc

  row_f <- tab[tab$Source == within_var, ]
  f_val <- row_f$F_value
  p_val <- row_f$p_value
  df1 <- row_f$Df
  df2 <- tab[tab$Source == "R\u00e9sidus", "Df"]

  # Decision
  p_eval <- if (isTRUE(mauchly$applicable) && !is.na(mauchly$p_value) && mauchly$p_value < alpha_val && isTRUE(corr$applicable)) {
    corr$p_gg
  } else {
    p_val
  }
  is_sig <- (!is.na(p_eval) && p_eval < alpha_val)

  # Section A : Presentation
  sec_a <- paste0(
    "## A. Pr\u00e9sentation de l'\u00e9tude\n\n",
    "- **Variable quantitative d\u00e9pendante** : `", y_var, "`\n",
    "- **Identifiant du sujet** : `", subj_var, "`\n",
    "- **Facteur intra-sujets** : `", within_var, "` (", data_info$k_levels, " modalit\u00e9s : ", paste(data_info$levels, collapse = ", "), ")\n",
    "- **Nombre de sujets analys\u00e9s** : ", data_info$final_subjects, " sujets complets\n",
    "- **Nombre total d'observations** : ", data_info$n_obs, " mesures\n\n"
  )

  # Section B : Methode
  sec_b <- paste0(
    "## B. M\u00e9thode statistique\n\n",
    "Une ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es \u00e0 un facteur intra-sujets a \u00e9t\u00e9 utilis\u00e9e car les m\u00eames sujets ont \u00e9t\u00e9 mesur\u00e9s de mani\u00e8re r\u00e9p\u00e9t\u00e9e dans diff\u00e9rentes conditions.\n\n",
    "Le mod\u00e8le statistique sous-jacent est formul\u00e9 selon la d\u00e9composition des sources d'erreur intra-sujets :\n\n",
    "```r\n",
    "aov(`", y_var, "` ~ `", within_var, "` + Error(`", subj_var, "`/`", within_var, "`))\n",
    "```\n\n"
  )

  # Section C : Verifications
  sec_c <- paste0(
    "## C. V\u00e9rifications des conditions d'application\n\n",
    "- **Compl\u00e9tude des donn\u00e9es** : ", data_info$final_subjects, " sujets pr\u00e9sentent un profil de mesures complet sur l'ensemble des ", data_info$k_levels, " modalit\u00e9s.\n",
    "- **Sujets exclus pour donn\u00e9es incompl\u00e8tes** : ", data_info$excluded_subjects, " sujet(s) exclu(s).\n",
    "- **Doublons Sujet \u00d7 Condition** : 0 doublon d\u00e9tect\u00e9 (chaque sujet dispose d'une mesure unique par modalit\u00e9).\n",
    "- **Distribution des r\u00e9sidus** : Les r\u00e9sidus correspondent aux \u00e9carts individuels par rapport au mod\u00e8le additif Sujet + Condition.\n\n"
  )

  # Section D : Tableau ANOVA
  sec_d <- paste0(
    "## D. Tableau de l'ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es\n\n",
    "| Source de variation | Df | Somme des carr\u00e9s (SS) | Carr\u00e9 moyen (MS) | F observ\u00e9 | p-value brute | \u03b7\u00b2 partiel (\u03b7p\u00b2) |\n",
    "|:---|---:|---:|---:|---:|---:|---:|\n",
    "| **", within_var, "** | ", row_f$Df, " | ", round(row_f$Sum_Sq, 3), " | ", round(row_f$Mean_Sq, 3), " | ", round(row_f$F_value, 4), " | ", format.pval(row_f$p_value, digits = 4, eps = 1e-4), " | ", round(row_f$eta_p_sq, 4), " |\n",
    "| **Sujets** | ", tab[tab$Source == "Sujets", "Df"], " | ", round(tab[tab$Source == "Sujets", "Sum_Sq"], 3), " | ", round(tab[tab$Source == "Sujets", "Mean_Sq"], 3), " | \u2014 | \u2014 | \u2014 |\n",
    "| **R\u00e9sidus (Sujet \u00d7 ", within_var, ")** | ", df2, " | ", round(tab[tab$Source == "R\u00e9sidus", "Sum_Sq"], 3), " | ", round(tab[tab$Source == "R\u00e9sidus", "Mean_Sq"], 3), " | \u2014 | \u2014 | \u2014 |\n\n"
  )

  # Section E : Sphericity
  sec_e <- if (isFALSE(mauchly$applicable) || data_info$k_levels == 2) {
    paste0(
      "## E. Test de sph\u00e9ricit\u00e9 et corrections\n\n",
      "Avec deux niveaux du facteur intra-sujets (K = 2), la condition de sph\u00e9ricit\u00e9 est automatiquement satisfaite (sph\u00e9ricit\u00e9 triviale et exacte, \u03b5 = 1.000).\n\n"
    )
  } else {
    paste0(
      "## E. Test de sph\u00e9ricit\u00e9 de Mauchly & Corrections d'Epsilon\n\n",
      "- **Test de Mauchly** : W = ", round(mauchly$w, 4), ", \u03c7\u00b2 = ", round(mauchly$statistic, 4), ", Df = ", mauchly$df, ", p = ", format.pval(mauchly$p_value, digits = 4, eps = 1e-4), "\n",
      "- **Epsilon de Greenhouse-Geisser (\u03b5_GG)** : ", round(corr$eps_gg, 4), " \u2192 F(", round(corr$df1_gg, 2), ", ", round(corr$df2_gg, 2), ") = ", round(f_val, 3), ", p_GG = ", format.pval(corr$p_gg, digits = 4, eps = 1e-4), "\n",
      "- **Epsilon de Huynh-Feldt (\u03b5_HF)** : ", round(corr$eps_hf, 4), " \u2192 F(", round(corr$df1_hf, 2), ", ", round(corr$df2_hf, 2), ") = ", round(f_val, 3), ", p_HF = ", format.pval(corr$p_hf, digits = 4, eps = 1e-4), "\n",
      "- **Diagnostic** : ", mauchly$message, "\n\n"
    )
  }

  # Section F : Decision
  sec_f <- paste0(
    "## F. D\u00e9cision statistique\n\n",
    if (is_sig) {
      paste0(
        "Au seuil de significativit\u00e9 \u03b1 = ", alpha_val, ", l'hypoth\u00e8se nulle d'\u00e9galit\u00e9 des moyennes est **rejet\u00e9e** (p = ",
        format.pval(p_eval, digits = 4, eps = 1e-4),
        if (isTRUE(mauchly$applicable) && mauchly$p_value < alpha_val) " corrig\u00e9e GG" else "",
        "). Il existe un effet statistiquement significatif du facteur intra-sujets `", within_var, "` sur la variable `", y_var, "`."
      )
    } else {
      paste0(
        "Au seuil de significativit\u00e9 \u03b1 = ", alpha_val, ", l'hypoth\u00e8se nulle d'\u00e9galit\u00e9 des moyennes n'est **pas rejet\u00e9e** (p = ",
        format.pval(p_eval, digits = 4, eps = 1e-4),
        "). Aucune diff\u00e9rence statistiquement significative n'est mise en \u00e9vidence entre les modalit\u00e9s de `", within_var, "`."
      )
    },
    "\n\n"
  )

  # Section G : Taille d'effet
  sec_g <- paste0(
    "## G. Tailles d'effet\n\n",
    "- **Eta-carr\u00e9 partiel (\u03b7p\u00b2)** : ", round(es$eta_p_sq, 4), " (", es$eta_p_sq_mag, ")\n",
    "- **Eta-carr\u00e9 g\u00e9n\u00e9ralis\u00e9 (\u03b7g\u00b2)** : ", round(es$eta_g_sq, 4), "\n\n",
    "*\u03b7p\u00b2 quantifie la proportion de variance intra-sujets expliqu\u00e9e par le facteur apr\u00e8s exclusion de la variabilit\u00e9 inter-sujets.*\n\n"
  )

  # Section H : Post-hoc (conditionnel)
  sec_h <- if (isTRUE(include_posthoc) && !is.null(ph) && nrow(ph) > 0) {
    ph_rows <- apply(ph, 1, function(r) {
      paste0(
        "| `", r[["Niveau_1"]], "` vs `", r[["Niveau_2"]], "` | ",
        round(as.numeric(r[["Difference"]]), 3), " | ",
        round(as.numeric(r[["SE"]]), 3), " | ",
        round(as.numeric(r[["t_value"]]), 3), " | ",
        r[["Df"]], " | ",
        format.pval(as.numeric(r[["p_value_raw"]]), digits = 4, eps = 1e-4), " | ",
        format.pval(as.numeric(r[["p_value_adj"]]), digits = 4, eps = 1e-4), " | ",
        if (as.logical(r[["sig"]])) "**Oui (Significatif)**" else "Non", " |"
      )
    })
    paste0(
      "## H. Comparaisons post-hoc deux \u00e0 deux (Holm)\n\n",
      "| Comparaison | Diff\u00e9rence moyenne | Erreur-Type (SE) | t observ\u00e9 | Df | p-value brute | p-value ajust\u00e9e (Holm) | Significatif (\u03b1 = ", alpha_val, ") |\n",
      "|:---|---:|---:|---:|---:|---:|---:|:---|\n",
      paste(ph_rows, collapse = "\n"),
      "\n\n"
    )
  } else {
    ""
  }

  # Section I : Graphiques
  sec_i <- paste0(
    "## I. Visualisations graphiques\n\n",
    "Deux graphiques compl\u00e9mentaires permettent d'\u00e9valuer les r\u00e9sultats :\n",
    "1. **Graphique Spaghetti** : Trajectoires individuelles de chaque sujet et \u00e9volution moyenne du groupe avec IC 95%.\n",
    "2. **Graphique des moyennes** : Profil synth\u00e9tique des moyennes et barres d'erreur \u00e0 95% de confiance.\n\n"
  )

  # Bloc de code reproductible
  r_chunk <- paste0(
    "```{r anova_rm_reproducible, message=FALSE, warning=FALSE}\n",
    "# 1. Preparation des donnees completes\n",
    "df_raw <- ", ramses_code_symbol(ds_name), "\n",
    "cols_needed <- c(", ramses_code_string(y_var), ", ", ramses_code_string(subj_var), ", ", ramses_code_string(within_var), ")\n",
    "df_clean <- df_raw[complete.cases(df_raw[, cols_needed]), ]\n",
    "\n# 2. Execution du moteur central Ramses\n",
    "rm_result <- ramses_anova_rm(\n",
    "  data = df_clean,\n",
    "  response = ", ramses_code_string(y_var), ",\n",
    "  subject = ", ramses_code_string(subj_var), ",\n",
    "  within_factor = ", ramses_code_string(within_var), ",\n",
    "  alpha = ", alpha_val, "\n",
    ")\n",
    "\n# 3. Affichage du tableau ANOVA et des moyennes\n",
    "print(rm_result$anova_table)\n",
    "print(rm_result$means_table)\n",
    if (isTRUE(include_posthoc)) "\n# 4. Comparaisons post-hoc (Holm)\nprint(rm_result$post_hoc)\n" else "",
    "\n# 5. Production des graphiques\n",
    "p_spaghetti <- ramses_plot_anova_rm(rm_result, type = 'spaghetti', interactive = FALSE)\n",
    "print(p_spaghetti)\n\n",
    "p_means <- ramses_plot_anova_rm(rm_result, type = 'means', interactive = FALSE)\n",
    "print(p_means)\n",
    "```\n"
  )

  paste0(
    "# Rapport d'Analyse : ANOVA \u00e0 Mesures R\u00e9p\u00e9t\u00e9es\n\n",
    sec_a,
    sec_b,
    sec_c,
    sec_d,
    sec_e,
    sec_f,
    sec_g,
    sec_h,
    sec_i,
    r_chunk
  )
}





#' Calcul securise de la taille d'effet pour differents tests statistiques
#'
#' @param test_type Type de test ("t_indep", "t_one_sample", "t_paired", "wilcox_indep", "wilcox_paired", "wilcox_one_sample", "anova", "kruskal", "chisq", "fisher", "mcnemar", "cor_pearson", "cor_spearman", "cor_kendall", etc.)
#' @param stat_result Objet de resultat de test (htest, aov, summary.aov, etc.)
#' @param y1 Vecteur numerique du groupe 1 (ou de la variable Y)
#' @param y2 Vecteur numerique du groupe 2 (ou de la seconde variable appariee Y2)
#' @param mu Valeur theorique pour test a 1 echantillon (defaut: 0)
#' @param var_equal Booleen indiquant l'hypothese d'egalite des variances pour t_indep (defaut: TRUE)
#' @param table_data Tableau de contingence (matrice / table) pour tests d'independance
#' @param tab Alias pour table_data
#' @param ... Arguments supplementaires ignores pour compatibilite
#' @return Liste contenant le nom de la mesure, le symbole, la valeur numerique, la magnitude qualitative et une chaine de description formatee.
#' @noRd
ramses_compute_effect_size <- function(test_type,
                                       stat_result = NULL,
                                       y1 = NULL,
                                       y2 = NULL,
                                       mu = 0,
                                       var_equal = TRUE,
                                       table_data = NULL,
                                       tab = NULL,
                                       ...) {
  tryCatch({
    if (is.null(table_data) && !is.null(tab)) {
      table_data <- tab
    }

    if (test_type == "t_indep") {
      # d de Cohen pour 2 echantillons independants
      if (is.null(y1) || is.null(y2)) return(NULL)
      v1 <- y1[!is.na(y1)]
      v2 <- y2[!is.na(y2)]
      n1 <- length(v1)
      n2 <- length(v2)
      if (n1 < 2 || n2 < 2) return(NULL)

      m1 <- mean(v1)
      m2 <- mean(v2)
      s1 <- stats::sd(v1)
      s2 <- stats::sd(v2)

      if (isTRUE(var_equal)) {
        # Pooled standard deviation (variance commune de Student)
        df_pool <- n1 + n2 - 2
        if (df_pool <= 0) return(NULL)
        s_denom <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / df_pool)
      } else {
        # Ecart-type moyen non combine (variances heterogenes / test de Welch)
        s_denom <- sqrt((s1^2 + s2^2) / 2)
      }

      if (is.na(s_denom) || s_denom <= 0) return(NULL)

      d <- (m1 - m2) / s_denom
      abs_d <- abs(d)

      mag <- if (abs_d < 0.2) {
        "effet n\u00e9gligeable"
      } else if (abs_d < 0.5) {
        "effet faible"
      } else if (abs_d < 0.8) {
        "effet moyen"
      } else {
        "effet fort"
      }

      desc_text <- if (isTRUE(var_equal)) {
        paste0("d de Cohen = ", round(d, 3), " (", mag, ", variance commune)")
      } else {
        paste0("d de Cohen = ", round(d, 3), " (", mag, ", ajust\u00e9 pour variances h\u00e9t\u00e9rog\u00e8nes)")
      }

      list(
        name = "d de Cohen",
        symbol = "d",
        value = d,
        formatted_value = as.character(round(d, 3)),
        magnitude = mag,
        description = desc_text,
        interpretation = desc_text
      )

    } else if (test_type == "t_one_sample") {
      # d de Cohen pour 1 echantillon
      if (is.null(y1)) return(NULL)
      v1 <- y1[!is.na(y1)]
      n <- length(v1)
      if (n < 2) return(NULL)

      m <- mean(v1)
      s <- stats::sd(v1)
      if (is.na(s) || s <= 0) return(NULL)

      d <- (m - mu) / s
      abs_d <- abs(d)

      mag <- if (abs_d < 0.2) {
        "effet n\u00e9gligeable"
      } else if (abs_d < 0.5) {
        "effet faible"
      } else if (abs_d < 0.8) {
        "effet moyen"
      } else {
        "effet fort"
      }

      list(
        name = "d de Cohen",
        symbol = "d",
        value = d,
        formatted_value = as.character(round(d, 3)),
        magnitude = mag,
        interpretation = paste0("d de Cohen = ", round(d, 3), " (", mag, ")")
      )

    } else if (test_type == "t_paired") {
      # d de Cohen pour donnees appariees
      if (is.null(y1) || is.null(y2)) return(NULL)
      complete_idx <- !is.na(y1) & !is.na(y2)
      v1 <- y1[complete_idx]
      v2 <- y2[complete_idx]
      n <- length(v1)
      if (n < 2) return(NULL)

      diffs <- v1 - v2
      m_diff <- mean(diffs)
      s_diff <- stats::sd(diffs)
      if (is.na(s_diff) || s_diff <= 0) return(NULL)

      d <- m_diff / s_diff
      abs_d <- abs(d)

      mag <- if (abs_d < 0.2) {
        "effet n\u00e9gligeable"
      } else if (abs_d < 0.5) {
        "effet faible"
      } else if (abs_d < 0.8) {
        "effet moyen"
      } else {
        "effet fort"
      }

      list(
        name = "d de Cohen (appari\u00e9)",
        symbol = "d",
        value = d,
        formatted_value = as.character(round(d, 3)),
        magnitude = mag,
        interpretation = paste0("d de Cohen = ", round(d, 3), " (", mag, ")")
      )

    } else if (test_type %in% c("wilcox_indep", "wilcox_paired", "wilcox_one_sample")) {
      # Taille d'effet r de Rosenthal pour tests de Wilcoxon / Mann-Whitney : r = |Z| / sqrt(N)
      if (is.null(stat_result) || is.null(stat_result$p.value)) return(NULL)
      p_val <- stat_result$p.value
      if (is.na(p_val) || p_val <= 0 || p_val >= 1) return(NULL)

      n_total <- if (test_type == "wilcox_indep") {
        if (is.null(y1) || is.null(y2)) return(NULL)
        length(y1[!is.na(y1)]) + length(y2[!is.na(y2)])
      } else if (test_type == "wilcox_paired") {
        if (is.null(y1) || is.null(y2)) return(NULL)
        sum(!is.na(y1) & !is.na(y2) & (y1 != y2))
      } else {
        if (is.null(y1)) return(NULL)
        sum(!is.na(y1) & (y1 != mu))
      }

      if (n_total <= 0) return(NULL)

      z_score <- abs(stats::qnorm(p_val / 2))
      r_val <- z_score / sqrt(n_total)
      if (is.na(r_val) || is.infinite(r_val)) return(NULL)
      r_val <- min(1, r_val)

      mag <- if (r_val < 0.1) {
        "effet n\u00e9gligeable"
      } else if (r_val < 0.3) {
        "effet faible"
      } else if (r_val < 0.5) {
        "effet moyen"
      } else {
        "effet fort"
      }

      list(
        name = "Taille d'effet r (Rosenthal)",
        symbol = "r",
        value = r_val,
        formatted_value = as.character(round(r_val, 3)),
        magnitude = mag,
        interpretation = paste0("Taille d'effet r = ", round(r_val, 3), " (", mag, ")")
      )

    } else if (test_type == "anova") {
      # Eta-carre (eta2) pour ANOVA 1 facteur
      if (is.null(stat_result)) return(NULL)
      smry <- if (inherits(stat_result, "summary.aov")) {
        stat_result[[1]]
      } else if (inherits(stat_result, "aov")) {
        summary(stat_result)[[1]]
      } else {
        return(NULL)
      }

      ss_group <- smry[["Sum Sq"]][1]
      ss_res <- smry[["Sum Sq"]][2]
      ss_total <- ss_group + ss_res

      if (is.null(ss_total) || is.na(ss_total) || ss_total <= 0) return(NULL)

      eta2 <- ss_group / ss_total
      if (is.na(eta2)) return(NULL)
      eta2 <- max(0, min(1, eta2))

      mag <- if (eta2 < 0.01) {
        "effet n\u00e9gligeable"
      } else if (eta2 < 0.06) {
        "effet faible"
      } else if (eta2 < 0.14) {
        "effet moyen"
      } else {
        "effet fort"
      }

      list(
        name = "Eta-carr\u00e9 (\u03b7\u00b2)",
        symbol = "\u03b7\u00b2",
        value = eta2,
        formatted_value = as.character(round(eta2, 3)),
        magnitude = mag,
        interpretation = paste0("Eta-carr\u00e9 \u03b7\u00b2 = ", round(eta2, 3), " (", mag, ", ", round(eta2 * 100, 1), "% de variance expliqu\u00e9e)")
      )

    } else if (test_type == "kruskal") {
      # Eta-carre H pour Kruskal-Wallis : eta2_H = (H - k + 1) / (N - k)
      if (is.null(stat_result) || is.null(stat_result$statistic)) return(NULL)
      h_stat <- unname(stat_result$statistic)
      df_k <- unname(stat_result$parameter)
      k_groups <- df_k + 1

      n_total <- if (!is.null(y1)) length(y1[!is.na(y1)]) else NULL
      if (is.null(n_total) || n_total <= k_groups) return(NULL)

      eta2_h <- (h_stat - k_groups + 1) / (n_total - k_groups)
      eta2_h <- max(0, min(1, eta2_h))

      mag <- if (eta2_h < 0.01) {
        "effet n\u00e9gligeable"
      } else if (eta2_h < 0.06) {
        "effet faible"
      } else if (eta2_h < 0.14) {
        "effet moyen"
      } else {
        "effet fort"
      }

      list(
        name = "Eta-carr\u00e9 H (\u03b7\u00b2_H)",
        symbol = "\u03b7\u00b2_H",
        value = eta2_h,
        formatted_value = as.character(round(eta2_h, 3)),
        magnitude = mag,
        interpretation = paste0("Eta-carr\u00e9 H = ", round(eta2_h, 3), " (", mag, ")")
      )

    } else if (test_type == "chisq") {
      # V de Cramer pour test du Chi-deux
      table_mat <- if (!is.null(table_data)) table_data else tab
      if (is.null(stat_result) || is.null(table_mat)) return(NULL)
      chi2 <- unname(stat_result$statistic)
      if (is.na(chi2) || chi2 < 0) return(NULL)

      n_total <- sum(table_mat)
      r_dim <- nrow(table_mat)
      c_dim <- ncol(table_mat)
      k_dim <- min(r_dim - 1, c_dim - 1)

      if (n_total <= 0 || k_dim < 1) return(NULL)

      v_cramer <- sqrt(chi2 / (n_total * k_dim))
      v_cramer <- max(0, min(1, v_cramer))

      mag <- if (v_cramer < 0.1) {
        "association n\u00e9gligeable"
      } else if (v_cramer < 0.3) {
        "association faible"
      } else if (v_cramer < 0.5) {
        "association mod\u00e9r\u00e9e"
      } else {
        "association forte"
      }

      list(
        name = "V de Cram\u00e9r",
        symbol = "V",
        value = v_cramer,
        formatted_value = as.character(round(v_cramer, 3)),
        magnitude = mag,
        description = paste0("V de Cram\u00e9r = ", round(v_cramer, 3), " (", mag, ")"),
        interpretation = paste0("V de Cram\u00e9r = ", round(v_cramer, 3), " (", mag, ")")
      )

    } else if (test_type %in% c("cor", "cor_pearson", "cor_spearman", "cor_kendall", "pearson", "spearman", "kendall")) {
      # Mesure d'association pour les correlations bivariees
      method_used <- if (grepl("spearman", test_type, ignore.case = TRUE)) {
        "spearman"
      } else if (grepl("kendall", test_type, ignore.case = TRUE)) {
        "kendall"
      } else {
        "pearson"
      }

      r_val <- NULL
      if (!is.null(stat_result) && !is.null(stat_result$estimate)) {
        r_val <- unname(stat_result$estimate[1])
      } else if (!is.null(y1) && !is.null(y2)) {
        complete_idx <- !is.na(y1) & !is.na(y2)
        v1 <- y1[complete_idx]
        v2 <- y2[complete_idx]
        if (length(v1) >= 3) {
          r_val <- suppressWarnings(stats::cor(v1, v2, method = method_used))
        }
      }

      if (is.null(r_val) || is.na(r_val)) return(NULL)

      abs_r <- abs(r_val)
      mag <- if (abs_r < 0.1) {
        "n\u00e9gligeable"
      } else if (abs_r < 0.3) {
        "faible"
      } else if (abs_r < 0.5) {
        "mod\u00e9r\u00e9e"
      } else {
        "forte"
      }

      name_str <- if (method_used == "pearson") {
        "Coefficient de corr\u00e9lation de Pearson"
      } else if (method_used == "spearman") {
        "Coefficient de Spearman (\u03c1)"
      } else {
        "Tau de Kendall (\u03c4)"
      }

      sym_str <- if (method_used == "pearson") {
        "r"
      } else if (method_used == "spearman") {
        "\u03c1"
      } else {
        "\u03c4"
      }

      desc_str <- paste0(name_str, " = ", round(r_val, 3), " (association ", mag, ")")

      list(
        name = name_str,
        symbol = sym_str,
        value = as.numeric(r_val),
        formatted_value = as.character(round(r_val, 3)),
        magnitude = mag,
        description = desc_str,
        interpretation = desc_str
      )

    } else if (test_type == "fisher") {
      table_mat <- if (!is.null(table_data)) table_data else tab
      or_val <- if (!is.null(stat_result) && !is.null(stat_result$estimate)) unname(stat_result$estimate[1]) else NULL
      if (!is.null(or_val) && is.numeric(or_val) && !is.na(or_val)) {
        list(
          name = "Odds Ratio (OR)",
          symbol = "OR",
          value = as.numeric(or_val),
          formatted_value = as.character(round(or_val, 3)),
          magnitude = if (or_val == 1) "nul" else if (or_val > 1) "positif" else "n\u00e9gatif",
          description = paste0("Odds Ratio = ", round(or_val, 3)),
          interpretation = paste0("Odds Ratio = ", round(or_val, 3))
        )
      } else if (!is.null(table_mat)) {
        n_tot <- sum(table_mat)
        k_d <- min(nrow(table_mat) - 1, ncol(table_mat) - 1)
        chi2_approx <- suppressWarnings(tryCatch(stats::chisq.test(table_mat)$statistic, error = function(e) NA))
        if (!is.na(chi2_approx) && n_tot > 0 && k_d >= 1) {
          v_c <- sqrt(chi2_approx / (n_tot * k_d))
          list(
            name = "V de Cram\u00e9r",
            symbol = "V",
            value = as.numeric(v_c),
            formatted_value = as.character(round(v_c, 3)),
            magnitude = if (v_c < 0.1) "association n\u00e9gligeable" else if (v_c < 0.3) "association faible" else if (v_c < 0.5) "association mod\u00e9r\u00e9e" else "association forte",
            description = paste0("V de Cram\u00e9r = ", round(v_c, 3)),
            interpretation = paste0("V de Cram\u00e9r = ", round(v_c, 3))
          )
        } else {
          NULL
        }
      } else {
        NULL
      }

    } else if (test_type == "prop_one") {
      p_obs <- if (!is.null(y1)) as.numeric(y1) else if (!is.null(stat_result$estimate)) as.numeric(stat_result$estimate[1]) else NULL
      p_theo <- if (!is.null(mu)) as.numeric(mu) else if (!is.null(stat_result$null.value)) as.numeric(stat_result$null.value[1]) else 0.5
      if (is.null(p_obs) || is.na(p_obs) || is.na(p_theo)) return(NULL)
      if (p_obs < 0 || p_obs > 1 || p_theo < 0 || p_theo > 1) return(NULL)

      h <- 2 * asin(sqrt(p_obs)) - 2 * asin(sqrt(p_theo))
      abs_h <- abs(h)
      mag <- if (abs_h < 0.2) {
        "effet n\u00e9gligeable"
      } else if (abs_h < 0.5) {
        "effet faible"
      } else if (abs_h < 0.8) {
        "effet moyen"
      } else {
        "effet fort"
      }

      list(
        name = "h de Cohen",
        symbol = "h",
        value = h,
        formatted_value = as.character(round(h, 3)),
        magnitude = mag,
        description = paste0("h de Cohen = ", round(h, 3), " (", mag, ")"),
        interpretation = paste0("h de Cohen = ", round(h, 3), " (", mag, ")")
      )

    } else if (test_type == "prop_two") {
      p1 <- if (!is.null(y1)) as.numeric(y1) else if (!is.null(stat_result$estimate) && length(stat_result$estimate) >= 2) as.numeric(stat_result$estimate[1]) else NULL
      p2 <- if (!is.null(y2)) as.numeric(y2) else if (!is.null(stat_result$estimate) && length(stat_result$estimate) >= 2) as.numeric(stat_result$estimate[2]) else NULL
      if (is.null(p1) || is.null(p2) || is.na(p1) || is.na(p2)) return(NULL)
      if (p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) return(NULL)

      h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
      abs_h <- abs(h)
      mag <- if (abs_h < 0.2) {
        "effet n\u00e9gligeable"
      } else if (abs_h < 0.5) {
        "effet faible"
      } else if (abs_h < 0.8) {
        "effet moyen"
      } else {
        "effet fort"
      }

      rr <- if (p2 > 0) p1 / p2 else NA
      or_val <- if (p2 > 0 && p2 < 1 && p1 < 1 && p1 > 0) (p1 / (1 - p1)) / (p2 / (1 - p2)) else NA

      list(
        name = "h de Cohen",
        symbol = "h",
        value = h,
        formatted_value = as.character(round(h, 3)),
        magnitude = mag,
        risk_ratio = rr,
        odds_ratio = or_val,
        description = paste0("h de Cohen = ", round(h, 3), " (", mag, ")", if (!is.na(rr)) paste0(" ; Risque Relatif = ", round(rr, 3)) else ""),
        interpretation = paste0("h de Cohen = ", round(h, 3), " (", mag, ")")
      )

    } else if (test_type == "mcnemar") {
      table_mat <- if (!is.null(table_data)) table_data else tab
      if (!is.null(table_mat) && all(dim(table_mat) == c(2, 2))) {
        b <- table_mat[1, 2]
        c <- table_mat[2, 1]
        or_mcnemar <- if (c > 0) b / c else NA
        list(
          name = "Ratio de discordance",
          symbol = "OR_disc",
          value = as.numeric(or_mcnemar),
          formatted_value = if (!is.na(or_mcnemar)) as.character(round(or_mcnemar, 3)) else "\u2014",
          magnitude = if (is.na(or_mcnemar) || or_mcnemar == 1) "nul" else if (or_mcnemar > 1) "positif" else "n\u00e9gatif",
          description = paste0("Ratio b/c = ", if (!is.na(or_mcnemar)) round(or_mcnemar, 3) else "\u2014"),
          interpretation = paste0("Ratio b/c = ", if (!is.na(or_mcnemar)) round(or_mcnemar, 3) else "\u2014")
        )
      } else {
        NULL
      }

    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
}

#' Calcul securise de la puissance post-hoc du test
#'
#' @param test_type Type de test ("t_indep", "t_one_sample", "t_paired", "anova", "chisq", "cor_pearson", etc.)
#' @param stat_result Objet de resultat de test
#' @param y1 Vecteur numerique du groupe 1 (ou variable Y)
#' @param y2 Vecteur numerique du groupe 2 (ou variable Y2)
#' @param mu Moyenne theorique
#' @param alpha Seuil de significativite alpha
#' @param alternative Type d'alternative ("two.sided", "less", "greater")
#' @param table_data Tableau de contingence (pour chisq)
#' @param var_equal Booleen indiquant si variances egales pour t_indep (defaut: TRUE)
#' @param tab Alias pour table_data
#' @param ... Arguments supplementaires ignores pour compatibilite
#' @return Liste avec la valeur de puissance (entre 0 et 1), le pourcentage formate, la description qualitative et le texte formate.
#' @noRd
ramses_compute_power <- function(test_type,
                                 stat_result = NULL,
                                 y1 = NULL,
                                 y2 = NULL,
                                 mu = 0,
                                 alpha = 0.05,
                                 alternative = "two.sided",
                                 table_data = NULL,
                                 var_equal = TRUE,
                                 tab = NULL,
                                 ...) {
  tryCatch({
    if (is.null(table_data) && !is.null(tab)) {
      table_data <- tab
    }
    alpha_num <- as.numeric(alpha)
    if (is.na(alpha_num) || alpha_num <= 0 || alpha_num >= 1) alpha_num <- 0.05
    alt_pwr <- if (alternative %in% c("less", "greater")) "one.sided" else "two.sided"

    if (test_type == "t_indep") {
      if (is.null(y1) || is.null(y2)) return(NULL)
      v1 <- y1[!is.na(y1)]
      v2 <- y2[!is.na(y2)]
      n1 <- length(v1)
      n2 <- length(v2)
      if (n1 < 2 || n2 < 2) return(NULL)

      m1 <- mean(v1)
      m2 <- mean(v2)
      s1 <- stats::sd(v1)
      s2 <- stats::sd(v2)

      delta <- abs(m1 - m2)
      if (isTRUE(var_equal)) {
        df_pool <- n1 + n2 - 2
        if (df_pool <= 0) return(NULL)
        s_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / df_pool)
      } else {
        s_sd <- sqrt((s1^2 + s2^2) / 2)
      }

      if (is.na(s_sd) || s_sd <= 0 || is.na(delta) || delta <= 0) return(NULL)

      # Moyenne harmonique des effectifs pour groupes non equilibres
      n_h <- (2 * n1 * n2) / (n1 + n2)

      pwr_obj <- stats::power.t.test(
        n = n_h,
        delta = delta,
        sd = s_sd,
        sig.level = alpha_num,
        type = "two.sample",
        alternative = alt_pwr
      )
      pwr_val <- pwr_obj$power

    } else if (test_type == "t_one_sample") {
      if (is.null(y1)) return(NULL)
      v1 <- y1[!is.na(y1)]
      n <- length(v1)
      if (n < 2) return(NULL)

      m <- mean(v1)
      s <- stats::sd(v1)
      delta <- abs(m - mu)

      if (is.na(s) || s <= 0 || is.na(delta) || delta <= 0) return(NULL)

      pwr_obj <- stats::power.t.test(
        n = n,
        delta = delta,
        sd = s,
        sig.level = alpha_num,
        type = "one.sample",
        alternative = alt_pwr
      )
      pwr_val <- pwr_obj$power

    } else if (test_type == "t_paired") {
      if (is.null(y1) || is.null(y2)) return(NULL)
      complete_idx <- !is.na(y1) & !is.na(y2)
      v1 <- y1[complete_idx]
      v2 <- y2[complete_idx]
      n <- length(v1)
      if (n < 2) return(NULL)

      diffs <- v1 - v2
      delta <- abs(mean(diffs))
      s_diff <- stats::sd(diffs)

      if (is.na(s_diff) || s_diff <= 0 || is.na(delta) || delta <= 0) return(NULL)

      pwr_obj <- stats::power.t.test(
        n = n,
        delta = delta,
        sd = s_diff,
        sig.level = alpha_num,
        type = "paired",
        alternative = alt_pwr
      )
      pwr_val <- pwr_obj$power

    } else if (test_type == "anova") {
      if (is.null(stat_result)) return(NULL)
      smry <- if (inherits(stat_result, "summary.aov")) {
        stat_result[[1]]
      } else if (inherits(stat_result, "aov")) {
        summary(stat_result)[[1]]
      } else {
        return(NULL)
      }

      df_grp <- smry[["Df"]][1]
      k_groups <- df_grp + 1
      df_res <- smry[["Df"]][2]
      n_total <- df_grp + df_res + 1
      n_avg <- n_total / k_groups

      ms_between <- smry[["Mean Sq"]][1]
      ms_within <- smry[["Mean Sq"]][2]

      var_between <- max(0, (ms_between - ms_within) / n_avg)
      var_within <- ms_within

      if (k_groups < 2 || n_avg < 2 || is.na(var_within) || var_within <= 0 || is.na(var_between) || var_between <= 0) {
        return(NULL)
      }

      pwr_obj <- stats::power.anova.test(
        groups = k_groups,
        n = n_avg,
        between.var = var_between,
        within.var = var_within,
        sig.level = alpha_num
      )
      pwr_val <- pwr_obj$power

    } else if (test_type == "chisq") {
      table_mat <- if (!is.null(table_data)) table_data else tab
      if (is.null(stat_result) && !is.null(table_mat)) {
        stat_result <- suppressWarnings(tryCatch(stats::chisq.test(table_mat), error = function(e) NULL))
      }
      if (is.null(stat_result)) return(NULL)
      chi2_obs <- unname(stat_result$statistic)
      df_val <- unname(stat_result$parameter)

      if (is.na(chi2_obs) || chi2_obs <= 0 || is.na(df_val) || df_val < 1) return(NULL)

      crit_val <- stats::qchisq(1 - alpha_num, df = df_val)
      pwr_val <- 1 - stats::pchisq(crit_val, df = df_val, ncp = chi2_obs)

    } else if (test_type %in% c("cor", "cor_pearson", "pearson")) {
      r_obs <- NULL
      n <- NULL
      if (!is.null(stat_result) && !is.null(stat_result$estimate) && !is.null(stat_result$parameter)) {
        r_obs <- unname(stat_result$estimate[1])
        n <- unname(stat_result$parameter) + 2
      } else if (!is.null(y1) && !is.null(y2)) {
        complete_idx <- !is.na(y1) & !is.na(y2)
        v1 <- y1[complete_idx]
        v2 <- y2[complete_idx]
        n <- length(v1)
        if (n >= 4) {
          r_obs <- suppressWarnings(stats::cor(v1, v2))
        }
      }
      if (is.null(r_obs) || is.null(n) || is.na(r_obs) || abs(r_obs) >= 1 || abs(r_obs) <= 0 || n < 4) return(NULL)

      zr <- atanh(abs(r_obs))
      z_crit <- if (alt_pwr == "one.sided") stats::qnorm(1 - alpha_num) else stats::qnorm(1 - alpha_num / 2)
      pwr_val <- stats::pnorm(zr * sqrt(n - 3) - z_crit)

    } else if (test_type == "prop_two") {
      p1 <- if (!is.null(y1)) as.numeric(y1) else if (!is.null(stat_result$estimate) && length(stat_result$estimate) >= 2) as.numeric(stat_result$estimate[1]) else NULL
      p2 <- if (!is.null(y2)) as.numeric(y2) else if (!is.null(stat_result$estimate) && length(stat_result$estimate) >= 2) as.numeric(stat_result$estimate[2]) else NULL
      n1 <- if (!is.null(table_data) && !is.null(table_data$n1)) as.numeric(table_data$n1) else 30
      n2 <- if (!is.null(table_data) && !is.null(table_data$n2)) as.numeric(table_data$n2) else 30

      if (is.null(p1) || is.null(p2) || is.na(p1) || is.na(p2) || p1 == p2) return(NULL)
      if (p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1 || n1 < 2 || n2 < 2) return(NULL)

      n_h <- (2 * n1 * n2) / (n1 + n2)
      pwr_obj <- stats::power.prop.test(
        n = max(2, round(n_h / 2)),
        p1 = p1,
        p2 = p2,
        sig.level = alpha_num,
        alternative = alt_pwr
      )
      pwr_val <- pwr_obj$power

    } else if (test_type == "prop_one") {
      p_obs <- if (!is.null(y1)) as.numeric(y1) else if (!is.null(stat_result$estimate)) as.numeric(stat_result$estimate[1]) else NULL
      p0 <- if (!is.null(mu)) as.numeric(mu) else if (!is.null(stat_result$null.value)) as.numeric(stat_result$null.value[1]) else 0.5
      n <- if (!is.null(table_data) && !is.null(table_data$n)) as.numeric(table_data$n) else 30

      if (is.null(p_obs) || is.na(p_obs) || is.na(p0) || p_obs == p0) return(NULL)
      if (p_obs <= 0 || p_obs >= 1 || p0 <= 0 || p0 >= 1 || n < 4) return(NULL)

      z_crit <- if (alt_pwr == "one.sided") stats::qnorm(1 - alpha_num) else stats::qnorm(1 - alpha_num / 2)
      num <- abs(p_obs - p0) * sqrt(n) - z_crit * sqrt(p0 * (1 - p0))
      denom <- sqrt(p_obs * (1 - p_obs))
      pwr_val <- if (denom > 0) stats::pnorm(num / denom) else NULL

    } else {
      return(NULL)
    }

    if (is.null(pwr_val) || is.na(pwr_val)) return(NULL)
    pwr_val <- max(0, min(1, pwr_val))

    mag <- if (pwr_val >= 0.80) {
      "puissance satisfaisante (\u2265 80%)"
    } else if (pwr_val >= 0.50) {
      "puissance mod\u00e9r\u00e9e"
    } else {
      "puissance faible (< 50%)"
    }

    pct_str <- paste0(round(pwr_val * 100, 1), "%")

    list(
      value = pwr_val,
      percentage = pct_str,
      magnitude = mag,
      interpretation = paste0("Puissance = ", pct_str, " (", mag, ")")
    )
  }, error = function(e) {
    NULL
  })
}


#' Evaluation de la normalite par groupe (test de Shapiro-Wilk)
#'
#' Calcule le test de normalite de Shapiro-Wilk separement pour chaque modalite
#' d'une variable de regroupement qualitative.
#'
#' @param df Data.frame contenant les donnees.
#' @param var_y Nom de la variable quantitative d'interet (chaine de caracteres).
#' @param var_group Nom de la variable qualitative de regroupement (chaine de caracteres).
#' @param alpha Niveau de significativite (defaut : 0.05).
#' @return Une liste contenant la table de synthese, les donnees QQ-plot et les metadonnees.
#' @export
ramses_compute_normality_by_group <- function(df, var_y, var_group, alpha = 0.05) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }

  if (is.null(var_y) || !is.character(var_y) || length(var_y) != 1 || !nzchar(var_y)) {
    stop("La variable quantitative 'var_y' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }

  if (!(var_y %in% names(df))) {
    stop(paste0("La variable quantitative '", var_y, "' n'existe pas dans le jeu de donn\u00e9es."))
  }

  if (is.null(var_group) || !is.character(var_group) || length(var_group) != 1 || !nzchar(var_group)) {
    stop("La variable de regroupement 'var_group' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }

  if (!(var_group %in% names(df))) {
    stop(paste0("La variable de regroupement '", var_group, "' n'existe pas dans le jeu de donn\u00e9es."))
  }

  if (var_y == var_group) {
    stop("La variable quantitative et la variable de regroupement doivent \u00eatre diff\u00e9rentes.")
  }

  if (!is.numeric(df[[var_y]])) {
    stop(paste0("La variable quantitative '", var_y, "' doit \u00eatre de type num\u00e9rique (enti\u00e8re ou double)."))
  }

  alpha_num <- as.numeric(alpha)
  if (is.na(alpha_num) || alpha_num <= 0 || alpha_num >= 1) {
    alpha_num <- 0.05
  }

  grp_col <- df[[var_group]]

  group_levels <- if (is.factor(grp_col)) {
    levels(droplevels(grp_col))
  } else {
    unique_vals <- unique(grp_col[!is.na(grp_col)])
    as.character(unique_vals)
  }

  if (length(group_levels) == 0) {
    return(list(
      var_y = var_y,
      var_group = var_group,
      alpha = alpha_num,
      summary_table = data.frame(
        Group = character(0),
        N = integer(0),
        W = numeric(0),
        p_value = numeric(0),
        Status = character(0),
        Conclusion = character(0),
        stringsAsFactors = FALSE
      ),
      qq_data = data.frame(
        group = character(0),
        y_observed = numeric(0),
        q_theoretical = numeric(0),
        stringsAsFactors = FALSE
      ),
      raw_data_by_group = list()
    ))
  }

  summary_rows <- list()
  qq_rows <- list()
  raw_list <- list()

  for (g in group_levels) {
    g_str <- as.character(g)
    sub_mask <- !is.na(grp_col) & (as.character(grp_col) == g_str)
    y_vals <- df[[var_y]][sub_mask]
    y_clean <- y_vals[!is.na(y_vals)]

    N <- length(y_clean)
    raw_list[[g_str]] <- y_clean

    stat_w <- NA_real_
    p_val <- NA_real_
    status <- "not_done"
    conclusion <- ""

    if (N < 3) {
      status <- "small_sample"
      conclusion <- "Test non r\u00e9alis\u00e9 : effectif insuffisant (N < 3)"
    } else if (N > 5000) {
      status <- "large_sample"
      conclusion <- "Test non r\u00e9alis\u00e9 : N > 5000"
    } else {
      if (all(y_clean == y_clean[1]) || (length(unique(y_clean)) == 1)) {
        status <- "constant_data"
        conclusion <- "Test non r\u00e9alis\u00e9 : valeurs constantes"
      } else {
        sh_res <- tryCatch({
          stats::shapiro.test(y_clean)
        }, error = function(e) e)

        if (inherits(sh_res, "error")) {
          status <- "error"
          conclusion <- paste0("Test non r\u00e9alis\u00e9 : ", sh_res$message)
        } else {
          stat_w <- unname(sh_res$statistic)
          p_val <- sh_res$p.value

          if (p_val >= alpha_num) {
            status <- "compatible"
            conclusion <- "Compatible avec la normalit\u00e9 (aucune d\u00e9viation significative d\u00e9tect\u00e9e)"
          } else {
            status <- "deviation"
            conclusion <- "\u00c9cart significatif \u00e0 la normalit\u00e9"
          }
        }
      }
    }

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      Group = g_str,
      N = N,
      W = stat_w,
      p_value = p_val,
      Status = status,
      Conclusion = conclusion,
      stringsAsFactors = FALSE
    )

    if (N > 0) {
      y_sorted <- sort(y_clean)
      probs <- (seq_len(N) - 0.5) / N
      mean_y <- mean(y_clean)
      sd_y <- stats::sd(y_clean)

      q_theo <- if (is.na(sd_y) || sd_y == 0) {
        rep(mean_y, N)
      } else {
        stats::qnorm(probs, mean = mean_y, sd = sd_y)
      }

      qq_rows[[length(qq_rows) + 1]] <- data.frame(
        group = rep(g_str, N),
        y_observed = y_sorted,
        q_theoretical = q_theo,
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  qq_df <- if (length(qq_rows) > 0) do.call(rbind, qq_rows) else data.frame(group = character(0), y_observed = numeric(0), q_theoretical = numeric(0), stringsAsFactors = FALSE)

  list(
    var_y = var_y,
    var_group = var_group,
    alpha = alpha_num,
    summary_table = summary_df,
    qq_data = qq_df,
    raw_data_by_group = raw_list
  )
}

#' Recuperation dynamique securisee de la version du package Ramses
#'
#' @return Chaine de caracteres representant la version (ex. "1.0.0").
#' @noRd
ramses_get_version <- function() {
  ver <- tryCatch(
    as.character(utils::packageVersion("Ramses")),
    error = function(e) NULL
  )
  if (!is.null(ver) && length(ver) == 1 && nzchar(ver)) {
    return(ver)
  }

  desc_paths <- c(
    system.file("DESCRIPTION", package = "Ramses"),
    "DESCRIPTION",
    "Ramses/DESCRIPTION",
    "../DESCRIPTION"
  )
  for (dp in desc_paths) {
    if (nzchar(dp) && file.exists(dp)) {
      dcf <- tryCatch(read.dcf(dp), error = function(e) NULL)
      if (!is.null(dcf) && "Version" %in% colnames(dcf)) {
        return(as.character(dcf[1, "Version"]))
      }
    }
  }
  "1.0.0"
}

# ==============================================================================
# TESTS DE PROPORTIONS (1 ET 2 ECHANTILLONS)
# ==============================================================================

#' Test d'une proportion contre une valeur theorique (prop.test ou binom.test)
#'
#' @param df Data.frame contenant les donnees.
#' @param var_outcome Nom de la variable qualitative ou binaire d'interet (chaine de caracteres).
#' @param success_modality Modalite consideree comme succes.
#' @param p0 Proportion theorique sous H0 (nombre strictement entre 0 et 1, defaut : 0.5).
#' @param alternative Type d'hypothese alternative ("two.sided", "less", "greater", defaut : "two.sided").
#' @param conf_level Niveau de confiance de l'intervalle (defaut : 0.95).
#' @param method Methode statistique ("prop" pour prop.test ou "binom" pour binom.test).
#' @param correct Booleen indiquant si la correction de continuite de Yates est appliquee (defaut : TRUE).
#' @return Liste structuree de resultats avec effectifs, proportions, statistiques, p-value, intervalle de confiance, hypotheses, interpretation et avertissements.
#' @export
ramses_test_one_prop <- function(df,
                                 var_outcome,
                                 success_modality,
                                 p0 = 0.5,
                                 alternative = c("two.sided", "less", "greater"),
                                 conf_level = 0.95,
                                 method = c("prop", "binom"),
                                 correct = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(var_outcome) || !is.character(var_outcome) || length(var_outcome) != 1 || !nzchar(var_outcome)) {
    stop("La variable d'int\u00e9r\u00eat 'var_outcome' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }
  if (!(var_outcome %in% names(df))) {
    stop(paste0("La variable '", var_outcome, "' n'existe pas dans le jeu de donn\u00e9es."))
  }

  p0_num <- as.numeric(p0)
  if (is.na(p0_num) || p0_num <= 0 || p0_num >= 1) {
    stop("La proportion th\u00e9orique 'p0' doit \u00eatre un nombre strictement compris entre 0 et 1.")
  }

  conf_num <- as.numeric(conf_level)
  if (is.na(conf_num) || conf_num <= 0 || conf_num >= 1) {
    conf_num <- 0.95
  }

  alternative <- match.arg(alternative)
  method <- match.arg(method)
  correct_bool <- isTRUE(correct)

  raw_vals <- df[[var_outcome]]
  vals_clean <- raw_vals[!is.na(raw_vals)]
  n_total <- length(vals_clean)

  if (n_total == 0) {
    stop(paste0("La variable '", var_outcome, "' ne contient aucune observation valide non manquante."))
  }

  if (is.null(success_modality) || length(success_modality) != 1 || is.na(success_modality)) {
    stop("La modalit\u00e9 de succ\u00e8s doit \u00eatre sp\u00e9cifi\u00e9e sous forme d'une valeur unique non manquante.")
  }

  succ_char <- as.character(success_modality)
  avail_modalities <- unique(as.character(vals_clean))
  if (!(succ_char %in% avail_modalities)) {
    stop(paste0(
      "La modalit\u00e9 de succ\u00e8s s\u00e9lectionn\u00e9e ('", succ_char,
      "') n'existe pas dans les observations non manquantes de la variable '", var_outcome, "'. ",
      "Modalit\u00e9s observ\u00e9es : ", paste0("'", avail_modalities, "'", collapse = ", "), "."
    ))
  }

  x_success <- sum(as.character(vals_clean) == succ_char)
  n_failure <- n_total - x_success
  p_obs <- x_success / n_total
  diff_val <- p_obs - p0_num

  # Verification des conditions d'approximation normale
  exp_succ <- n_total * p0_num
  exp_fail <- n_total * (1 - p0_num)
  conditions_met <- (exp_succ >= 5) && (exp_fail >= 5)
  warn_msg <- NULL
  if (method == "prop" && !conditions_met) {
    warn_msg <- paste0(
      "Avertissement statistique : Faibles effectifs attendus sous H0. Les conditions de validit\u00e9 de l'approximation normale (n * p0 >= 5 et n * (1 - p0) >= 5) ne sont pas satisfaites (n * p0 = ",
      round(exp_succ, 2), ", n * (1 - p0) = ", round(exp_fail, 2),
      "). L'utilisation du test exact binomial (binom.test) est fortement recommand\u00e9e."
    )
  }

  htest_res <- if (method == "prop") {
    suppressWarnings(stats::prop.test(
      x = x_success,
      n = n_total,
      p = p0_num,
      alternative = alternative,
      conf.level = conf_num,
      correct = correct_bool
    ))
  } else {
    stats::binom.test(
      x = x_success,
      n = n_total,
      p = p0_num,
      alternative = alternative,
      conf.level = conf_num
    )
  }

  p_val <- htest_res$p.value
  stat_val <- if (!is.null(htest_res$statistic)) unname(htest_res$statistic) else x_success
  stat_name <- if (!is.null(htest_res$statistic)) names(htest_res$statistic) else "x (succ\u00e8s)"
  df_param <- if (!is.null(htest_res$parameter)) unname(htest_res$parameter) else NA
  ci_vals <- as.numeric(htest_res$conf.int)

  # Taille d'effet et puissance
  es <- ramses_compute_effect_size("prop_one", y1 = p_obs, mu = p0_num)
  pwr <- ramses_compute_power(
    "prop_one",
    y1 = p_obs,
    mu = p0_num,
    alpha = 1 - conf_num,
    alternative = alternative,
    table_data = list(n = n_total)
  )

  # Formulations pedagogiques des hypotheses
  alt_sym <- if (alternative == "two.sided") "\u2260" else if (alternative == "less") "<" else ">"
  alt_txt <- if (alternative == "two.sided") "diff\u00e9rente de" else if (alternative == "less") "inf\u00e9rieure \u00e0" else "sup\u00e9rieure \u00e0"

  h0_str <- paste0("H0 : p = ", round(p0_num, 4), " (la proportion dans la population est \u00e9gale \u00e0 la proportion th\u00e9orique)")
  h1_str <- paste0("H1 : p ", alt_sym, " ", round(p0_num, 4), " (la proportion dans la population est ", alt_txt, " la proportion th\u00e9orique)")

  # Decision statistique stricte (ne jamais ecrire 'H0 est vraie')
  alpha_val <- 1 - conf_num
  is_sig <- p_val < alpha_val

  decision_str <- if (is_sig) {
    paste0(
      "La proportion observ\u00e9e est de ", round(p_obs * 100, 1), " % (", x_success, " succ\u00e8s sur ", n_total, " observations). ",
      "Le test donne une p-value de ", format.pval(p_val, digits = 4, eps = 0.0001), ". ",
      "Au seuil de significativit\u00e9 de ", round(alpha_val * 100), " %, nous rejetons l'hypoth\u00e8se nulle H0. ",
      "La diff\u00e9rence entre la proportion observ\u00e9e (", round(p_obs * 100, 1), " %) et la proportion th\u00e9orique de r\u00e9f\u00e9rence (", round(p0_num * 100, 1), " %) est statistiquement significative."
    )
  } else {
    paste0(
      "La proportion observ\u00e9e est de ", round(p_obs * 100, 1), " % (", x_success, " succ\u00e8s sur ", n_total, " observations). ",
      "Le test donne une p-value de ", format.pval(p_val, digits = 4, eps = 0.0001), ". ",
      "Au seuil de significativit\u00e9 de ", round(alpha_val * 100), " %, nous ne rejetons pas l'hypoth\u00e8se nulle H0. ",
      "La diff\u00e9rence observ\u00e9e entre la proportion observ\u00e9e (", round(p_obs * 100, 1), " %) et la proportion th\u00e9orique (", round(p0_num * 100, 1), " %) n'est pas statistiquement significative."
    )
  }

  # Generation de code R securise
  col_code <- ramses_code_column("df", var_outcome)
  succ_lit <- ramses_code_string(succ_char)
  code_text <- paste0(
    "# Test d'une proportion (", if (method == "prop") "prop.test" else "binom.test", ")\n",
    "x_vec <- ", col_code, "\n",
    "x_clean <- x_vec[!is.na(x_vec)]\n",
    "n_total <- length(x_clean)\n",
    "n_success <- sum(as.character(x_clean) == ", succ_lit, ")\n",
    if (method == "prop") {
      paste0(
        "stats::prop.test(x = n_success, n = n_total, p = ", p0_num,
        ", alternative = ", ramses_code_string(alternative),
        ", conf.level = ", conf_num,
        ", correct = ", correct_bool, ")"
      )
    } else {
      paste0(
        "stats::binom.test(x = n_success, n = n_total, p = ", p0_num,
        ", alternative = ", ramses_code_string(alternative),
        ", conf.level = ", conf_num, ")"
      )
    }
  )

  list(
    var_outcome = var_outcome,
    success_modality = succ_char,
    n = n_total,
    x = x_success,
    n_failure = n_failure,
    p_obs = p_obs,
    p0 = p0_num,
    diff = diff_val,
    conf_level = conf_num,
    conf_int = ci_vals,
    method = method,
    correct = correct_bool,
    alternative = alternative,
    statistic = stat_val,
    statistic_name = stat_name,
    parameter = df_param,
    p_value = p_val,
    conditions_met = conditions_met,
    exp_success = exp_succ,
    exp_failure = exp_fail,
    warning_msg = warn_msg,
    effect_size = es,
    power = pwr,
    hypotheses = list(h0 = h0_str, h1 = h1_str),
    decision = decision_str,
    is_significant = is_sig,
    code = code_text,
    htest = htest_res
  )
}

#' Test de comparaison de deux proportions entre deux groupes independants
#'
#' @param df Data.frame contenant les donnees.
#' @param var_outcome Nom de la variable qualitative representant le critere de succes.
#' @param var_group Nom de la variable qualitative definissant les groupes.
#' @param success_modality Modalite consideree comme succes.
#' @param group_levels Vecteur de deux modalites de var_group a comparer (si NULL, les 2 premieres modalites uniques sont utilisees).
#' @param alternative Type d'hypothese alternative ("two.sided", "less", "greater", defaut : "two.sided").
#' @param conf_level Niveau de confiance pour l'intervalle (defaut : 0.95).
#' @param method Methode statistique : "prop" (test asymptotique prop.test) ou "fisher" (test exact de Fisher fisher.test).
#' @param correct Booleen indiquant si la correction de continuite de Yates est appliquee (defaut : TRUE).
#' @return Liste structuree avec effectifs par groupe, proportions, difference, ratio, statistiques, p-value, intervalle de confiance, hypotheses, interpretation et avertissements.
#' @export
ramses_test_two_props <- function(df,
                                  var_outcome,
                                  var_group,
                                  success_modality,
                                  group_levels = NULL,
                                  alternative = c("two.sided", "less", "greater"),
                                  conf_level = 0.95,
                                  method = c("prop", "fisher"),
                                  correct = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(var_outcome) || !is.character(var_outcome) || length(var_outcome) != 1 || !nzchar(var_outcome)) {
    stop("La variable d'int\u00e9r\u00eat 'var_outcome' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }
  if (!(var_outcome %in% names(df))) {
    stop(paste0("La variable '", var_outcome, "' n'existe pas dans le jeu de donn\u00e9es."))
  }
  if (is.null(var_group) || !is.character(var_group) || length(var_group) != 1 || !nzchar(var_group)) {
    stop("La variable de regroupement 'var_group' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }
  if (!(var_group %in% names(df))) {
    stop(paste0("La variable de groupe '", var_group, "' n'existe pas dans le jeu de donn\u00e9es."))
  }
  if (var_outcome == var_group) {
    stop("La variable de crit\u00e8re et la variable de regroupement doivent \u00eatre diff\u00e9rentes.")
  }

  conf_num <- as.numeric(conf_level)
  if (is.na(conf_num) || conf_num <= 0 || conf_num >= 1) {
    conf_num <- 0.95
  }

  alternative <- match.arg(alternative)
  method <- match.arg(method)
  correct_bool <- isTRUE(correct)

  # Nettoyage des valeurs manquantes
  valid_idx <- !is.na(df[[var_outcome]]) & !is.na(df[[var_group]])
  df_clean <- df[valid_idx, , drop = FALSE]

  if (nrow(df_clean) == 0) {
    stop("Aucune observation conjointe valide non manquante pour ces deux variables.")
  }

  avail_groups <- unique(as.character(df_clean[[var_group]]))
  if (is.null(group_levels)) {
    if (length(avail_groups) < 2) {
      stop("La variable de regroupement doit comporter au moins 2 modalit\u00e9s distinctes.")
    }
    group_levels <- avail_groups[1:2]
  } else {
    if (length(group_levels) != 2) {
      stop("Le param\u00e8tre 'group_levels' doit comporter exactement 2 modalit\u00e9s.")
    }
    if (!all(group_levels %in% avail_groups)) {
      stop("Les modalit\u00e9s de groupes sp\u00e9cifi\u00e9es ne sont pas toutes pr\u00e9sentes dans les donn\u00e9es.")
    }
  }

  grp1 <- as.character(group_levels[1])
  grp2 <- as.character(group_levels[2])
  succ_char <- as.character(success_modality)

  sub_df <- df_clean[as.character(df_clean[[var_group]]) %in% c(grp1, grp2), , drop = FALSE]

  g_col <- as.character(sub_df[[var_group]])
  y_col <- as.character(sub_df[[var_outcome]])

  n1 <- sum(g_col == grp1)
  n2 <- sum(g_col == grp2)

  if (n1 == 0 || n2 == 0) {
    stop("L'un des deux groupes ne poss\u00e8de aucune observation.")
  }

  x1 <- sum(g_col == grp1 & y_col == succ_char)
  x2 <- sum(g_col == grp2 & y_col == succ_char)

  p1 <- x1 / n1
  p2 <- x2 / n2
  diff_p <- p1 - p2

  # Ratios
  rr <- if (p2 > 0) p1 / p2 else NA
  or_val <- if (p2 > 0 && p2 < 1 && p1 < 1 && p1 > 0) (x1 * (n2 - x2)) / (x2 * (n1 - x1)) else NA

  # Verification des conditions d'application (effectifs attendus sous H0)
  p_bar <- (x1 + x2) / (n1 + n2)
  exp_counts <- c(n1 * p_bar, n1 * (1 - p_bar), n2 * p_bar, n2 * (1 - p_bar))
  min_exp <- min(exp_counts)
  conditions_met <- (min_exp >= 5)
  warn_msg <- NULL
  if (method == "prop" && !conditions_met) {
    warn_msg <- paste0(
      "Avertissement statistique : Faibles effectifs attendus sous H0. Les conditions de validit\u00e9 de l'approximation normale ne sont pas satisfaites (effectif th\u00e9orique minimal sous H0 = ",
      round(min_exp, 2), " < 5). L'utilisation du test exact de Fisher (fisher.test) est fortement recommand\u00e9e."
    )
  }

  mat_2x2 <- matrix(
    c(x1, n1 - x1, x2, n2 - x2),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c(grp1, grp2), c("Succes", "Echec"))
  )

  htest_res <- if (method == "prop") {
    suppressWarnings(stats::prop.test(
      x = c(x1, x2),
      n = c(n1, n2),
      alternative = alternative,
      conf.level = conf_num,
      correct = correct_bool
    ))
  } else {
    stats::fisher.test(
      mat_2x2,
      alternative = alternative,
      conf.level = conf_num
    )
  }

  p_val <- htest_res$p.value
  stat_val <- if (!is.null(htest_res$statistic)) unname(htest_res$statistic) else NA
  stat_name <- if (!is.null(htest_res$statistic)) names(htest_res$statistic) else "Test exact"
  df_param <- if (!is.null(htest_res$parameter)) unname(htest_res$parameter) else NA

  # Intervalle de confiance de la difference p1 - p2
  ci_diff <- if (method == "prop") {
    as.numeric(htest_res$conf.int)
  } else {
    ref_prop <- suppressWarnings(stats::prop.test(
      x = c(x1, x2),
      n = c(n1, n2),
      alternative = alternative,
      conf.level = conf_num,
      correct = correct_bool
    ))
    as.numeric(ref_prop$conf.int)
  }

  # Intervalles de confiance individuels
  ci1 <- as.numeric(suppressWarnings(stats::prop.test(x1, n1, conf.level = conf_num))$conf.int)
  ci2 <- as.numeric(suppressWarnings(stats::prop.test(x2, n2, conf.level = conf_num))$conf.int)

  # Taille d'effet et puissance
  es <- ramses_compute_effect_size("prop_two", y1 = p1, y2 = p2)
  pwr <- ramses_compute_power(
    "prop_two",
    y1 = p1,
    y2 = p2,
    alpha = 1 - conf_num,
    alternative = alternative,
    table_data = list(n1 = n1, n2 = n2)
  )

  # Hypotheses
  alt_sym <- if (alternative == "two.sided") "\u2260" else if (alternative == "less") "<" else ">"
  alt_txt <- if (alternative == "two.sided") "diff\u00e9rentes" else if (alternative == "less") "inf\u00e9rieure dans le groupe 1" else "sup\u00e9rieure dans le groupe 1"

  h0_str <- "H0 : p1 = p2 (les deux groupes pr\u00e9sentent la m\u00eame proportion de succ\u00e8s dans la population)"
  h1_str <- paste0("H1 : p1 ", alt_sym, " p2 (la proportion de succ\u00e8s est ", alt_txt, ")")

  # Decision statistique
  alpha_val <- 1 - conf_num
  is_sig <- p_val < alpha_val

  decision_str <- if (is_sig) {
    paste0(
      "Dans le groupe '", grp1, "', la proportion observ\u00e9e est de ", round(p1 * 100, 1), " % (", x1, "/", n1, ") ",
      "contre ", round(p2 * 100, 1), " % (", x2, "/", n2, ") dans le groupe '", grp2, "' ",
      "(diff\u00e9rence observ\u00e9e : ", round(diff_p * 100, 1), " points de pourcentage). ",
      "Le test donne une p-value de ", format.pval(p_val, digits = 4, eps = 0.0001), ". ",
      "Au seuil de ", round(alpha_val * 100), " %, nous rejetons l'hypoth\u00e8se nulle H0. ",
      "Il existe une diff\u00e9rence statistiquement significative entre les proportions des deux groupes."
    )
  } else {
    paste0(
      "Dans le groupe '", grp1, "', la proportion observ\u00e9e est de ", round(p1 * 100, 1), " % (", x1, "/", n1, ") ",
      "contre ", round(p2 * 100, 1), " % (", x2, "/", n2, ") dans le groupe '", grp2, "' ",
      "(diff\u00e9rence observ\u00e9e : ", round(diff_p * 100, 1), " points de pourcentage). ",
      "Le test donne une p-value de ", format.pval(p_val, digits = 4, eps = 0.0001), ". ",
      "Au seuil de ", round(alpha_val * 100), " %, nous ne rejetons pas l'hypoth\u00e8se nulle H0. ",
      "La diff\u00e9rence observ\u00e9e entre les deux groupes n'est pas statistiquement significative."
    )
  }

  # Code R reproductible
  col_y_code <- ramses_code_column("df", var_outcome)
  col_g_code <- ramses_code_column("df", var_group)
  succ_lit <- ramses_code_string(succ_char)
  grp1_lit <- ramses_code_string(grp1)
  grp2_lit <- ramses_code_string(grp2)

  code_text <- paste0(
    "# Comparaison de deux proportions (", if (method == "prop") "prop.test" else "fisher.test", ")\n",
    "sub_df <- df[!is.na(", col_y_code, ") & !is.na(", col_g_code, ") & (as.character(", col_g_code, ") %in% c(", grp1_lit, ", ", grp2_lit, ")), ]\n",
    "n1 <- sum(as.character(sub_df[[", ramses_code_string(var_group), "]]) == ", grp1_lit, ")\n",
    "n2 <- sum(as.character(sub_df[[", ramses_code_string(var_group), "]]) == ", grp2_lit, ")\n",
    "x1 <- sum(as.character(sub_df[[", ramses_code_string(var_group), "]]) == ", grp1_lit, " & as.character(sub_df[[", ramses_code_string(var_outcome), "]]) == ", succ_lit, ")\n",
    "x2 <- sum(as.character(sub_df[[", ramses_code_string(var_group), "]]) == ", grp2_lit, " & as.character(sub_df[[", ramses_code_string(var_outcome), "]]) == ", succ_lit, ")\n",
    "tab_props <- data.frame(\n",
    "  Groupe = c(", grp1_lit, ", ", grp2_lit, "),\n",
    "  Succes = c(x1, x2),\n",
    "  Total = c(n1, n2),\n",
    "  Proportion = c(x1 / n1, x2 / n2)\n",
    ")\n",
    "print(tab_props)\n",
    if (method == "prop") {
      paste0(
        "stats::prop.test(x = c(x1, x2), n = c(n1, n2), alternative = ", ramses_code_string(alternative),
        ", conf.level = ", conf_num, ", correct = ", correct_bool, ")"
      )
    } else {
      paste0(
        "mat_2x2 <- matrix(c(x1, n1 - x1, x2, n2 - x2), nrow = 2, byrow = TRUE)\n",
        "stats::fisher.test(mat_2x2, alternative = ", ramses_code_string(alternative), ", conf.level = ", conf_num, ")"
      )
    }
  )

  list(
    var_outcome = var_outcome,
    var_group = var_group,
    success_modality = succ_char,
    group_levels = c(grp1, grp2),
    groups_summary = data.frame(
      Groupe = c(grp1, grp2),
      Succes = c(x1, x2),
      Total = c(n1, n2),
      Proportion = c(p1, p2),
      stringsAsFactors = FALSE
    ),
    diff = diff_p,
    relative_risk = rr,
    odds_ratio = or_val,
    conf_level = conf_num,
    conf_int_diff = ci_diff,
    conf_int_grp1 = ci1,
    conf_int_grp2 = ci2,
    method = method,
    correct = correct_bool,
    alternative = alternative,
    statistic = stat_val,
    statistic_name = stat_name,
    parameter = df_param,
    p_value = p_val,
    conditions_met = conditions_met,
    min_expected = min_exp,
    warning_msg = warn_msg,
    effect_size = es,
    power = pwr,
    hypotheses = list(h0 = h0_str, h1 = h1_str),
    decision = decision_str,
    is_significant = is_sig,
    table_2x2 = mat_2x2,
    code = code_text,
    htest = htest_res
  )
}



#' Pr\u00e9pare le fichier de donn\u00e9es pour le rapport HTML (copie et chemin relatif)
#'
#' @param source_datapath Chemin absolu du fichier source actuel (par ex. upload temp).
#' @param original_name Nom original du fichier (pour conserver l'extension et le sens).
#' @param report_dir R\u00e9pertoire de travail du rapport HTML o\u00f9 le fichier doit \u00eatre copi\u00e9.
#' @return Le chemin relatif (vers `data/...`) \u00e0 ins\u00e9rer dans le Rmd.
#' @noRd
ramses_prepare_report_data_file <- function(source_datapath, original_name, report_dir) {
  if (is.null(source_datapath) || !file.exists(source_datapath)) {
    stop(paste0("Le fichier source de donn\u00e9es (", original_name, ") n'est plus accessible ou n'existe pas."))
  }
  
  data_dir <- file.path(report_dir, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Nettoyage s\u00e9curis\u00e9 du nom de fichier (garder lettres, chiffres, . _ -)
  # Mais on veut conserver l'extension intacte.
  ext <- tools::file_ext(original_name)
  base_name <- tools::file_path_sans_ext(original_name)
  
  clean_base <- gsub("[^A-Za-z0-9_.-]", "_", base_name)
  if (clean_base == "" || clean_base == "_") clean_base <- "dataset"
  
  safe_name <- paste0(clean_base, ".", ext)
  
  # \u00c9viter l'\u00e9crasement silencieux si un fichier avec le m\u00eame nom existe
  target_path <- file.path(data_dir, safe_name)
  counter <- 1
  while (file.exists(target_path)) {
    # V\u00e9rifier si c'est exactement le m\u00eame fichier (par exemple, appel multiple pour le m\u00eame rapport)
    # Dans ce cas, on peut r\u00e9utiliser. Sinon, on renomme.
    # Pour faire simple, on va toujours incr\u00e9menter sauf s'il fait la m\u00eame taille (approximation).
    # Mieux : on incr\u00e9mente toujours si c'est pas le m\u00eame chemin source, mais le source est un fichier temp \u00e0 chaque upload.
    target_path <- file.path(data_dir, paste0(clean_base, "_", counter, ".", ext))
    counter <- counter + 1
  }
  
  file.copy(source_datapath, target_path, overwrite = TRUE)
  
  # Retourner le chemin relatif
  rel_path <- file.path("data", basename(target_path))
  # Remplacer les slashs inverses Windows par des slashs UNIX pour le code R
  rel_path <- gsub("\\\\", "/", rel_path)
  
  return(rel_path)
}

#' Liste les jeux de donn\u00e9es (data.frame, tibble, data.table) disponibles dans un environnement R
#'
#' @param envir Environnement \u00e0 inspecter (par d\u00e9faut \code{.GlobalEnv}).
#' @return Un \code{data.frame} contenant les m\u00e9tadonn\u00e9es des objets d\u00e9tect\u00e9s (name, nrow, ncol, class, size, valid).
#' @export
ramses_list_workspace_datasets <- function(envir = .GlobalEnv) {
  if (!is.environment(envir)) {
    return(data.frame(
      name = character(0),
      nrow = integer(0),
      ncol = integer(0),
      class = character(0),
      size = character(0),
      valid = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  obj_names <- ls(envir = envir, all.names = FALSE)
  if (length(obj_names) == 0) {
    return(data.frame(
      name = character(0),
      nrow = integer(0),
      ncol = integer(0),
      class = character(0),
      size = character(0),
      valid = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- list()
  for (nm in obj_names) {
    # R\u00e9cup\u00e9ration s\u00e9curis\u00e9e directe sans chercher dans les environnements parents
    obj <- tryCatch({
      get(nm, envir = envir, inherits = FALSE)
    }, error = function(e) NULL)

    if (is.null(obj)) next

    if (is.data.frame(obj)) {
      nr <- nrow(obj)
      nc <- ncol(obj)
      cls <- class(obj)[1]
      sz <- tryCatch({
        format(utils::object.size(obj), units = "auto")
      }, error = function(e) "")

      is_valid <- is.numeric(nr) && is.numeric(nc) && nr > 0 && nc > 0

      results[[length(results) + 1]] <- list(
        name = nm,
        nrow = as.integer(nr),
        ncol = as.integer(nc),
        class = as.character(cls),
        size = as.character(sz),
        valid = is_valid
      )
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      name = character(0),
      nrow = integer(0),
      ncol = integer(0),
      class = character(0),
      size = character(0),
      valid = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  df_res <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
  # Tri alphab\u00e9tique par nom
  df_res <- df_res[order(df_res$name), , drop = FALSE]
  rownames(df_res) <- NULL
  return(df_res)
}

#' R\u00e9cup\u00e8re et valide un jeu de donn\u00e9es depuis un environnement R
#'
#' @param name Nom de l'objet dans l'environnement.
#' @param envir Environnement source (par d\u00e9faut \code{.GlobalEnv}).
#' @return Un \code{data.frame} pr\u00eat pour Ramses.
#' @export
ramses_get_workspace_dataset <- function(name, envir = .GlobalEnv) {
  if (missing(name) || !is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("Le nom du jeu de donn\u00e9es doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }

  if (!is.environment(envir)) {
    stop("L'environnement sp\u00e9cifi\u00e9 n'est pas valide.")
  }

  if (!exists(name, envir = envir, inherits = FALSE)) {
    stop(paste0("L'objet '", name, "' n'existe pas dans l'environnement R sp\u00e9cifi\u00e9."))
  }

  obj <- get(name, envir = envir, inherits = FALSE)

  if (!is.data.frame(obj)) {
    stop(paste0("L'objet '", name, "' n'est pas un tableau de donn\u00e9es (data.frame, tibble ou data.table)."))
  }

  if (nrow(obj) == 0 || ncol(obj) == 0) {
    stop(paste0("Le tableau '", name, "' est vide (0 observation ou 0 variable) et ne peut pas \u00eatre utilis\u00e9."))
  }

  # Conversion propre en data.frame standard sans modifier l'objet original dans .GlobalEnv
  final_df <- as.data.frame(obj)

  return(final_df)
}

#' Composant UI reutilisable pour l'affichage de tuiles statistiques (Dashboard)
#'
#' Construit un conteneur responsive de cartes/tuiles statistiques sobre et moderne,
#' respectant fidelement le design system de Ramses (Bootstrap cards, typographie, espacements).
#'
#' @param tiles Liste de listes, chaque element contenant au moins 'label' et 'value',
#'   et optionnellement 'subtext', 'status' (ex: "primary", "success", "danger", "dark"),
#'   ou 'title_attr'.
#' @param title Titre facultatif du bloc (ex: "Indicateurs cles"). Si fourni,
#'   une banniere/titre sobre est affichee au-dessus des tuiles.
#' @param n_cols Nombre maximal de colonnes souhaitees sur desktop (NULL = calcul automatique).
#' @param card_wrapper Booleen indiquant s'il faut envelopper les tuiles dans un bslib/card.
#' @return Un objet \code{shiny.tag} (div).
#' @export
ramses_result_tiles <- function(tiles, title = NULL, n_cols = NULL, card_wrapper = FALSE) {
  if (is.null(tiles) || length(tiles) == 0) return(NULL)

  valid_tiles <- tiles[!vapply(tiles, is.null, logical(1))]
  n_tiles <- length(valid_tiles)
  if (n_tiles == 0) return(NULL)

  # Determination automatique des classes Bootstrap responsive
  col_class <- if (!is.null(n_cols)) {
    paste0("col-6 col-sm-4 col-md-", max(1, min(12, floor(12 / n_cols))))
  } else if (n_tiles == 1) {
    "col-12 col-sm-6 col-md-4"
  } else if (n_tiles == 2) {
    "col-6 col-sm-6 col-md-6"
  } else if (n_tiles == 3) {
    "col-6 col-sm-4 col-md-4"
  } else if (n_tiles == 4) {
    "col-6 col-sm-6 col-md-3"
  } else if (n_tiles == 5) {
    "col-6 col-sm-4 col-lg"
  } else if (n_tiles == 6) {
    "col-6 col-sm-4 col-md-2"
  } else if (n_tiles <= 8) {
    "col-6 col-sm-3 col-md-3"
  } else {
    "col-6 col-sm-4 col-md-3 col-lg-2"
  }

  tile_nodes <- lapply(valid_tiles, function(item) {
    lbl <- if (!is.null(item$label)) as.character(item$label) else ""
    val <- if (!is.null(item$value)) {
      if (is.na(item$value) || length(item$value) == 0) "\u2014" else as.character(item$value)
    } else "\u2014"
    sub <- if (!is.null(item$subtext) && !is.na(item$subtext) && nzchar(as.character(item$subtext))) as.character(item$subtext) else NULL
    status <- if (!is.null(item$status) && nzchar(as.character(item$status))) as.character(item$status) else "dark"

    val_class <- switch(
      status,
      "primary" = "text-primary",
      "success" = "text-success",
      "danger" = "text-danger",
      "warning" = "text-warning",
      "info" = "text-info",
      "secondary" = "text-secondary",
      "text-dark"
    )

    # Gestion de la taille de texte selon la longueur de la valeur
    font_class <- if (nchar(val) > 18) {
      "small fw-bold"
    } else if (nchar(val) > 12) {
      "fs-6 fw-bold"
    } else {
      "fs-5 fw-bold"
    }

    shiny::div(
      class = paste0(col_class, " mb-2"),
      shiny::div(
        class = "p-2 bg-white rounded border shadow-sm h-100 d-flex flex-column justify-content-between",
        style = "min-height: 72px; border-color: #E5E7EB !important;",
        shiny::div(
          class = "text-muted extra-small fw-semibold text-uppercase text-truncate mb-1",
          title = lbl,
          lbl
        ),
        shiny::div(
          class = paste0(font_class, " ", val_class, " text-truncate"),
          title = val,
          val
        ),
        if (!is.null(sub)) {
          shiny::div(
            class = "text-muted extra-small text-truncate mt-1",
            title = sub,
            sub
          )
        } else {
          shiny::tags$span(style = "display: none;")
        }
      )
    )
  })

  row_content <- shiny::div(
    class = "row g-2 text-center mb-3 align-items-stretch",
    tile_nodes
  )

  if (!is.null(title) && nzchar(title)) {
    shiny::div(
      class = "mb-3",
      shiny::div(
        class = "d-flex align-items-center gap-2 mb-2",
        shiny::tags$span(class = "fw-semibold text-dark small", title)
      ),
      row_content
    )
  } else if (card_wrapper) {
    shiny::div(
      class = "card mb-3 shadow-sm border-0 bg-light",
      shiny::div(
        class = "card-body py-2 px-3",
        row_content
      )
    )
  } else {
    row_content
  }
}

#' Restitution pedagogique et bien structuree d'un modele de regression lineaire (lm)
#'
#' @param reg_state Objet reactiveValues ou liste contenant 'model', 'var_y', 'vars_x', 'alpha'
#' @param alpha Seuil de significativite par defaut (0.05)
#' @return Un objet \code{shiny.tag} (tagList) structure en 5 etapes
#' @noRd
ramses_render_lm_results <- function(reg_state, alpha = 0.05) {
  mod <- reg_state$model
  if (is.null(mod) || !inherits(mod, "lm")) {
    return(shiny::p(class = "text-danger small", "Mod\u00e8le de r\u00e9gression lin\u00e9aire invalide ou indisponible."))
  }

  var_y <- reg_state$var_y
  vars_x <- reg_state$vars_x
  alpha_val <- as.numeric(if (!is.null(reg_state$alpha)) reg_state$alpha else alpha)
  if (is.na(alpha_val) || alpha_val <= 0 || alpha_val >= 1) alpha_val <- 0.05

  smry <- summary(mod)
  coef_table <- as.data.frame(smry$coefficients)

  # Intervalles de confiance a 95%
  ci_matrix <- tryCatch(stats::confint(mod, level = 0.95), error = function(e) NULL)

  # 1. TEST GLOBAL DU MODELE (Statistique F de Fisher)
  fstat <- smry$fstatistic
  if (!is.null(fstat) && length(fstat) == 3) {
    f_val <- as.numeric(fstat[1])
    df1 <- as.numeric(fstat[2])
    df2 <- as.numeric(fstat[3])
    f_pval <- stats::pf(f_val, df1, df2, lower.tail = FALSE)
  } else {
    f_val <- NA
    df1 <- NA
    df2 <- NA
    f_pval <- NA
  }

  is_f_sig <- !is.na(f_pval) && f_pval < alpha_val

  global_test_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold d-flex justify-content-between align-items-center py-2 border-bottom",
      shiny::tags$span("1. Test global du mod\u00e8le"),
      shiny::tags$span(
        class = paste0("badge ", if (is_f_sig) "bg-success" else "bg-secondary"),
        if (is_f_sig) paste0("Statistiquement significatif (p < ", alpha_val, ")") else "Non significatif (p \u2265 0.05)"
      )
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::div(
        class = "row align-items-center g-2",
        shiny::div(
          class = "col-md-5 border-end text-center py-1",
          shiny::div(class = "text-muted small", "Statistique F de Fisher & ddl"),
          shiny::div(
            class = "fs-5 fw-bold text-dark",
            if (!is.na(f_val)) paste0("F(", df1, ", ", df2, ") = ", round(f_val, 2)) else "N/A"
          ),
          shiny::div(
            class = "small fw-semibold text-primary",
            if (!is.na(f_pval)) paste0("p-value = ", format.pval(f_pval, digits = 4, eps = 0.0001)) else "N/A"
          )
        ),
        shiny::div(
          class = "col-md-7 ps-md-3",
          shiny::tags$p(
            class = "mb-0 small text-dark",
            if (is_f_sig) {
              shiny::HTML(paste0("<strong>Interpr\u00e9tation :</strong> Le mod\u00e8le est statistiquement significatif (F(", df1, ", ", df2, ") = ", round(f_val, 2), " ; ", format.pval(f_pval, digits = 4, eps = 0.0001), ") : les variables explicatives consid\u00e9r\u00e9es apportent collectivement une information statistiquement significative sur la variable \u00e9tudi\u00e9e (<em>", htmltools::htmlEscape(var_y), "</em>)."))
            } else {
              shiny::HTML(paste0("<strong>Interpr\u00e9tation :</strong> Le mod\u00e8le n'est pas statistiquement significatif au seuil de 5 % (F(", df1, ", ", df2, ") = ", round(f_val, 2), " ; ", format.pval(f_pval, digits = 4, eps = 0.0001), "). Les donn\u00e9es ne fournissent pas suffisamment d'\u00e9l\u00e9ments pour conclure que les variables explicatives apportent collectivement une information statistiquement significative sur la variable \u00e9tudi\u00e9e (<em>", htmltools::htmlEscape(var_y), "</em>)."))
            }
          )
        )
      )
    )
  )

  # 2. QUALITE DU MODELE
  r2 <- if (!is.null(smry$r.squared)) smry$r.squared else 0
  r2_adj <- if (!is.null(smry$adj.r.squared)) smry$adj.r.squared else 0
  sigma <- if (!is.null(smry$sigma)) smry$sigma else 0
  resids <- stats::residuals(mod)
  rmse <- sqrt(mean(resids^2, na.rm = TRUE))

  quality_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom",
      "2. Qualit\u00e9 du mod\u00e8le"
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::div(
        class = "row text-center g-2 mb-2",
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(class = "p-2 bg-white rounded border shadow-sm",
            shiny::div(class = "text-muted small", "R\u00b2 (Variance expliqu\u00e9e)"),
            shiny::div(class = "fs-6 fw-bold text-primary", paste0(round(r2 * 100, 1), " %")),
            shiny::div(class = "text-muted extra-small", paste0("R\u00b2 = ", round(r2, 4)))
          )
        ),
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(class = "p-2 bg-white rounded border shadow-sm",
            shiny::div(class = "text-muted small", "R\u00b2 Ajust\u00e9"),
            shiny::div(class = "fs-6 fw-bold text-dark", round(r2_adj, 4)),
            shiny::div(class = "text-muted extra-small", paste0(round(r2_adj * 100, 1), " %"))
          )
        ),
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(class = "p-2 bg-white rounded border shadow-sm",
            shiny::div(class = "text-muted small", "RMSE (Erreur quadratique)"),
            shiny::div(class = "fs-6 fw-bold text-dark", round(rmse, 4)),
            shiny::div(class = "text-muted extra-small", paste0("Unit\u00e9 : ", htmltools::htmlEscape(var_y)))
          )
        ),
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(class = "p-2 bg-white rounded border shadow-sm",
            shiny::div(class = "text-muted small", "Erreur std r\u00e9siduelle (\u03c3)"),
            shiny::div(class = "fs-6 fw-bold text-dark", round(sigma, 4)),
            shiny::div(class = "text-muted extra-small", paste0("ddl r\u00e9siduels = ", if (!is.null(smry$df.residual)) smry$df.residual else "N/A"))
          )
        )
      ),
      shiny::div(
        class = "small text-secondary bg-white p-2 rounded border",
        shiny::tags$ul(
          class = "mb-0 ps-3",
          shiny::tags$li(
            class = "mb-1",
            shiny::HTML(paste0("<strong>R\u00b2 = ", round(r2, 4), " :</strong> Le mod\u00e8le explique environ <strong>", round(r2 * 100, 1), " %</strong> de la variabilit\u00e9 observ\u00e9e de la variable r\u00e9ponse (<em>", htmltools::htmlEscape(var_y), "</em>). Le R\u00b2 mesure la proportion de variance expliqu\u00e9e et constitue un indicateur d'ajustement, sans \u00eatre une preuve automatique d'ad\u00e9quation absolue du mod\u00e8le."))
          ),
          shiny::tags$li(
            class = "mb-1",
            shiny::HTML(paste0("<strong>R\u00b2 ajust\u00e9 = ", round(r2_adj, 4), " :</strong> Le R\u00b2 ajust\u00e9 tient compte du nombre de variables pr\u00e9sentes dans le mod\u00e8le. Il est particuli\u00e8rement utile lorsqu'on compare des mod\u00e8les contenant un nombre diff\u00e9rent de variables."))
          ),
          shiny::tags$li(
            class = "mb-1",
            shiny::HTML(paste0("<strong>RMSE = ", round(rmse, 4), " :</strong> Le RMSE (Root Mean Square Error) donne une indication de l'ampleur typique des erreurs de pr\u00e9diction du mod\u00e8le, exprim\u00e9e dans la m\u00eame unit\u00e9 que la variable r\u00e9ponse (<em>", htmltools::htmlEscape(var_y), "</em>)."))
          ),
          shiny::tags$li(
            shiny::HTML(paste0("<strong>Erreur standard r\u00e9siduelle \u03c3 = ", round(sigma, 4), " :</strong> Estimation de la dispersion des r\u00e9sidus autour de la droite de r\u00e9gression (exprim\u00e9e dans l'unit\u00e9 de <em>", htmltools::htmlEscape(var_y), "</em>)."))
          )
        )
      )
    )
  )

  # 3. COEFFICIENTS ET INTERPRETATION DES EFFETS
  xlevels <- mod$xlevels

  term_rows <- lapply(rownames(coef_table), function(term) {
    row <- coef_table[term, ]
    est <- row[[1]]
    se <- row[[2]]
    t_stat <- row[[3]]
    pval <- row[[4]]
    sig <- !is.na(pval) && pval < alpha_val

    ci_str <- "N/A"
    ci_low <- NA
    ci_high <- NA
    if (!is.null(ci_matrix) && term %in% rownames(ci_matrix)) {
      ci_low <- ci_matrix[term, 1]
      ci_high <- ci_matrix[term, 2]
      if (!is.na(ci_low) && !is.na(ci_high)) {
        ci_str <- paste0("[", round(ci_low, 4), " ; ", round(ci_high, 4), "]")
      }
    }

    clean_label <- term
    interp_txt <- ""
    is_factor_contrast <- FALSE

    if (term == "(Intercept)") {
      clean_label <- "(Intercept / Constante)"
      interp_txt <- paste0("Valeur moyenne estim\u00e9e de '", var_y, "' lorsque toutes les variables explicatives quantitatives sont \u00e0 0 et les facteurs \u00e0 leur modalit\u00e9 de r\u00e9f\u00e9rence.")
    } else {
      matched_fvar <- NULL
      matched_ref <- NULL
      matched_lvl <- NULL

      if (!is.null(xlevels) && length(xlevels) > 0) {
        for (fvar in names(xlevels)) {
          ref_lvl <- xlevels[[fvar]][1]
          other_lvls <- xlevels[[fvar]][-1]
          for (lvl in other_lvls) {
            possible_terms <- c(
              paste0(fvar, lvl),
              paste0("`", fvar, "`", lvl),
              paste0(fvar, "`", lvl, "`")
            )
            if (term %in% possible_terms) {
              matched_fvar <- fvar
              matched_ref <- ref_lvl
              matched_lvl <- lvl
              break
            }
          }
          if (!is.null(matched_fvar)) break
        }
      }

      if (!is.null(matched_fvar)) {
        is_factor_contrast <- TRUE
        clean_label <- paste0(matched_fvar, " : ", matched_lvl, " vs ", matched_ref, " (r\u00e9f\u00e9rence)")
        if (!is.na(est)) {
          if (est > 0) {
            interp_txt <- paste0("Toutes choses \u00e9gales par ailleurs, la valeur moyenne estim\u00e9e de '", var_y, "' pour la modalit\u00e9 '", matched_lvl, "' est sup\u00e9rieure de ", round(abs(est), 4), " \u00e0 celle de la modalit\u00e9 '", matched_ref, "' (modalit\u00e9 de r\u00e9f\u00e9rence).")
          } else if (est < 0) {
            interp_txt <- paste0("Toutes choses \u00e9gales par ailleurs, la valeur moyenne estim\u00e9e de '", var_y, "' pour la modalit\u00e9 '", matched_lvl, "' est inf\u00e9rieure de ", round(abs(est), 4), " \u00e0 celle de la modalit\u00e9 '", matched_ref, "' (modalit\u00e9 de r\u00e9f\u00e9rence).")
          } else {
            interp_txt <- paste0("Toutes choses \u00e9gales par ailleurs, la valeur moyenne estim\u00e9e de '", var_y, "' pour la modalit\u00e9 '", matched_lvl, "' est identique \u00e0 celle de la modalit\u00e9 '", matched_ref, "' (r\u00e9f\u00e9rence).")
          }
        }
      } else {
        clean_label <- term
        if (!is.na(est)) {
          if (est > 0) {
            interp_txt <- paste0("Toutes choses \u00e9gales par ailleurs, une augmentation de 1 unit\u00e9 de '", term, "' est associ\u00e9e \u00e0 une augmentation moyenne estim\u00e9e de ", round(abs(est), 4), " de '", var_y, "'.")
          } else if (est < 0) {
            interp_txt <- paste0("Toutes choses \u00e9gales par ailleurs, une augmentation de 1 unit\u00e9 de '", term, "' est associ\u00e9e \u00e0 une diminution moyenne estim\u00e9e de ", round(abs(est), 4), " de '", var_y, "'.")
          } else {
            interp_txt <- paste0("Toutes choses \u00e9gales par ailleurs, une variation de '", term, "' n'est associ\u00e9e \u00e0 aucune modification moyenne estim\u00e9e de '", var_y, "'.")
          }
        }
      }
    }

    list(
      raw_term = term,
      label = clean_label,
      est = est,
      se = se,
      ci_str = ci_str,
      ci_low = ci_low,
      ci_high = ci_high,
      t_stat = t_stat,
      pval = pval,
      sig = sig,
      interp = interp_txt,
      is_factor = is_factor_contrast
    )
  })

  table_rows_ui <- lapply(term_rows, function(info) {
    shiny::tags$tr(
      shiny::tags$td(class = "text-start fw-medium", info$label),
      shiny::tags$td(if (!is.na(info$est)) round(info$est, 4) else "N/A"),
      shiny::tags$td(if (!is.na(info$se)) round(info$se, 4) else "N/A"),
      shiny::tags$td(class = "font-monospace small", info$ci_str),
      shiny::tags$td(if (!is.na(info$t_stat)) round(info$t_stat, 3) else "N/A"),
      shiny::tags$td(class = "fw-bold", if (!is.na(info$pval)) format.pval(info$pval, digits = 4, eps = 0.0001) else "N/A"),
      shiny::tags$td(
        shiny::tags$span(
          class = paste0("badge ", if (info$sig) "bg-success" else "bg-secondary"),
          if (info$sig) paste0("p < ", alpha_val) else "NS"
        )
      )
    )
  })

  interp_items_ui <- lapply(term_rows[vapply(term_rows, function(x) x$raw_term != "(Intercept)", logical(1))], function(info) {
    shiny::tags$li(
      class = "mb-2",
      shiny::tags$div(
        class = "fw-semibold text-dark",
        paste0(info$label, " (\u03b2 = ", if (!is.na(info$est)) round(info$est, 4) else "N/A", ", IC95% ", info$ci_str, ", p = ", if (!is.na(info$pval)) format.pval(info$pval, digits = 4, eps = 0.0001) else "N/A", ")")
      ),
      shiny::tags$div(class = "text-muted small ps-2", info$interp)
    )
  })

  coefs_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom",
      "3. Coefficients du mod\u00e8le et interpr\u00e9tation des effets"
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::tags$div(
        class = "table-responsive mb-3",
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle bg-white mb-0",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Terme"),
              shiny::tags$th("Coefficient (\u03b2)"),
              shiny::tags$th("Erreur-type (SE)"),
              shiny::tags$th("IC 95 %"),
              shiny::tags$th("t value"),
              shiny::tags$th("p-value"),
              shiny::tags$th("Significativit\u00e9")
            )
          ),
          shiny::tags$tbody(table_rows_ui)
        )
      ),
      shiny::div(
        class = "alert alert-info py-2 px-3 small mb-3",
        shiny::HTML("<strong>Note p\u00e9dagogique sur les IC95% :</strong> L'intervalle de confiance \u00e0 95 % donne une plage de valeurs compatibles avec les donn\u00e9es et le mod\u00e8le sous les hypoth\u00e8ses utilis\u00e9es. La p-value seule ne r\u00e9sume pas l'effet : \u00e9valuez conjointement la direction du coefficient, son ampleur, son incertitude (IC95 %) et sa significativit\u00e9.")
      ),
      shiny::tags$h6(class = "fw-bold text-dark mb-2 small", "D\u00e9tail de l'interpr\u00e9tation coefficient par coefficient :"),
      shiny::tags$ul(class = "ps-3 mb-0", interp_items_ui)
    )
  )

  # 4. DIAGNOSTICS DE VALIDITE DU MODELE
  n_obs <- length(resids)
  shapiro_res <- if (n_obs >= 3 && n_obs <= 5000) tryCatch(stats::shapiro.test(resids), error = function(e) NULL) else NULL
  p_pred <- length(vars_x)
  ratio_np <- n_obs / max(1, p_pred)

  diag_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("4. V\u00e9rification du mod\u00e8le (Diagnostics)"),
      shiny::tags$span(class = "badge bg-info text-dark", "Diagnostic")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::tags$ul(
        class = "mb-0 ps-3 small text-dark",
        shiny::tags$li(
          class = "mb-1",
          shiny::tags$strong("Nombre d'observations par pr\u00e9dicteur : "),
          paste0("n = ", n_obs, " observations pour p = ", p_pred, " pr\u00e9dicteur(s) (ratio n/p = ", round(ratio_np, 1), "). "),
          if (ratio_np >= 15) {
            shiny::tags$span(class = "text-success fw-semibold", "\u2714 Taille d'\u00e9chantillon suffisante (\u2265 15 obs/pr\u00e9dicteur).")
          } else {
            shiny::tags$span(class = "text-warning fw-semibold", "\u26a0 Ratio faible (< 15 obs/pr\u00e9dicteur) : risque d'instabilit\u00e9 des estimations.")
          }
        ),
        shiny::tags$li(
          class = "mb-1",
          shiny::tags$strong("Normalit\u00e9 des r\u00e9sidus (Shapiro-Wilk) : "),
          if (!is.null(shapiro_res)) {
            if (shapiro_res$p.value >= 0.05 || n_obs >= 30) {
              shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Hypoth\u00e8se de normalit\u00e9 accept\u00e9e (W = ", round(shapiro_res$statistic, 4), ", p = ", round(shapiro_res$p.value, 4), if (n_obs >= 30) ", n \u2265 30 robuste" else "", ")."))
            } else {
              shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 \u00c9cart \u00e0 la normalit\u00e9 des r\u00e9sidus (W = ", round(shapiro_res$statistic, 4), ", p = ", round(shapiro_res$p.value, 4), " < 0.05). Interpr\u00e9ter les intervalles de confiance avec prudence."))
            }
          } else {
            "Non calcul\u00e9 (n < 3 ou n > 5000)."
          }
        ),
        shiny::tags$li(
          shiny::tags$strong("Lin\u00e9arit\u00e9 & Homosc\u00e9dasticit\u00e9 : "),
          "A consulter dans l'onglet 'Diagnostic des r\u00e9sidus' (graphique r\u00e9sidus vs valeurs ajust\u00e9es et Normal Q-Q plot)."
        )
      )
    )
  )

  # 5. CONCLUSION SYNTHETIQUE AUTOMATIQUE
  sig_terms <- term_rows[vapply(term_rows, function(x) x$sig && x$raw_term != "(Intercept)", logical(1))]

  conclusion_parts <- c()
  if (is_f_sig) {
    conclusion_parts <- c(
      conclusion_parts,
      paste0("Le mod\u00e8le de r\u00e9gression lin\u00e9aire est statistiquement significatif (F(", df1, ", ", df2, ") = ", round(f_val, 2), " ; p = ", format.pval(f_pval, digits = 4, eps = 0.0001), ").")
    )
  } else {
    conclusion_parts <- c(
      conclusion_parts,
      "Le mod\u00e8le de r\u00e9gression lin\u00e9aire n'est pas statistiquement significatif au seuil de 5 %."
    )
  }

  conclusion_parts <- c(
    conclusion_parts,
    paste0("Il explique environ ", round(r2 * 100, 1), " % de la variabilit\u00e9 observ\u00e9e de la variable r\u00e9ponse '", var_y, "' (R\u00b2 = ", round(r2, 3), ", R\u00b2 ajust\u00e9 = ", round(r2_adj, 3), ").")
  )

  if (length(sig_terms) > 0) {
    sig_desc <- vapply(sig_terms, function(x) {
      if (x$is_factor) {
        paste0("la modalit\u00e9 '", x$label, "'")
      } else {
        if (!is.na(x$est) && x$est > 0) paste0("la variable '", x$raw_term, "' (associ\u00e9e positivement)") else paste0("la variable '", x$raw_term, "' (associ\u00e9e n\u00e9gativement)")
      }
    }, character(1))
    conclusion_parts <- c(
      conclusion_parts,
      paste0("Toutes choses \u00e9gales par ailleurs, les variables/modalit\u00e9s pr\u00e9sentant une association statistiquement significative avec '", var_y, "' sont : ", paste(sig_desc, collapse = ", "), ".")
    )
  } else {
    conclusion_parts <- c(
      conclusion_parts,
      "Aucun coefficient individuel ne s'av\u00e8re statistiquement significatif."
    )
  }

  conclusion_parts <- c(
    conclusion_parts,
    "Rappel : Les associations observ\u00e9es traduisent des relations statistiques et ne doivent pas \u00eatre automatiquement interpr\u00e9t\u00e9es comme une relation de causalit\u00e9 directe."
  )

  conclusion_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("5. Conclusion et synth\u00e8se"),
      shiny::tags$span(class = "badge bg-primary", "Synth\u00e8se")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::tags$p(class = "mb-0 text-dark leading-relaxed", paste(conclusion_parts, collapse = " "))
    )
  )

  shiny::tagList(
    global_test_ui,
    quality_ui,
    coefs_ui,
    diag_ui,
    conclusion_ui,
    shiny::tags$details(
      shiny::tags$summary(class = "text-muted small cursor-pointer my-2", "Afficher le r\u00e9sum\u00e9 R brut (summary.lm)"),
      shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small font-monospace", paste(utils::capture.output(print(smry)), collapse = "\n"))
    )
  )
}

#' Restitution p\u00e9dagogique structur\u00e9e en 7 \u00e9tapes pour la R\u00e9gression Logistique Binaire
#'
#' @param reg_state Liste ou reactiveValues contenant le mod\u00e8le glm, les variables et les effectifs.
#' @param alpha Seuil de significativit\u00e9 (0.05 par d\u00e9faut).
#' @return Structure HTML Shiny compl\u00e8te pr\u00eate \u00e0 l'affichage.
#' @noRd
ramses_render_logistic_results <- function(reg_state, alpha = 0.05) {
  mod <- reg_state$model
  if (is.null(mod) || !inherits(mod, "glm")) {
    return(shiny::p(class = "text-danger small", "Mod\u00e8le de r\u00e9gression logistique invalide ou indisponible."))
  }

  var_y <- reg_state$var_y
  vars_x <- reg_state$vars_x
  alpha_val <- as.numeric(if (!is.null(reg_state$alpha)) reg_state$alpha else alpha)
  if (is.na(alpha_val) || alpha_val <= 0 || alpha_val >= 1) alpha_val <- 0.05

  smry <- summary(mod)
  coef_table <- as.data.frame(smry$coefficients)

  # Effectifs
  n_init <- if (!is.null(reg_state$n_initial)) reg_state$n_initial else stats::nobs(mod)
  n_used <- stats::nobs(mod)
  n_excl <- max(0, n_init - n_used)
  evt_name <- if (!is.null(reg_state$event_label)) reg_state$event_label else "1"
  ref_name <- if (!is.null(reg_state$ref_label)) reg_state$ref_label else "0"

  # Intervalles de confiance a 95% des coefficients par confint.default (loi normale asymptotique)
  ci_matrix <- tryCatch(stats::confint.default(mod, level = 0.95), error = function(e) NULL)

  # =========================================================================
  # 1. TEST GLOBAL DU MODELE (Likelihood Ratio Test - LRT)
  # =========================================================================
  # Comparaison modele nul vs modele complet : Chi2 = Null Deviance - Residual Deviance
  dev_null <- mod$null.deviance
  dev_resid <- mod$deviance
  df_null <- mod$df.null
  df_resid <- mod$df.residual
  lrt_stat <- dev_null - dev_resid
  lrt_df <- df_null - df_resid
  lrt_pval <- if (!is.na(lrt_stat) && !is.na(lrt_df) && lrt_df > 0) stats::pchisq(lrt_stat, df = lrt_df, lower.tail = FALSE) else NA
  is_lrt_sig <- !is.na(lrt_pval) && lrt_pval < alpha_val

  global_test_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("1. Test global du mod\u00e8le (Rapport de Vraisemblance / LRT)"),
      shiny::tags$span(
        class = paste0("badge ", if (is_lrt_sig) "bg-success" else "bg-secondary"),
        if (is_lrt_sig) "Mod\u00e8le significatif" else "Mod\u00e8le non significatif"
      )
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::div(
        class = "d-flex align-items-center gap-3 mb-2 flex-wrap",
        shiny::div(
          class = "badge text-dark border p-2",
          style = "background-color: #F3F4F6; border-color: #D1D5DB !important;",
          shiny::tags$span(class = "fw-normal text-muted", "Effectif : "),
          shiny::tags$strong(paste0("N initial = ", n_init, " | N utilis\u00e9 = ", n_used, if (n_excl > 0) paste0(" (", n_excl, " exclu(s) NA)") else ""))
        ),
        shiny::div(
          class = "badge text-dark border p-2",
          style = "background-color: #F3F4F6; border-color: #D1D5DB !important;",
          shiny::tags$span(class = "fw-normal text-muted", "Statistique Chi-deux : "),
          shiny::tags$strong(paste0("\u03c7\u00b2(", lrt_df, ") = ", if (!is.na(lrt_stat)) round(lrt_stat, 2) else "N/A"))
        ),
        shiny::div(
          class = "badge text-dark border p-2",
          style = "background-color: #F3F4F6; border-color: #D1D5DB !important;",
          shiny::tags$span(class = "fw-normal text-muted", "p-value globale : "),
          shiny::tags$strong(if (!is.na(lrt_pval)) format.pval(lrt_pval, digits = 4, eps = 0.0001) else "N/A")
        )
      ),
      shiny::div(
        class = "small text-dark",
        if (is_lrt_sig) {
          shiny::tags$p(class = "mb-1 text-success fw-semibold",
            "\u2714 Le mod\u00e8le complet apporte une am\u00e9lioration statistiquement significative par rapport au mod\u00e8le nul."
          )
        } else {
          shiny::tags$p(class = "mb-1 text-secondary fw-semibold",
            "\u26a0 Les donn\u00e9es ne fournissent pas suffisamment d'\u00e9l\u00e9ments pour conclure que le mod\u00e8le complet am\u00e9liore significativement le mod\u00e8le nul."
          )
        },
        shiny::tags$p(class = "mb-0 text-muted extra-small",
          "Hypoth\u00e8ses : H\u2080 : \u03b2\u2081 = ... = \u03b2_p = 0 (tous les Odds Ratios valent 1) vs H\u2081 : au moins un coefficient est non nul. Ce test du rapport de vraisemblance \u00e9value l'apport combin\u00e9 de l'ensemble des pr\u00e9dicteurs."
        )
      )
    )
  )

  # =========================================================================
  # 2. QUALITE DU MODELE (Deviance, AIC, Pseudo-R2 de McFadden)
  # =========================================================================
  mcfadden_r2 <- if (!is.na(dev_null) && dev_null > 0) 1 - (dev_resid / dev_null) else NA
  aic_val <- smry$aic

  quality_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("2. Qualit\u00e9 du mod\u00e8le et ajustement"),
      shiny::tags$span(class = "badge bg-secondary text-white", "Ajustement")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::div(
        class = "row g-2 text-center mb-2",
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(
            class = "p-2 rounded bg-white border",
            shiny::div(class = "text-muted extra-small fw-semibold", "PSEUDO-R\u00b2 (MCFADDEN)"),
            shiny::div(class = "fs-6 fw-bold text-dark", if (!is.na(mcfadden_r2)) round(mcfadden_r2, 4) else "N/A"),
            shiny::div(class = "text-muted extra-small", "Ajustement relatif")
          )
        ),
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(
            class = "p-2 rounded bg-white border",
            shiny::div(class = "text-muted extra-small fw-semibold", "AIC"),
            shiny::div(class = "fs-6 fw-bold text-dark", if (!is.null(aic_val)) round(aic_val, 1) else "N/A"),
            shiny::div(class = "text-muted extra-small", "Crit\u00e8re d'information")
          )
        ),
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(
            class = "p-2 rounded bg-white border",
            shiny::div(class = "text-muted extra-small fw-semibold", "D\u00c9VIANCE R\u00c9SIDUELLE"),
            shiny::div(class = "fs-6 fw-bold text-dark", round(dev_resid, 2)),
            shiny::div(class = "text-muted extra-small", paste0("sur ", df_resid, " ddl"))
          )
        ),
        shiny::div(
          class = "col-6 col-md-3",
          shiny::div(
            class = "p-2 rounded bg-white border",
            shiny::div(class = "text-muted extra-small fw-semibold", "D\u00c9VIANCE NULLE"),
            shiny::div(class = "fs-6 fw-bold text-dark", round(dev_null, 2)),
            shiny::div(class = "text-muted extra-small", paste0("sur ", df_null, " ddl"))
          )
        )
      ),
      shiny::div(
        class = "small text-secondary bg-white p-2 rounded border",
        shiny::tags$ul(
          class = "mb-0 ps-3",
          shiny::tags$li(
            class = "mb-1",
            shiny::HTML(paste0("<strong>Pseudo-R\u00b2 de McFadden = ", round(mcfadden_r2, 4), " :</strong> Le pseudo-R\u00b2 compare la log-vraisemblance du mod\u00e8le complet \u00e0 celle du mod\u00e8le nul. Il s'agit d'un <em>indicateur descriptif d'ajustement</em>, et non de la proportion exacte de variance expliqu\u00e9e au sens du R\u00b2 lin\u00e9aire. Ne pas appliquer de seuils rigides pour le qualifier."))
          ),
          shiny::tags$li(
            shiny::HTML(paste0("<strong>AIC = ", round(aic_val, 1), " & D\u00e9viance = ", round(dev_resid, 2), " :</strong> Mesures de qualit\u00e9 d'ajustement p\u00e9nalis\u00e9es par la complexit\u00e9 du mod\u00e8le. Des valeurs plus faibles indiquent un meilleur compromis parcimonie/ajustement lors de comparaisons de mod\u00e8les."))
          )
        )
      )
    )
  )

  # =========================================================================
  # 3 & 4. TABLEAU DES COEFFICIENTS (BETA) ET ODDS RATIOS (OR)
  # =========================================================================
  xlevels <- mod$xlevels

  term_rows <- lapply(rownames(coef_table), function(term) {
    row <- coef_table[term, ]
    est <- row[[1]]
    se <- row[[2]]
    z_stat <- row[[3]]
    pval <- row[[4]]
    sig <- !is.na(pval) && pval < alpha_val

    # IC 95% pour Beta
    ci_str_beta <- "N/A"
    ci_low_beta <- NA
    ci_high_beta <- NA
    if (!is.null(ci_matrix) && term %in% rownames(ci_matrix)) {
      ci_low_beta <- ci_matrix[term, 1]
      ci_high_beta <- ci_matrix[term, 2]
      if (!is.na(ci_low_beta) && !is.na(ci_high_beta)) {
        ci_str_beta <- paste0("[", round(ci_low_beta, 4), " ; ", round(ci_high_beta, 4), "]")
      }
    }

    # Odds Ratio et IC95% OR
    or_val <- if (!is.na(est)) exp(est) else NA
    ci_str_or <- "N/A"
    or_low <- NA
    or_high <- NA
    if (!is.na(ci_low_beta) && !is.na(ci_high_beta)) {
      or_low <- exp(ci_low_beta)
      or_high <- exp(ci_high_beta)
      ci_str_or <- paste0("[", round(or_low, 3), " ; ", round(or_high, 3), "]")
    }

    clean_label <- term
    interp_or_txt <- ""
    is_factor_contrast <- FALSE

    if (term == "(Intercept)") {
      clean_label <- "(Intercept / Constante)"
      interp_or_txt <- paste0("Cote (odds) de base de l'\u00e9v\u00e9nement ('", evt_name, "') lorsque tous les pr\u00e9dicteurs continus sont \u00e0 z\u00e9ro et les pr\u00e9dicteurs qualitatifs \u00e0 leur modalit\u00e9 de r\u00e9f\u00e9rence.")
    } else {
      matched_fvar <- NULL
      matched_ref <- NULL
      matched_lvl <- NULL

      if (!is.null(xlevels) && length(xlevels) > 0) {
        for (fvar in names(xlevels)) {
          ref_lvl <- xlevels[[fvar]][1]
          other_lvls <- xlevels[[fvar]][-1]
          for (lvl in other_lvls) {
            possible_terms <- c(
              paste0(fvar, lvl),
              paste0("`", fvar, "`", lvl),
              paste0(fvar, "`", lvl, "`")
            )
            if (term %in% possible_terms) {
              matched_fvar <- fvar
              matched_ref <- ref_lvl
              matched_lvl <- lvl
              break
            }
          }
          if (!is.null(matched_fvar)) break
        }
      }

      if (!is.null(matched_fvar)) {
        is_factor_contrast <- TRUE
        clean_label <- paste0(matched_fvar, " : ", matched_lvl, " vs ", matched_ref, " (r\u00e9f\u00e9rence)")
        if (!is.na(or_val)) {
          if (round(or_val, 3) == 1) {
            interp_or_txt <- paste0("Par rapport \u00e0 la modalit\u00e9 '", matched_ref, "' (r\u00e9f\u00e9rence), les odds de l'\u00e9v\u00e9nement ne sont pas modifi\u00e9es selon l'estimation du mod\u00e8le (OR = 1.000).")
          } else if (or_val > 1) {
            pct_var <- round((or_val - 1) * 100, 1)
            interp_or_txt <- paste0("Toutes choses \u00e9gales par ailleurs, par rapport \u00e0 la modalit\u00e9 '", matched_ref, "' (r\u00e9f\u00e9rence), les odds de l'\u00e9v\u00e9nement ('", evt_name, "') sont multipli\u00e9es par ", round(or_val, 3), " (soit une augmentation d'environ +", pct_var, " % des odds).")
          } else {
            pct_var <- round((1 - or_val) * 100, 1)
            interp_or_txt <- paste0("Toutes choses \u00e9gales par ailleurs, par rapport \u00e0 la modalit\u00e9 '", matched_ref, "' (r\u00e9f\u00e9rence), les odds de l'\u00e9v\u00e9nement ('", evt_name, "') sont multipli\u00e9es par ", round(or_val, 3), " (soit une diminution d'environ -", pct_var, " % des odds).")
          }
        }
      } else {
        clean_label <- term
        if (!is.na(or_val)) {
          if (round(or_val, 3) == 1) {
            interp_or_txt <- paste0("Une augmentation d'une unit\u00e9 de '", term, "' n'est associ\u00e9e \u00e0 aucune modification des odds selon l'estimation du mod\u00e8le.")
          } else if (or_val > 1) {
            pct_var <- round((or_val - 1) * 100, 1)
            interp_or_txt <- paste0("Toutes choses \u00e9gales par ailleurs, une augmentation d'une unit\u00e9 de '", term, "' est associ\u00e9e \u00e0 une multiplication des odds de l'\u00e9v\u00e9nement ('", evt_name, "') par ", round(or_val, 3), " (soit environ +", pct_var, " % des odds).")
          } else {
            pct_var <- round((1 - or_val) * 100, 1)
            interp_or_txt <- paste0("Toutes choses \u00e9gales par ailleurs, une augmentation d'une unit\u00e9 de '", term, "' est associ\u00e9e \u00e0 une multiplication des odds de l'\u00e9v\u00e9nement ('", evt_name, "') par ", round(or_val, 3), " (soit environ -", pct_var, " % des odds).")
          }
        }
      }
    }

    list(
      raw_term = term,
      label = clean_label,
      est = est,
      se = se,
      ci_str_beta = ci_str_beta,
      z_stat = z_stat,
      pval = pval,
      sig = sig,
      or_val = or_val,
      ci_str_or = ci_str_or,
      interp = interp_or_txt,
      is_factor = is_factor_contrast
    )
  })

  # Tableau 3 : Coefficients Beta
  table_beta_rows_ui <- lapply(term_rows, function(info) {
    shiny::tags$tr(
      shiny::tags$td(class = "text-start fw-medium", info$label),
      shiny::tags$td(if (!is.na(info$est)) round(info$est, 4) else "N/A"),
      shiny::tags$td(if (!is.na(info$se)) round(info$se, 4) else "N/A"),
      shiny::tags$td(if (!is.na(info$z_stat)) round(info$z_stat, 3) else "N/A"),
      shiny::tags$td(class = "fw-bold", if (!is.na(info$pval)) format.pval(info$pval, digits = 4, eps = 0.0001) else "N/A"),
      shiny::tags$td(class = "font-monospace small", info$ci_str_beta),
      shiny::tags$td(
        shiny::tags$span(
          class = paste0("badge ", if (info$sig) "bg-success" else "bg-secondary"),
          if (info$sig) paste0("p < ", alpha_val) else "NS"
        )
      )
    )
  })

  coefs_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom",
      "3. Coefficients du mod\u00e8le (\u00e9chelle logit)"
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::tags$div(
        class = "table-responsive mb-1",
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle bg-white mb-0",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Terme"),
              shiny::tags$th("Estimation (\u03b2)"),
              shiny::tags$th("Erreur-type (SE)"),
              shiny::tags$th("z value"),
              shiny::tags$th("p-value"),
              shiny::tags$th("IC 95 % [\u03b2]"),
              shiny::tags$th("Significativit\u00e9")
            )
          ),
          shiny::tags$tbody(table_beta_rows_ui)
        )
      )
    )
  )

  # Tableau 4 : Odds Ratios et effets
  non_intercept_terms <- term_rows[vapply(term_rows, function(x) x$raw_term != "(Intercept)", logical(1))]
  
  table_or_rows_ui <- lapply(non_intercept_terms, function(info) {
    shiny::tags$tr(
      shiny::tags$td(class = "text-start fw-medium", info$label),
      shiny::tags$td(class = "fw-bold text-dark", if (!is.na(info$or_val)) round(info$or_val, 3) else "N/A"),
      shiny::tags$td(class = "font-monospace small", info$ci_str_or),
      shiny::tags$td(class = "text-start small text-muted", info$interp)
    )
  })

  or_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("4. Odds Ratios (OR) et analyse des effets"),
      shiny::tags$span(class = "badge bg-dark", "Rapport de cotes")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::div(
        class = "alert alert-info py-2 px-3 small mb-2",
        shiny::HTML("<strong>Distinction fondamentale :</strong> L'Odds Ratio (OR = e^\u03b2) d\u00e9crit une variation relative des <strong>odds (cotes)</strong>, et <em>non directement une variation en pourcentage de la probabilit\u00e9 ou du risque</em>. Un OR de 1,20 signifie +20 % sur les odds de survenue de l'\u00e9v\u00e9nement, pas +20 % de probabilit\u00e9 brute.")
      ),
      shiny::tags$div(
        class = "table-responsive mb-1",
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle bg-white mb-0",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Pr\u00e9dicteur"),
              shiny::tags$th("Odds Ratio (OR)"),
              shiny::tags$th("IC 95 % OR"),
              shiny::tags$th(class = "text-start", "Interpr\u00e9tation de l'effet")
            )
          ),
          shiny::tags$tbody(table_or_rows_ui)
        )
      )
    )
  )

  # =========================================================================
  # 5. PERFORMANCE PREDICTIVE (Seuil 0.50, Confusion Matrix, Accuracy, Sens/Spec)
  # =========================================================================
  fitted_probs <- stats::predict(mod, type = "response")
  y_obs <- mod$y # 0 ou 1
  y_pred_class <- ifelse(fitted_probs >= 0.5, 1, 0)

  tp <- sum(y_obs == 1 & y_pred_class == 1) # Vrais positifs
  tn <- sum(y_obs == 0 & y_pred_class == 0) # Vrais negatifs
  fp <- sum(y_obs == 0 & y_pred_class == 1) # Faux positifs
  fn <- sum(y_obs == 1 & y_pred_class == 0) # Faux negatifs

  n_total <- length(y_obs)
  accuracy <- if (n_total > 0) (tp + tn) / n_total else NA
  sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA # Taux de vrais positifs
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA # Taux de vrais negatifs

  predictive_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("5. Performance pr\u00e9dictive & Matrice de confusion (seuil 0,50)"),
      shiny::tags$span(class = "badge bg-secondary text-white", "Classification")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::div(
        class = "row g-3 align-items-center",
        shiny::div(
          class = "col-md-6",
          shiny::tags$div(class = "text-muted extra-small mb-1 fw-semibold", "MATRICE DE CONFUSION (Observations vs Pr\u00e9dictions) :"),
          shiny::tags$table(
            class = "table table-sm table-bordered text-center align-middle bg-white mb-0 small",
            shiny::tags$thead(
              class = "table-light",
              shiny::tags$tr(
                shiny::tags$th(rowspan = 2, class = "align-middle", "Observ\u00e9"),
                shiny::tags$th(colspan = 2, "Pr\u00e9dit (seuil \u03c4 = 0,50)")
              ),
              shiny::tags$tr(
                shiny::tags$th(paste0(ref_name, " (0)")),
                shiny::tags$th(paste0(evt_name, " (1)"))
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "fw-bold", paste0(ref_name, " (0)")),
                shiny::tags$td(class = "bg-light fw-bold", tn),
                shiny::tags$td(fp)
              ),
              shiny::tags$tr(
                shiny::tags$td(class = "fw-bold", paste0(evt_name, " (1)")),
                shiny::tags$td(fn),
                shiny::tags$td(class = "bg-light fw-bold text-success", tp)
              )
            )
          )
        ),
        shiny::div(
          class = "col-md-6",
          shiny::div(
            class = "d-flex flex-column gap-2",
            shiny::div(
              class = "p-2 rounded bg-white border d-flex justify-content-between align-items-center",
              shiny::tags$span(class = "small text-dark", "Exactitude globale (Accuracy) :"),
              shiny::tags$span(class = "fw-bold text-dark fs-6", if (!is.na(accuracy)) paste0(round(accuracy * 100, 1), " %") else "N/A")
            ),
            shiny::div(
              class = "p-2 rounded bg-white border d-flex justify-content-between align-items-center",
              shiny::tags$span(class = "small text-dark", paste0("Sensibilit\u00e9 (taux de d\u00e9tection de '", evt_name, "') :")),
              shiny::tags$span(class = "fw-bold text-dark fs-6", if (!is.na(sensitivity)) paste0(round(sensitivity * 100, 1), " %") else "N/A")
            ),
            shiny::div(
              class = "p-2 rounded bg-white border d-flex justify-content-between align-items-center",
              shiny::tags$span(class = "small text-dark", paste0("Sp\u00e9cificit\u00e9 (taux de d\u00e9tection de '", ref_name, "') :")),
              shiny::tags$span(class = "fw-bold text-dark fs-6", if (!is.na(specificity)) paste0(round(specificity * 100, 1), " %") else "N/A")
            )
          )
        )
      ),
      shiny::div(
        class = "mt-2 p-2 bg-white rounded border small text-muted",
        shiny::tags$span("Note : Ces indicateurs \u00e9valuent la capacit\u00e9 de classification du mod\u00e8le sur l'\u00e9chantillon d'apprentissage. Ils r\u00e9pondent \u00e0 une question diff\u00e9rente de celle du test global de significativit\u00e9 statistique.")
      )
    )
  )

  # =========================================================================
  # 6. DIAGNOSTICS DU MODELE (EPV, Separation, Convergence)
  # =========================================================================
  n_events <- sum(y_obs == 1)
  n_nonevents <- sum(y_obs == 0)
  min_event <- min(n_events, n_nonevents)
  p_pred <- length(vars_x)
  epv <- if (p_pred > 0) min_event / p_pred else 0

  # Detection de separation complete ou quasi-complete (SE anormalement elevee > 20 ou coefs aberrants)
  large_se_detected <- any(coef_table[, 2] > 20, na.rm = TRUE)
  not_converged <- !isTRUE(mod$converged)

  diag_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("6. V\u00e9rification du mod\u00e8le (Diagnostics adapt\u00e9s)"),
      shiny::tags$span(class = "badge bg-info text-dark", "Diagnostic")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::tags$ul(
        class = "mb-0 ps-3 small text-dark",
        shiny::tags$li(
          class = "mb-1",
          shiny::tags$strong("\u00c9v\u00e9nements par variable (R\u00e8gle EPV de prudence) : "),
          paste0("\u00c9v\u00e9nements minoritaires = ", min_event, " pour ", p_pred, " pr\u00e9dicteur(s) (EPV = ", round(epv, 1), "). "),
          if (epv >= 10) {
            shiny::tags$span(class = "text-success fw-semibold", "\u2714 Ratio satisfaisant (EPV \u2265 10). Stabilit\u00e9 num\u00e9rique des estimations satisfaisante.")
          } else {
            shiny::tags$span(class = "text-warning fw-semibold", "\u26a0 Attention : EPV < 10. Il s'agit d'une r\u00e8gle empirique de prudence sur la stabilit\u00e9 des estimations et le risque de sur-ajustement (ce n'est pas un test de puissance statistique).")
          }
        ),
        shiny::tags$li(
          class = "mb-1",
          shiny::tags$strong("Contr\u00f4le de la s\u00e9paration compl\u00e8te / quasi-compl\u00e8te : "),
          if (large_se_detected) {
            shiny::tags$span(class = "text-danger fw-semibold", "\u26a0 Attention : D\u00e9tection d'erreurs-types (SE) tr\u00e8s \u00e9lev\u00e9es (> 20). Les donn\u00e9es peuvent pr\u00e9senter un probl\u00e8me de s\u00e9paration (quasi-)compl\u00e8te entre les cat\u00e9gories. Les coefficients et odds ratios peuvent \u00eatre instables et doivent \u00eatre interpr\u00e9t\u00e9s avec grande prudence.")
          } else {
            shiny::tags$span(class = "text-success fw-semibold", "\u2714 Aucune s\u00e9paration quasi-compl\u00e8te \u00e9vidente d\u00e9tect\u00e9e (erreurs-types r\u00e9guli\u00e8res).")
          }
        ),
        shiny::tags$li(
          class = "mb-1",
          shiny::tags$strong("Convergence de l'algorithme : "),
          if (not_converged) {
            shiny::tags$span(class = "text-danger fw-semibold", "\u26a0 L'algorithme d'estimation n'a pas converg\u00e9 !")
          } else {
            shiny::tags$span(class = "text-success fw-semibold", "\u2714 Convergence atteinte avec succ\u00e8s.")
          }
        ),
        shiny::tags$li(
          shiny::tags$strong("R\u00e9sidus de d\u00e9viance et levier : "),
          "A consulter dans l'onglet 'Diagnostics adapt\u00e9s' (graphique des r\u00e9sidus de d\u00e9viance et distances de Cook pour d\u00e9tecter les observations influentes)."
        )
      )
    )
  )

  # =========================================================================
  # 7. CONCLUSION ET SYNTHESE AUTOMATIQUE
  # =========================================================================
  sig_or_terms <- non_intercept_terms[vapply(non_intercept_terms, function(x) x$sig, logical(1))]

  conclusion_parts <- c()
  if (is_lrt_sig) {
    conclusion_parts <- c(
      conclusion_parts,
      paste0("Le mod\u00e8le de r\u00e9gression logistique est globalement significatif par rapport au mod\u00e8le nul (\u03c7\u00b2(", lrt_df, ") = ", round(lrt_stat, 2), " ; p = ", format.pval(lrt_pval, digits = 4, eps = 0.0001), ").")
    )
  } else {
    conclusion_parts <- c(
      conclusion_parts,
      "Le mod\u00e8le de r\u00e9gression logistique n'apporte pas d'am\u00e9lioration statistiquement significative au seuil de 5 % par rapport au mod\u00e8le nul."
    )
  }

  conclusion_parts <- c(
    conclusion_parts,
    paste0("Le pseudo-R\u00b2 de McFadden est estim\u00e9 \u00e0 ", round(mcfadden_r2, 3), " (AIC = ", round(aic_val, 1), ") avec une exactitude pr\u00e9dictive de ", if (!is.na(accuracy)) paste0(round(accuracy * 100, 1), " %") else "N/A", " au seuil standard de 0,50.")
  )

  if (length(sig_or_terms) > 0) {
    sig_desc <- vapply(sig_or_terms, function(x) {
      if (x$is_factor) {
        paste0("la modalit\u00e9 '", x$label, "' (OR = ", round(x$or_val, 2), ")")
      } else {
        paste0("la variable '", x$raw_term, "' (OR = ", round(x$or_val, 2), ")")
      }
    }, character(1))
    conclusion_parts <- c(
      conclusion_parts,
      paste0("Toutes choses \u00e9gales par ailleurs, les facteurs pr\u00e9sentant une association statistiquement significative avec l'\u00e9v\u00e9nement '", evt_name, "' sont : ", paste(sig_desc, collapse = ", "), ".")
    )
  } else {
    conclusion_parts <- c(
      conclusion_parts,
      "Aucun coefficient individuel ne s'av\u00e8re statistiquement significatif au seuil retenu."
    )
  }

  conclusion_parts <- c(
    conclusion_parts,
    "Rappel m\u00e9thodologique : Les Odds Ratios mesurent des variations de cotes statistiques et ne doivent pas \u00eatre confondus avec des variations directes de probabilit\u00e9s ni interpr\u00e9t\u00e9s comme des relations causales sans justification exp\u00e9rimentale."
  )

  conclusion_ui <- shiny::div(
    class = "card mb-3 shadow-sm border-0 bg-light",
    shiny::div(
      class = "card-header bg-white fw-bold py-2 border-bottom d-flex justify-content-between align-items-center",
      shiny::tags$span("7. Conclusion et synth\u00e8se"),
      shiny::tags$span(class = "badge bg-primary", "Synth\u00e8se")
    ),
    shiny::div(
      class = "card-body py-2 px-3",
      shiny::tags$p(class = "mb-0 text-dark leading-relaxed", paste(conclusion_parts, collapse = " "))
    )
  )

  shiny::tagList(
    global_test_ui,
    quality_ui,
    coefs_ui,
    or_ui,
    predictive_ui,
    diag_ui,
    conclusion_ui,
    shiny::tags$details(
      shiny::tags$summary(class = "text-muted small cursor-pointer my-2", "Afficher le r\u00e9sum\u00e9 R brut (summary.glm)"),
      shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small font-monospace", paste(utils::capture.output(print(smry)), collapse = "\n"))
    )
  )
}



