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
#' @return Un objet de classe \code{formula} interpretable par R.
#' @noRd
ramses_formula <- function(response = NULL, terms, env = parent.frame()) {
  if (is.null(terms) || length(terms) == 0) {
    stop("terms doit etre un vecteur de noms de colonnes non vide.")
  }

  rhs_expr <- if (length(terms) == 1) {
    rlang::sym(terms[[1]])
  } else {
    Reduce(function(acc, x) rlang::call2("+", acc, rlang::sym(x)), terms[-1], init = rlang::sym(terms[[1]]))
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
#' @return Chaine de caracteres representant la formule en code R.
#' @noRd
ramses_formula_code <- function(response = NULL, terms) {
  if (is.null(terms) || length(terms) == 0) {
    stop("terms doit etre un vecteur de noms de colonnes non vide.")
  }
  rhs_str <- paste(vapply(terms, ramses_code_symbol, character(1)), collapse = " + ")
  if (is.null(response) || !nzchar(response)) {
    paste0("~ ", rhs_str)
  } else {
    paste0(ramses_code_symbol(response), " ~ ", rhs_str)
  }
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



#' Prépare le fichier de données pour le rapport HTML (copie et chemin relatif)
#'
#' @param source_datapath Chemin absolu du fichier source actuel (par ex. upload temp).
#' @param original_name Nom original du fichier (pour conserver l'extension et le sens).
#' @param report_dir Répertoire de travail du rapport HTML où le fichier doit être copié.
#' @return Le chemin relatif (vers `data/...`) à insérer dans le Rmd.
#' @noRd
ramses_prepare_report_data_file <- function(source_datapath, original_name, report_dir) {
  if (is.null(source_datapath) || !file.exists(source_datapath)) {
    stop(paste0("Le fichier source de données (", original_name, ") n'est plus accessible ou n'existe pas."))
  }
  
  data_dir <- file.path(report_dir, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Nettoyage sécurisé du nom de fichier (garder lettres, chiffres, . _ -)
  # Mais on veut conserver l'extension intacte.
  ext <- tools::file_ext(original_name)
  base_name <- tools::file_path_sans_ext(original_name)
  
  clean_base <- gsub("[^A-Za-z0-9_.-]", "_", base_name)
  clean_base <- gsub("_+", "_", clean_base)
  if (clean_base == "" || clean_base == "_") clean_base <- "dataset"
  
  safe_name <- paste0(clean_base, ".", ext)
  
  # Éviter l'écrasement silencieux si un fichier avec le même nom existe
  target_path <- file.path(data_dir, safe_name)
  counter <- 1
  while (file.exists(target_path)) {
    # Vérifier si c'est exactement le même fichier (par exemple, appel multiple pour le même rapport)
    # Dans ce cas, on peut réutiliser. Sinon, on renomme.
    # Pour faire simple, on va toujours incrémenter sauf s'il fait la même taille (approximation).
    # Mieux : on incrémente toujours si c'est pas le même chemin source, mais le source est un fichier temp à chaque upload.
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
