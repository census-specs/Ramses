# =========================================================================
# Ramses - Module de P\u00e9dagogie et d'Hypoth\u00e8ses des Tests Statistiques
# =========================================================================

#' G\u00e9n\u00e9rateur de carte p\u00e9dagogique standardis\u00e9e
#'
#' Construit un conteneur HTML structur\u00e9 avec les trois sections cl\u00e9s :
#' 1. Hypoth\u00e8ses statistiques (H0, H1)
#' 2. Conditions d'application m\u00e9thodologiques (v\u00e9rifi\u00e9es sur donn\u00e9es r\u00e9elles)
#' 3. Guide d'interpr\u00e9tation bas\u00e9 sur alpha et la p-value
#'
#' @param h0_txt Texte de l'hypoth\u00e8se nulle H0.
#' @param h1_txt Texte de l'hypoth\u00e8se alternative H1.
#' @param conditions_items Liste de tags HTML (\code{shiny::tags$li}) d\u00e9taillant chaque condition.
#' @param guide_txt Texte ou tags HTML expliquant l'interpr\u00e9tation d\u00e9cisionnelle.
#' @param extra_blocks Liste facultative de blocs additionnels (ex. mesures d'association, post-hoc).
#' @return Un objet \code{shiny.tag} (div).
#' @noRd
ramses_pedagogy_container <- function(h0_txt,
                                      h1_txt,
                                      conditions_items,
                                      guide_txt,
                                      extra_blocks = NULL) {
  shiny::div(
    class = "space-y-3",

    # Section 1 : Hypoth\u00e8ses
    shiny::div(
      class = "p-3 rounded bg-light border",
      shiny::tags$h6(class = "fw-bold text-dark mb-2", "1. Hypoth\u00e8ses statistiques :"),
      shiny::tags$ul(
        class = "mb-0 ps-3 small",
        shiny::tags$li(
          shiny::tags$strong("Hypoth\u00e8se nulle (H0) : "),
          h0_txt
        ),
        shiny::tags$li(
          shiny::tags$strong("Hypoth\u00e8se alternative (H1) : "),
          h1_txt
        )
      )
    ),

    # Section 2 : Conditions d'application
    shiny::div(
      class = "p-3 rounded bg-light border",
      shiny::tags$h6(class = "fw-bold text-dark mb-2", "2. Conditions d'application m\u00e9thodologiques :"),
      shiny::tags$ul(
        class = "mb-0 ps-3 small",
        conditions_items
      )
    ),

    # Section 3 : Guide d'interpr\u00e9tation
    shiny::div(
      class = "p-3 rounded bg-light border",
      shiny::tags$h6(class = "fw-bold text-dark mb-2", "3. Guide d'interpr\u00e9tation & R\u00e8gle de d\u00e9cision :"),
      shiny::tags$div(
        class = "small text-secondary mb-0",
        guide_txt
      )
    ),

    # Blocs suppl\u00e9mentaires facultatifs
    extra_blocks
  )
}

#' Message d'attente p\u00e9dagogique avant ex\u00e9cution
#'
#' @param test_label Libell\u00e9 du test
#' @return Un \code{shiny.tag} invitant au lancement de l'analyse.
#' @noRd
ramses_pedagogy_waiting_ui <- function(test_label = "le test") {
  shiny::div(
    class = "p-4 text-center text-muted border rounded bg-light",
    shiny::tags$i(class = "fa fa-graduation-cap fa-2x mb-2 text-secondary opacity-75"),
    shiny::tags$h6(class = "fw-semibold text-dark", "Hypoth\u00e8ses & P\u00e9dagogie"),
    shiny::tags$p(
      class = "small mb-0",
      paste0(
        "S\u00e9lectionnez vos variables et vos param\u00e8tres, puis lancez ",
        test_label,
        " pour afficher la formulation exacte des hypoth\u00e8ses, l'\u00e9valuation des conditions d'application sur vos donn\u00e9es et le guide d'interpr\u00e9tation d\u00e9taill\u00e9."
      )
    )
  )
}

# =========================================================================
# 1. P\u00c9DAGOGIE : TESTS DE NORMALIT\u00c9 ET DE VARIANCE
# =========================================================================

#' P\u00e9dagogie pour les tests de normalit\u00e9 et de variance
#'
#' @param norm_state Objet reactiveValues de l'\u00e9tat de normalit\u00e9.
#' @param df Dataframe actif.
#' @return Un objet \code{shiny.tag}.
#' @noRd
ramses_pedagogy_norm <- function(norm_state, df) {
  if (!isTRUE(norm_state$calculated) || is.null(norm_state$var) || is.null(df)) {
    return(ramses_pedagogy_waiting_ui("l'\u00e9valuation de normalit\u00e9"))
  }

  var_name <- norm_state$var
  group_name <- norm_state$group
  alpha_val <- as.numeric(norm_state$alpha)
  y_raw <- df[[var_name]]
  y_clean <- y_raw[!is.na(y_raw)]
  n_clean <- length(y_clean)

  p_val <- tryCatch(as.numeric(norm_state$result$p.value), error = function(e) NA_real_)
  is_significant <- !is.na(p_val) && (p_val < alpha_val)

  test_type <- norm_state$test

  if (test_type == "shapiro") {
    h0_txt <- paste0(
      "La variable quantitative '", var_name,
      "' suit une distribution normale dans la population d'origine."
    )
    h1_txt <- paste0(
      "La distribution de '", var_name,
      "' s'\u00e9carte significativement d'une loi normale."
    )

    cond_n_ok <- (n_clean >= 3) && (n_clean <= 5000)
    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Nature des donn\u00e9es : "),
        "Variable quantitative continue. ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong(paste0("Taille d'\u00e9chantillon (n = ", n_clean, ") : ")),
        if (cond_n_ok) {
          shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (n = ", n_clean, " observations ; 3 \u2264 n \u2264 5000 observations requises par l'algorithme de Shapiro-Wilk)."))
        } else if (n_clean > 5000) {
          shiny::tags$span(class = "text-warning fw-semibold", paste0("\u26a0 Attention : n = ", n_clean, " > 5000. Le test de Shapiro-Wilk devient excessivement sensible \u00e0 d'infimes d\u00e9viations d\u00e9pourvues de port\u00e9e pratique."))
        } else {
          shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 Non satisfait : n = ", n_clean, " < 3 observations. Puissance statistique insuffisante."))
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance des observations : "),
        "Chaque observation doit \u00eatre ind\u00e9pendante des autres (\u00e9chantillonnage al\u00e9atoire sans mesure r\u00e9p\u00e9t\u00e9e)."
      )
    )

    decision_txt <- if (is_significant) {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette l'hypoth\u00e8se nulle H0. Les donn\u00e9es pr\u00e9sentent un \u00e9cart statistiquement significatif \u00e0 la normalit\u00e9. ",
        "Pour de petits \u00e9chantillons (n < 30), privil\u00e9giez des alternatives non-param\u00e9triques (Wilcoxon, Kruskal-Wallis, Spearman)."
      )
    } else {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne peut pas rejeter l'hypoth\u00e8se nulle H0. L'hypoth\u00e8se de normalit\u00e9 est recevable. ",
        "Les tests param\u00e9triques classiques (test t, ANOVA, Pearson) peuvent \u00eatre employ\u00e9s en toute confiance."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1",
        "Le test de Shapiro-Wilk \u00e9value la concordance entre les quantiles observ\u00e9s et les quantiles d'une distribution gaussienne th\u00e9orique."
      ),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else if (test_type == "shapiro_group") {
    h0_txt <- paste0(
      "La variable '", var_name,
      "' suit une distribution normale au sein de chacun des sous-groupes d\u00e9finis par '", group_name, "'."
    )
    h1_txt <- paste0(
      "La distribution de '", var_name,
      "' s'\u00e9carte de la normalit\u00e9 dans au moins l'un des groupes."
    )

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Condition intra-groupe : "),
        "La normalit\u00e9 doit \u00eatre satisfaite dans chaque groupe ind\u00e9pendamment pour la validit\u00e9 des tests param\u00e9triques de comparaison."
      ),
      shiny::tags$li(
        shiny::tags$strong("Taille minimale par groupe : "),
        "Chaque sous-groupe doit comporter au moins 3 observations non manquantes."
      )
    )

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1",
        "Si l'un des groupes pr\u00e9sente une p-value inf\u00e9rieure \u00e0 \u03b1, la condition de normalit\u00e9 globale pour un test param\u00e9trique strict est compromise."
      )
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else if (test_type == "bartlett") {
    h0_txt <- paste0(
      "Les variances de '", var_name,
      "' sont \u00e9gales (homoc\u00e9dasticit\u00e9) entre tous les groupes de '", group_name, "' (\u03c3\u00b2_1 = \u03c3\u00b2_2 = ...)."
    )
    h1_txt <- paste0(
      "Au moins deux groupes pr\u00e9sentent des variances significativement diff\u00e9rentes (\u00e9t\u00e9roc\u00e9dasticit\u00e9)."
    )

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Sensibilit\u00e9 \u00e0 la normalit\u00e9 : "),
        shiny::tags$span(class = "text-warning fw-semibold", "\u26a0 Hypoth\u00e8se forte : "),
        "Le test de Bartlett suppose imp\u00e9rativement que la variable est normalement distribu\u00e9e dans chaque groupe. En cas d'\u00e9cart \u00e0 la normalit\u00e9, le test de Fligner-Killeen est vivement recommand\u00e9."
      ),
      shiny::tags$li(
        shiny::tags$strong("Effectif par groupe : "),
        "Chaque groupe doit contenir au moins 3 observations."
      )
    )

    decision_txt <- if (is_significant) {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette H0. Les variances sont h\u00e9t\u00e9rog\u00e8nes. Privil\u00e9giez l'ANOVA de Welch ou des m\u00e9thodes robustes."
      )
    } else {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne rejette pas H0. L'hypoth\u00e8se d'\u00e9galit\u00e9 des variances (homoc\u00e9dasticit\u00e9) est respect\u00e9e."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "V\u00e9rifie le postulat d'homog\u00e9n\u00e9it\u00e9 des variances requis par l'ANOVA de Fisher classique."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else if (test_type == "fligner") {
    h0_txt <- paste0(
      "Les variances de '", var_name,
      "' sont homog\u00e8nes \u00e0 travers les modalit\u00e9s de '", group_name, "'."
    )
    h1_txt <- paste0(
      "Les variances diff\u00e8rent significativement entre au moins deux groupes."
    )

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Test non-param\u00e9trique robuste : "),
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9 : "),
        "Ce test est bas\u00e9 sur les rangs des \u00e9carts aux m\u00e9dianes. Il ne requiert pas la normalit\u00e9 des distributions."
      ),
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance des groupes : "),
        "Les groupes doivent \u00eatre mutuellement ind\u00e9pendants."
      )
    )

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Alternative non-param\u00e9trique au test de Bartlett, id\u00e9ale lorsque la distribution des donn\u00e9es est asym\u00e9trique ou comporte des valeurs atypiques.")
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else {
    # Kolmogorov-Smirnov
    h0_txt <- paste0("La variable '", var_name, "' suit une loi normale th\u00e9orique de m\u00eame moyenne et \u00e9cart-type.")
    h1_txt <- paste0("La variable '", var_name, "' ne suit pas cette loi normale.")
    cond_items <- list(
      shiny::tags$li("Variable continue sans trop d'ex aequo.")
    )
    guide_content <- shiny::tags$p("Compare la fonction de r\u00e9partition empirique \u00e0 la loi th\u00e9orique.")
    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))
  }
}

# =========================================================================
# 2. P\u00c9DAGOGIE : COMPARAISON DE 2 GROUPES (STUDENT & WILCOXON)
# =========================================================================

#' P\u00e9dagogie pour les tests de comparaison de deux \u00e9chantillons
#'
#' @param two_state Objet reactiveValues de l'\u00e9tat \u00e0 deux groupes.
#' @param df Dataframe actif.
#' @return Un objet \code{shiny.tag}.
#' @noRd
ramses_pedagogy_two <- function(two_state, df) {
  if (!isTRUE(two_state$calculated) || is.null(two_state$result) || is.null(df)) {
    return(ramses_pedagogy_waiting_ui("la comparaison de moyennes / rangs"))
  }

  test_type <- two_state$test
  alt <- two_state$alternative
  alpha_val <- as.numeric(two_state$alpha)
  p_val <- tryCatch(as.numeric(two_state$result$p.value), error = function(e) NA_real_)
  is_significant <- !is.na(p_val) && (p_val < alpha_val)

  var_y <- two_state$var_y

  # -------------------------------------------------------------------------
  # 2.1 Test t \u00e0 un \u00e9chantillon
  # -------------------------------------------------------------------------
  if (test_type == "t_one_sample") {
    mu0 <- two_state$mu_val
    h0_txt <- paste0("La moyenne de la population est \u00e9gale \u00e0 la valeur de r\u00e9f\u00e9rence (\u03bc = ", mu0, ").")
    h1_txt <- switch(
      alt,
      "two.sided" = paste0("La moyenne est diff\u00e9rente de la valeur de r\u00e9f\u00e9rence (\u03bc \u2260 ", mu0, ")."),
      "less" = paste0("La moyenne est strictement inf\u00e9rieure \u00e0 la valeur de r\u00e9f\u00e9rence (\u03bc < ", mu0, ")."),
      "greater" = paste0("La moyenne est strictement sup\u00e9rieure \u00e0 la valeur de r\u00e9f\u00e9rence (\u03bc > ", mu0, ").")
    )

    y_clean <- na.omit(df[[var_y]])
    n_obs <- length(y_clean)
    shapiro_res <- if (n_obs >= 3 && n_obs <= 5000) tryCatch(stats::shapiro.test(y_clean), error = function(e) NULL) else NULL

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Nature des donn\u00e9es : "),
        "Variable quantitative continue '", var_y, "'. ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong(paste0("Taille d'\u00e9chantillon (n = ", n_obs, ") : ")),
        if (n_obs >= 30) {
          shiny::tags$span(class = "text-success fw-semibold", "\u2714 Robuste (n \u2265 30) : en vertu du Th\u00e9or\u00e8me Central Limite (TCL), la distribution de la moyenne \u00e9chantillonnale est asymptotiquement normale.")
        } else {
          shiny::tags$span(class = "text-warning fw-semibold", "\u26a0 \u00c9chantillon r\u00e9duit (n < 30) : la normalit\u00e9 stricte de la variable est indispensable.")
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 des donn\u00e9es : "),
        if (!is.null(shapiro_res)) {
          if (shapiro_res$p.value >= 0.05) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Shapiro-Wilk p = ", round(shapiro_res$p.value, 4), " \u2265 0.05)."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 Non satisfait (test de Shapiro-Wilk p = ", round(shapiro_res$p.value, 4), " < 0.05). Privil\u00e9giez le test des rangs sign\u00e9s de Wilcoxon si n < 30."))
          }
        } else {
          "Effectif hors bornes pour le test de Shapiro-Wilk."
        }
      )
    )

    decision_txt <- if (is_significant) {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette l'hypoth\u00e8se nulle H0. La moyenne observ\u00e9e dans votre \u00e9chantillon pr\u00e9sente un \u00e9cart statistiquement significatif avec la valeur th\u00e9orique ", mu0, "."
      )
    } else {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne peut pas rejeter H0. L'\u00e9cart constat\u00e9 entre la moyenne observ\u00e9e et ", mu0, " peut raisonnablement r\u00e9sulter des seules fluctuations d'\u00e9chantillonnage."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Compare la moyenne observ\u00e9e \u00e0 une norme ou valeur cible attendue."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  # -------------------------------------------------------------------------
  # 2.2 Test t \u00e0 deux \u00e9chantillons ind\u00e9pendants (Student / Welch)
  # -------------------------------------------------------------------------
  } else if (test_type == "t_indep") {
    var_g <- two_state$var_group
    sub_df <- df[!is.na(df[[var_y]]) & !is.na(df[[var_g]]), ]
    if (!is.null(two_state$selected_modalities) && length(two_state$selected_modalities) == 2) {
      sub_df <- sub_df[sub_df[[var_g]] %in% two_state$selected_modalities, ]
    }
    levels_g <- unique(as.character(sub_df[[var_g]]))

    g1_name <- if (length(levels_g) >= 1) levels_g[1] else "Groupe 1"
    g2_name <- if (length(levels_g) >= 2) levels_g[2] else "Groupe 2"

    h0_txt <- paste0("Les moyennes de population des deux groupes sont \u00e9gales (\u03bc_", g1_name, " = \u03bc_", g2_name, ").")
    h1_txt <- switch(
      alt,
      "two.sided" = paste0("Les moyennes des deux groupes sont diff\u00e9rentes (\u03bc_", g1_name, " \u2260 \u03bc_", g2_name, ")."),
      "less" = paste0("La moyenne du groupe '", g1_name, "' est strictement inf\u00e9rieure \u00e0 celle de '", g2_name, "' (\u03bc_", g1_name, " < \u03bc_", g2_name, ")."),
      "greater" = paste0("La moyenne du groupe '", g1_name, "' est strictement sup\u00e9rieure \u00e0 celle de '", g2_name, "' (\u03bc_", g1_name, " > \u03bc_", g2_name, ").")
    )

    y_g1 <- sub_df[[var_y]][sub_df[[var_g]] == g1_name]
    y_g2 <- sub_df[[var_y]][sub_df[[var_g]] == g2_name]
    n1 <- length(y_g1)
    n2 <- length(y_g2)

    var_test_p <- tryCatch({
      if (n1 >= 2 && n2 >= 2) stats::var.test(y_g1, y_g2)$p.value else NA_real_
    }, error = function(e) NA_real_)

    var_equal_opt <- isTRUE(two_state$var_equal)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance des \u00e9chantillons : "),
        paste0("Deux groupes distincts (n1 = ", n1, " dans '", g1_name, "', n2 = ", n2, " dans '", g2_name, "')."),
        shiny::tags$span(class = "text-success fw-semibold", " \u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Taille et normalit\u00e9 : "),
        if (n1 >= 30 && n2 >= 30) {
          shiny::tags$span(class = "text-success fw-semibold", "\u2714 Grands effectifs (n1, n2 \u2265 30) : le test t est robuste aux \u00e9carts mod\u00e9r\u00e9s \u00e0 la normalit\u00e9.")
        } else {
          shiny::tags$span(class = "text-warning fw-semibold", "\u26a0 Petits effectifs (n < 30) : la normalit\u00e9 au sein de chaque groupe doit \u00eatre inspect\u00e9e.")
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Homog\u00e9n\u00e9it\u00e9 des variances : "),
        if (!var_equal_opt) {
          shiny::tags$span(class = "text-success fw-semibold", "\u2714 Test t de Welch appliqu\u00e9 : il ne suppose PAS l'\u00e9galit\u00e9 des variances et ajuste automatiquement les degr\u00e9s de libert\u00e9. C'est l'approche moderne recommand\u00e9e.")
        } else {
          if (!is.na(var_test_p) && var_test_p < 0.05) {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 Variances in\u00e9gales d\u00e9tect\u00e9es (test F p = ", round(var_test_p, 4), " < 0.05). L'utilisation de la variante de Welch (sans cocher 'variances \u00e9gales') est fortement conseill\u00e9e."))
          } else {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Variances compatibles (test F de Fisher p = ", round(var_test_p, 4), " \u2265 0.05)."))
          }
        }
      )
    )

    decision_txt <- if (is_significant) {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette l'hypoth\u00e8se nulle H0. Il existe une diff\u00e9rence statistiquement significative entre les deux moyennes de population."
      )
    } else {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne rejette pas H0. La diff\u00e9rence constat\u00e9e entre les deux \u00e9chantillons n'atteint pas le seuil de significativit\u00e9."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Compare la moyenne d'une variable quantitative entre deux modalit\u00e9s ind\u00e9pendantes."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  # -------------------------------------------------------------------------
  # 2.3 Test t appari\u00e9
  # -------------------------------------------------------------------------
  } else if (test_type == "t_paired") {
    var_x2 <- two_state$var_x2
    sub_df <- df[!is.na(df[[var_y]]) & !is.na(df[[var_x2]]), ]
    diff_vals <- sub_df[[var_y]] - sub_df[[var_x2]]
    n_pairs <- length(diff_vals)

    h0_txt <- paste0("La moyenne des diff\u00e9rences entre '", var_y, "' et '", var_x2, "' est nulle (\u03bc_diff = 0).")
    h1_txt <- switch(
      alt,
      "two.sided" = paste0("La moyenne des diff\u00e9rences est non nulle (\u03bc_diff \u2260 0)."),
      "less" = paste0("La premi\u00e8re mesure '", var_y, "' est inf\u00e9rieure \u00e0 la seconde '", var_x2, "' (\u03bc_diff < 0)."),
      "greater" = paste0("La premi\u00e8re mesure '", var_y, "' est sup\u00e9rieure \u00e0 la seconde '", var_x2, "' (\u03bc_diff > 0).")
    )

    shapiro_diff <- if (n_pairs >= 3 && n_pairs <= 5000) tryCatch(stats::shapiro.test(diff_vals), error = function(e) NULL) else NULL

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Donn\u00e9es appari\u00e9es : "),
        paste0("Chaque individu dispose de 2 mesures li\u00e9es (n = ", n_pairs, " paires compl\u00e8tes). "),
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 de la diff\u00e9rence (d = Y - Y2) : "),
        if (!is.null(shapiro_diff)) {
          if (shapiro_diff$p.value >= 0.05 || n_pairs >= 30) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Condition satisfaite (Shapiro-Wilk sur diff\u00e9rences p = ", round(shapiro_diff$p.value, 4), if (n_pairs >= 30) ", n \u2265 30 robuste" else "", ")."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 Non satisfait (Shapiro-Wilk p = ", round(shapiro_diff$p.value, 4), " < 0.05 et n < 30). Utilisez le test de Wilcoxon appari\u00e9."))
          }
        } else {
          "Effectif de paires analys\u00e9."
        }
      )
    )

    decision_txt <- if (is_significant) {
      paste0("P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). On rejette H0. L'\u00e9volution / diff\u00e9rence moyenne entre les deux mesures est statistiquement significative.")
    } else {
      paste0("P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). On ne rejette pas H0. Pas d'\u00e9volution significative mise en \u00e9vidence.")
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Test t con\u00e7u pour les plans avant-apr\u00e8s ou mesures r\u00e9p\u00e9t\u00e9es sur les m\u00eames sujets."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  # -------------------------------------------------------------------------
  # 2.4 Wilcoxon ind\u00e9pendant (Mann-Whitney)
  # -------------------------------------------------------------------------
  } else if (test_type == "wilcox_indep") {
    var_g <- two_state$var_group
    sub_df <- df[!is.na(df[[var_y]]) & !is.na(df[[var_g]]), ]
    levels_g <- unique(as.character(sub_df[[var_g]]))
    g1_name <- if (length(levels_g) >= 1) levels_g[1] else "Groupe 1"
    g2_name <- if (length(levels_g) >= 2) levels_g[2] else "Groupe 2"

    h0_txt <- paste0("Les distributions de '", var_y, "' dans les deux populations sont identiques (probabilit\u00e9 de sup\u00e9riorit\u00e9 stochastique P(Y1 > Y2) = 0.5).")
    h1_txt <- switch(
      alt,
      "two.sided" = paste0("Les deux distributions pr\u00e9sentent un d\u00e9calage stochastique l'une par rapport \u00e0 l'autre."),
      "less" = paste0("Les valeurs du groupe '", g1_name, "' ont tendance \u00e0 \u00eatre inf\u00e9rieures \u00e0 celles de '", g2_name, "'."),
      "greater" = paste0("Les valeurs du groupe '", g1_name, "' ont tendance \u00e0 \u00eatre sup\u00e9rieures \u00e0 celles de '", g2_name, "'.")
    )

    has_ties <- any(duplicated(sub_df[[var_y]]))

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Donn\u00e9es au moins ordinales : "),
        "La proc\u00e9dure repose sur les rangs des observations. Aucune hypoth\u00e8se de normalit\u00e9 n'est requise. ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Gestion des ex aequo (ties) : "),
        if (has_ties) {
          shiny::tags$span(class = "text-info fw-semibold", "\u2139 Des valeurs identiques (ex aequo) sont pr\u00e9sentes. Une approximation normale asymptotique avec correction de continuit\u00e9 est employ\u00e9e.")
        } else {
          shiny::tags$span(class = "text-success fw-semibold", "\u2714 Aucun ex aequo d\u00e9tect\u00e9 : calcul exact de la p-value r\u00e9alisable.")
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Interpr\u00e9tation en termes de m\u00e9dianes : "),
        "Pour conclure rigoureusement \u00e0 une diff\u00e9rence de m\u00e9dianes, les deux distributions doivent avoir une forme g\u00e9om\u00e9trique similaire."
      )
    )

    decision_txt <- if (is_significant) {
      paste0("P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). On rejette H0. Un d\u00e9calage significatif de distribution existe entre les deux groupes.")
    } else {
      paste0("P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). On ne rejette pas H0. Pas de diff\u00e9rence de distribution d\u00e9montr\u00e9e.")
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Test non-param\u00e9trique de comparaison de deux \u00e9chantillons ind\u00e9pendants (Mann-Whitney U)."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  # -------------------------------------------------------------------------
  # 2.5 Wilcoxon sign\u00e9 (1 \u00e9chantillon ou appari\u00e9)
  # -------------------------------------------------------------------------
  } else {
    is_paired <- (test_type == "wilcox_paired")
    h0_txt <- if (is_paired) {
      "La m\u00e9diane des diff\u00e9rences entre mesures appari\u00e9es est \u00e9gale \u00e0 z\u00e9ro (distribution sym\u00e9trique)."
    } else {
      paste0("La m\u00e9diane de la population est \u00e9gale \u00e0 la valeur de r\u00e9f\u00e9rence (\u03b7 = ", two_state$mu_val, ").")
    }

    h1_txt <- switch(
      alt,
      "two.sided" = "La m\u00e9diane est diff\u00e9rente de la valeur de r\u00e9f\u00e9rence.",
      "less" = "La m\u00e9diane est strictement inf\u00e9rieure \u00e0 la r\u00e9f\u00e9rence.",
      "greater" = "La m\u00e9diane est strictement sup\u00e9rieure \u00e0 la r\u00e9f\u00e9rence."
    )

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Sym\u00e9trie de la distribution : "),
        "Le test des rangs sign\u00e9s suppose que la distribution sous-jacente des diff\u00e9rences est sym\u00e9trique autour de la m\u00e9diane th\u00e9orique."
      ),
      shiny::tags$li(
        shiny::tags$strong("Gestion des diff\u00e9rences nulles : "),
        "Les paires dont la diff\u00e9rence vaut exactement z\u00e9ro sont \u00e9limin\u00e9es de l'analyse (proc\u00e9dure standard de Wilcoxon)."
      )
    )

    decision_txt <- if (is_significant) {
      paste0("P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). On rejette H0 au profit de l'hypoth\u00e8se alternative.")
    } else {
      paste0("P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). Pas de d\u00e9viation statistiquement significative observ\u00e9e.")
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Test des rangs sign\u00e9s de Wilcoxon pour donn\u00e9es non-param\u00e9triques."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))
  }
}

# =========================================================================
# 3. P\u00c9DAGOGIE : COMPARAISON 3+ GROUPES (ANOVA & KRUSKAL-WALLIS)
# =========================================================================

#' P\u00e9dagogie pour l'ANOVA et Kruskal-Wallis
#'
#' @param multi_state Objet reactiveValues de l'\u00e9tat multi-groupes.
#' @param df Dataframe actif.
#' @return Un objet \code{shiny.tag}.
#' @noRd
ramses_pedagogy_multi <- function(multi_state, df) {
  if (!isTRUE(multi_state$calculated) || is.null(multi_state$result) || is.null(df)) {
    return(ramses_pedagogy_waiting_ui("la comparaison de 3+ groupes"))
  }

  test_type <- multi_state$test
  alpha_val <- as.numeric(multi_state$alpha)
  var_y <- multi_state$var_y
  var_g <- multi_state$var_group

  sub_df <- df[!is.na(df[[var_y]]) & !is.na(df[[var_g]]), ]
  group_levels <- unique(as.character(sub_df[[var_g]]))
  k_groups <- length(group_levels)

  if (test_type == "anova_twoway") {
    var_g1 <- multi_state$var_group
    var_g2 <- multi_state$var_group2
    has_int <- isTRUE(multi_state$interaction)
    res_obj <- multi_state$result

    h0_txt <- if (has_int) {
      paste0("1. Aucun effet principal de '", var_g1, "' ; 2. Aucun effet principal de '", var_g2, "' ; 3. Aucune interaction entre '", var_g1, "' et '", var_g2, "' (l'effet de ", var_g1, " ne d\u00e9pend pas du niveau de ", var_g2, ").")
    } else {
      paste0("1. Aucun effet principal de '", var_g1, "' (moyennes \u00e9gales) ; 2. Aucun effet principal de '", var_g2, "' (moyennes \u00e9gales).")
    }

    h1_txt <- if (has_int) {
      "Au moins un effet principal ou l'interaction entre les deux facteurs est statistiquement significatif dans la population."
    } else {
      "Au moins l'un des deux facteurs explique une variation significative des moyennes de la variable d\u00e9pendante."
    }

    shapiro_res <- if (!is.null(res_obj) && !is.null(res_obj$shapiro)) res_obj$shapiro else NULL
    fligner_res <- if (!is.null(res_obj) && !is.null(res_obj$fligner)) res_obj$fligner else NULL
    n_total <- if (!is.null(res_obj) && !is.null(res_obj$n_obs)) res_obj$n_obs else nrow(sub_df)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance des observations : "),
        "Les observations doivent \u00eatre ind\u00e9pendantes entre les sujets et entre les cellules du plan factoriel (d\u00e9pend du protocole exp\u00e9rimental)."
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 des r\u00e9sidus du mod\u00e8le : "),
        if (!is.null(shapiro_res)) {
          if (shapiro_res$p.value >= 0.05 || n_total >= 30) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Shapiro-Wilk sur les r\u00e9sidus p = ", round(shapiro_res$p.value, 4), if (n_total >= 30) ", effectif total n \u2265 30 robuste" else "", ")."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 D\u00e9viation de normalit\u00e9 (Shapiro-Wilk sur r\u00e9sidus p = ", round(shapiro_res$p.value, 4), " < 0.05). Interpr\u00e9ter avec prudence si les effectifs par cellule sont faibles."))
          }
        } else {
          "Effectif analys\u00e9."
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Homog\u00e9n\u00e9it\u00e9 des variances (Fligner-Killeen) : "),
        if (!is.null(fligner_res)) {
          if (fligner_res$p.value >= 0.05) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Fligner-Killeen p = ", round(fligner_res$p.value, 4), " \u2265 0.05 : variances homog\u00e8nes entre cellules)."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 H\u00e9t\u00e9rog\u00e9n\u00e9it\u00e9 des variances (test de Fligner-Killeen p = ", round(fligner_res$p.value, 4), " < 0.05). Prudence accrue sur les F-tests si les effectifs par cellule sont in\u00e9gaux."))
          }
        } else {
          "Test de variances non calculable."
        }
      )
    )

    t_info <- if (!is.null(res_obj)) res_obj$terms_info else list()
    term_ab <- paste0(var_g1, ":", var_g2)

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-2", "L'ANOVA factorielle d\u00e9compose la variance en effets principaux de chaque facteur et en effet d'interaction conjointe."),
      shiny::tags$ul(
        class = "ps-3 mb-2 space-y-1 small",
        shiny::tags$li(
          shiny::tags$strong(paste0("Facteur ", var_g1, " : ")),
          "Permet-il d'expliquer une diff\u00e9rence de moyenne globale ?",
          if (!is.null(t_info[[var_g1]])) {
            if (t_info[[var_g1]]$sig) shiny::tags$span(class = "text-success fw-bold ms-1", "[Significatif]") else shiny::tags$span(class = "text-muted ms-1", "[Non significatif]")
          }
        ),
        shiny::tags$li(
          shiny::tags$strong(paste0("Facteur ", var_g2, " : ")),
          "Permet-il d'expliquer une diff\u00e9rence de moyenne globale ?",
          if (!is.null(t_info[[var_g2]])) {
            if (t_info[[var_g2]]$sig) shiny::tags$span(class = "text-success fw-bold ms-1", "[Significatif]") else shiny::tags$span(class = "text-muted ms-1", "[Non significatif]")
          }
        ),
        if (has_int) {
          shiny::tags$li(
            shiny::tags$strong("Interaction A \u00d7 B : "),
            "L'effet de A d\u00e9pend-il du niveau de B ?",
            if (!is.null(t_info[[term_ab]])) {
              if (t_info[[term_ab]]$sig) shiny::tags$span(class = "text-danger fw-bold ms-1", "[Interaction significative !]") else shiny::tags$span(class = "text-muted ms-1", "[Non significative]")
            }
          )
        }
      ),
      if (has_int && !is.null(t_info[[term_ab]]) && t_info[[term_ab]]$sig) {
        shiny::div(
          class = "alert alert-warning py-2 px-3 small mt-2 mb-0",
          shiny::tags$strong("\u26a0 Attention : "),
          "L'interaction \u00e9tant statistiquement significative, l'effet d'un facteur varie selon la modalit\u00e9 de l'autre. Il est recommand\u00e9 d'\u00e9tudier les effets simples (profils d'interaction) plut\u00f4t que d'interpr\u00e9ter isol\u00e9ment les effets principaux."
        )
      } else {
        shiny::tags$p(class = "text-muted small mb-0", "Une interaction signifie que l'effet d'un facteur d\u00e9pend du niveau de l'autre facteur.")
      }
    )

    extra_block <- if (isTRUE(multi_state$post_hoc)) {
      shiny::div(
        class = "p-3 rounded bg-light border",
        shiny::tags$h6(class = "fw-bold text-dark mb-2", "Comparaisons multiples (Post-Hoc) :"),
        shiny::tags$p(
          class = "small text-secondary mb-0",
          "Comparaisons multiples pour les effets factoriels : fonctionnalit\u00e9 pr\u00e9vue dans une phase ult\u00e9rieure (analyse des moyennes marginales ajust\u00e9es et contrastes simples)."
        )
      )
    } else NULL

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content, extra_block))

  } else if (test_type == "anova_rm") {
    rm_res <- multi_state$result
    w_factor <- rm_res$within_factor
    s_var <- rm_res$subject
    k_levs <- if (!is.null(rm_res$data_info$k_levels)) rm_res$data_info$k_levels else 0
    n_subj <- if (!is.null(rm_res$data_info$final_subjects)) rm_res$data_info$final_subjects else 0
    n_obs <- if (!is.null(rm_res$data_info$n_obs)) rm_res$data_info$n_obs else 0
    tab <- rm_res$anova_table
    mauchly <- rm_res$mauchly
    corrections <- rm_res$corrections
    ph <- rm_res$post_hoc

    h0_txt <- paste0(
      "Les moyennes de la variable '", var_y, "' sont \u00e9gales entre tous les niveaux du facteur intra-sujets '", w_factor, "' ",
      "(\u03bc_1 = \u03bc_2 = ... = \u03bc_", k_levs, ")."
    )
    h1_txt <- paste0(
      "Au moins un niveau du facteur intra-sujets '", w_factor, "' pr\u00e9sente une moyenne significativement diff\u00e9rente dans la population."
    )

    is_sph_viol <- isTRUE(mauchly$applicable) && is.numeric(mauchly$p_value) && !is.na(mauchly$p_value) && (mauchly$p_value < alpha_val)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Structure des donn\u00e9es & Appariement : "),
        shiny::tags$span(
          class = "text-success fw-semibold",
          paste0("\u2714 Plan complet et \u00e9quilibr\u00e9 (N = ", n_subj, " sujets mesur\u00e9s chacun sur ", k_levs, " modalit\u00e9s, soit ", n_obs, " observations). ")
        ),
        "Cette analyse est utilis\u00e9e lorsque les m\u00eames sujets sont mesur\u00e9s plusieurs fois. Les observations d'un m\u00eame sujet sont li\u00e9es. L'analyse tient compte de cette d\u00e9pendance en isolant la variabilit\u00e9 inter-sujets."
      ),
      shiny::tags$li(
        shiny::tags$strong("Hypoth\u00e8se de sph\u00e9ricit\u00e9 (Mauchly) : "),
        if (isTRUE(mauchly$applicable)) {
          w_val_str <- if (is.numeric(mauchly$w) && length(mauchly$w) == 1 && !is.na(mauchly$w)) round(mauchly$w, 4) else "\u2014"
          p_val_str <- if (is.numeric(mauchly$p_value) && length(mauchly$p_value) == 1 && !is.na(mauchly$p_value)) format.pval(mauchly$p_value, digits = 4) else "\u2014"
          if (!is_sph_viol) {
            shiny::tags$span(
              class = "text-success fw-semibold",
              paste0("\u2714 Valid\u00e9 (Test de Mauchly W = ", w_val_str, ", p = ", p_val_str, " \u2265 0.05). Les variances de toutes les diff\u00e9rences entre paires sont \u00e9gales. Le F-test standard est valide.")
            )
          } else {
            shiny::tags$span(
              class = "text-danger fw-semibold",
              paste0("\u26a0 Violation de la sph\u00e9ricit\u00e9 (Test de Mauchly W = ", w_val_str, ", p = ", p_val_str, " < 0.05). Les variances des diff\u00e9rences diff\u00e8rent. L'utilisation des corrections d'\u00e9psilon (Greenhouse-Geisser ou Huynh-Feldt) est requise.")
            )
          }
        } else {
          shiny::tags$span(
            class = "text-success fw-semibold",
            "\u2714 Avec 2 modalit\u00e9s, l'hypoth\u00e8se de sph\u00e9ricit\u00e9 est automatiquement satisfaite et le test de Mauchly n'est pas n\u00e9cessaire."
          )
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 des r\u00e9sidus intra-sujets : "),
        "La distribution des r\u00e9sidus du mod\u00e8le intra-sujets doit \u00eatre approximativement normale (robuste pour des \u00e9chantillons mod\u00e9r\u00e9s \u00e0 grands)."
      )
    )

    row_w <- if (!is.null(tab)) tab[tab$Source == w_factor, ] else NULL
    p_raw <- if (!is.null(row_w) && nrow(row_w) > 0 && is.numeric(row_w$p_value)) row_w$p_value[1] else NA_real_
    p_use <- if (is_sph_viol && !is.null(corrections) && is.numeric(corrections$p_gg)) corrections$p_gg else p_raw
    sig_f <- is.numeric(p_use) && !is.na(p_use) && (p_use < alpha_val)

    guide_content <- shiny::tags$div(
      shiny::tags$p(
        class = "mb-2",
        "L'ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es compare les moyennes d'une variable quantitative mesur\u00e9e plusieurs fois sur les m\u00eames individus (par exemple : \u00c9volution avant/apr\u00e8s traitement, suivi temporel \u00e0 plusieurs dates, r\u00e9ponses \u00e0 diff\u00e9rentes conditions exp\u00e9rimentales)."
      ),
      shiny::tags$ul(
        class = "ps-3 mb-2 space-y-1 small",
        shiny::tags$li(
          shiny::tags$strong(paste0("Effet du facteur intra-sujets '", w_factor, "' : ")),
          if (sig_f) {
            p_disp <- if (is.numeric(p_use) && !is.na(p_use)) format.pval(p_use, digits = 4) else "\u2014"
            shiny::tags$span(class = "text-success fw-bold ms-1", paste0("[Significatif, p = ", p_disp, "] - Les mesures diff\u00e8rent entre les conditions/temps."))
          } else {
            p_disp <- if (is.numeric(p_use) && !is.na(p_use)) format.pval(p_use, digits = 4) else "\u2014"
            shiny::tags$span(class = "text-muted ms-1", paste0("[Non significatif, p = ", p_disp, "] - Pas de diff\u00e9rence significative d\u00e9tect\u00e9e."))
          }
        ),
        if (is_sph_viol && !is.null(corrections) && isTRUE(corrections$applicable)) {
          eps_gg_str <- if (is.numeric(corrections$eps_gg) && !is.na(corrections$eps_gg)) round(corrections$eps_gg, 3) else "\u2014"
          p_gg_str <- if (is.numeric(corrections$p_gg) && !is.na(corrections$p_gg)) format.pval(corrections$p_gg, digits = 4) else "\u2014"
          p_hf_str <- if (is.numeric(corrections$p_hf) && !is.na(corrections$p_hf)) format.pval(corrections$p_hf, digits = 4) else "\u2014"
          shiny::tags$li(
            shiny::tags$strong("Correction recommand\u00e9e : "),
            paste0(
              "En pr\u00e9sence de non-sph\u00e9ricit\u00e9, si \u03b5_GG = ", eps_gg_str, " < 0.75, Greenhouse-Geisser est recommand\u00e9 (p = ",
              p_gg_str, ") ; si \u03b5 > 0.75, Huynh-Feldt peut \u00eatre pr\u00e9f\u00e9r\u00e9 (p = ", p_hf_str, ")."
            )
          )
        } else NULL
      )
    )

    extra_block <- if (!is.null(ph) && is.data.frame(ph) && nrow(ph) > 0) {
      shiny::div(
        class = "p-3 rounded bg-light border mt-3",
        shiny::tags$h6(class = "fw-bold text-dark mb-2", "Comparaisons par paires intra-sujets (Holm) :"),
        shiny::tags$div(
          class = "table-responsive",
          shiny::tags$table(
            class = "table table-sm table-striped table-hover small mb-0",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Comparaison"),
                shiny::tags$th("Diff\u00e9rence moyenne"),
                shiny::tags$th("Erreur-type (SE)"),
                shiny::tags$th("Statistique t"),
                shiny::tags$th("ddl"),
                shiny::tags$th("p-value brute"),
                shiny::tags$th("p-value Holm"),
                shiny::tags$th("Significativit\u00e9")
              )
            ),
            shiny::tags$tbody(
              lapply(seq_len(nrow(ph)), function(idx) {
                row_item <- ph[idx, ]
                diff_str <- if (is.numeric(row_item$Difference) && !is.na(row_item$Difference)) round(row_item$Difference, 3) else "\u2014"
                se_str <- if (is.numeric(row_item$SE) && !is.na(row_item$SE)) round(row_item$SE, 3) else "\u2014"
                t_str <- if (is.numeric(row_item$t_value) && !is.na(row_item$t_value)) round(row_item$t_value, 3) else "\u2014"
                df_str <- if (!is.null(row_item$Df) && !is.na(row_item$Df)) row_item$Df else if (!is.null(row_item$df) && !is.na(row_item$df)) row_item$df else "\u2014"
                p_raw_str <- if (is.numeric(row_item$p_value_raw) && !is.na(row_item$p_value_raw)) format.pval(row_item$p_value_raw, digits = 4) else "\u2014"
                p_adj_str <- if (is.numeric(row_item$p_value_adj) && !is.na(row_item$p_value_adj)) format.pval(row_item$p_value_adj, digits = 4) else "\u2014"

                shiny::tags$tr(
                  shiny::tags$td(paste0(row_item$Niveau_1, " vs ", row_item$Niveau_2)),
                  shiny::tags$td(diff_str),
                  shiny::tags$td(se_str),
                  shiny::tags$td(t_str),
                  shiny::tags$td(df_str),
                  shiny::tags$td(p_raw_str),
                  shiny::tags$td(class = "fw-bold", p_adj_str),
                  shiny::tags$td(
                    if (isTRUE(row_item$sig)) {
                      shiny::tags$span(class = "badge bg-success", "Significatif")
                    } else {
                      shiny::tags$span(class = "badge bg-secondary", "Non sign.")
                    }
                  )
                )
              })
            )
          )
        )
      )
    } else NULL

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content, extra_block))

  } else if (test_type == "ancova") {
    var_fact <- multi_state$var_group
    var_cov <- multi_state$var_covar
    res_obj <- multi_state$result

    h0_txt <- paste0(
      "1. Facteur '", var_fact, "' : Apr\u00e8s prise en compte de la covariable '", var_cov, "', les moyennes ajust\u00e9es des groupes sont \u00e9gales. ",
      "2. Covariable '", var_cov, "' : La covariable n'a aucun effet lin\u00e9aire significatif sur '", var_y, "' apr\u00e8s ajustement sur le groupe."
    )
    h1_txt <- paste0(
      "1. Facteur '", var_fact, "' : Au moins une moyenne ajust\u00e9e diff\u00e8re significativement entre les groupes. ",
      "2. Covariable '", var_cov, "' : La covariable est significativement associ\u00e9e \u00e0 la variable d\u00e9pendante."
    )

    slopes_info <- if (!is.null(res_obj) && !is.null(res_obj$slopes_test)) res_obj$slopes_test else NULL
    shapiro_res <- if (!is.null(res_obj) && !is.null(res_obj$shapiro)) res_obj$shapiro else NULL
    fligner_res <- if (!is.null(res_obj) && !is.null(res_obj$fligner)) res_obj$fligner else NULL
    n_total <- if (!is.null(res_obj) && !is.null(res_obj$n_obs)) res_obj$n_obs else nrow(sub_df)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Homog\u00e9n\u00e9it\u00e9 des pentes (Parall\u00e9lisme) : "),
        if (!is.null(slopes_info) && !is.na(slopes_info$p_value)) {
          if (isTRUE(slopes_info$pentes_homogenes)) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (Test Facteur \u00d7 Covariable p = ", round(slopes_info$p_value, 4), " > \u03b1). Les pentes de r\u00e9gression sont comparables entre groupes. L'hypoth\u00e8se de pente commune est respect\u00e9e."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 Attention : Les pentes diff\u00e8rent significativement entre les groupes (p = ", round(slopes_info$p_value, 4), " \u2264 \u03b1). L'hypoth\u00e8se de pentes communes de l'ANCOVA classique n'est pas respect\u00e9e. Les moyennes ajust\u00e9es issues du mod\u00e8le \u00e0 pente commune doivent donc \u00eatre interpr\u00e9t\u00e9es avec prudence."))
          }
        } else {
          "Test d'homog\u00e9n\u00e9it\u00e9 des pentes \u00e9valu\u00e9."
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 des r\u00e9sidus : "),
        if (!is.null(shapiro_res)) {
          if (shapiro_res$p.value >= 0.05 || n_total >= 30) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Shapiro-Wilk sur les r\u00e9sidus p = ", round(shapiro_res$p.value, 4), if (n_total >= 30) ", n \u2265 30 robuste" else "", ")."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 D\u00e9viation de normalit\u00e9 (Shapiro-Wilk p = ", round(shapiro_res$p.value, 4), " < 0.05). \u00c0 interpr\u00e9ter avec prudence si l'effectif est faible."))
          }
        } else {
          "Effectif analys\u00e9."
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Homog\u00e9n\u00e9it\u00e9 des variances r\u00e9siduelles (Fligner-Killeen) : "),
        if (!is.null(fligner_res)) {
          if (fligner_res$p.value >= 0.05) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Fligner-Killeen p = ", round(fligner_res$p.value, 4), " \u2265 0.05)."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 H\u00e9t\u00e9rog\u00e9n\u00e9it\u00e9 des variances r\u00e9siduelles (p = ", round(fligner_res$p.value, 4), " < 0.05)."))
          }
        } else {
          "Test de variances r\u00e9siduelles."
        }
      )
    )

    a_tab <- if (!is.null(res_obj)) res_obj$anova_table else NULL
    row_f <- if (!is.null(a_tab)) a_tab[a_tab$Term == var_fact, ] else NULL
    row_c <- if (!is.null(a_tab)) a_tab[a_tab$Term == var_cov, ] else NULL

    guide_content <- shiny::tags$div(
      shiny::tags$p(
        class = "mb-2",
        "L'ANCOVA (Analyse de Covariance) permet de comparer plusieurs groupes tout en tenant compte de l'effet d'une variable quantitative appel\u00e9e covariable (par exemple, comparer le rendement de plusieurs vari\u00e9t\u00e9s en tenant compte de l'\u00e2ge des plants)."
      ),
      shiny::tags$ul(
        class = "ps-3 mb-2 space-y-1 small",
        shiny::tags$li(
          shiny::tags$strong(paste0("Facteur ", var_fact, " : ")),
          "Existe-t-il une diff\u00e9rence entre les groupes apr\u00e8s contr\u00f4le de la covariable ?",
          if (!is.null(row_f) && nrow(row_f) > 0 && !is.na(row_f$p_value[1])) {
            if (row_f$p_value[1] < alpha_val) shiny::tags$span(class = "text-success fw-bold ms-1", "[Significatif]") else shiny::tags$span(class = "text-muted ms-1", "[Non significatif]")
          }
        ),
        shiny::tags$li(
          shiny::tags$strong(paste0("Covariable ", var_cov, " : ")),
          "La covariable apporte-t-elle un ajustement lin\u00e9aire significatif ?",
          if (!is.null(row_c) && nrow(row_c) > 0 && !is.na(row_c$p_value[1])) {
            if (row_c$p_value[1] < alpha_val) shiny::tags$span(class = "text-success fw-bold ms-1", "[Significative]") else shiny::tags$span(class = "text-muted ms-1", "[Non significative]")
          }
        )
      ),
      shiny::tags$p(
        class = "text-muted small mb-0",
        "Les effets du facteur et de la covariable sont \u00e9valu\u00e9s apr\u00e8s prise en compte de l'autre terme du mod\u00e8le. Cette approche correspond au principe des sommes des carr\u00e9s de type II pour ce mod\u00e8le additif."
      )
    )

    extra_block <- if (!is.null(res_obj) && !is.null(res_obj$post_hoc) && nrow(res_obj$post_hoc) > 0) {
      ph <- res_obj$post_hoc
      shiny::div(
        class = "p-3 rounded bg-light border mt-3",
        shiny::tags$h6(class = "fw-bold text-dark mb-2", "Comparaisons deux \u00e0 deux des moyennes ajust\u00e9es (Correction de Holm) :"),
        shiny::tags$div(
          class = "table-responsive",
          shiny::tags$table(
            class = "table table-sm table-striped table-hover small mb-0",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Comparaison"),
                shiny::tags$th("Diff\u00e9rence"),
                shiny::tags$th("SE"),
                shiny::tags$th("Statistique t"),
                shiny::tags$th("p-value brute"),
                shiny::tags$th("p-value Holm"),
                shiny::tags$th("Significativit\u00e9")
              )
            ),
            shiny::tags$tbody(
              lapply(seq_len(nrow(ph)), function(idx) {
                row_item <- ph[idx, ]
                shiny::tags$tr(
                  shiny::tags$td(paste0(row_item$Groupe_1, " vs ", row_item$Groupe_2)),
                  shiny::tags$td(round(row_item$Difference, 3)),
                  shiny::tags$td(round(row_item$SE, 3)),
                  shiny::tags$td(round(row_item$t_value, 3)),
                  shiny::tags$td(format.pval(row_item$p_value_raw, digits = 3)),
                  shiny::tags$td(format.pval(row_item$p_value_adj, digits = 3)),
                  shiny::tags$td(
                    if (isTRUE(row_item$sig)) {
                      shiny::tags$span(class = "badge bg-success", "Significatif")
                    } else {
                      shiny::tags$span(class = "badge bg-secondary", "Non sign.")
                    }
                  )
                )
              })
            )
          )
        )
      )
    } else NULL

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content, extra_block))

  } else if (test_type == "anova") {
    h0_txt <- paste0("Toutes les moyennes des ", k_groups, " groupes de population sont \u00e9gales (\u03bc_1 = \u03bc_2 = ... = \u03bc_", k_groups, ").")
    h1_txt <- "Au moins une moyenne de groupe est significativement diff\u00e9rente des autres dans la population."

    # Conditions ANOVA :
    # 1. Normalit\u00e9 des r\u00e9sidus
    aov_mod <- tryCatch(stats::aov(stats::as.formula(paste0("`", var_y, "` ~ `", var_g, "`")), data = sub_df), error = function(e) NULL)
    resids <- if (!is.null(aov_mod)) stats::residuals(aov_mod) else NULL
    shapiro_res <- if (!is.null(resids) && length(resids) >= 3 && length(resids) <= 5000) {
      tryCatch(stats::shapiro.test(resids), error = function(e) NULL)
    } else NULL

    # 2. Homoc\u00e9dasticit\u00e9 (Bartlett)
    bartlett_res <- tryCatch(stats::bartlett.test(stats::as.formula(paste0("`", var_y, "` ~ `", var_g, "`")), data = sub_df), error = function(e) NULL)

    group_counts <- table(sub_df[[var_g]])
    min_count <- min(group_counts)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance & Effectifs (", k_groups, " groupes, min n_k = ", min_count, ") : "),
        "Les observations doivent \u00eatre ind\u00e9pendantes au sein de chaque groupe et entre les groupes."
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 des r\u00e9sidus du mod\u00e8le : "),
        if (!is.null(shapiro_res)) {
          if (shapiro_res$p.value >= 0.05 || nrow(sub_df) >= 30) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Shapiro-Wilk sur les r\u00e9sidus p = ", round(shapiro_res$p.value, 4), if (nrow(sub_df) >= 30) ", effectif total n \u2265 30 robuste" else "", ")."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 D\u00e9viation de normalit\u00e9 (Shapiro-Wilk sur r\u00e9sidus p = ", round(shapiro_res$p.value, 4), " < 0.05). Kruskal-Wallis est recommand\u00e9 si les effectifs sont r\u00e9duits."))
          }
        } else {
          "Effectif analys\u00e9."
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Homog\u00e9n\u00e9it\u00e9 des variances (Homoc\u00e9dasticit\u00e9) : "),
        if (!is.null(bartlett_res)) {
          if (bartlett_res$p.value >= 0.05) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Bartlett p = ", round(bartlett_res$p.value, 4), " \u2265 0.05 : variances homog\u00e8nes)."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 Variances h\u00e9t\u00e9rog\u00e8nes (test de Bartlett p = ", round(bartlett_res$p.value, 4), " < 0.05). Privil\u00e9giez l'ANOVA de Welch ou Kruskal-Wallis."))
          }
        } else {
          "Test de variances non calculable."
        }
      )
    )

    f_p_val <- tryCatch(summary(multi_state$result)[[1]][["Pr(>F)"]][1], error = function(e) NA_real_)
    decision_txt <- if (!is.na(f_p_val) && f_p_val < alpha_val) {
      paste0(
        "P-value globale = ", format.pval(f_p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette H0 : au moins deux groupes pr\u00e9sentent une moyenne significativement diff\u00e9rente. ",
        "Consultez les comparaisons par paires (tests Post-Hoc de Tukey HSD) pour identifier pr\u00e9cis\u00e9ment quelles modalit\u00e9s diff\u00e8rent."
      )
    } else {
      paste0(
        "P-value globale = ", format.pval(f_p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne peut pas rejeter H0. L'effet du facteur n'atteint pas le seuil de significativit\u00e9."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "L'ANOVA d\u00e9compose la variance totale en variance inter-groupes (due au facteur) et variance intra-groupes (r\u00e9siduelle)."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    extra_block <- if (isTRUE(multi_state$post_hoc)) {
      shiny::div(
        class = "p-3 rounded bg-light border",
        shiny::tags$h6(class = "fw-bold text-dark mb-2", "4. Proc\u00e9dure Post-Hoc (Tukey HSD) :"),
        shiny::tags$p(
          class = "small text-secondary mb-0",
          "Le test HSD de Tukey (Honestly Significant Difference) contr\u00f4le le taux d'erreur familial (Family-Wise Error Rate) pour \u00e9viter l'inflation du risque de faux positifs d\u00fb \u00e0 la multiplicit\u00e9 des comparaisons par paires."
        )
      )
    } else NULL

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content, extra_block))

  } else {
    # Kruskal-Wallis
    h0_txt <- paste0("Les distributions de '", var_y, "' sont identiques dans tous les ", k_groups, " groupes de population.")
    h1_txt <- "Au moins un groupe pr\u00e9sente une distribution stochastiquement d\u00e9cal\u00e9e par rapport aux autres."

    kw_p_val <- tryCatch(multi_state$result$p.value, error = function(e) NA_real_)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Variable ordinale ou continue : "),
        "Kruskal-Wallis s'appuie sur le classement par rangs de l'ensemble des observations. ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance des groupes : "),
        "Les groupes doivent \u00eatre mutuellement exclusifs et ind\u00e9pendants."
      ),
      shiny::tags$li(
        shiny::tags$strong("Interpr\u00e9tation : "),
        "Si les distributions ont des formes comparables, le test compare les m\u00e9dianes des groupes. Sinon, il compare les rangs moyens."
      )
    )

    decision_txt <- if (!is.na(kw_p_val) && kw_p_val < alpha_val) {
      paste0("P-value = ", format.pval(kw_p_val, digits = 4), " < \u03b1 (", alpha_val, "). On rejette H0. Des diff\u00e9rences significatives existent entre les modalit\u00e9s.")
    } else {
      paste0("P-value = ", format.pval(kw_p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). On ne rejette pas H0.")
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Alternative non-param\u00e9trique \u00e0 l'ANOVA pour comparer 3 groupes ou plus."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))
  }
}

# =========================================================================
# 4. P\u00c9DAGOGIE : CONTINGENCE & QUALITATIF (CHI-2, FISHER, MCNEMAR)
# =========================================================================

#' P\u00e9dagogie pour les tests de contingence
#'
#' @param cont_state Objet reactiveValues de l'\u00e9tat de contingence.
#' @param df Dataframe actif.
#' @return Un objet \code{shiny.tag}.
#' @noRd
ramses_pedagogy_cont <- function(cont_state, df) {
  if (!isTRUE(cont_state$calculated) || is.null(cont_state$result) || is.null(df)) {
    return(ramses_pedagogy_waiting_ui("l'analyse de contingence"))
  }

  test_type <- cont_state$test
  alpha_val <- as.numeric(cont_state$alpha)
  var_row <- cont_state$var_row
  var_col <- cont_state$var_col
  tab <- cont_state$tab

  p_val <- tryCatch(as.numeric(cont_state$result$p.value), error = function(e) NA_real_)
  is_significant <- !is.na(p_val) && (p_val < alpha_val)

  if (test_type == "chisq") {
    h0_txt <- paste0("Les variables '", var_row, "' et '", var_col, "' sont ind\u00e9pendantes dans la population (P(A \u2229 B) = P(A) \u00d7 P(B)).")
    h1_txt <- paste0("Les deux variables sont associ\u00e9es (d\u00e9pendance statistique entre les modalit\u00e9s).")

    # Calcul des effectifs th\u00e9oriques attendus sous H0
    exp_counts <- tryCatch(cont_state$result$expected, error = function(e) NULL)
    min_exp <- if (!is.null(exp_counts)) min(exp_counts) else NA_real_
    pct_less_5 <- if (!is.null(exp_counts)) mean(exp_counts < 5) * 100 else NA_real_

    # R\u00e8gle de Cochran : Aucun effectif attendu < 1 et au maximum 20% des effectifs < 5
    cochran_ok <- !is.na(min_exp) && !is.na(pct_less_5) && (min_exp >= 1) && (pct_less_5 <= 20)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Ind\u00e9pendance des observations : "),
        "Chaque sujet n'appartient qu'\u00e0 une seule cellule du tableau de contingence. ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Condition des effectifs th\u00e9oriques attendus (R\u00e8gle de Cochran) : "),
        if (cochran_ok) {
          shiny::tags$span(
            class = "text-success fw-semibold",
            paste0("\u2714 R\u00e8gle satisfaite : effectif th\u00e9orique minimal = ", round(min_exp, 2), " (\u2265 1), et ", round(pct_less_5, 1), "% des cases ont un effectif < 5 (\u2264 20%). L'approximation asymptotique du Chi-deux est pleinement valide.")
          )
        } else {
          shiny::tags$span(
            class = "text-danger fw-semibold",
            paste0("\u26a0 R\u00e8gle non satisfaite : effectif th\u00e9orique minimal = ", round(min_exp, 2), if (min_exp < 1) " (< 1)" else "", ", et ", round(pct_less_5, 1), "% des cases sont < 5 (> 20%). L'approximation du Chi-deux est fragile : le Test exact de Fisher est fortement recommand\u00e9.")
          )
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Correction de continuit\u00e9 de Yates : "),
        if (isTRUE(cont_state$correct)) {
          "Appliqu\u00e9e (recommand\u00e9e pour les tables 2x2 afin de r\u00e9duire le risque de faux positifs)."
        } else {
          "Non appliqu\u00e9e (formulation classique de Pearson)."
        }
      )
    )

    decision_txt <- if (is_significant) {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette l'hypoth\u00e8se nulle H0. Il existe une liaison statistiquement significative entre '", var_row, "' et '", var_col, "'. ",
        "Consultez les r\u00e9sidus standardis\u00e9s pour d\u00e9celer les attractions ou r\u00e9pulsions entre modalit\u00e9s."
      )
    } else {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne peut pas rejeter l'ind\u00e9pendance H0. La r\u00e9partition observ\u00e9e est compatible avec un mod\u00e8le d'ind\u00e9pendance statistique."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Compare les effectifs observ\u00e9s aux effectifs qui seraient attendus si les deux variables \u00e9taient ind\u00e9pendantes."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else if (test_type == "fisher") {
    h0_txt <- paste0("Les variables '", var_row, "' et '", var_col, "' sont ind\u00e9pendantes (pour un tableau 2x2, Odds Ratio = 1).")
    h1_txt <- "Les deux variables sont associ\u00e9es (Odds Ratio diff\u00e9rent de 1)."

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Test exact combinatoire : "),
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Sans approximation asymptotique : "),
        "Le test exact de Fisher calcule la probabilit\u00e9 exacte sous loi hyperg\u00e9om\u00e9trique, conditionnellement aux marges fix\u00e9es du tableau. Il reste strictement exact quel que soit l'effectif, m\u00eame inf\u00e9rieur \u00e0 5."
      )
    )

    decision_txt <- if (is_significant) {
      paste0("P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). On rejette H0 : association significative entre les deux facteurs.")
    } else {
      paste0("P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). Pas d'association significative d\u00e9montr\u00e9e.")
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "M\u00e9thode exacte de r\u00e9f\u00e9rence pour les petits effectifs ou cellules rares."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else {
    # McNemar
    h0_txt <- "Les proportions marginales des deux conditions appari\u00e9es sont identiques (p_01 = p_10)."
    h1_txt <- "Les proportions marginales diff\u00e8rent significativement (changement d'\u00e9tat asym\u00e9trique)."

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Donn\u00e9es appari\u00e9es : "),
        "Tableau 2x2 de mesures r\u00e9p\u00e9t\u00e9es ou paires appari\u00e9es (ex: avant/apr\u00e8s). ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Nombre de paires discordantes (b + c) : "),
        "L'approximation asymptotique du chi-deux suppose id\u00e9alement au moins 10 paires discordantes."
      )
    )

    guide_content <- shiny::tags$p("Test de McNemar pour l'\u00e9volution d'une variable binaire appari\u00e9e.")
    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))
  }
}

# =========================================================================
# 5. P\u00c9DAGOGIE : TESTS DE CORR\u00c9LATION (PEARSON, SPEARMAN, KENDALL)
# =========================================================================

#' P\u00e9dagogie pour les tests de corr\u00e9lation
#'
#' @param cor_state Objet reactiveValues de l'\u00e9tat de corr\u00e9lation.
#' @param df Dataframe actif.
#' @return Un objet \code{shiny.tag}.
#' @noRd
ramses_pedagogy_cor <- function(cor_state, df) {
  if (!isTRUE(cor_state$calculated) || is.null(cor_state$result) || is.null(df)) {
    return(ramses_pedagogy_waiting_ui("l'analyse de corr\u00e9lation"))
  }

  method <- cor_state$method
  alt <- cor_state$alternative
  alpha_val <- as.numeric(cor_state$alpha)
  var_x <- cor_state$var_x
  var_y <- cor_state$var_y

  sub_df <- df[!is.na(df[[var_x]]) & !is.na(df[[var_y]]), ]
  n_obs <- nrow(sub_df)

  p_val <- tryCatch(as.numeric(cor_state$result$p.value), error = function(e) NA_real_)
  is_significant <- !is.na(p_val) && (p_val < alpha_val)

  if (method == "pearson") {
    h0_txt <- paste0("La corr\u00e9lation lin\u00e9aire dans la population entre '", var_x, "' et '", var_y, "' est nulle (\u03c1 = 0).")
    h1_txt <- switch(
      alt,
      "two.sided" = paste0("La corr\u00e9lation lin\u00e9aire est diff\u00e9rente de z\u00e9ro (\u03c1 \u2260 0)."),
      "greater" = paste0("Il existe une corr\u00e9lation lin\u00e9aire positive (\u03c1 > 0)."),
      "less" = paste0("Il existe une corr\u00e9lation lin\u00e9aire n\u00e9gative (\u03c1 < 0).")
    )

    shapiro_x <- if (n_obs >= 3 && n_obs <= 5000) tryCatch(stats::shapiro.test(sub_df[[var_x]]), error = function(e) NULL) else NULL
    shapiro_y <- if (n_obs >= 3 && n_obs <= 5000) tryCatch(stats::shapiro.test(sub_df[[var_y]]), error = function(e) NULL) else NULL

    norm_both_ok <- !is.null(shapiro_x) && !is.null(shapiro_y) && (shapiro_x$p.value >= 0.05) && (shapiro_y$p.value >= 0.05)

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Variables quantitatives continues : "),
        paste0("'", var_x, "' et '", var_y, "' (n = ", n_obs, " observations compl\u00e8tes). "),
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 bivari\u00e9e (approch\u00e9e par la normalit\u00e9 univari\u00e9e) : "),
        if (norm_both_ok || n_obs >= 30) {
          shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (p_shapiro(X) = ", if (!is.null(shapiro_x)) round(shapiro_x$p.value, 4) else "N/A", ", p_shapiro(Y) = ", if (!is.null(shapiro_y)) round(shapiro_y$p.value, 4) else "N/A", if (n_obs >= 30) ", n \u2265 30 robuste" else "", ")."))
        } else {
          shiny::tags$span(class = "text-warning fw-semibold", "\u26a0 Normalit\u00e9 contest\u00e9e sur un petit effectif : le coefficient de rangs de Spearman est recommand\u00e9.")
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Lin\u00e9arit\u00e9 de la relation : "),
        "Le coefficient de Pearson mesure uniquement la force d'une association LIN\u00c9AIRE. V\u00e9rifiez le nuage de points pour d\u00e9celer d'\u00e9ventuelles relations curvilignes ou valeurs atypiques influentes."
      )
    )

    decision_txt <- if (is_significant) {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). ",
        "On rejette H0. La corr\u00e9lation lin\u00e9aire observ\u00e9e est statistiquement significative."
      )
    } else {
      paste0(
        "P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). ",
        "On ne peut pas rejeter H0. L'association n'atteint pas le seuil de significativit\u00e9."
      )
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Le r de Pearson varie de -1 (\u00e9volution inverse parfaite) \u00e0 +1 (coh\u00e9rence lin\u00e9aire parfaite), avec 0 indiquant l'absence de lien lin\u00e9aire."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else if (method == "spearman") {
    h0_txt <- paste0("Il n'existe pas d'association monotone dans la population entre '", var_x, "' et '", var_y, "' (\u03c1_s = 0).")
    h1_txt <- switch(
      alt,
      "two.sided" = "Il existe une association monotone entre les deux variables (\u03c1_s \u2260 0).",
      "greater" = "Il existe une association monotone croissante (\u03c1_s > 0).",
      "less" = "Il existe une association monotone d\u00e9croissante (\u03c1_s < 0)."
    )

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Donn\u00e9es ordinales ou quantitatives : "),
        "Repose sur les rangs des donn\u00e9es sans exiger de normalit\u00e9 ni de lin\u00e9arit\u00e9 stricte. ",
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("Robustesse aux valeurs extr\u00eames : "),
        "Le rho de Spearman n'est pas fauss\u00e9 par la pr\u00e9sence de points aberrants isol\u00e9s."
      )
    )

    decision_txt <- if (is_significant) {
      paste0("P-value = ", format.pval(p_val, digits = 4), " < \u03b1 (", alpha_val, "). On rejette H0 : pr\u00e9sence d'une association monotone significative.")
    } else {
      paste0("P-value = ", format.pval(p_val, digits = 4), " \u2265 \u03b1 (", alpha_val, "). Pas d'association monotone d\u00e9montr\u00e9e.")
    }

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Mesure si l'augmentation de X s'accompagne de mani\u00e8re g\u00e9n\u00e9rale d'une augmentation (ou diminution) de Y."),
      shiny::tags$p(class = "mb-0 fw-semibold", decision_txt)
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else {
    # Kendall
    h0_txt <- paste0("Ind\u00e9pendance ou concordance nulle entre les rangs des deux variables (\u03c4 = 0).")
    h1_txt <- "Concordance ou discordance significative des rangs (\u03c4 \u2260 0)."
    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Concordance de paires : "),
        "M\u00e9thode id\u00e9ale pour les petits \u00e9chantillons ou lorsqu'il existe un nombre important d'ex aequo."
      )
    )
    guide_content <- shiny::tags$p("Le tau de Kendall quantifie la diff\u00e9rence entre paires concordantes et discordantes.")
    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))
  }
}

# =========================================================================
# 6. P\u00c9DAGOGIE : R\u00c9GRESSIONS & MOD\u00c9LISATION (LM & GLM LOGISTIQUE)
# =========================================================================

#' P\u00e9dagogie pour les r\u00e9gressions lin\u00e9aire et logistique
#'
#' @param reg_state Objet reactiveValues de l'\u00e9tat de mod\u00e9lisation.
#' @param df Dataframe actif.
#' @return Un objet \code{shiny.tag}.
#' @noRd
ramses_pedagogy_reg <- function(reg_state, df) {
  if (!isTRUE(reg_state$calculated) || is.null(reg_state$model) || is.null(df)) {
    return(ramses_pedagogy_waiting_ui("l'ajustement du mod\u00e8le de r\u00e9gression"))
  }

  mod <- if (!is.null(reg_state$model)) reg_state$model else NULL
  var_y <- reg_state$var_y
  vars_x <- reg_state$vars_x
  alpha_val <- as.numeric(reg_state$alpha)

  model_type <- if (!is.null(reg_state$model_type) && length(reg_state$model_type) > 0 && nzchar(reg_state$model_type)) {
    reg_state$model_type
  } else if (!is.null(mod) && inherits(mod, "glm")) {
    "logistic"
  } else {
    "linear"
  }

  if (identical(model_type, "linear")) {
    p_pred <- length(vars_x)
    h0_txt <- paste0(
      "Hypoth\u00e8se globale (Test F) : Aucun des pr\u00e9dicteurs n'explique la variabilit\u00e9 de '", var_y,
      "' (\u03b2_1 = \u03b2_2 = ... = \u03b2_p = 0, R\u00b2 = 0)."
    )
    h1_txt <- "Au moins un des pr\u00e9dicteurs a un coefficient non nul et contribue significativement \u00e0 l'explication de Y."

    resids <- stats::residuals(mod)
    n_resids <- length(resids)
    shapiro_res <- if (n_resids >= 3 && n_resids <= 5000) tryCatch(stats::shapiro.test(resids), error = function(e) NULL) else NULL

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong(paste0("Rapport d'effectif (n = ", n_resids, ", p = ", p_pred, " pr\u00e9dicteurs) : ")),
        if (n_resids >= 15 * p_pred) {
          shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Puissance satisfaisante (\u2265 15 observations par pr\u00e9dicteur)."))
        } else {
          shiny::tags$span(class = "text-warning fw-semibold", paste0("\u26a0 Effectif mod\u00e9r\u00e9 : risque potentiel de sur-ajustement (overfitting)."))
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Normalit\u00e9 des r\u00e9sidus : "),
        if (!is.null(shapiro_res)) {
          if (shapiro_res$p.value >= 0.05 || n_resids >= 30) {
            shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Valid\u00e9 (test de Shapiro-Wilk sur r\u00e9sidus p = ", round(shapiro_res$p.value, 4), if (n_resids >= 30) ", n \u2265 30 robuste" else "", ")."))
          } else {
            shiny::tags$span(class = "text-danger fw-semibold", paste0("\u26a0 D\u00e9viation de normalit\u00e9 des r\u00e9sidus (p = ", round(shapiro_res$p.value, 4), " < 0.05). Les intervalles de confiance doivent \u00eatre interpr\u00e9t\u00e9s avec prudence."))
          }
        } else {
          "R\u00e9sidus analys\u00e9s."
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Homosc\u00e9dasticit\u00e9 & Lin\u00e9arit\u00e9 : "),
        "La variance des r\u00e9sidus doit \u00eatre constante sur toute la plage des valeurs pr\u00e9dites. Consultez l'onglet graphique diagnostique (R\u00e9sidus vs Valeurs ajust\u00e9es)."
      )
    )

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1",
        "Pour chaque coefficient individuel \u03b2_j, le test t \u00e9value si la variable apporte une contribution unique significative au mod\u00e8le en contr\u00f4lant toutes les autres variables explicatives."
      ),
      shiny::tags$p(class = "mb-0",
        "Le coefficient de d\u00e9termination R\u00b2 repr\u00e9sente la proportion de variance de Y expliqu\u00e9e par le mod\u00e8le."
      )
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))

  } else {
    # R\u00e9gression logistique binaire
    h0_txt <- paste0("Aucun pr\u00e9dicteur n'am\u00e9liore la mod\u00e9lisation de l'\u00e9v\u00e9nement par rapport au mod\u00e8le nul (\u03b2_1 = ... = \u03b2_p = 0, tous les Odds Ratios = 1).")
    h1_txt <- "Au moins un des pr\u00e9dicteurs modifie significativement les odds (cotes) de survenue de l'\u00e9v\u00e9nement."

    y_vals <- mod$y
    n_events <- sum(y_vals == 1)
    n_nonevents <- sum(y_vals == 0)
    min_event <- min(n_events, n_nonevents)
    p_pred <- length(vars_x)
    epv <- if (p_pred > 0) min_event / p_pred else 0

    cond_items <- list(
      shiny::tags$li(
        shiny::tags$strong("Nature de la variable cible : "),
        paste0("Variable d\u00e9pendante binaire '", var_y, "' (", n_events, " succ\u00e8s / ", n_nonevents, " \u00e9checs). "),
        shiny::tags$span(class = "text-success fw-semibold", "\u2714 Valid\u00e9")
      ),
      shiny::tags$li(
        shiny::tags$strong("\u00c9v\u00e9nements par Variable (R\u00e8gle empirique EPV de prudence) : "),
        if (epv >= 10) {
          shiny::tags$span(class = "text-success fw-semibold", paste0("\u2714 Ratio satisfaisant : EPV = ", round(epv, 1), " \u2265 10 \u00e9v\u00e9nements par pr\u00e9dicteur (stabilit\u00e9 num\u00e9rique convenable)."))
        } else {
          shiny::tags$span(class = "text-warning fw-semibold", paste0("\u26a0 Attention : EPV = ", round(epv, 1), " < 10. R\u00e8gle de prudence sur la stabilit\u00e9 num\u00e9rique et le risque de sur-ajustement (ce n'est pas un test de puissance)."))
        }
      ),
      shiny::tags$li(
        shiny::tags$strong("Interpr\u00e9tation rigoureuse des Odds Ratios (OR = exp(\u03b2)) : "),
        "L'Odds Ratio quantifie une variation relative de cotes (odds = p / (1 - p)). Un OR > 1 indique que l'exposition augmente la cote de l'\u00e9v\u00e9nement, ce qui ne doit pas \u00eatre confondu avec un risque relatif (RR) ni une variation lin\u00e9aire directe de probabilit\u00e9."
      )
    )

    guide_content <- shiny::tags$div(
      shiny::tags$p(class = "mb-1", "Mod\u00e9lise le logarithme de la cote (logit(p) = ln(p / (1 - p))) en fonction d'une combinaison lin\u00e9aire des pr\u00e9dicteurs."),
      shiny::tags$p(class = "mb-0 text-muted extra-small", "Rappel : Odds (Cote) = P / (1 - P). L'Odds Ratio compare les cotes entre deux sous-groupes ou pour un saut unitaire du pr\u00e9dicteur.")
    )

    return(ramses_pedagogy_container(h0_txt, h1_txt, cond_items, guide_content))
  }
}
