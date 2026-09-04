#' Sous-interface : Conditions d'application & Normalité
#'
#' @param id Identifiant de namespace Shiny
#' @return Interface utilisateur (tagList)
#' @noRd
mod_tests_norm_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "row g-3",
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm border h-100",
        bslib::card_header(
          class = "py-2 bg-light d-flex justify-content-between align-items-center",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Normalité & Variances")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_norm_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("norm_test_direct"),
            label = "Test à réaliser :",
            choices = c(
              "Shapiro-Wilk (normalité n <= 5000)" = "shapiro",
              "Kolmogorov-Smirnov (normalité vs pnorm)" = "ks",
              "Test de Bartlett (homogénéité variances normale)" = "bartlett",
              "Test de Fligner-Killeen (homogénéité non-paramétrique)" = "fligner"
            ),
            selected = "shapiro"
          ),
          shiny::selectInput(
            inputId = ns("norm_var_direct"),
            label = "Variable numérique d'intérêt (Y) :",
            choices = NULL
          ),
          shiny::conditionalPanel(
            condition = "input.norm_test_direct == 'bartlett' || input.norm_test_direct == 'fligner'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("norm_group_direct"),
              label = "Variable qualitative de groupe (X) :",
              choices = NULL
            )
          ),
          shiny::selectInput(
            inputId = ns("norm_alpha_direct"),
            label = "Niveau de significativité (alpha) :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_norm"),
            label = "Exécuter & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("norm_status_badge")),
      bslib::navset_card_tab(
        id = ns("norm_results_tabs"),
        bslib::nav_panel(
          title = "Résultats & Inférence",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("norm_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Graphiques d'évaluation",
          bslib::card_header(
            class = "py-2 bg-light d-flex justify-content-between align-items-center",
            shiny::tags$span(shiny::tags$strong("Évaluation graphique")),
            shiny::radioButtons(
              inputId = ns("norm_plot_choice"),
              label = NULL,
              choices = c("Q-Q Plot" = "qq", "Histogramme & Courbe" = "hist"),
              inline = TRUE
            )
          ),
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("norm_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Comparaison 2 Groupes
#'
#' @noRd
mod_tests_two_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "row g-3",
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm border h-100",
        bslib::card_header(
          class = "py-2 bg-light d-flex justify-content-between align-items-center",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Test t de Student & Wilcoxon")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_two_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("two_test_direct"),
            label = "Type de test :",
            choices = c(
              "Échantillon unique (comparaison à une moyenne théorique mu)" = "t_one_sample",
              "Deux échantillons indépendants" = "t_indep",
              "Échantillons appariés" = "t_paired",
              "Test non-paramétrique : Wilcoxon signé (échantillon unique)" = "wilcox_one_sample",
              "Test non-paramétrique : Wilcoxon / Mann-Whitney (indépendant)" = "wilcox_indep",
              "Test non-paramétrique : Wilcoxon signé (apparié)" = "wilcox_paired"
            ),
            selected = "t_indep"
          ),
          shiny::selectInput(
            inputId = ns("two_var_y_direct"),
            label = "Variable quantitative continue (Y) :",
            choices = NULL
          ),
          shiny::conditionalPanel(
            condition = "input.two_test_direct == 't_one_sample' || input.two_test_direct == 'wilcox_one_sample'",
            ns = ns,
            shiny::numericInput(
              inputId = ns("two_mu_val_direct"),
              label = "Moyenne théorique (mu) :",
              value = 0,
              step = 0.5
            )
          ),
          shiny::conditionalPanel(
            condition = "input.two_test_direct == 't_indep' || input.two_test_direct == 'wilcox_indep'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("two_var_group_direct"),
              label = "Variable qualitative de groupe (X) :",
              choices = NULL
            ),
            shiny::uiOutput(ns("two_modalities_selector_direct_ui")),
            shiny::conditionalPanel(
              condition = "input.two_test_direct == 't_indep'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("two_var_equal_direct"),
                label = "Variances égales supposées (Student vs Welch)",
                value = FALSE
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "input.two_test_direct == 't_paired' || input.two_test_direct == 'wilcox_paired'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("two_var_x2_direct"),
              label = "Seconde variable quantitative appariée (Y2) :",
              choices = NULL
            )
          ),
          shiny::selectInput(
            inputId = ns("two_alternative_direct"),
            label = "Hypothèse alternative :",
            choices = c(
              "Bilatérale (différence != 0 / moyenne != mu)" = "two.sided",
              "Unilatérale gauche (moyenne < mu / groupe 1 < groupe 2)" = "less",
              "Unilatérale droite (moyenne > mu / groupe 1 > groupe 2)" = "greater"
            ),
            selected = "two.sided"
          ),
          shiny::selectInput(
            inputId = ns("two_alpha_direct"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_two"),
            label = "Exécuter & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("two_status_badge")),
      bslib::navset_card_tab(
        id = ns("two_results_tabs"),
        bslib::nav_panel(
          title = "Inférence & Décision",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("two_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Visualisation diagnostique",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("two_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Comparaison 3+ Groupes
#'
#' @noRd
mod_tests_multi_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "row g-3",
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm border h-100",
        bslib::card_header(
          class = "py-2 bg-light d-flex justify-content-between align-items-center",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Comparaison 3+ Groupes")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_multi_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("multi_test_direct"),
            label = "Test à effectuer :",
            choices = c(
              "ANOVA à 1 facteur (aov) + Tukey HSD Post-hoc" = "anova",
              "Test de Kruskal-Wallis (non-paramétrique)" = "kruskal"
            ),
            selected = "anova"
          ),
          shiny::selectInput(
            inputId = ns("multi_var_y_direct"),
            label = "Variable quantitative dépendante (Y) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("multi_var_group_direct"),
            label = "Variable de regroupement (Facteur X) :",
            choices = NULL
          ),
          shiny::checkboxInput(
            inputId = ns("multi_posthoc_direct"),
            label = "Calculer les tests Post-Hoc (Tukey HSD / Dunn)",
            value = TRUE
          ),
          shiny::selectInput(
            inputId = ns("multi_alpha_direct"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_multi"),
            label = "Exécuter & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("multi_status_badge")),
      bslib::navset_card_tab(
        id = ns("multi_results_tabs"),
        bslib::nav_panel(
          title = "Tableau ANOVA & Post-Hoc",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("multi_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Distribution par groupe",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("multi_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Analyse de contingence
#'
#' @noRd
mod_tests_cont_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "row g-3",
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm border h-100",
        bslib::card_header(
          class = "py-2 bg-light d-flex justify-content-between align-items-center",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Contingence & Qualitatif")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_cont_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("cont_test_direct"),
            label = "Test d'indépendance / association :",
            choices = c(
              "Test du Chi-deux (chisq.test)" = "chisq",
              "Test exact de Fisher (fisher.test)" = "fisher",
              "Test de McNemar (données appariées)" = "mcnemar"
            ),
            selected = "chisq"
          ),
          shiny::selectInput(
            inputId = ns("cont_var_row_direct"),
            label = "Variable qualitative en ligne (Ligne) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("cont_var_col_direct"),
            label = "Variable qualitative en colonne (Colonne) :",
            choices = NULL
          ),
          shiny::conditionalPanel(
            condition = "input.cont_test_direct == 'chisq'",
            ns = ns,
            shiny::checkboxInput(
              inputId = ns("cont_correct_direct"),
              label = "Correction de continuité de Yates",
              value = TRUE
            )
          ),
          shiny::selectInput(
            inputId = ns("cont_alpha_direct"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_cont"),
            label = "Exécuter & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("cont_status_badge")),
      bslib::navset_card_tab(
        id = ns("cont_results_tabs"),
        bslib::nav_panel(
          title = "Tableau de contingence & Test",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("cont_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Diagramme en barres empilées",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("cont_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Tests de corrélation
#'
#' @noRd
mod_tests_cor_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "row g-3",
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm border h-100",
        bslib::card_header(
          class = "py-2 bg-light d-flex justify-content-between align-items-center",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Tests de Corrélation")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_cor_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("cor_method_direct"),
            label = "Coefficient de corrélation :",
            choices = c(
              "Pearson (paramétrique - relation linéaire)" = "pearson",
              "Spearman (non-paramétrique - rangs)" = "spearman",
              "Kendall (tau - concordance des paires)" = "kendall"
            ),
            selected = "pearson"
          ),
          shiny::selectInput(
            inputId = ns("cor_var_x_direct"),
            label = "Première variable (X) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("cor_var_y_direct"),
            label = "Seconde variable (Y) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("cor_alternative_direct"),
            label = "Hypothèse alternative :",
            choices = c(
              "Bilatérale (corrélation != 0)" = "two.sided",
              "Unilatérale positive (r > 0)" = "greater",
              "Unilatérale négative (r < 0)" = "less"
            ),
            selected = "two.sided"
          ),
          shiny::selectInput(
            inputId = ns("cor_alpha_direct"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_cor"),
            label = "Exécuter & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("cor_status_badge")),
      bslib::navset_card_tab(
        id = ns("cor_results_tabs"),
        bslib::nav_panel(
          title = "Coefficient & Inférence",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("cor_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Nuage de points & Régression",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("cor_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Modélisation
#'
#' @noRd
mod_tests_reg_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "row g-3",
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm border h-100",
        bslib::card_header(
          class = "py-2 bg-light d-flex justify-content-between align-items-center",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Modélisation")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_reg_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("reg_model_direct"),
            label = "Type de régression :",
            choices = c(
              "Régression Linéaire Simple / Multiple (lm)" = "linear",
              "Régression Logistique Binaire (glm binomial)" = "logistic"
            ),
            selected = "linear"
          ),
          shiny::selectInput(
            inputId = ns("reg_var_y_direct"),
            label = "Variable Dépendante (Y) :",
            choices = NULL
          ),
          shiny::selectizeInput(
            inputId = ns("reg_vars_x_direct"),
            label = "Variables Explicatives (X) :",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("reg_alpha_direct"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_reg"),
            label = "Ajuster le modèle",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("reg_status_badge")),
      bslib::navset_card_tab(
        id = ns("reg_results_tabs"),
        bslib::nav_panel(
          title = "Coefficients & Qualité",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("reg_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Diagnostic des résidus",
          bslib::card_header(
            class = "py-2 bg-light d-flex justify-content-between align-items-center",
            shiny::tags$span(shiny::tags$strong("Graphique résiduel")),
            shiny::radioButtons(
              inputId = ns("reg_plot_choice"),
              label = NULL,
              choices = c("Résidus vs Valeurs ajustées" = "rvf", "Q-Q Plot résidus" = "qq"),
              inline = TRUE
            )
          ),
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("reg_plot"), height = "400px")
          )
        )
      )
    )
  )
}

#' @title Interface utilisateur pour le module de tests statistiques et modélisation
#'
#' @description Construit l'interface utilisateur pour les tests d'inférence statistique
#'   et de modélisation (normalité, comparaison 2 groupes, ANOVA/Kruskal-Wallis,
#'   tests d'indépendance du Chi-2 et Fisher, corrélations, régression linéaire et logistique).
#'
#' @param id Identifiant de namespace Shiny.
#' @return Un objet tagList d'interface Shiny (\code{shiny.tag}).
#' @export
mod_tests_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    id = ns("tabs_inferential_tests"),
    title = shiny::div(
      class = "d-flex align-items-center gap-2",
      shiny::tags$span(style = "font-weight: 600;", "Inférence Statistique & Modélisation")
    ),
    bslib::nav_panel(
      title = "Normalité & Variances",
      mod_tests_norm_ui(id)
    ),
    bslib::nav_panel(
      title = "Test t & Wilcoxon (1 et 2 Éch.)",
      mod_tests_two_ui(id)
    ),
    bslib::nav_panel(
      title = "Comparaison 3+ Groupes",
      mod_tests_multi_ui(id)
    ),
    bslib::nav_panel(
      title = "Contingence & Qualitatif",
      mod_tests_cont_ui(id)
    ),
    bslib::nav_panel(
      title = "Corrélations",
      mod_tests_cor_ui(id)
    ),
    bslib::nav_panel(
      title = "Modélisation",
      mod_tests_reg_ui(id)
    )
  )
}

#' @title Logique serveur pour le module de tests statistiques et modélisation
#'
#' @description Exécute les tests statistiques d'hypothèse paramétriques et
#'   non paramétriques, génère les sorties statistiques formatées, produit
#'   les visualisations diagnostiques Plotly et consigne les commandes reproductibles
#'   dans le journal R Markdown.
#'
#' @param id Identifiant de namespace Shiny.
#' @param data_holder reactiveValues contenant \code{df} et \code{name}.
#' @param append_to_rmd Fonction de rappel pour injecter le code dans le journal Rmd.
#' @return Un module serveur Shiny.
#' @export
mod_tests_server <- function(id, data_holder, append_to_rmd) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers d'identification des variables
    get_num_vars <- shiny::reactive({
      df <- data_holder$df
      if (is.null(df) || ncol(df) == 0) return(character(0))
      names(df)[vapply(df, is.numeric, logical(1))]
    })

    get_cat_vars <- shiny::reactive({
      df <- data_holder$df
      if (is.null(df) || ncol(df) == 0) return(character(0))
      names(df)[vapply(df, function(x) is.factor(x) || is.character(x) || is.logical(x) || length(unique(x)) <= 15, logical(1))]
    })

    # =========================================================================
    # ETAT RÉACTIF DES ANALYSES
    # =========================================================================
    norm_state <- shiny::reactiveValues(
      test = "shapiro",
      var = NULL,
      group = NULL,
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      error = NULL
    )

    two_state <- shiny::reactiveValues(
      test = "t_indep",
      var_y = NULL,
      var_group = NULL,
      var_x2 = NULL,
      mu_val = 0,
      selected_modalities = NULL,
      paired = FALSE,
      var_equal = FALSE,
      alternative = "two.sided",
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      error = NULL
    )

    multi_state <- shiny::reactiveValues(
      test = "anova",
      var_y = NULL,
      var_group = NULL,
      post_hoc = TRUE,
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      post_hoc_result = NULL,
      error = NULL
    )

    cont_state <- shiny::reactiveValues(
      test = "chisq",
      var_row = NULL,
      var_col = NULL,
      correct = TRUE,
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      tab = NULL,
      error = NULL
    )

    cor_state <- shiny::reactiveValues(
      method = "pearson",
      var_x = NULL,
      var_y = NULL,
      alternative = "two.sided",
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      error = NULL
    )

    reg_state <- shiny::reactiveValues(
      model_type = "linear",
      var_y = NULL,
      vars_x = NULL,
      alpha = 0.05,
      calculated = FALSE,
      model = NULL,
      error = NULL
    )

    # Mise à jour automatique des choix de variables dans les interfaces directes
    shiny::observe({
      df <- data_holder$df
      if (is.null(df) || nrow(df) == 0) return()
      num_cols <- get_num_vars()
      cat_cols <- get_cat_vars()
      all_cols <- names(df)

      shiny::updateSelectInput(session, "norm_var_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "norm_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)

      shiny::updateSelectInput(session, "two_var_y_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "two_var_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "two_var_x2_direct", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)

      shiny::updateSelectInput(session, "multi_var_y_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "multi_var_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)

      shiny::updateSelectInput(session, "cont_var_row_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "cont_var_col_direct", choices = cat_cols, selected = if (length(cat_cols) > 1) cat_cols[2] else NULL)

      shiny::updateSelectInput(session, "cor_var_x_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "cor_var_y_direct", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)

      shiny::updateSelectInput(session, "reg_var_y_direct", choices = if (input$reg_model_direct == "linear") num_cols else all_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectizeInput(session, "reg_vars_x_direct", choices = all_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)
    })

    # =========================================================================
    # 1. NORMALITÉ & HOMOGÉNÉITÉ DES VARIANCES
    # =========================================================================
    run_norm_analysis <- function(test_type, var_y, group_x, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || !nzchar(var_y)) {
        shiny::showNotification("Veuillez sélectionner une variable valide.", type = "warning")
        return()
      }
      norm_state$test <- test_type
      norm_state$var <- var_y
      norm_state$group <- group_x
      norm_state$alpha <- as.numeric(alpha_val)

      val <- df[[norm_state$var]]
      val <- val[!is.na(val)]

      res <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        if (norm_state$test == "shapiro") {
          res <- stats::shapiro.test(val)
          code_entry <- paste0("# Test de normalité de Shapiro-Wilk\nshapiro.test(", ds_name, "$", norm_state$var, ")")
        } else if (norm_state$test == "ks") {
          res <- stats::ks.test(val, "pnorm", mean = mean(val), sd = stats::sd(val))
          code_entry <- paste0("# Test de Kolmogorov-Smirnov\nks.test(", ds_name, "$", norm_state$var, ", 'pnorm', mean = mean(", ds_name, "$", norm_state$var, ", na.rm = TRUE), sd = sd(", ds_name, "$", norm_state$var, ", na.rm = TRUE))")
        } else if (norm_state$test == "bartlett") {
          req(norm_state$group)
          fml <- as.formula(paste0(norm_state$var, " ~ ", norm_state$group))
          res <- stats::bartlett.test(fml, data = df)
          code_entry <- paste0("# Test d'homogénéité des variances de Bartlett\nbartlett.test(", norm_state$var, " ~ ", norm_state$group, ", data = ", ds_name, ")")
        } else if (norm_state$test == "fligner") {
          req(norm_state$group)
          fml <- as.formula(paste0(norm_state$var, " ~ ", norm_state$group))
          res <- stats::fligner.test(fml, data = df)
          code_entry <- paste0("# Test de Fligner-Killeen\nfligner.test(", norm_state$var, " ~ ", norm_state$group, ", data = ", ds_name, ")")
        }
      }, error = function(e) {
        err <- e$message
      })

      norm_state$result <- res
      norm_state$error <- err
      norm_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Conditions d'application : ", norm_state$test, " (", norm_state$var, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Test de normalité/homogénéité exécuté !", type = "message")
    }

    shiny::observeEvent(input$btn_run_norm, {
      run_norm_analysis(input$norm_test_direct, input$norm_var_direct, input$norm_group_direct, input$norm_alpha_direct)
    })

    shiny::observeEvent(input$btn_open_norm_modal, {
      num_cols <- get_num_vars()
      cat_cols <- get_cat_vars()

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Paramètres : Normalité & Homogénéité des Variances")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_norm"), "Exécuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("norm_test_choice"),
              label = "Test à réaliser :",
              choices = c(
                "Shapiro-Wilk (normalité n <= 5000)" = "shapiro",
                "Kolmogorov-Smirnov (normalité vs pnorm)" = "ks",
                "Test de Bartlett (homogénéité variances normale)" = "bartlett",
                "Test de Fligner-Killeen (homogénéité non-paramétrique)" = "fligner"
              ),
              selected = norm_state$test
            ),
            shiny::selectInput(
              inputId = ns("norm_var_select"),
              label = "Variable numérique d'intérêt (Y) :",
              choices = num_cols,
              selected = norm_state$var
            ),
            shiny::conditionalPanel(
              condition = "input.norm_test_choice == 'bartlett' || input.norm_test_choice == 'fligner'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("norm_group_select"),
                label = "Variable qualitative de groupe (X) :",
                choices = cat_cols,
                selected = norm_state$group
              )
            ),
            shiny::selectInput(
              inputId = ns("norm_alpha_select"),
              label = "Niveau de significativité (alpha) :",
              choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
              selected = as.character(norm_state$alpha)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_norm, {
      shiny::removeModal()
      run_norm_analysis(input$norm_test_choice, input$norm_var_select, input$norm_group_select, input$norm_alpha_select)
    })

    output$norm_status_badge <- shiny::renderUI({
      if (!norm_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Exécuter' pour lancer le test."))
      }
      if (!is.null(norm_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", norm_state$error)))
      }
      p_val <- norm_state$result$p.value
      sig <- p_val < norm_state$alpha
      decision <- if (sig) {
        paste0("Rejet de H0 au seuil alpha = ", norm_state$alpha, " (p = ", format.pval(p_val, digits = 3), ")")
      } else {
        paste0("Non-rejet de H0 au seuil alpha = ", norm_state$alpha, " (p = ", format.pval(p_val, digits = 3), ")")
      }
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-warning" else "alert-success"),
        shiny::tags$span(decision),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("Test : ", norm_state$test))
      )
    })

    output$norm_alpha_badge <- shiny::renderUI({
      shiny::tags$span(class = "badge bg-light text-dark border", paste0("alpha = ", norm_state$alpha))
    })

    output$norm_results_ui <- shiny::renderUI({
      req(norm_state$calculated)
      if (!is.null(norm_state$error)) {
        return(shiny::p(class = "text-danger small", norm_state$error))
      }
      res <- norm_state$result
      p_val <- res$p.value
      stat_val <- unname(res$statistic)
      stat_name <- names(res$statistic)

      is_normal_rejected <- p_val < norm_state$alpha
      interp_text <- if (norm_state$test %in% c("shapiro", "ks")) {
        if (is_normal_rejected) {
          "La p-value est inférieure au seuil critique : l'hypothèse nulle de normalité est rejetée. Les données ne suivent pas une distribution normale standard (envisager un test non paramétrique)."
        } else {
          "La p-value est supérieure au seuil critique : on ne rejette pas l'hypothèse de normalité. Les conditions d'application pour les tests paramétriques semblent satisfaites."
        }
      } else {
        if (is_normal_rejected) {
          "L'hypothèse d'égalité des variances est rejetée (hétéroscédasticité). Utiliser la correction de Welch."
        } else {
          "L'hypothèse d'homogénéité des variances (homoscédasticité) est acceptée."
        }
      }

      shiny::tagList(
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle mb-3",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Méthode"),
              shiny::tags$th("Statistique"),
              shiny::tags$th("ddl (si applicable)"),
              shiny::tags$th("p-value"),
              shiny::tags$th("Décision (alpha)")
            )
          ),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "text-start fw-medium", res$method),
              shiny::tags$td(paste0(stat_name, " = ", round(stat_val, 4))),
              shiny::tags$td(if (!is.null(res$parameter)) round(res$parameter, 2) else "—"),
              shiny::tags$td(class = "fw-bold", format.pval(p_val, digits = 4, eps = 0.0001)),
              shiny::tags$td(
                shiny::tags$span(
                  class = paste0("badge ", if (is_normal_rejected) "bg-danger" else "bg-success"),
                  if (is_normal_rejected) "Significatif (Rejet H0)" else "Non significatif"
                )
              )
            )
          )
        ),
        shiny::div(
          class = "p-2 rounded bg-light border text-secondary small mb-3",
          shiny::tags$strong("Interprétation : "),
          interp_text
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher la sortie console R brute"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(capture.output(print(res)), collapse = "\n"))
        )
      )
    })

    output$norm_plot <- plotly::renderPlotly({
      df <- data_holder$df
      req(df, norm_state$var)
      vals <- df[[norm_state$var]]
      vals <- vals[!is.na(vals)]

      if (input$norm_plot_choice == "qq") {
        sorted_vals <- sort(vals)
        n <- length(sorted_vals)
        probs <- (1:n - 0.5) / n
        theo_quantiles <- stats::qnorm(probs, mean = mean(vals), sd = stats::sd(vals))

        plotly::plot_ly() %>%
          plotly::add_trace(
            x = theo_quantiles,
            y = sorted_vals,
            type = "scatter",
            mode = "markers",
            name = "Points observés",
            marker = list(color = "#3b82f6", size = 6, opacity = 0.8)
          ) %>%
          plotly::add_lines(
            x = range(theo_quantiles),
            y = range(theo_quantiles),
            name = "Droite théorique normale",
            line = list(color = "#ef4444", dash = "dash", width = 2)
          ) %>%
          plotly::layout(
            title = list(text = paste0("Q-Q Plot Normal : ", norm_state$var), font = list(size = 13)),
            xaxis = list(title = "Quantiles théoriques (Normale)"),
            yaxis = list(title = "Quantiles observés"),
            hovermode = "closest"
          )
      } else {
        plotly::plot_ly(x = vals, type = "histogram", nbinsx = 25, marker = list(color = "#6366f1", line = list(color = "white", width = 1))) %>%
          plotly::layout(
            title = list(text = paste0("Distribution de ", norm_state$var), font = list(size = 13)),
            xaxis = list(title = norm_state$var),
            yaxis = list(title = "Effectif (Fréquence)")
          )
      }
    })

    # =========================================================================
    # 2. COMPARAISON DE MOYENNES (TEST T DE STUDENT & WILCOXON)
    # =========================================================================
    group_modalities_direct <- shiny::reactive({
      df <- data_holder$df
      var_g <- input$two_var_group_direct
      if (is.null(df) || is.null(var_g) || !(var_g %in% names(df))) return(character(0))
      col_data <- df[[var_g]]
      col_data <- col_data[!is.na(col_data)]
      unique(as.character(col_data))
    })

    output$two_modalities_selector_direct_ui <- shiny::renderUI({
      mods <- group_modalities_direct()
      if (length(mods) > 2) {
        shiny::tagList(
          shiny::div(
            class = "alert alert-warning py-1 px-2 small mb-2",
            paste0("Variable à ", length(mods), " modalités : veuillez en sélectionner exactement 2 pour le test.")
          ),
          shiny::selectizeInput(
            inputId = ns("two_selected_modalities_direct"),
            label = "Modalités à comparer (exactement 2) :",
            choices = mods,
            selected = mods[1:2],
            multiple = TRUE,
            options = list(maxItems = 2, plugins = list("remove_button"))
          ),
          shiny::uiOutput(ns("two_modalities_warning_direct"))
        )
      } else if (length(mods) == 2) {
        shiny::div(
          class = "small text-muted mb-2",
          paste0("2 modalités détectées : ", mods[1], " vs ", mods[2])
        )
      } else if (length(mods) < 2 && length(mods) > 0) {
        shiny::div(
          class = "alert alert-danger py-1 px-2 small mb-2",
          "La variable de groupe sélectionnée possède moins de 2 modalités valides."
        )
      }
    })

    output$two_modalities_warning_direct <- shiny::renderUI({
      sel <- input$two_selected_modalities_direct
      if (!is.null(sel) && length(sel) != 2) {
        shiny::div(
          class = "text-danger small fw-semibold mt-1",
          "Attention : vous devez sélectionner exactement 2 modalités."
        )
      }
    })

    group_modalities_modal <- shiny::reactive({
      df <- data_holder$df
      var_g <- input$two_var_group
      if (is.null(df) || is.null(var_g) || !(var_g %in% names(df))) return(character(0))
      col_data <- df[[var_g]]
      col_data <- col_data[!is.na(col_data)]
      unique(as.character(col_data))
    })

    output$two_modalities_selector_modal_ui <- shiny::renderUI({
      mods <- group_modalities_modal()
      if (length(mods) > 2) {
        shiny::tagList(
          shiny::div(
            class = "alert alert-warning py-1 px-2 small mb-2",
            paste0("Variable à ", length(mods), " modalités : veuillez en sélectionner exactement 2 pour le test.")
          ),
          shiny::selectizeInput(
            inputId = ns("two_selected_modalities_modal"),
            label = "Modalités à comparer (exactement 2) :",
            choices = mods,
            selected = if (!is.null(two_state$selected_modalities) && all(two_state$selected_modalities %in% mods)) two_state$selected_modalities else mods[1:2],
            multiple = TRUE,
            options = list(maxItems = 2, plugins = list("remove_button"))
          ),
          shiny::uiOutput(ns("two_modalities_warning_modal"))
        )
      } else if (length(mods) == 2) {
        shiny::div(
          class = "small text-muted mb-2",
          paste0("2 modalités détectées : ", mods[1], " vs ", mods[2])
        )
      } else if (length(mods) < 2 && length(mods) > 0) {
        shiny::div(
          class = "alert alert-danger py-1 px-2 small mb-2",
          "La variable de groupe sélectionnée possède moins de 2 modalités valides."
        )
      }
    })

    output$two_modalities_warning_modal <- shiny::renderUI({
      sel <- input$two_selected_modalities_modal
      if (!is.null(sel) && length(sel) != 2) {
        shiny::div(
          class = "text-danger small fw-semibold mt-1",
          "Attention : vous devez sélectionner exactement 2 modalités."
        )
      }
    })

    run_two_analysis <- function(test_type, var_y, var_group, var_x2, mu_val, selected_modalities, alt, var_eq, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || !nzchar(var_y)) {
        shiny::showNotification("Veuillez sélectionner une variable valide.", type = "warning")
        return()
      }
      two_state$test <- test_type
      two_state$var_y <- var_y
      two_state$var_group <- var_group
      two_state$var_x2 <- var_x2
      two_state$mu_val <- if (!is.null(mu_val) && !is.na(as.numeric(mu_val))) as.numeric(mu_val) else 0
      two_state$selected_modalities <- selected_modalities
      two_state$alternative <- alt
      two_state$var_equal <- isTRUE(var_eq)
      two_state$alpha <- as.numeric(alpha_val)

      res <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        if (two_state$test == "t_one_sample") {
          req(two_state$var_y)
          y_vals <- df[[two_state$var_y]]
          y_vals <- y_vals[!is.na(y_vals)]
          res <- stats::t.test(
            y_vals,
            mu = two_state$mu_val,
            alternative = two_state$alternative,
            conf.level = 1 - two_state$alpha
          )
          code_entry <- paste0(
            "# Test t de Student à échantillon unique (comparaison à mu = ", two_state$mu_val, ")\n",
            "t.test(", ds_name, "$", two_state$var_y, ", mu = ", two_state$mu_val,
            ", alternative = '", two_state$alternative, "')"
          )

        } else if (two_state$test == "wilcox_one_sample") {
          req(two_state$var_y)
          y_vals <- df[[two_state$var_y]]
          y_vals <- y_vals[!is.na(y_vals)]
          res <- stats::wilcox.test(
            y_vals,
            mu = two_state$mu_val,
            alternative = two_state$alternative,
            conf.int = TRUE
          )
          code_entry <- paste0(
            "# Test de Wilcoxon signé à échantillon unique (comparaison à mu = ", two_state$mu_val, ")\n",
            "wilcox.test(", ds_name, "$", two_state$var_y, ", mu = ", two_state$mu_val,
            ", alternative = '", two_state$alternative, "')"
          )

        } else if (two_state$test == "t_indep") {
          req(two_state$var_group)
          all_grps <- unique(as.character(df[[two_state$var_group]]))
          all_grps <- all_grps[!is.na(all_grps)]

          if (length(all_grps) > 2) {
            if (is.null(two_state$selected_modalities) || length(two_state$selected_modalities) != 2) {
              stop("Veuillez sélectionner exactement 2 modalités à comparer pour la variable de groupe.")
            }
            mods <- two_state$selected_modalities
            sub_df <- subset(df, df[[two_state$var_group]] %in% mods)
            fml <- as.formula(paste0(two_state$var_y, " ~ ", two_state$var_group))
            res <- stats::t.test(
              fml,
              data = sub_df,
              var.equal = two_state$var_equal,
              alternative = two_state$alternative,
              conf.level = 1 - two_state$alpha
            )
            code_entry <- paste0(
              "# Sous-ensemble filtré sur les 2 modalités à comparer\n",
              "data_sub <- subset(", ds_name, ", ", two_state$var_group, " %in% c(\"", mods[1], "\", \"", mods[2], "\"))\n",
              "# Test t de Student pour 2 échantillons indépendants\n",
              "t.test(", two_state$var_y, " ~ ", two_state$var_group, ", data = data_sub",
              ", var.equal = ", two_state$var_equal, ", alternative = '", two_state$alternative, "')"
            )
          } else {
            sub_df <- df[!is.na(df[[two_state$var_group]]), ]
            fml <- as.formula(paste0(two_state$var_y, " ~ ", two_state$var_group))
            res <- stats::t.test(
              fml,
              data = sub_df,
              var.equal = two_state$var_equal,
              alternative = two_state$alternative,
              conf.level = 1 - two_state$alpha
            )
            code_entry <- paste0(
              "# Test t de Student pour 2 échantillons indépendants\n",
              "t.test(", two_state$var_y, " ~ ", two_state$var_group, ", data = ", ds_name,
              ", var.equal = ", two_state$var_equal, ", alternative = '", two_state$alternative, "')"
            )
          }

        } else if (two_state$test == "wilcox_indep") {
          req(two_state$var_group)
          all_grps <- unique(as.character(df[[two_state$var_group]]))
          all_grps <- all_grps[!is.na(all_grps)]

          if (length(all_grps) > 2) {
            if (is.null(two_state$selected_modalities) || length(two_state$selected_modalities) != 2) {
              stop("Veuillez sélectionner exactement 2 modalités à comparer pour la variable de groupe.")
            }
            mods <- two_state$selected_modalities
            sub_df <- subset(df, df[[two_state$var_group]] %in% mods)
            fml <- as.formula(paste0(two_state$var_y, " ~ ", two_state$var_group))
            res <- stats::wilcox.test(
              fml,
              data = sub_df,
              alternative = two_state$alternative,
              conf.int = TRUE
            )
            code_entry <- paste0(
              "# Sous-ensemble filtré sur les 2 modalités à comparer\n",
              "data_sub <- subset(", ds_name, ", ", two_state$var_group, " %in% c(\"", mods[1], "\", \"", mods[2], "\"))\n",
              "# Test de Wilcoxon / Mann-Whitney U pour 2 échantillons indépendants\n",
              "wilcox.test(", two_state$var_y, " ~ ", two_state$var_group, ", data = data_sub",
              ", alternative = '", two_state$alternative, "')"
            )
          } else {
            sub_df <- df[!is.na(df[[two_state$var_group]]), ]
            fml <- as.formula(paste0(two_state$var_y, " ~ ", two_state$var_group))
            res <- stats::wilcox.test(
              fml,
              data = sub_df,
              alternative = two_state$alternative,
              conf.int = TRUE
            )
            code_entry <- paste0(
              "# Test de Wilcoxon / Mann-Whitney U pour 2 échantillons indépendants\n",
              "wilcox.test(", two_state$var_y, " ~ ", two_state$var_group, ", data = ", ds_name,
              ", alternative = '", two_state$alternative, "')"
            )
          }

        } else if (two_state$test == "t_paired") {
          req(two_state$var_x2)
          res <- stats::t.test(
            df[[two_state$var_y]],
            df[[two_state$var_x2]],
            paired = TRUE,
            alternative = two_state$alternative,
            conf.level = 1 - two_state$alpha
          )
          code_entry <- paste0(
            "# Test t de Student pour séries appariées\n",
            "t.test(", ds_name, "$", two_state$var_y, ", ", ds_name, "$", two_state$var_x2,
            ", paired = TRUE, alternative = '", two_state$alternative, "')"
          )

        } else if (two_state$test == "wilcox_paired") {
          req(two_state$var_x2)
          res <- stats::wilcox.test(
            df[[two_state$var_y]],
            df[[two_state$var_x2]],
            paired = TRUE,
            alternative = two_state$alternative
          )
          code_entry <- paste0(
            "# Test de Wilcoxon signé pour séries appariées\n",
            "wilcox.test(", ds_name, "$", two_state$var_y, ", ", ds_name, "$", two_state$var_x2,
            ", paired = TRUE, alternative = '", two_state$alternative, "')"
          )
        }
      }, error = function(e) {
        err <<- e$message
      })

      two_state$result <- res
      two_state$error <- err
      two_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Test t / Comparaison : ", two_state$test),
          code = code_entry
        )
      }
      if (is.null(err)) {
        shiny::showNotification("Test statistique exécuté avec succès !", type = "message")
      } else {
        shiny::showNotification(paste0("Erreur : ", err), type = "error")
      }
    }

    shiny::observeEvent(input$btn_run_two, {
      sel_mods <- if (input$two_test_direct %in% c("t_indep", "wilcox_indep")) {
        input$two_selected_modalities_direct
      } else {
        NULL
      }

      run_two_analysis(
        test_type = input$two_test_direct,
        var_y = input$two_var_y_direct,
        var_group = input$two_var_group_direct,
        var_x2 = input$two_var_x2_direct,
        mu_val = input$two_mu_val_direct,
        selected_modalities = sel_mods,
        alt = input$two_alternative_direct,
        var_eq = input$two_var_equal_direct,
        alpha_val = input$two_alpha_direct
      )
    })

    shiny::observeEvent(input$btn_open_two_modal, {
      num_cols <- get_num_vars()
      cat_cols <- get_cat_vars()

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Paramètres : Test t & Wilcoxon")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_two"), "Exécuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("two_test_choice"),
              label = "Type de test :",
              choices = c(
                "Échantillon unique (comparaison à une moyenne théorique mu)" = "t_one_sample",
                "Deux échantillons indépendants" = "t_indep",
                "Échantillons appariés" = "t_paired",
                "Test non-paramétrique : Wilcoxon signé (échantillon unique)" = "wilcox_one_sample",
                "Test non-paramétrique : Wilcoxon / Mann-Whitney (indépendant)" = "wilcox_indep",
                "Test non-paramétrique : Wilcoxon signé (apparié)" = "wilcox_paired"
              ),
              selected = two_state$test
            ),
            shiny::selectInput(
              inputId = ns("two_var_y"),
              label = "Variable quantitative continue (Y) :",
              choices = num_cols,
              selected = two_state$var_y
            ),
            shiny::conditionalPanel(
              condition = "input.two_test_choice == 't_one_sample' || input.two_test_choice == 'wilcox_one_sample'",
              ns = ns,
              shiny::numericInput(
                inputId = ns("two_mu_val"),
                label = "Moyenne théorique (mu) :",
                value = two_state$mu_val,
                step = 0.5
              )
            ),
            shiny::conditionalPanel(
              condition = "input.two_test_choice == 't_indep' || input.two_test_choice == 'wilcox_indep'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("two_var_group"),
                label = "Variable qualitative de groupe (X) :",
                choices = cat_cols,
                selected = two_state$var_group
              ),
              shiny::uiOutput(ns("two_modalities_selector_modal_ui")),
              shiny::conditionalPanel(
                condition = "input.two_test_choice == 't_indep'",
                ns = ns,
                shiny::checkboxInput(
                  inputId = ns("two_var_equal"),
                  label = "Supposer l'égalité des variances (Student standard au lieu de Welch)",
                  value = two_state$var_equal
                )
              )
            ),
            shiny::conditionalPanel(
              condition = "input.two_test_choice == 't_paired' || input.two_test_choice == 'wilcox_paired'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("two_var_x2"),
                label = "Seconde variable quantitative appariée (Y2) :",
                choices = num_cols,
                selected = two_state$var_x2
              )
            ),
            shiny::selectInput(
              inputId = ns("two_alternative"),
              label = "Hypothèse alternative :",
              choices = c(
                "Bilatérale (différence != 0 / moyenne != mu)" = "two.sided",
                "Unilatérale gauche (moyenne < mu / groupe 1 < groupe 2)" = "less",
                "Unilatérale droite (moyenne > mu / groupe 1 > groupe 2)" = "greater"
              ),
              selected = two_state$alternative
            ),
            shiny::selectInput(
              inputId = ns("two_alpha_select"),
              label = "Seuil alpha :",
              choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
              selected = as.character(two_state$alpha)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_two, {
      shiny::removeModal()
      sel_mods <- if (input$two_test_choice %in% c("t_indep", "wilcox_indep")) {
        input$two_selected_modalities_modal
      } else {
        NULL
      }

      run_two_analysis(
        test_type = input$two_test_choice,
        var_y = input$two_var_y,
        var_group = input$two_var_group,
        var_x2 = input$two_var_x2,
        mu_val = input$two_mu_val,
        selected_modalities = sel_mods,
        alt = input$two_alternative,
        var_eq = input$two_var_equal,
        alpha_val = input$two_alpha_select
      )
    })

    output$two_status_badge <- shiny::renderUI({
      if (!two_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Sélectionnez vos paramètres et cliquez sur 'Exécuter & Journaliser'."))
      }
      if (!is.null(two_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", two_state$error)))
      }
      p_val <- two_state$result$p.value
      sig <- p_val < two_state$alpha
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) "Différence statistiquement significative (H0 rejetée) !" else "Pas de différence statistiquement significative (H0 conservée)"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 4, eps = 0.0001)))
      )
    })

    output$two_results_ui <- shiny::renderUI({
      req(two_state$calculated)
      if (!is.null(two_state$error)) {
        return(shiny::p(class = "text-danger small", two_state$error))
      }
      res <- two_state$result
      p_val <- res$p.value
      stat_val <- unname(res$statistic)
      stat_name <- names(res$statistic)
      sig <- p_val < two_state$alpha

      is_one_sample <- two_state$test %in% c("t_one_sample", "wilcox_one_sample")
      is_paired <- two_state$test %in% c("t_paired", "wilcox_paired")

      interp <- if (is_one_sample) {
        obs_val <- if (!is.null(res$estimate)) round(res$estimate[1], 3) else "observée"
        if (sig) {
          paste0("Au risque alpha = ", two_state$alpha, ", la moyenne observée de '", two_state$var_y, "' (", obs_val, ") diffère significativement de la moyenne théorique mu = ", two_state$mu_val, " (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). On rejette l'hypothèse nulle.")
        } else {
          paste0("Au risque alpha = ", two_state$alpha, ", la moyenne observée de '", two_state$var_y, "' (", obs_val, ") ne diffère pas significativement de la moyenne théorique mu = ", two_state$mu_val, " (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). On ne peut pas rejeter l'hypothèse nulle.")
        }
      } else if (is_paired) {
        if (sig) {
          paste0("Au risque alpha = ", two_state$alpha, ", la différence moyenne entre les séries appariées '", two_state$var_y, "' et '", two_state$var_x2, "' est statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). On rejette l'hypothèse nulle.")
        } else {
          paste0("Au risque alpha = ", two_state$alpha, ", la différence moyenne entre les séries appariées n'est pas statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). On conserve l'hypothèse nulle.")
        }
      } else {
        if (sig) {
          paste0("Au risque alpha = ", two_state$alpha, ", la différence observée entre les deux groupes de '", two_state$var_group, "' est statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). On rejette l'hypothèse nulle d'égalité.")
        } else {
          paste0("Au risque alpha = ", two_state$alpha, ", la différence observée entre les deux groupes n'est pas statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). On ne peut pas rejeter l'hypothèse nulle.")
        }
      }

      estimate_display <- if (is_one_sample) {
        if (!is.null(res$estimate)) paste0("Moyenne observée = ", round(res$estimate[1], 3), " (vs mu = ", two_state$mu_val, ")") else paste0("mu = ", two_state$mu_val)
      } else if (is_paired) {
        if (!is.null(res$estimate)) paste0("Différence moyenne = ", round(res$estimate[1], 3)) else "—"
      } else {
        if (!is.null(res$estimate) && length(res$estimate) >= 2) {
          paste0(names(res$estimate)[1], " : ", round(res$estimate[1], 3), " | ", names(res$estimate)[2], " : ", round(res$estimate[2], 3))
        } else if (!is.null(res$estimate)) {
          paste0(round(res$estimate[1], 3))
        } else {
          "—"
        }
      }

      shiny::tagList(
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle mb-3",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Méthode"),
              shiny::tags$th("Statistique"),
              shiny::tags$th("ddl"),
              shiny::tags$th("Estimation observée"),
              shiny::tags$th("p-value"),
              shiny::tags$th("Décision")
            )
          ),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "text-start fw-medium", res$method),
              shiny::tags$td(paste0(stat_name, " = ", round(stat_val, 3))),
              shiny::tags$td(if (!is.null(res$parameter)) round(res$parameter, 2) else "—"),
              shiny::tags$td(class = "small", estimate_display),
              shiny::tags$td(class = "fw-bold", format.pval(p_val, digits = 4, eps = 0.0001)),
              shiny::tags$td(
                shiny::tags$span(
                  class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                  if (sig) "Significatif (Rejet H0)" else "Non significatif (Conservation H0)"
                )
              )
            )
          )
        ),
        if (!is.null(res$conf.int)) {
          shiny::div(
            class = "small text-muted mb-2",
            paste0("Intervalle de confiance à ", round((1 - two_state$alpha)*100), "% : [",
                   round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
          )
        },
        shiny::div(
          class = "p-2 rounded bg-light border text-secondary small mb-3",
          shiny::tags$strong("Interprétation statistique : "),
          interp
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher la sortie console R détaillée"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(capture.output(print(res)), collapse = "\n"))
        )
      )
    })

    output$two_plot <- plotly::renderPlotly({
      df <- data_holder$df
      req(df, two_state$var_y)

      if (two_state$test %in% c("t_one_sample", "wilcox_one_sample")) {
        y_vals <- df[[two_state$var_y]]
        y_vals <- y_vals[!is.na(y_vals)]
        mu_val <- two_state$mu_val

        plotly::plot_ly(
          y = y_vals,
          type = "box",
          name = two_state$var_y,
          boxpoints = "all",
          jitter = 0.3,
          pointpos = -1.8,
          marker = list(color = "#1F2937")
        ) %>%
          plotly::add_lines(
            x = c(-0.5, 0.5),
            y = c(mu_val, mu_val),
            name = paste0("mu théorique = ", mu_val),
            line = list(color = "#DC2626", dash = "dash", width = 2),
            inherit = FALSE
          ) %>%
          plotly::layout(
            title = list(text = paste0("Distribution de ", two_state$var_y, " vs moyenne théorique mu = ", mu_val), font = list(size = 13)),
            xaxis = list(title = "", showticklabels = FALSE),
            yaxis = list(title = two_state$var_y)
          )

      } else if (two_state$test %in% c("t_indep", "wilcox_indep") && !is.null(two_state$var_group)) {
        grp_col <- two_state$var_group
        clean_df <- df[!is.na(df[[two_state$var_y]]) & !is.na(df[[grp_col]]), ]

        if (!is.null(two_state$selected_modalities) && length(two_state$selected_modalities) == 2) {
          clean_df <- clean_df[clean_df[[grp_col]] %in% two_state$selected_modalities, ]
        }
        clean_df[[grp_col]] <- droplevels(as.factor(clean_df[[grp_col]]))

        if (two_state$test == "t_indep") {
          form_t <- stats::as.formula(paste0("`", two_state$var_y, "` ~ `", grp_col, "`"))
          t_res <- tryCatch({
            stats::t.test(
              formula = form_t,
              data = clean_df,
              var.equal = isTRUE(two_state$var_equal),
              alternative = two_state$alternative
            )
          }, error = function(e) NULL)

          t_title <- if (!is.null(t_res)) {
            t_stat <- round(unname(t_res$statistic), 2)
            p_val <- t_res$p.value
            p_str <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
            paste0("Test t : t = ", t_stat, ", ", p_str)
          } else {
            paste0("Test t : ", two_state$var_y, " selon ", grp_col)
          }

          p <- ggplot2::ggplot(clean_df, ggplot2::aes(x = .data[[grp_col]], y = .data[[two_state$var_y]], group = .data[[grp_col]])) +
            ggplot2::geom_boxplot(fill = "#F3F4F6", color = "#1F2937", width = 0.4) +
            ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "#4B5563") +
            ggplot2::labs(
              title = t_title,
              x = grp_col,
              y = two_state$var_y
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              text = ggplot2::element_text(family = "Aptos Narrow"),
              plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
              axis.title = ggplot2::element_text(size = 11, color = "#374151"),
              axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
              panel.grid.minor = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
            )

          tryCatch({
            plotly::ggplotly(p)
          }, error = function(e) {
            tryCatch({
              plotly::plotly_build(p)
            }, error = function(e2) {
              plotly::plot_ly(
                data = clean_df,
                x = as.formula(paste0("~", grp_col)),
                y = as.formula(paste0("~", two_state$var_y)),
                type = "box",
                boxpoints = "all",
                jitter = 0.3
              )
            })
          })
        } else {
          w_res <- tryCatch({
            stats::wilcox.test(
              formula = stats::as.formula(paste0("`", two_state$var_y, "` ~ `", grp_col, "`")),
              data = clean_df,
              alternative = two_state$alternative
            )
          }, error = function(e) NULL)

          w_title <- if (!is.null(w_res)) {
            w_stat <- round(unname(w_res$statistic), 2)
            p_val <- w_res$p.value
            p_str <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
            paste0("Test de Wilcoxon : W = ", w_stat, ", ", p_str)
          } else {
            paste0("Wilcoxon : ", two_state$var_y, " selon ", grp_col)
          }

          p <- ggplot2::ggplot(clean_df, ggplot2::aes(x = .data[[grp_col]], y = .data[[two_state$var_y]], group = .data[[grp_col]])) +
            ggplot2::geom_boxplot(fill = "#F3F4F6", color = "#1F2937", width = 0.4) +
            ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "#4B5563") +
            ggplot2::labs(
              title = w_title,
              x = grp_col,
              y = two_state$var_y
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              text = ggplot2::element_text(family = "Aptos Narrow"),
              plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
              axis.title = ggplot2::element_text(size = 11, color = "#374151"),
              axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
              panel.grid.minor = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
            )

          tryCatch({
            plotly::ggplotly(p)
          }, error = function(e) {
            tryCatch({
              plotly::plotly_build(p)
            }, error = function(e2) {
              plotly::plot_ly(
                data = clean_df,
                x = as.formula(paste0("~", grp_col)),
                y = as.formula(paste0("~", two_state$var_y)),
                type = "box",
                boxpoints = "all",
                jitter = 0.3
              )
            })
          })
        }
      } else if (!is.null(two_state$var_x2)) {
        clean_paired <- df[!is.na(df[[two_state$var_y]]) & !is.na(df[[two_state$var_x2]]), ]
        v1 <- clean_paired[[two_state$var_y]]
        v2 <- clean_paired[[two_state$var_x2]]
        comp_df <- data.frame(
          val = c(v1, v2),
          variable = factor(c(rep(two_state$var_y, length(v1)), rep(two_state$var_x2, length(v2))))
        )
        plotly::plot_ly(
          data = comp_df,
          x = ~variable,
          y = ~val,
          color = ~variable,
          type = "box",
          boxpoints = "all",
          jitter = 0.3
        ) %>%
          plotly::layout(
            title = list(text = "Comparaison des séries appariées", font = list(size = 13)),
            xaxis = list(title = "Variable"),
            yaxis = list(title = "Valeur")
          )
      }
    })

    # =========================================================================
    # 3. COMPARAISON DE 3+ GROUPES (ANOVA / KRUSKAL-WALLIS)
    # =========================================================================
    run_multi_analysis <- function(test_type, var_y, var_group, post_hoc_opt, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || is.null(var_group)) {
        shiny::showNotification("Veuillez sélectionner une variable dépendante et un facteur de groupe.", type = "warning")
        return()
      }
      multi_state$test <- test_type
      multi_state$var_y <- var_y
      multi_state$var_group <- var_group
      multi_state$post_hoc <- isTRUE(post_hoc_opt)
      multi_state$alpha <- as.numeric(alpha_val)

      res <- NULL
      post_res <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        fml <- as.formula(paste0(multi_state$var_y, " ~ ", multi_state$var_group))
        if (multi_state$test == "anova") {
          aov_fit <- stats::aov(fml, data = df)
          res <- aov_fit
          if (multi_state$post_hoc) {
            post_res <- stats::TukeyHSD(aov_fit)
          }
          code_entry <- paste0(
            "# Analyse de variance à 1 facteur (ANOVA)\n",
            "mod_aov <- aov(", multi_state$var_y, " ~ ", multi_state$var_group, ", data = ", ds_name, ")\n",
            "summary(mod_aov)\n",
            if (multi_state$post_hoc) "TukeyHSD(mod_aov)" else ""
          )
        } else {
          res <- stats::kruskal.test(fml, data = df)
          if (multi_state$post_hoc) {
            post_res <- stats::pairwise.wilcox.test(df[[multi_state$var_y]], df[[multi_state$var_group]], p.adjust.method = "bonferroni")
          }
          code_entry <- paste0(
            "# Test non-paramétrique de Kruskal-Wallis\n",
            "kruskal.test(", multi_state$var_y, " ~ ", multi_state$var_group, ", data = ", ds_name, ")\n",
            if (multi_state$post_hoc) paste0("pairwise.wilcox.test(", ds_name, "$", multi_state$var_y, ", ", ds_name, "$", multi_state$var_group, ", p.adjust.method = 'bonferroni')") else ""
          )
        }
      }, error = function(e) {
        err <- e$message
      })

      multi_state$result <- res
      multi_state$post_hoc_result <- post_res
      multi_state$error <- err
      multi_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Comparaison 3+ Groupes : ", multi_state$test),
          code = code_entry
        )
      }
      shiny::showNotification("ANOVA / Kruskal-Wallis exécutée !", type = "message")
    }

    shiny::observeEvent(input$btn_run_multi, {
      run_multi_analysis(
        input$multi_test_direct,
        input$multi_var_y_direct,
        input$multi_var_group_direct,
        input$multi_posthoc_direct,
        input$multi_alpha_direct
      )
    })

    shiny::observeEvent(input$btn_open_multi_modal, {
      num_cols <- get_num_vars()
      cat_cols <- get_cat_vars()

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Paramètres : Comparaison de 3+ Groupes")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_multi"), "Exécuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("multi_test_choice"),
              label = "Test à effectuer :",
              choices = c(
                "ANOVA à 1 facteur (aov) + Tukey HSD Post-hoc" = "anova",
                "Test de Kruskal-Wallis (non-paramétrique)" = "kruskal"
              ),
              selected = multi_state$test
            ),
            shiny::selectInput(
              inputId = ns("multi_var_y"),
              label = "Variable quantitative dépendante (Y) :",
              choices = num_cols,
              selected = multi_state$var_y
            ),
            shiny::selectInput(
              inputId = ns("multi_var_group"),
              label = "Variable qualitative de regroupement (Facteur X) :",
              choices = cat_cols,
              selected = multi_state$var_group
            ),
            shiny::checkboxInput(
              inputId = ns("multi_posthoc_opt"),
              label = "Calculer les tests Post-Hoc (comparaisons par paires)",
              value = multi_state$post_hoc
            ),
            shiny::selectInput(
              inputId = ns("multi_alpha_select"),
              label = "Seuil alpha :",
              choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
              selected = as.character(multi_state$alpha)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_multi, {
      shiny::removeModal()
      run_multi_analysis(
        input$multi_test_choice,
        input$multi_var_y,
        input$multi_var_group,
        input$multi_posthoc_opt,
        input$multi_alpha_select
      )
    })

    output$multi_status_badge <- shiny::renderUI({
      if (!multi_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Exécuter' pour lancer l'ANOVA ou le test de Kruskal-Wallis."))
      }
      if (!is.null(multi_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", multi_state$error)))
      }
      p_val <- if (multi_state$test == "anova") {
        smry <- summary(multi_state$result)
        smry[[1]][["Pr(>F)"]][1]
      } else {
        multi_state$result$p.value
      }
      sig <- p_val < multi_state$alpha
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) "Effet de groupe globalement significatif (au moins 2 groupes diffèrent)" else "Aucune différence globale significative détectée"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
      )
    })

    output$multi_results_ui <- shiny::renderUI({
      req(multi_state$calculated)
      if (!is.null(multi_state$error)) {
        return(shiny::p(class = "text-danger small", multi_state$error))
      }

      if (multi_state$test == "anova") {
        smry <- summary(multi_state$result)[[1]]
        f_val <- smry[["F value"]][1]
        p_val <- smry[["Pr(>F)"]][1]
        df_group <- smry[["Df"]][1]
        df_res <- smry[["Df"]][2]

        shiny::tagList(
          shiny::tags$table(
            class = "table table-sm table-bordered text-center align-middle mb-3",
            shiny::tags$thead(
              class = "table-light",
              shiny::tags$tr(
                shiny::tags$th("Source"),
                shiny::tags$th("ddl"),
                shiny::tags$th("Somme des carrés"),
                shiny::tags$th("Carré moyen"),
                shiny::tags$th("F value"),
                shiny::tags$th("Pr(>F)")
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-start fw-bold", multi_state$var_group),
                shiny::tags$td(df_group),
                shiny::tags$td(round(smry[["Sum Sq"]][1], 2)),
                shiny::tags$td(round(smry[["Mean Sq"]][1], 2)),
                shiny::tags$td(class = "fw-bold", round(f_val, 3)),
                shiny::tags$td(class = "fw-bold text-primary", format.pval(p_val, digits = 4, eps = 0.0001))
              ),
              shiny::tags$tr(
                shiny::tags$td(class = "text-start text-muted", "Résidus"),
                shiny::tags$td(df_res),
                shiny::tags$td(round(smry[["Sum Sq"]][2], 2)),
                shiny::tags$td(round(smry[["Mean Sq"]][2], 2)),
                shiny::tags$td("—"),
                shiny::tags$td("—")
              )
            )
          ),
          if (!is.null(multi_state$post_hoc_result)) {
            shiny::div(
              class = "mt-3",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Comparaisons par paires (Tukey HSD) :"),
              shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(capture.output(print(multi_state$post_hoc_result)), collapse = "\n"))
            )
          }
        )
      } else {
        res <- multi_state$result
        p_val <- res$p.value
        stat_val <- unname(res$statistic)
        sig <- p_val < multi_state$alpha

        shiny::tagList(
          shiny::tags$table(
            class = "table table-sm table-bordered text-center align-middle mb-3",
            shiny::tags$thead(
              class = "table-light",
              shiny::tags$tr(
                shiny::tags$th("Méthode"),
                shiny::tags$th("Chi-deux (H)"),
                shiny::tags$th("ddl"),
                shiny::tags$th("p-value"),
                shiny::tags$th("Conclusion")
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-start fw-medium", res$method),
                shiny::tags$td(round(stat_val, 3)),
                shiny::tags$td(round(res$parameter, 1)),
                shiny::tags$td(class = "fw-bold", format.pval(p_val, digits = 4, eps = 0.0001)),
                shiny::tags$td(
                  shiny::tags$span(
                    class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                    if (sig) "Différence significative" else "Non significatif"
                  )
                )
              )
            )
          ),
          if (!is.null(multi_state$post_hoc_result)) {
            shiny::div(
              class = "mt-3",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Tests de Wilcoxon par paires (Bonferroni) :"),
              shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(capture.output(print(multi_state$post_hoc_result)), collapse = "\n"))
            )
          }
        )
      }
    })

    output$multi_plot <- plotly::renderPlotly({
      df <- data_holder$df
      req(df, multi_state$var_y, multi_state$var_group)
      grp_col <- multi_state$var_group
      y_col <- multi_state$var_y

      clean_df <- df[!is.na(df[[y_col]]) & !is.na(df[[grp_col]]), ]
      req(nrow(clean_df) > 0)
      clean_df[[grp_col]] <- droplevels(as.factor(clean_df[[grp_col]]))

      title_text <- if (multi_state$calculated && !is.null(multi_state$result)) {
        if (multi_state$test == "anova") {
          smry <- summary(multi_state$result)[[1]]
          f_val <- round(smry[["F value"]][1], 2)
          p_val <- smry[["Pr(>F)"]][1]
          p_str <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
          paste0("ANOVA : F = ", f_val, ", ", p_str)
        } else {
          h_val <- round(unname(multi_state$result$statistic), 2)
          p_val <- multi_state$result$p.value
          p_str <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
          paste0("Kruskal-Wallis : Chi2 = ", h_val, ", ", p_str)
        }
      } else {
        paste0("Comparaison multiple : ", y_col, " par ", grp_col)
      }

      p <- ggplot2::ggplot(clean_df, ggplot2::aes(x = .data[[grp_col]], y = .data[[y_col]], group = .data[[grp_col]])) +
        ggplot2::geom_boxplot(fill = "#F3F4F6", color = "#1F2937", width = 0.45) +
        ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "#4B5563") +
        ggplot2::labs(
          title = title_text,
          x = grp_col,
          y = y_col
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(family = "Aptos Narrow"),
          plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
          axis.title = ggplot2::element_text(size = 11, color = "#374151"),
          axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
        )

      tryCatch({
        plotly::ggplotly(p)
      }, error = function(e) {
        tryCatch({
          plotly::plotly_build(p)
        }, error = function(e2) {
          plotly::plot_ly(
            data = clean_df,
            x = as.formula(paste0("~", grp_col)),
            y = as.formula(paste0("~", y_col)),
            type = "box",
            boxpoints = "all",
            jitter = 0.3
          ) %>%
            plotly::layout(
              title = list(text = title_text, font = list(size = 13)),
              xaxis = list(title = grp_col),
              yaxis = list(title = y_col)
            )
        })
      })
    })

    # =========================================================================
    # 4. ANALYSE DE CONTINGENCE & QUALITATIF
    # =========================================================================
    run_cont_analysis <- function(test_type, var_row, var_col, correct_opt, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_row) || is.null(var_col)) {
        shiny::showNotification("Veuillez sélectionner deux variables qualitatives.", type = "warning")
        return()
      }
      cont_state$test <- test_type
      cont_state$var_row <- var_row
      cont_state$var_col <- var_col
      cont_state$correct <- isTRUE(correct_opt)
      cont_state$alpha <- as.numeric(alpha_val)

      res <- NULL
      err <- NULL
      tab <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        tab <- table(df[[cont_state$var_row]], df[[cont_state$var_col]])
        if (cont_state$test == "chisq") {
          res <- stats::chisq.test(tab, correct = cont_state$correct)
          code_entry <- paste0(
            "# Tableau de contingence et Test du Chi-deux\n",
            "tab <- table(", ds_name, "$", cont_state$var_row, ", ", ds_name, "$", cont_state$var_col, ")\n",
            "tab\n",
            "chisq.test(tab, correct = ", cont_state$correct, ")"
          )
        } else if (cont_state$test == "fisher") {
          res <- stats::fisher.test(tab)
          code_entry <- paste0(
            "# Test Exact de Fisher\n",
            "tab <- table(", ds_name, "$", cont_state$var_row, ", ", ds_name, "$", cont_state$var_col, ")\n",
            "fisher.test(tab)"
          )
        } else if (cont_state$test == "mcnemar") {
          res <- stats::mcnemar.test(tab)
          code_entry <- paste0(
            "# Test de McNemar\n",
            "tab <- table(", ds_name, "$", cont_state$var_row, ", ", ds_name, "$", cont_state$var_col, ")\n",
            "mcnemar.test(tab)"
          )
        }
      }, error = function(e) {
        err <- e$message
      })

      cont_state$result <- res
      cont_state$tab <- tab
      cont_state$error <- err
      cont_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Contingence : ", cont_state$test, " (", cont_state$var_row, " x ", cont_state$var_col, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Test de contingence exécuté !", type = "message")
    }

    shiny::observeEvent(input$btn_run_cont, {
      run_cont_analysis(
        input$cont_test_direct,
        input$cont_var_row_direct,
        input$cont_var_col_direct,
        input$cont_correct_direct,
        input$cont_alpha_direct
      )
    })

    shiny::observeEvent(input$btn_open_cont_modal, {
      cat_cols <- get_cat_vars()

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Paramètres : Analyse de Contingence")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_cont"), "Exécuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("cont_test_choice"),
              label = "Test d'indépendance / association :",
              choices = c(
                "Test du Chi-deux (chisq.test)" = "chisq",
                "Test exact de Fisher (fisher.test)" = "fisher",
                "Test de McNemar (données appariées)" = "mcnemar"
              ),
              selected = cont_state$test
            ),
            shiny::selectInput(
              inputId = ns("cont_var_row"),
              label = "Variable qualitative en ligne (Ligne) :",
              choices = cat_cols,
              selected = cont_state$var_row
            ),
            shiny::selectInput(
              inputId = ns("cont_var_col"),
              label = "Variable qualitative en colonne (Colonne) :",
              choices = cat_cols,
              selected = cont_state$var_col
            ),
            shiny::conditionalPanel(
              condition = "input.cont_test_choice == 'chisq'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("cont_correct_opt"),
                label = "Correction de continuité de Yates",
                value = cont_state$correct
              )
            ),
            shiny::selectInput(
              inputId = ns("cont_alpha_select"),
              label = "Seuil alpha :",
              choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
              selected = as.character(cont_state$alpha)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_cont, {
      shiny::removeModal()
      run_cont_analysis(
        input$cont_test_choice,
        input$cont_var_row,
        input$cont_var_col,
        input$cont_correct_opt,
        input$cont_alpha_select
      )
    })

    output$cont_status_badge <- shiny::renderUI({
      if (!cont_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Exécuter' pour lancer le Chi-deux ou le test de Fisher."))
      }
      if (!is.null(cont_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", cont_state$error)))
      }
      p_val <- cont_state$result$p.value
      sig <- p_val < cont_state$alpha
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) "Association statistiquement significative entre les deux variables !" else "Variables indépendantes (aucune liaison significative)"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
      )
    })

    output$cont_results_ui <- shiny::renderUI({
      req(cont_state$calculated)
      if (!is.null(cont_state$error)) {
        return(shiny::p(class = "text-danger small", cont_state$error))
      }
      res <- cont_state$result
      p_val <- res$p.value
      stat_val <- unname(res$statistic)
      stat_name <- names(res$statistic)
      sig <- p_val < cont_state$alpha

      shiny::tagList(
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle mb-3",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Méthode"),
              shiny::tags$th("Statistique"),
              shiny::tags$th("ddl"),
              shiny::tags$th("p-value"),
              shiny::tags$th("Conclusion")
            )
          ),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "text-start fw-medium", res$method),
              shiny::tags$td(if (!is.null(stat_val)) paste0(stat_name, " = ", round(stat_val, 3)) else "—"),
              shiny::tags$td(if (!is.null(res$parameter)) round(res$parameter, 1) else "—"),
              shiny::tags$td(class = "fw-bold", format.pval(p_val, digits = 4, eps = 0.0001)),
              shiny::tags$td(
                shiny::tags$span(
                  class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                  if (sig) "Liaison significative" else "Indépendance"
                )
              )
            )
          )
        ),
        shiny::div(
          class = "mt-3",
          shiny::tags$h6(class = "fw-bold text-dark mb-1", "Tableau des effectifs observés :"),
          shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(capture.output(print(cont_state$tab)), collapse = "\n"))
        )
      )
    })

    output$cont_plot <- plotly::renderPlotly({
      req(cont_state$calculated, cont_state$tab)
      tab_df <- as.data.frame(cont_state$tab)
      names(tab_df) <- c("Ligne", "Colonne", "Freq")

      plotly::plot_ly(
        data = tab_df,
        x = ~Ligne,
        y = ~Freq,
        color = ~Colonne,
        type = "bar"
      ) %>%
        plotly::layout(
          barmode = "stack",
          title = list(text = paste0("Contingence : ", cont_state$var_row, " x ", cont_state$var_col), font = list(size = 13)),
          xaxis = list(title = cont_state$var_row),
          yaxis = list(title = "Effectifs (Fréquence)")
        )
    })

    # =========================================================================
    # 5. TESTS DE CORRÉLATION (PEARSON, SPEARMAN, KENDALL)
    # =========================================================================
    run_cor_analysis <- function(method_choice, var_x, var_y, alt, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_x) || is.null(var_y)) {
        shiny::showNotification("Veuillez choisir 2 variables numériques.", type = "warning")
        return()
      }
      cor_state$method <- method_choice
      cor_state$var_x <- var_x
      cor_state$var_y <- var_y
      cor_state$alternative <- alt
      cor_state$alpha <- as.numeric(alpha_val)

      res <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        res <- stats::cor.test(
          df[[cor_state$var_x]],
          df[[cor_state$var_y]],
          method = cor_state$method,
          alternative = cor_state$alternative,
          conf.level = 1 - cor_state$alpha,
          exact = FALSE
        )
        code_entry <- paste0(
          "# Test de corrélation bivariée (", cor_state$method, ")\n",
          "cor.test(", ds_name, "$", cor_state$var_x, ", ", ds_name, "$", cor_state$var_y,
          ", method = '", cor_state$method, "', alternative = '", cor_state$alternative, "')"
        )
      }, error = function(e) {
        err <- e$message
      })

      cor_state$result <- res
      cor_state$error <- err
      cor_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Corrélation : ", cor_state$method, " (", cor_state$var_x, " & ", cor_state$var_y, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Test de corrélation calculé !", type = "message")
    }

    shiny::observeEvent(input$btn_run_cor, {
      run_cor_analysis(
        input$cor_method_direct,
        input$cor_var_x_direct,
        input$cor_var_y_direct,
        input$cor_alternative_direct,
        input$cor_alpha_direct
      )
    })

    shiny::observeEvent(input$btn_open_cor_modal, {
      num_cols <- get_num_vars()

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Paramètres : Test de Corrélation")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_cor"), "Exécuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("cor_method_choice"),
              label = "Coefficient de corrélation :",
              choices = c(
                "Pearson (paramétrique - relation linéaire)" = "pearson",
                "Spearman (non-paramétrique - rangs)" = "spearman",
                "Kendall (tau - concordance des paires)" = "kendall"
              ),
              selected = cor_state$method
            ),
            shiny::selectInput(
              inputId = ns("cor_var_x"),
              label = "Première variable (X) :",
              choices = num_cols,
              selected = cor_state$var_x
            ),
            shiny::selectInput(
              inputId = ns("cor_var_y"),
              label = "Seconde variable (Y) :",
              choices = num_cols,
              selected = cor_state$var_y
            ),
            shiny::selectInput(
              inputId = ns("cor_alternative"),
              label = "Hypothèse alternative :",
              choices = c(
                "Bilatérale (corrélation != 0)" = "two.sided",
                "Unilatérale positive (r > 0)" = "greater",
                "Unilatérale négative (r < 0)" = "less"
              ),
              selected = cor_state$alternative
            ),
            shiny::selectInput(
              inputId = ns("cor_alpha_select"),
              label = "Seuil alpha :",
              choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
              selected = as.character(cor_state$alpha)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_cor, {
      shiny::removeModal()
      run_cor_analysis(
        input$cor_method_choice,
        input$cor_var_x,
        input$cor_var_y,
        input$cor_alternative,
        input$cor_alpha_select
      )
    })

    output$cor_status_badge <- shiny::renderUI({
      if (!cor_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Exécuter' pour calculer le test de corrélation."))
      }
      if (!is.null(cor_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", cor_state$error)))
      }
      p_val <- cor_state$result$p.value
      r_val <- unname(cor_state$result$estimate)
      sig <- p_val < cor_state$alpha
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) paste0("Corrélation significative (r = ", round(r_val, 3), ")") else "Aucune corrélation statistiquement significative"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
      )
    })

    output$cor_results_ui <- shiny::renderUI({
      req(cor_state$calculated)
      if (!is.null(cor_state$error)) {
        return(shiny::p(class = "text-danger small", cor_state$error))
      }
      res <- cor_state$result
      p_val <- res$p.value
      r_val <- unname(res$estimate)
      r_name <- names(res$estimate)
      sig <- p_val < cor_state$alpha

      interp <- if (sig) {
        sens <- if (r_val > 0) "positive (les deux variables augmentent conjointement)" else "négative (une variable augmente quand l'autre diminue)"
        force <- if (abs(r_val) > 0.7) "forte" else if (abs(r_val) > 0.4) "modérée" else "faible"
        paste0("Corrélation ", force, " et ", sens, ", statistiquement significative au seuil alpha = ", cor_state$alpha, " (p = ", format.pval(p_val, digits = 4), ").")
      } else {
        paste0("Le coefficient observé (", round(r_val, 3), ") n'est pas statistiquement différent de 0 au seuil alpha = ", cor_state$alpha, ".")
      }

      shiny::tagList(
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle mb-3",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Méthode"),
              shiny::tags$th("Coefficient"),
              shiny::tags$th("Statistique (t/z/S)"),
              shiny::tags$th("p-value"),
              shiny::tags$th("Conclusion")
            )
          ),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "text-start fw-medium", res$method),
              shiny::tags$td(class = "fw-bold text-primary", paste0(r_name, " = ", round(r_val, 3))),
              shiny::tags$td(round(unname(res$statistic), 3)),
              shiny::tags$td(class = "fw-bold", format.pval(p_val, digits = 4, eps = 0.0001)),
              shiny::tags$td(
                shiny::tags$span(
                  class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                  if (sig) "Significatif" else "Non significatif"
                )
              )
            )
          )
        ),
        if (!is.null(res$conf.int)) {
          shiny::div(
            class = "small text-muted mb-2",
            paste0("Intervalle de confiance à ", round((1 - cor_state$alpha)*100), "% : [",
                   round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
          )
        },
        shiny::div(
          class = "p-2 rounded bg-light border text-secondary small mb-3",
          shiny::tags$strong("Interprétation : "),
          interp
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher la sortie console R"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(capture.output(print(res)), collapse = "\n"))
        )
      )
    })

    output$cor_plot <- plotly::renderPlotly({
      df <- data_holder$df
      req(df, cor_state$var_x, cor_state$var_y)
      clean_df <- df[!is.na(df[[cor_state$var_x]]) & !is.na(df[[cor_state$var_y]]), ]

      x_vals <- clean_df[[cor_state$var_x]]
      y_vals <- clean_df[[cor_state$var_y]]

      fit <- stats::lm(y_vals ~ x_vals)
      x_grid <- seq(min(x_vals), max(x_vals), length.out = 100)
      y_pred <- stats::predict(fit, newdata = data.frame(x_vals = x_grid))

      plotly::plot_ly() %>%
        plotly::add_trace(
          x = x_vals,
          y = y_vals,
          type = "scatter",
          mode = "markers",
          name = "Observations",
          marker = list(color = "#3b82f6", size = 7, opacity = 0.7)
        ) %>%
        plotly::add_lines(
          x = x_grid,
          y = y_pred,
          name = "Droite de régression",
          line = list(color = "#ef4444", width = 2)
        ) %>%
        plotly::layout(
          title = list(text = paste0("Corrélation : ", cor_state$var_x, " vs ", cor_state$var_y), font = list(size = 13)),
          xaxis = list(title = cor_state$var_x),
          yaxis = list(title = cor_state$var_y)
        )
    })

    # =========================================================================
    # 6. MODÉLISATION (RÉGRESSION LINÉAIRE & LOGISTIQUE)
    # =========================================================================
    run_reg_analysis <- function(model_choice, var_y, vars_x, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || length(vars_x) == 0) {
        shiny::showNotification("Veuillez sélectionner la variable dépendante Y et au moins une variable explicative X.", type = "warning")
        return()
      }
      reg_state$model_type <- model_choice
      reg_state$var_y <- var_y
      reg_state$vars_x <- vars_x
      reg_state$alpha <- as.numeric(alpha_val)

      fit <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        fml_str <- paste0(reg_state$var_y, " ~ ", paste(reg_state$vars_x, collapse = " + "))
        fml <- as.formula(fml_str)

        if (reg_state$model_type == "linear") {
          fit <- stats::lm(fml, data = df)
          code_entry <- paste0(
            "# Régression linéaire (lm)\n",
            "mod_lm <- lm(", fml_str, ", data = ", ds_name, ")\n",
            "summary(mod_lm)"
          )
        } else {
          sub_df <- df
          if (is.factor(sub_df[[reg_state$var_y]]) || is.character(sub_df[[reg_state$var_y]])) {
            levs <- unique(sub_df[[reg_state$var_y]])
            levs <- levs[!is.na(levs)]
            if (length(levs) > 2) {
              sub_df <- sub_df[sub_df[[reg_state$var_y]] %in% levs[1:2], ]
            }
            sub_df[[reg_state$var_y]] <- as.factor(sub_df[[reg_state$var_y]])
          }
          fit <- stats::glm(fml, data = sub_df, family = stats::binomial(link = "logit"))
          code_entry <- paste0(
            "# Régression logistique binaire (glm binomial)\n",
            "mod_glm <- glm(", fml_str, ", data = ", ds_name, ", family = binomial(link = 'logit'))\n",
            "summary(mod_glm)"
          )
        }
      }, error = function(e) {
        err <- e$message
      })

      reg_state$model <- fit
      reg_state$error <- err
      reg_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Modélisation : ", reg_state$model_type, " (", reg_state$var_y, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Modèle ajusté et consigné avec succès !", type = "message")
    }

    shiny::observeEvent(input$btn_run_reg, {
      run_reg_analysis(
        input$reg_model_direct,
        input$reg_var_y_direct,
        input$reg_vars_x_direct,
        input$reg_alpha_direct
      )
    })

    shiny::observeEvent(input$btn_open_reg_modal, {
      num_cols <- get_num_vars()
      all_cols <- names(data_holder$df)

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Paramètres : Modélisation Statistique")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_reg"), "Ajuster le modèle & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("reg_model_choice"),
              label = "Type de régression :",
              choices = c(
                "Régression Linéaire Simple / Multiple (lm)" = "linear",
                "Régression Logistique Binaire (glm binomial)" = "logistic"
              ),
              selected = reg_state$model_type
            ),
            shiny::selectInput(
              inputId = ns("reg_var_y"),
              label = "Variable Dépendante (Y) :",
              choices = if (reg_state$model_type == "linear") num_cols else all_cols,
              selected = reg_state$var_y
            ),
            shiny::selectizeInput(
              inputId = ns("reg_vars_x"),
              label = "Variables Explicatives / Prédictives (X) :",
              choices = all_cols,
              selected = reg_state$vars_x,
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            ),
            shiny::selectInput(
              inputId = ns("reg_alpha_select"),
              label = "Seuil alpha :",
              choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
              selected = as.character(reg_state$alpha)
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_reg, {
      shiny::removeModal()
      run_reg_analysis(
        input$reg_model_choice,
        input$reg_var_y,
        input$reg_vars_x,
        input$reg_alpha_select
      )
    })

    output$reg_status_badge <- shiny::renderUI({
      if (!reg_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ajuster le modèle' pour lancer l'estimation."))
      }
      if (!is.null(reg_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur d'ajustement : ", reg_state$error)))
      }
      shiny::div(
        class = "alert alert-success py-2 px-3 small d-flex justify-content-between align-items-center",
        shiny::tags$span("Modèle ajusté avec succès : ", paste(reg_state$var_y, "~", paste(reg_state$vars_x, collapse = " + "))),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", if (reg_state$model_type == "linear") "Régression Linéaire" else "Régression Logistique")
      )
    })

    output$reg_results_ui <- shiny::renderUI({
      req(reg_state$calculated)
      if (!is.null(reg_state$error)) {
        return(shiny::p(class = "text-danger small", reg_state$error))
      }
      smry <- summary(reg_state$model)
      coef_table <- as.data.frame(smry$coefficients)

      shiny::tagList(
        shiny::tags$h6(class = "fw-bold text-dark mb-2", "Tableau des coefficients du modèle :"),
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle mb-3",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("Terme"),
              shiny::tags$th("Estimation (Beta)"),
              shiny::tags$th("Erreur-type (SE)"),
              shiny::tags$th(if (reg_state$model_type == "linear") "t value" else "z value"),
              shiny::tags$th("Pr(>|t| ou |z|)"),
              shiny::tags$th("Significativité")
            )
          ),
          shiny::tags$tbody(
            lapply(rownames(coef_table), function(term) {
              row <- coef_table[term, ]
              est <- row[[1]]
              se <- row[[2]]
              stat <- row[[3]]
              pval <- row[[4]]
              sig <- pval < reg_state$alpha

              shiny::tags$tr(
                shiny::tags$td(class = "text-start fw-medium", term),
                shiny::tags$td(round(est, 4)),
                shiny::tags$td(round(se, 4)),
                shiny::tags$td(round(stat, 3)),
                shiny::tags$td(class = "fw-bold", format.pval(pval, digits = 4, eps = 0.0001)),
                shiny::tags$td(
                  shiny::tags$span(
                    class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                    if (sig) "p < alpha" else "NS"
                  )
                )
              )
            })
          )
        ),
        if (reg_state$model_type == "linear") {
          shiny::div(
            class = "p-2 rounded bg-light border text-secondary small mb-3",
            shiny::tags$strong("Qualité globale : "),
            paste0("R² = ", round(smry$r.squared, 3), " | R² ajusté = ", round(smry$adj.r.squared, 3),
                   " | Erreur résiduelle type = ", round(smry$sigma, 3))
          )
        } else {
          shiny::div(
            class = "p-2 rounded bg-light border text-secondary small mb-3",
            shiny::tags$strong("Qualité globale : "),
            paste0("Déviance résiduelle = ", round(smry$deviance, 2), " (sur ", smry$df.residual, " ddl) | AIC = ", round(smry$aic, 1))
          )
        },
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher le résumé R complet"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(capture.output(print(smry)), collapse = "\n"))
        )
      )
    })

    output$reg_plot <- plotly::renderPlotly({
      req(reg_state$calculated, reg_state$model)
      mod <- reg_state$model
      resids <- stats::residuals(mod)
      fitted_vals <- stats::fitted(mod)

      if (input$reg_plot_choice == "rvf") {
        plotly::plot_ly() %>%
          plotly::add_trace(
            x = fitted_vals,
            y = resids,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#8b5cf6", size = 6, opacity = 0.8),
            name = "Résidus"
          ) %>%
          plotly::add_lines(
            x = range(fitted_vals),
            y = c(0, 0),
            line = list(color = "#ef4444", dash = "dash"),
            name = "Ligne 0"
          ) %>%
          plotly::layout(
            title = list(text = "Résidus vs Valeurs ajustées", font = list(size = 13)),
            xaxis = list(title = "Valeurs ajustées (Fitted values)"),
            yaxis = list(title = "Résidus")
          )
      } else {
        sorted_res <- sort(resids)
        n <- length(sorted_res)
        probs <- (1:n - 0.5) / n
        theo <- stats::qnorm(probs, mean = 0, sd = stats::sd(resids))

        plotly::plot_ly() %>%
          plotly::add_trace(
            x = theo,
            y = sorted_res,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#3b82f6", size = 6),
            name = "Résidus observés"
          ) %>%
          plotly::add_lines(
            x = range(theo),
            y = range(theo),
            line = list(color = "#ef4444", dash = "dash"),
            name = "Normale théorique"
          ) %>%
          plotly::layout(
            title = list(text = "Normal Q-Q Plot des Résidus", font = list(size = 13)),
            xaxis = list(title = "Quantiles théoriques"),
            yaxis = list(title = "Résidus")
          )
      }
    })

  })
}
