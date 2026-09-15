#' Sous-interface : Conditions d'application & Normalit\u00e9
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
            shiny::tags$strong("Normalit\u00e9 & Variances")
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
            label = "Test \u00e0 r\u00e9aliser :",
            choices = c(
              "Shapiro-Wilk (normalit\u00e9 n <= 5000)" = "shapiro",
              "Shapiro-Wilk par groupe (normalit\u00e9 par groupe)" = "shapiro_group",
              "Kolmogorov-Smirnov (normalit\u00e9 vs pnorm)" = "ks",
              "Test de Bartlett (homog\u00e9n\u00e9it\u00e9 variances normale)" = "bartlett",
              "Test de Fligner-Killeen (homog\u00e9n\u00e9it\u00e9 non-param\u00e9trique)" = "fligner"
            ),
            selected = "shapiro"
          ),
          shiny::selectInput(
            inputId = ns("norm_var_direct"),
            label = "Variable num\u00e9rique d'int\u00e9r\u00eat (Y) :",
            choices = NULL
          ),
          shiny::conditionalPanel(
            condition = "input.norm_test_direct == 'bartlett' || input.norm_test_direct == 'fligner' || input.norm_test_direct == 'shapiro_group'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("norm_group_direct"),
              label = "Variable qualitative de groupe (X) :",
              choices = NULL
            )
          ),
          shiny::selectInput(
            inputId = ns("norm_alpha_direct"),
            label = "Niveau de significativit\u00e9 (alpha) :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_norm"),
            label = "Ex\u00e9cuter & Journaliser",
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
          title = "R\u00e9sultats & Inf\u00e9rence",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("norm_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Graphiques d'\u00e9valuation",
          bslib::card_header(
            class = "py-2 bg-light d-flex justify-content-between align-items-center",
            shiny::tags$span(shiny::tags$strong("\u00c9valuation graphique")),
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
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("norm_pedagogy_ui"))
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
              "\u00c9chantillon unique (comparaison \u00e0 une moyenne th\u00e9orique mu)" = "t_one_sample",
              "Deux \u00e9chantillons ind\u00e9pendants" = "t_indep",
              "\u00c9chantillons appari\u00e9s" = "t_paired",
              "Test non-param\u00e9trique : Wilcoxon sign\u00e9 (\u00e9chantillon unique)" = "wilcox_one_sample",
              "Test non-param\u00e9trique : Wilcoxon / Mann-Whitney (ind\u00e9pendant)" = "wilcox_indep",
              "Test non-param\u00e9trique : Wilcoxon sign\u00e9 (appari\u00e9)" = "wilcox_paired"
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
              label = "Moyenne th\u00e9orique (mu) :",
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
                label = "Variances \u00e9gales suppos\u00e9es (Student vs Welch)",
                value = FALSE
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "input.two_test_direct == 't_paired' || input.two_test_direct == 'wilcox_paired'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("two_var_x2_direct"),
              label = "Seconde variable quantitative appari\u00e9e (Y2) :",
              choices = NULL
            )
          ),
          shiny::selectInput(
            inputId = ns("two_alternative_direct"),
            label = "Hypoth\u00e8se alternative :",
            choices = c(
              "Bilat\u00e9rale (diff\u00e9rence != 0 / moyenne != mu)" = "two.sided",
              "Unilat\u00e9rale gauche (moyenne < mu / groupe 1 < groupe 2)" = "less",
              "Unilat\u00e9rale droite (moyenne > mu / groupe 1 > groupe 2)" = "greater"
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
            label = "Ex\u00e9cuter & Journaliser",
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
          title = "Inf\u00e9rence & D\u00e9cision",
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
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("two_pedagogy_ui"))
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
            label = "Test \u00e0 effectuer :",
            choices = c(
              "ANOVA \u00e0 1 facteur (aov) + Tukey HSD Post-hoc" = "anova",
              "ANOVA factorielle \u00e0 2 facteurs (inter-sujets)" = "anova_twoway",
              "ANCOVA (1 facteur + 1 covariable quantitative)" = "ancova",
              "ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es (1 facteur intra-sujets)" = "anova_rm",
              "Test de Kruskal-Wallis (non-param\u00e9trique)" = "kruskal"
            ),
            selected = "anova"
          ),
          shiny::selectInput(
            inputId = ns("multi_var_y_direct"),
            label = "Variable quantitative d\u00e9pendante (Y) :",
            choices = NULL
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct != 'anova_rm'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("multi_var_group_direct"),
              label = "Facteur 1 (X1) / Groupe :",
              choices = NULL
            )
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct == 'anova_rm'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("multi_var_subject_direct"),
              label = "Identifiant du sujet (Sujet) :",
              choices = NULL
            ),
            shiny::selectInput(
              inputId = ns("multi_var_within_direct"),
              label = "Facteur intra-sujets (Temps / Condition) :",
              choices = NULL
            )
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct == 'anova_twoway'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("multi_var_group2_direct"),
              label = "Facteur 2 (X2) :",
              choices = NULL
            ),
            shiny::checkboxInput(
              inputId = ns("multi_interaction_direct"),
              label = "Inclure l'interaction (Facteur 1 \u00d7 Facteur 2)",
              value = TRUE
            )
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct == 'ancova'",
            ns = ns,
            shiny::selectInput(
              inputId = ns("multi_var_covar_direct"),
              label = "Covariable quantitative (C) :",
              choices = NULL
            ),
            shiny::checkboxInput(
              inputId = ns("multi_posthoc_ancova_direct"),
              label = "Calculer les comparaisons par paires (Holm)",
              value = TRUE
            )
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct == 'anova_rm'",
            ns = ns,
            shiny::checkboxInput(
              inputId = ns("multi_posthoc_rm_direct"),
              label = "Calculer les comparaisons par paires (Holm)",
              value = TRUE
            )
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct == 'anova'",
            ns = ns,
            shiny::checkboxInput(
              inputId = ns("multi_posthoc_direct"),
              label = "Calculer les tests Post-Hoc (Tukey HSD)",
              value = TRUE
            )
          ),
          shiny::conditionalPanel(
            condition = "input.multi_test_direct == 'kruskal'",
            ns = ns,
            shiny::checkboxInput(
              inputId = ns("multi_posthoc_kruskal_direct"),
              label = "Calculer les tests Post-Hoc (Wilcoxon par paires)",
              value = TRUE
            )
          ),
          shiny::selectInput(
            inputId = ns("multi_alpha_direct"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_multi"),
            label = "Ex\u00e9cuter & Journaliser",
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
            shiny::conditionalPanel(
              condition = "input.multi_test_direct == 'anova_rm'",
              ns = ns,
              shiny::div(
                class = "px-3 pt-2 pb-0 d-flex justify-content-end align-items-center gap-2",
                shiny::radioButtons(
                  inputId = ns("rm_plot_type_direct"),
                  label = NULL,
                  choices = c(
                    "Trajectoires individuelles + Moyenne" = "spaghetti",
                    "Moyennes & IC 95%" = "means"
                  ),
                  selected = "spaghetti",
                  inline = TRUE
                )
              )
            ),
            plotly::plotlyOutput(ns("multi_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("multi_pedagogy_ui"))
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
            label = "Test d'ind\u00e9pendance / association :",
            choices = c(
              "Test du Chi-deux (chisq.test)" = "chisq",
              "Test exact de Fisher (fisher.test)" = "fisher",
              "Test de McNemar (donn\u00e9es appari\u00e9es)" = "mcnemar"
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
              label = "Correction de continuit\u00e9 de Yates",
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
            label = "Ex\u00e9cuter & Journaliser",
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
          title = "Diagramme en barres empil\u00e9es",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("cont_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("cont_pedagogy_ui"))
          )
        )
      )
    )
  )
}

#' Sous-interface : Tests de proportions (1 et 2 groupes)
#'
#' @param id Identifiant de namespace Shiny
#' @return Interface utilisateur (tagList)
#' @export
mod_tests_prop_ui <- function(id) {
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
            shiny::tags$strong("Tests de Proportions")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_prop_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::radioButtons(
            inputId = ns("prop_analysis_type_direct"),
            label = "Que voulez-vous tester ?",
            choices = c(
              "Une proportion (\u00e9chantillon unique vs p0)" = "one_sample",
              "Deux proportions (comparaison 2 groupes)" = "two_samples"
            ),
            selected = "one_sample"
          ),
          shiny::conditionalPanel(
            condition = "input.prop_analysis_type_direct == 'one_sample'",
            ns = ns,
            shiny::div(
              class = "alert alert-info py-1 px-2 small mb-2",
              "Compare la proportion observ\u00e9e d'un \u00e9v\u00e9nement \u00e0 une proportion th\u00e9orique de r\u00e9f\u00e9rence (p0)."
            ),
            shiny::selectInput(
              inputId = ns("prop_one_var_direct"),
              label = "Variable d'int\u00e9r\u00eat (binaire / qualitative) :",
              choices = NULL
            ),
            shiny::selectInput(
              inputId = ns("prop_one_success_direct"),
              label = "Modalit\u00e9 consid\u00e9r\u00e9e comme \u00ab succ\u00e8s \u00bb :",
              choices = NULL
            ),
            shiny::numericInput(
              inputId = ns("prop_one_p0_direct"),
              label = "Proportion th\u00e9orique de r\u00e9f\u00e9rence (p0) :",
              value = 0.5,
              min = 0.001,
              max = 0.999,
              step = 0.05
            ),
            shiny::selectInput(
              inputId = ns("prop_one_method_direct"),
              label = "M\u00e9thode de test :",
              choices = c(
                "Test asymptotique du Chi-deux (prop.test)" = "prop",
                "Test exact binomial (binom.test)" = "binom"
              ),
              selected = "prop"
            ),
            shiny::conditionalPanel(
              condition = "input.prop_one_method_direct == 'prop'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("prop_one_correct_direct"),
                label = "Correction de continuit\u00e9 de Yates",
                value = TRUE
              )
            ),
            shiny::selectInput(
              inputId = ns("prop_one_alt_direct"),
              label = "Hypoth\u00e8se alternative (H1) :",
              choices = c(
                "Bilat\u00e9rale (p \u2260 p0)" = "two.sided",
                "Unilat\u00e9rale gauche (p < p0)" = "less",
                "Unilat\u00e9rale droite (p > p0)" = "greater"
              ),
              selected = "two.sided"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.prop_analysis_type_direct == 'two_samples'",
            ns = ns,
            shiny::div(
              class = "alert alert-info py-1 px-2 small mb-2",
              "Compare les proportions d'un m\u00eame \u00e9v\u00e9nement entre deux groupes ind\u00e9pendants."
            ),
            shiny::selectInput(
              inputId = ns("prop_two_var_outcome_direct"),
              label = "Variable crit\u00e8re (succ\u00e8s / \u00e9chec) :",
              choices = NULL
            ),
            shiny::selectInput(
              inputId = ns("prop_two_success_direct"),
              label = "Modalit\u00e9 consid\u00e9r\u00e9e comme \u00ab succ\u00e8s \u00bb :",
              choices = NULL
            ),
            shiny::selectInput(
              inputId = ns("prop_two_var_group_direct"),
              label = "Variable qualitative de regroupement (X) :",
              choices = NULL
            ),
            shiny::uiOutput(ns("prop_two_group_mods_direct_ui")),
            shiny::selectInput(
              inputId = ns("prop_two_method_direct"),
              label = "M\u00e9thode de test :",
              choices = c(
                "Test asymptotique de comparaison (prop.test)" = "prop",
                "Test exact de Fisher (fisher.test)" = "fisher"
              ),
              selected = "prop"
            ),
            shiny::conditionalPanel(
              condition = "input.prop_two_method_direct == 'prop'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("prop_two_correct_direct"),
                label = "Correction de continuit\u00e9 de Yates",
                value = TRUE
              )
            ),
            shiny::selectInput(
              inputId = ns("prop_two_alt_direct"),
              label = "Hypoth\u00e8se alternative (H1) :",
              choices = c(
                "Bilat\u00e9rale (p1 \u2260 p2)" = "two.sided",
                "Unilat\u00e9rale gauche (p1 < p2)" = "less",
                "Unilat\u00e9rale droite (p1 > p2)" = "greater"
              ),
              selected = "two.sided"
            )
          ),
          shiny::selectInput(
            inputId = ns("prop_alpha_direct"),
            label = "Seuil de significativit\u00e9 (alpha) :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_prop"),
            label = "Ex\u00e9cuter & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("prop_status_badge")),
      bslib::navset_card_tab(
        id = ns("prop_results_tabs"),
        bslib::nav_panel(
          title = "R\u00e9sultats & Inf\u00e9rence",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("prop_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Graphique des proportions",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("prop_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("prop_pedagogy_ui"))
          )
        )
      )
    )
  )
}

#' Sous-interface : Tests de corr\u00e9lation
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
            shiny::tags$strong("Tests de Corr\u00e9lation")
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
            label = "Coefficient de corr\u00e9lation :",
            choices = c(
              "Pearson (param\u00e9trique - relation lin\u00e9aire)" = "pearson",
              "Spearman (non-param\u00e9trique - rangs)" = "spearman",
              "Kendall (tau - concordance des paires)" = "kendall"
            ),
            selected = "pearson"
          ),
          shiny::selectInput(
            inputId = ns("cor_var_x_direct"),
            label = "Premi\u00e8re variable (X) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("cor_var_y_direct"),
            label = "Seconde variable (Y) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("cor_alternative_direct"),
            label = "Hypoth\u00e8se alternative :",
            choices = c(
              "Bilat\u00e9rale (corr\u00e9lation != 0)" = "two.sided",
              "Unilat\u00e9rale positive (r > 0)" = "greater",
              "Unilat\u00e9rale n\u00e9gative (r < 0)" = "less"
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
            label = "Ex\u00e9cuter & Journaliser",
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
          title = "Coefficient & Inf\u00e9rence",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("cor_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Nuage de points & R\u00e9gression",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("cor_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("cor_pedagogy_ui"))
          )
        )
      )
    )
  )
}

#' Sous-interface : Mod\u00e9lisation
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
            shiny::tags$strong("Mod\u00e9lisation")
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
            label = "Type de r\u00e9gression :",
            choices = c(
              "R\u00e9gression Lin\u00e9aire Simple / Multiple (lm)" = "linear",
              "R\u00e9gression Logistique Binaire (glm binomial)" = "logistic"
            ),
            selected = "linear"
          ),
          shiny::selectInput(
            inputId = ns("reg_var_y_direct"),
            label = "Variable D\u00e9pendante (Y) :",
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
            label = "Ajuster le mod\u00e8le",
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
          title = "Coefficients & Qualit\u00e9",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("reg_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Diagnostic des r\u00e9sidus",
          bslib::card_header(
            class = "py-2 bg-light d-flex justify-content-between align-items-center",
            shiny::tags$span(shiny::tags$strong("Graphique r\u00e9siduel")),
            shiny::radioButtons(
              inputId = ns("reg_plot_choice"),
              label = NULL,
              choices = c("R\u00e9sidus vs Valeurs ajust\u00e9es" = "rvf", "Q-Q Plot r\u00e9sidus" = "qq"),
              inline = TRUE
            )
          ),
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("reg_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("reg_pedagogy_ui"))
          )
        )
      )
    )
  )
}

#' @title Interface utilisateur pour le module de tests statistiques et mod\u00e9lisation
#'
#' @description Construit l'interface utilisateur pour les tests d'inf\u00e9rence statistique
#'   et de mod\u00e9lisation (normalit\u00e9, comparaison 2 groupes, ANOVA/Kruskal-Wallis,
#'   tests d'ind\u00e9pendance du Chi-2 et Fisher, corr\u00e9lations, r\u00e9gression lin\u00e9aire et logistique).
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
      shiny::tags$span(style = "font-weight: 600;", "Inf\u00e9rence Statistique & Mod\u00e9lisation")
    ),
    bslib::nav_panel(
      title = "Normalit\u00e9 & Variances",
      mod_tests_norm_ui(id)
    ),
    bslib::nav_panel(
      title = "Test t & Wilcoxon (1 et 2 \u00c9ch.)",
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
      title = "Tests de Proportions",
      mod_tests_prop_ui(id)
    ),
    bslib::nav_panel(
      title = "Corr\u00e9lations",
      mod_tests_cor_ui(id)
    ),
    bslib::nav_panel(
      title = "Mod\u00e9lisation",
      mod_tests_reg_ui(id)
    )
  )
}

#' @title Logique serveur pour le module de tests statistiques et mod\u00e9lisation
#'
#' @description Ex\u00e9cute les tests statistiques d'hypoth\u00e8se param\u00e9triques et
#'   non param\u00e9triques, g\u00e9n\u00e8re les sorties statistiques format\u00e9es, produit
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
    # ETAT R\u00c9ACTIF DES ANALYSES
    # =========================================================================
    norm_state <- shiny::reactiveValues(
      test = "shapiro",
      var = NULL,
      group = NULL,
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      group_result = NULL,
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
      effect_size = NULL,
      power = NULL,
      error = NULL
    )

    multi_state <- shiny::reactiveValues(
      test = "anova",
      var_y = NULL,
      var_group = NULL,
      var_group2 = NULL,
      var_covar = NULL,
      var_subject = NULL,
      var_within = NULL,
      interaction = TRUE,
      post_hoc = TRUE,
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      post_hoc_result = NULL,
      effect_size = NULL,
      power = NULL,
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
      effect_size = NULL,
      power = NULL,
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
      effect_size = NULL,
      power = NULL,
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

    prop_state <- shiny::reactiveValues(
      analysis_type = "one_sample",
      var_outcome = NULL,
      success_modality = NULL,
      p0 = 0.5,
      var_group = NULL,
      selected_groups = NULL,
      method = "prop",
      correct = TRUE,
      alternative = "two.sided",
      alpha = 0.05,
      calculated = FALSE,
      result = NULL,
      error = NULL
    )

    # Reinitialisation des etats des tests lors d'un changement de jeu de donnees
    shiny::observeEvent(data_holder$df, {
      # 1. Normalite & Homogeneite
      norm_state$var <- NULL
      norm_state$group <- NULL
      norm_state$calculated <- FALSE
      norm_state$result <- NULL
      norm_state$group_result <- NULL
      norm_state$error <- NULL

      # 2. Deux groupes
      two_state$var_y <- NULL
      two_state$var_group <- NULL
      two_state$var_x2 <- NULL
      two_state$selected_modalities <- NULL
      two_state$calculated <- FALSE
      two_state$result <- NULL
      two_state$effect_size <- NULL
      two_state$power <- NULL
      two_state$error <- NULL

      # 3. 3+ groupes
      multi_state$var_y <- NULL
      multi_state$var_group <- NULL
      multi_state$var_group2 <- NULL
      multi_state$var_covar <- NULL
      multi_state$var_subject <- NULL
      multi_state$var_within <- NULL
      multi_state$calculated <- FALSE
      multi_state$result <- NULL
      multi_state$post_hoc_result <- NULL
      multi_state$effect_size <- NULL
      multi_state$power <- NULL
      multi_state$error <- NULL

      # 4. Contingence
      cont_state$var_row <- NULL
      cont_state$var_col <- NULL
      cont_state$calculated <- FALSE
      cont_state$result <- NULL
      cont_state$tab <- NULL
      cont_state$effect_size <- NULL
      cont_state$power <- NULL
      cont_state$error <- NULL

      # 5. Correlation
      cor_state$var_x <- NULL
      cor_state$var_y <- NULL
      cor_state$calculated <- FALSE
      cor_state$result <- NULL
      cor_state$effect_size <- NULL
      cor_state$power <- NULL
      cor_state$error <- NULL

      # 6. Modelisation / Regression
      reg_state$var_y <- NULL
      reg_state$vars_x <- NULL
      reg_state$calculated <- FALSE
      reg_state$model <- NULL
      reg_state$error <- NULL

      # 7. Proportions
      prop_state$var_outcome <- NULL
      prop_state$success_modality <- NULL
      prop_state$var_group <- NULL
      prop_state$selected_groups <- NULL
      prop_state$calculated <- FALSE
      prop_state$result <- NULL
      prop_state$error <- NULL
    }, ignoreNULL = FALSE)

    # Mise \u00e0 jour automatique des choix de variables dans les interfaces directes
    shiny::observe({
      df <- data_holder$df
      num_cols <- get_num_vars()
      cat_cols <- get_cat_vars()
      all_cols <- if (!is.null(df) && is.data.frame(df)) names(df) else character(0)

      shiny::updateSelectInput(session, "norm_var_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "norm_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)

      shiny::updateSelectInput(session, "two_var_y_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "two_var_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "two_var_x2_direct", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)

      shiny::updateSelectInput(session, "multi_var_y_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "multi_var_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "multi_var_subject_direct", choices = all_cols, selected = if (length(all_cols) > 0) all_cols[1] else NULL)
      shiny::updateSelectInput(session, "multi_var_within_direct", choices = all_cols, selected = if (length(all_cols) > 1) all_cols[2] else NULL)
      shiny::updateSelectInput(session, "multi_var_group2_direct", choices = cat_cols, selected = if (length(cat_cols) > 1) cat_cols[2] else NULL)
      shiny::updateSelectInput(session, "multi_var_covar_direct", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)

      shiny::updateSelectInput(session, "cont_var_row_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "cont_var_col_direct", choices = cat_cols, selected = if (length(cat_cols) > 1) cat_cols[2] else NULL)

      shiny::updateSelectInput(session, "cor_var_x_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "cor_var_y_direct", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)

      reg_choices <- if (!is.null(input$reg_model_direct) && input$reg_model_direct == "linear") num_cols else all_cols
      shiny::updateSelectInput(session, "reg_var_y_direct", choices = reg_choices, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectizeInput(session, "reg_vars_x_direct", choices = all_cols, selected = if (length(num_cols) > 1) num_cols[2] else NULL)

      shiny::updateSelectInput(session, "prop_one_var_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "prop_two_var_outcome_direct", choices = cat_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else NULL)
      shiny::updateSelectInput(session, "prop_two_var_group_direct", choices = cat_cols, selected = if (length(cat_cols) > 1) cat_cols[2] else if (length(cat_cols) > 0) cat_cols[1] else NULL)
    })

    # =========================================================================
    # 1. NORMALIT\u00c9 & HOMOG\u00c9N\u00c9IT\u00c9 DES VARIANCES
    # =========================================================================
    run_norm_analysis <- function(test_type, var_y, group_x, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || !nzchar(var_y)) {
        shiny::showNotification("Veuillez s\u00e9lectionner une variable valide.", type = "warning")
        return()
      }
      norm_state$test <- test_type
      norm_state$var <- var_y
      norm_state$group <- group_x
      norm_state$alpha <- as.numeric(alpha_val)

      val <- df[[norm_state$var]]
      val <- val[!is.na(val)]

      res <- NULL
      grp_res <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        if (norm_state$test == "shapiro") {
          if (length(val) < 3) {
            stop("Effectif insuffisant pour le test de Shapiro-Wilk (minimum 3 observations requis).")
          }
          if (length(val) > 5000) {
            stop("Effectif trop grand pour le test de Shapiro-Wilk (maximum 5000 observations).")
          }
          if (stats::sd(val) == 0) {
            stop("Toutes les valeurs sont identiques (variance nulle) : le test de Shapiro-Wilk ne peut pas \u00eatre calcul\u00e9.")
          }
          res <- stats::shapiro.test(val)
          code_entry <- paste0("# Test de normalite de Shapiro-Wilk\nshapiro.test(", ramses_code_column(ds_name, norm_state$var), ")")
        } else if (norm_state$test == "shapiro_group") {
          shiny::req(norm_state$group)
          grp_res <- ramses_compute_normality_by_group(df, norm_state$var, norm_state$group, norm_state$alpha)
          code_entry <- paste0(
            "# Test de normalite de Shapiro-Wilk par groupe\n",
            "by(", ramses_code_column(ds_name, norm_state$var), ", ", ramses_code_column(ds_name, norm_state$group), ", function(x) {\n",
            "  x <- x[!is.na(x)]\n",
            "  if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x) else list(N = length(x), note = 'Effectif hors limites (3..5000)')\n",
            "})"
          )
        } else if (norm_state$test == "ks") {
          res <- suppressWarnings(stats::ks.test(val, "pnorm", mean = mean(val), sd = stats::sd(val)))
          code_entry <- paste0("# Test de Kolmogorov-Smirnov\nks.test(", ramses_code_column(ds_name, norm_state$var), ", 'pnorm', mean = mean(", ramses_code_column(ds_name, norm_state$var), ", na.rm = TRUE), sd = sd(", ramses_code_column(ds_name, norm_state$var), ", na.rm = TRUE))")
        } else if (norm_state$test == "bartlett") {
          shiny::req(norm_state$group)
          fml <- ramses_formula(response = norm_state$var, terms = norm_state$group)
          fml_code <- ramses_formula_code(response = norm_state$var, terms = norm_state$group)
          res <- stats::bartlett.test(fml, data = df)
          code_entry <- paste0("# Test d'homogeneite des variances de Bartlett\nbartlett.test(", fml_code, ", data = ", ramses_code_symbol(ds_name), ")")
        } else if (norm_state$test == "fligner") {
          shiny::req(norm_state$group)
          fml <- ramses_formula(response = norm_state$var, terms = norm_state$group)
          fml_code <- ramses_formula_code(response = norm_state$var, terms = norm_state$group)
          res <- stats::fligner.test(fml, data = df)
          code_entry <- paste0("# Test de Fligner-Killeen\nfligner.test(", fml_code, ", data = ", ramses_code_symbol(ds_name), ")")
        }
      }, error = function(e) {
        err <- e$message
      })

      norm_state$result <- res
      norm_state$group_result <- grp_res
      norm_state$error <- err
      norm_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Conditions d'application : ", norm_state$test, " (", norm_state$var, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Test de normalit\u00e9/homog\u00e9n\u00e9it\u00e9 ex\u00e9cut\u00e9 !", type = "message")
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
            shiny::tags$span(style = "font-weight: 600;", "Param\u00e8tres : Normalit\u00e9 & Homog\u00e9n\u00e9it\u00e9 des Variances")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_norm"), "Ex\u00e9cuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("norm_test_choice"),
              label = "Test \u00e0 r\u00e9aliser :",
              choices = c(
                "Shapiro-Wilk (normalit\u00e9 n <= 5000)" = "shapiro",
                "Shapiro-Wilk par groupe (normalit\u00e9 par groupe)" = "shapiro_group",
                "Kolmogorov-Smirnov (normalit\u00e9 vs pnorm)" = "ks",
                "Test de Bartlett (homog\u00e9n\u00e9it\u00e9 variances normale)" = "bartlett",
                "Test de Fligner-Killeen (homog\u00e9n\u00e9it\u00e9 non-param\u00e9trique)" = "fligner"
              ),
              selected = norm_state$test
            ),
            shiny::selectInput(
              inputId = ns("norm_var_select"),
              label = "Variable num\u00e9rique d'int\u00e9r\u00eat (Y) :",
              choices = num_cols,
              selected = norm_state$var
            ),
            shiny::conditionalPanel(
              condition = "input.norm_test_choice == 'bartlett' || input.norm_test_choice == 'fligner' || input.norm_test_choice == 'shapiro_group'",
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
              label = "Niveau de significativit\u00e9 (alpha) :",
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
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ex\u00e9cuter' pour lancer le test."))
      }
      if (!is.null(norm_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", norm_state$error)))
      }

      if (norm_state$test == "shapiro_group") {
        if (is.null(norm_state$group_result)) return(NULL)
        st <- norm_state$group_result$summary_table
        n_grps <- nrow(st)
        n_dev <- sum(st$Status == "deviation", na.rm = TRUE)
        badge_cls <- if (n_dev > 0) "alert-warning" else "alert-success"
        msg <- if (n_dev > 0) {
          paste0("\u00c9valuation par groupe : ", n_dev, " groupe(s) sur ", n_grps, " pr\u00e9sente(nt) un \u00e9cart significatif \u00e0 la normalit\u00e9.")
        } else {
          paste0("\u00c9valuation par groupe : l'ensemble des ", n_grps, " groupe(s) est compatible avec la normalit\u00e9.")
        }
        return(shiny::div(
          class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", badge_cls),
          shiny::tags$span(msg),
          shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("Alpha = ", norm_state$alpha))
        ))
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
      shiny::req(norm_state$calculated)
      if (!is.null(norm_state$error)) {
        return(shiny::p(class = "text-danger small", norm_state$error))
      }

      if (norm_state$test == "shapiro_group") {
        shiny::req(norm_state$group_result)
        st <- norm_state$group_result$summary_table

        table_rows <- lapply(seq_len(nrow(st)), function(i) {
          row <- st[i, ]
          w_str <- if (!is.na(row$W)) round(row$W, 4) else "\u2014"
          p_str <- if (!is.na(row$p_value)) format.pval(row$p_value, digits = 4, eps = 0.0001) else "\u2014"

          badge_class <- switch(
            row$Status,
            "compatible" = "bg-success",
            "deviation" = "bg-danger",
            "bg-secondary"
          )

          shiny::tags$tr(
            shiny::tags$td(class = "text-start fw-medium", row$Group),
            shiny::tags$td(row$N),
            shiny::tags$td(w_str),
            shiny::tags$td(class = "fw-bold", p_str),
            shiny::tags$td(
              shiny::tags$span(class = paste0("badge ", badge_class), row$Conclusion)
            )
          )
        })

        tiles_norm_grp <- list(
          list(label = "Groupes analys\u00e9s", value = as.character(nrow(st)), subtext = paste0("Facteur : ", norm_state$group)),
          list(label = "Effectif total (N)", value = as.character(sum(st$N)), subtext = "Observations"),
          list(label = "Groupes normaux", value = paste0(sum(st$Status == "compatible"), " / ", nrow(st)), subtext = paste0("\u03b1 = ", norm_state$alpha)),
          list(label = "Statut global", value = if (all(st$Status == "compatible")) "Compatible" else "\u00c9cart d\u00e9tect\u00e9", status = if (all(st$Status == "compatible")) "success" else "warning", subtext = if (all(st$Status == "compatible")) "Tous groupes normaux" else paste0(sum(st$Status == "deviation"), " groupe(s) d\u00e9viant(s)"))
        )

        return(shiny::tagList(
          ramses_result_tiles(tiles_norm_grp, title = paste0("R\u00e9sum\u00e9 du test de normalit\u00e9 par groupe (", norm_state$var, ")")),
          shiny::tags$div(
            class = "table-responsive",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-3",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th(paste0("Groupe (", norm_state$group, ")")),
                  shiny::tags$th("Effectif (N)"),
                  shiny::tags$th("Statistique W"),
                  shiny::tags$th("p-value"),
                  shiny::tags$th("Conclusion (Shapiro-Wilk)")
                )
              ),
              shiny::tags$tbody(table_rows)
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "fw-bold text-dark mb-1", "\u00c9l\u00e9ments d'interpr\u00e9tation & Pr\u00e9cautions m\u00e9thodologiques :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              shiny::tags$li(
                shiny::tags$strong("Interpr\u00e9tation du p-value : "),
                "Une p-value \u2265 ", norm_state$alpha, " indique qu'aucune d\u00e9viation significative \u00e0 la normalit\u00e9 n'a \u00e9t\u00e9 d\u00e9tect\u00e9e par le test (compatible avec l'hypoth\u00e8se de normalit\u00e9). Cela ne constitue pas une preuve absolue de normalit\u00e9 parfaite."
              ),
              shiny::tags$li(
                shiny::tags$strong("\u00c9valuation graphique compl\u00e9mentaire : "),
                "Le test de Shapiro-Wilk doit toujours \u00eatre compl\u00e9t\u00e9 par une inspection visuelle. Veuillez consulter l'onglet 'Graphiques d'\u00e9valuation' (Q-Q plot par groupe) afin d'examiner l'alignement des points observ\u00e9s."
              )
            )
          )
        ))
      }

      res <- norm_state$result
      p_val <- res$p.value
      stat_val <- unname(res$statistic)
      stat_name <- names(res$statistic)

      is_normal_rejected <- p_val < norm_state$alpha
      interp_text <- if (norm_state$test %in% c("shapiro", "ks")) {
        if (is_normal_rejected) {
          "La p-value est inf\u00e9rieure au seuil critique : l'hypoth\u00e8se nulle de normalit\u00e9 est rejet\u00e9e. Les donn\u00e9es ne suivent pas une distribution normale standard (envisager un test non param\u00e9trique)."
        } else {
          "La p-value est sup\u00e9rieure au seuil critique : on ne rejette pas l'hypoth\u00e8se de normalit\u00e9. Les conditions d'application pour les tests param\u00e9triques semblent satisfaites."
        }
      } else {
        if (is_normal_rejected) {
          "L'hypoth\u00e8se d'\u00e9galit\u00e9 des variances est rejet\u00e9e (h\u00e9t\u00e9rosc\u00e9dasticit\u00e9). Utiliser la correction de Welch."
        } else {
          "L'hypoth\u00e8se d'homog\u00e9n\u00e9it\u00e9 des variances (homosc\u00e9dasticit\u00e9) est accept\u00e9e."
        }
      }

      df_act <- data_holder$df
      n_obs <- if (!is.null(df_act) && !is.null(norm_state$var) && norm_state$var %in% names(df_act)) sum(!is.na(df_act[[norm_state$var]])) else "\u2014"
      tiles_norm_single <- list(
        list(label = "Variable", value = norm_state$var, subtext = res$method),
        list(label = "Effectif (n)", value = as.character(n_obs), subtext = "Observations valides"),
        list(label = paste0("Statistique (", stat_name, ")"), value = as.character(round(stat_val, 4)), subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else NULL),
        list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = paste0("Seuil \u03b1 = ", norm_state$alpha), status = if (is_normal_rejected) "danger" else "success"),
        list(label = "D\u00e9cision", value = if (is_normal_rejected) "Rejet H0" else "Conservation H0", status = if (is_normal_rejected) "danger" else "success", subtext = if (is_normal_rejected) "Non normal" else "Normal")
      )

      shiny::tagList(
        ramses_result_tiles(tiles_norm_single, title = "Indicateurs cl\u00e9s du test"),
        shiny::tags$table(
          class = "table table-sm table-bordered text-center align-middle mb-3",
          shiny::tags$thead(
            class = "table-light",
            shiny::tags$tr(
              shiny::tags$th("M\u00e9thode"),
              shiny::tags$th("Statistique"),
              shiny::tags$th("ddl (si applicable)"),
              shiny::tags$th("p-value"),
              shiny::tags$th("D\u00e9cision (alpha)")
            )
          ),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "text-start fw-medium", res$method),
              shiny::tags$td(paste0(stat_name, " = ", round(stat_val, 4))),
              shiny::tags$td(if (!is.null(res$parameter)) round(res$parameter, 2) else "\u2014"),
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
          shiny::tags$strong("Interpr\u00e9tation : "),
          interp_text
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher la sortie console R brute"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(utils::capture.output(print(res)), collapse = "\n"))
        )
      )
    })

    output$norm_plot <- plotly::renderPlotly({
      df <- data_holder$df
      shiny::req(df, norm_state$var)

      if (norm_state$test == "shapiro_group") {
        shiny::req(norm_state$group_result)
        qq_df <- norm_state$group_result$qq_data
        shiny::req(nrow(qq_df) > 0)

        grp_var_name <- norm_state$group

        p <- ggplot2::ggplot(qq_df, ggplot2::aes(x = q_theoretical, y = y_observed, color = .data[["group"]])) +
          ggplot2::geom_point(alpha = 0.8, size = 2) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#EF4444", linewidth = 0.8) +
          ggplot2::facet_wrap(~ group, scales = "free") +
          ggplot2::labs(
            title = paste0("Q-Q Plots de normalit\u00e9 par groupe : ", norm_state$var, " selon ", grp_var_name),
            x = "Quantiles th\u00e9oriques (Normale)",
            y = "Quantiles observ\u00e9s",
            color = grp_var_name
          ) +
          ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
          ggplot2::theme(
            text = ggplot2::element_text(family = "sans"),
            plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
            axis.title = ggplot2::element_text(size = 11, color = "#374151"),
            axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
            strip.text = ggplot2::element_text(face = "bold", size = 10, color = "#1F2937"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
          )

        return(plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans")))
      }

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
            name = "Points observ\u00e9s",
            marker = list(color = "#3b82f6", size = 6, opacity = 0.8)
          ) %>%
          plotly::add_lines(
            x = range(theo_quantiles),
            y = range(theo_quantiles),
            name = "Droite th\u00e9orique normale",
            line = list(color = "#ef4444", dash = "dash", width = 2)
          ) %>%
          plotly::layout(font = list(family = "IBM Plex Sans"), 
            title = list(text = paste0("Q-Q Plot Normal : ", norm_state$var), font = list(size = 13)),
            xaxis = list(title = "Quantiles th\u00e9oriques (Normale)"),
            yaxis = list(title = "Quantiles observ\u00e9s"),
            hovermode = "closest"
          )
      } else {
        plotly::plot_ly(x = vals, type = "histogram", nbinsx = 25, marker = list(color = "#6366f1", line = list(color = "white", width = 1))) %>%
          plotly::layout(font = list(family = "IBM Plex Sans"), 
            title = list(text = paste0("Distribution de ", norm_state$var), font = list(size = 13)),
            xaxis = list(title = norm_state$var),
            yaxis = list(title = "Effectif (Fr\u00e9quence)")
          )
      }
    })

    output$norm_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_norm(norm_state, data_holder$df)
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
            paste0("Variable \u00e0 ", length(mods), " modalit\u00e9s : veuillez en s\u00e9lectionner exactement 2 pour le test.")
          ),
          shiny::selectizeInput(
            inputId = ns("two_selected_modalities_direct"),
            label = "Modalit\u00e9s \u00e0 comparer (exactement 2) :",
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
          paste0("2 modalit\u00e9s d\u00e9tect\u00e9es : ", mods[1], " vs ", mods[2])
        )
      } else if (length(mods) < 2 && length(mods) > 0) {
        shiny::div(
          class = "alert alert-danger py-1 px-2 small mb-2",
          "La variable de groupe s\u00e9lectionn\u00e9e poss\u00e8de moins de 2 modalit\u00e9s valides."
        )
      }
    })

    output$two_modalities_warning_direct <- shiny::renderUI({
      sel <- input$two_selected_modalities_direct
      if (!is.null(sel) && length(sel) != 2) {
        shiny::div(
          class = "text-danger small fw-semibold mt-1",
          "Attention : vous devez s\u00e9lectionner exactement 2 modalit\u00e9s."
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
            paste0("Variable \u00e0 ", length(mods), " modalit\u00e9s : veuillez en s\u00e9lectionner exactement 2 pour le test.")
          ),
          shiny::selectizeInput(
            inputId = ns("two_selected_modalities_modal"),
            label = "Modalit\u00e9s \u00e0 comparer (exactement 2) :",
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
          paste0("2 modalit\u00e9s d\u00e9tect\u00e9es : ", mods[1], " vs ", mods[2])
        )
      } else if (length(mods) < 2 && length(mods) > 0) {
        shiny::div(
          class = "alert alert-danger py-1 px-2 small mb-2",
          "La variable de groupe s\u00e9lectionn\u00e9e poss\u00e8de moins de 2 modalit\u00e9s valides."
        )
      }
    })

    output$two_modalities_warning_modal <- shiny::renderUI({
      sel <- input$two_selected_modalities_modal
      if (!is.null(sel) && length(sel) != 2) {
        shiny::div(
          class = "text-danger small fw-semibold mt-1",
          "Attention : vous devez s\u00e9lectionner exactement 2 modalit\u00e9s."
        )
      }
    })

    run_two_analysis <- function(test_type, var_y, var_group, var_x2, mu_val, selected_modalities, alt, var_eq, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || !nzchar(var_y)) {
        shiny::showNotification("Veuillez s\u00e9lectionner une variable valide.", type = "warning")
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
      es <- NULL
      pwr <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        if (two_state$test == "t_one_sample") {
          two_state$method_note <- NULL
          shiny::req(two_state$var_y)
          y_vals <- df[[two_state$var_y]]
          y_vals <- y_vals[!is.na(y_vals)]
          res <- stats::t.test(
            y_vals,
            mu = two_state$mu_val,
            alternative = two_state$alternative,
            conf.level = 1 - two_state$alpha
          )
          es <- ramses_compute_effect_size("t_one_sample", y1 = y_vals, mu = two_state$mu_val)
          pwr <- ramses_compute_power("t_one_sample", y1 = y_vals, mu = two_state$mu_val, alpha = two_state$alpha, alternative = two_state$alternative)
          code_entry <- paste0(
            "# Test t de Student a echantillon unique (comparaison a mu = ", two_state$mu_val, ")\n",
            "t.test(", ramses_code_column(ds_name, two_state$var_y), ", mu = ", two_state$mu_val,
            ", alternative = ", ramses_code_string(two_state$alternative), ")"
          )

        } else if (two_state$test == "wilcox_one_sample") {
          shiny::req(two_state$var_y)
          y_vals <- df[[two_state$var_y]]
          y_vals <- y_vals[!is.na(y_vals)]
          diffs <- y_vals - two_state$mu_val
          diffs_nz <- diffs[diffs != 0]
          has_ties_or_zeros <- (length(diffs) != length(diffs_nz)) || any(duplicated(abs(diffs_nz)))
          use_exact <- !has_ties_or_zeros
          if (!use_exact) {
            two_state$method_note <- "Des ex-aequo ou des z\u00e9ros par rapport \u00e0 la m\u00e9diane th\u00e9orique sont pr\u00e9sents. Le calcul exact n'est pas disponible ; une approximation asymptotique a \u00e9t\u00e9 utilis\u00e9e pour la p-value et l'intervalle de confiance."
          } else {
            two_state$method_note <- NULL
          }
          res <- stats::wilcox.test(
            y_vals,
            mu = two_state$mu_val,
            alternative = two_state$alternative,
            conf.int = TRUE,
            exact = use_exact
          )
          es <- ramses_compute_effect_size("wilcox_one_sample", stat_result = res, y1 = y_vals, mu = two_state$mu_val)
          pwr <- NULL
          code_entry <- paste0(
            "# Test de Wilcoxon signe a echantillon unique (comparaison a mu = ", two_state$mu_val, ")\n",
            "wilcox.test(", ramses_code_column(ds_name, two_state$var_y), ", mu = ", two_state$mu_val,
            ", alternative = ", ramses_code_string(two_state$alternative),
            if (!use_exact) ", exact = FALSE" else "", ")"
          )

        } else if (two_state$test == "t_indep") {
          two_state$method_note <- NULL
          shiny::req(two_state$var_group)
          all_grps <- unique(as.character(df[[two_state$var_group]]))
          all_grps <- all_grps[!is.na(all_grps)]
          fml <- ramses_formula(response = two_state$var_y, terms = two_state$var_group)
          fml_code <- ramses_formula_code(response = two_state$var_y, terms = two_state$var_group)

          if (length(all_grps) > 2) {
            if (is.null(two_state$selected_modalities) || length(two_state$selected_modalities) != 2) {
              stop("Veuillez selectionner exactement 2 modalites a comparer pour la variable de groupe.")
            }
            mods <- two_state$selected_modalities
            sub_df <- subset(df, df[[two_state$var_group]] %in% mods)
            res <- stats::t.test(
              fml,
              data = sub_df,
              var.equal = two_state$var_equal,
              alternative = two_state$alternative,
              conf.level = 1 - two_state$alpha
            )
            y1_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[1]]
            y2_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[2]]
            es <- ramses_compute_effect_size("t_indep", y1 = y1_v, y2 = y2_v)
            pwr <- ramses_compute_power("t_indep", y1 = y1_v, y2 = y2_v, alpha = two_state$alpha, alternative = two_state$alternative)
            mods_code <- ramses_code_string(mods)
            code_entry <- paste0(
              "# Sous-ensemble filtre sur les 2 modalites a comparer\n",
              "data_sub <- subset(", ramses_code_symbol(ds_name), ", ", ramses_code_column(ds_name, two_state$var_group), " %in% ", mods_code, ")\n",
              "# Test t de Student pour 2 echantillons independants\n",
              "t.test(", fml_code, ", data = data_sub",
              ", var.equal = ", two_state$var_equal, ", alternative = ", ramses_code_string(two_state$alternative), ")"
            )
          } else {
            mods <- all_grps
            two_state$selected_modalities <- mods
            sub_df <- df[!is.na(df[[two_state$var_group]]) & df[[two_state$var_group]] %in% mods, ]
            res <- stats::t.test(
              fml,
              data = sub_df,
              var.equal = two_state$var_equal,
              alternative = two_state$alternative,
              conf.level = 1 - two_state$alpha
            )
            y1_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[1]]
            y2_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[2]]
            es <- ramses_compute_effect_size("t_indep", y1 = y1_v, y2 = y2_v)
            pwr <- ramses_compute_power("t_indep", y1 = y1_v, y2 = y2_v, alpha = two_state$alpha, alternative = two_state$alternative)
            code_entry <- paste0(
              "# Test t de Student pour 2 echantillons independants\n",
              "t.test(", fml_code, ", data = ", ramses_code_symbol(ds_name),
              ", var.equal = ", two_state$var_equal, ", alternative = ", ramses_code_string(two_state$alternative), ")"
            )
          }

        } else if (two_state$test == "wilcox_indep") {
          shiny::req(two_state$var_group)
          all_grps <- unique(as.character(df[[two_state$var_group]]))
          all_grps <- all_grps[!is.na(all_grps)]
          fml <- ramses_formula(response = two_state$var_y, terms = two_state$var_group)
          fml_code <- ramses_formula_code(response = two_state$var_y, terms = two_state$var_group)

          if (length(all_grps) > 2) {
            if (is.null(two_state$selected_modalities) || length(two_state$selected_modalities) != 2) {
              stop("Veuillez selectionner exactement 2 modalites a comparer pour la variable de groupe.")
            }
            mods <- two_state$selected_modalities
            sub_df <- subset(df, df[[two_state$var_group]] %in% mods)
            y1_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[1]]
            y2_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[2]]
            y1_v <- y1_v[!is.na(y1_v)]
            y2_v <- y2_v[!is.na(y2_v)]
            has_ties <- any(duplicated(c(y1_v, y2_v)))
            use_exact <- !has_ties
            if (!use_exact) {
              two_state$method_note <- "Des ex-aequo sont pr\u00e9sents dans les donn\u00e9es. Le calcul exact n'est pas disponible ; une approximation asymptotique a \u00e9t\u00e9 utilis\u00e9e pour la p-value et l'intervalle de confiance."
            } else {
              two_state$method_note <- NULL
            }
            res <- stats::wilcox.test(
              fml,
              data = sub_df,
              alternative = two_state$alternative,
              conf.int = TRUE,
              exact = use_exact
            )
            es <- ramses_compute_effect_size("wilcox_indep", stat_result = res, y1 = y1_v, y2 = y2_v)
            pwr <- NULL
            mods_code <- ramses_code_string(mods)
            code_entry <- paste0(
              "# Sous-ensemble filtre sur les 2 modalites a comparer\n",
              "data_sub <- subset(", ramses_code_symbol(ds_name), ", ", ramses_code_column(ds_name, two_state$var_group), " %in% ", mods_code, ")\n",
              "# Test de Wilcoxon / Mann-Whitney U pour 2 echantillons independants\n",
              "wilcox.test(", fml_code, ", data = data_sub",
              ", alternative = ", ramses_code_string(two_state$alternative),
              if (!use_exact) ", exact = FALSE" else "", ")"
            )
          } else {
            mods <- all_grps
            two_state$selected_modalities <- mods
            sub_df <- df[!is.na(df[[two_state$var_group]]) & df[[two_state$var_group]] %in% mods, ]
            y1_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[1]]
            y2_v <- sub_df[[two_state$var_y]][as.character(sub_df[[two_state$var_group]]) == mods[2]]
            y1_v <- y1_v[!is.na(y1_v)]
            y2_v <- y2_v[!is.na(y2_v)]
            has_ties <- any(duplicated(c(y1_v, y2_v)))
            use_exact <- !has_ties
            if (!use_exact) {
              two_state$method_note <- "Des ex-aequo sont pr\u00e9sents dans les donn\u00e9es. Le calcul exact n'est pas disponible ; une approximation asymptotique a \u00e9t\u00e9 utilis\u00e9e pour la p-value et l'intervalle de confiance."
            } else {
              two_state$method_note <- NULL
            }
            res <- stats::wilcox.test(
              fml,
              data = sub_df,
              alternative = two_state$alternative,
              conf.int = TRUE,
              exact = use_exact
            )
            es <- ramses_compute_effect_size("wilcox_indep", stat_result = res, y1 = y1_v, y2 = y2_v)
            pwr <- NULL
            code_entry <- paste0(
              "# Test de Wilcoxon / Mann-Whitney U pour 2 echantillons independants\n",
              "wilcox.test(", fml_code, ", data = ", ramses_code_symbol(ds_name),
              ", alternative = ", ramses_code_string(two_state$alternative),
              if (!use_exact) ", exact = FALSE" else "", ")"
            )
          }

        } else if (two_state$test == "t_paired") {
          two_state$method_note <- NULL
          shiny::req(two_state$var_x2)
          res <- stats::t.test(
            df[[two_state$var_y]],
            df[[two_state$var_x2]],
            paired = TRUE,
            alternative = two_state$alternative,
            conf.level = 1 - two_state$alpha
          )
          v1 <- df[[two_state$var_y]]
          v2 <- df[[two_state$var_x2]]
          es <- ramses_compute_effect_size("t_paired", y1 = v1, y2 = v2)
          pwr <- ramses_compute_power("t_paired", y1 = v1, y2 = v2, alpha = two_state$alpha, alternative = two_state$alternative)
          code_entry <- paste0(
            "# Test t de Student pour series appariees\n",
            "t.test(", ramses_code_column(ds_name, two_state$var_y), ", ", ramses_code_column(ds_name, two_state$var_x2),
            ", paired = TRUE, alternative = ", ramses_code_string(two_state$alternative), ")"
          )

        } else if (two_state$test == "wilcox_paired") {
          shiny::req(two_state$var_x2)
          v1 <- df[[two_state$var_y]]
          v2 <- df[[two_state$var_x2]]
          cc <- !is.na(v1) & !is.na(v2)
          v1 <- v1[cc]
          v2 <- v2[cc]
          d <- v1 - v2
          d_nz <- d[d != 0]
          has_ties_or_zeros <- (length(d) != length(d_nz)) || any(duplicated(abs(d_nz)))
          use_exact <- !has_ties_or_zeros
          if (!use_exact) {
            two_state$method_note <- "Des ex-aequo ou des diff\u00e9rences nulles sont pr\u00e9sents dans les paires. Le calcul exact n'est pas disponible ; une approximation asymptotique a \u00e9t\u00e9 utilis\u00e9e pour la p-value et l'intervalle de confiance."
          } else {
            two_state$method_note <- NULL
          }
          res <- stats::wilcox.test(
            v1,
            v2,
            paired = TRUE,
            alternative = two_state$alternative,
            conf.int = TRUE,
            exact = use_exact
          )
          es <- ramses_compute_effect_size("wilcox_paired", stat_result = res, y1 = v1, y2 = v2)
          pwr <- NULL
          code_entry <- paste0(
            "# Test de Wilcoxon signe pour series appariees\n",
            "wilcox.test(", ramses_code_column(ds_name, two_state$var_y), ", ", ramses_code_column(ds_name, two_state$var_x2),
            ", paired = TRUE, alternative = ", ramses_code_string(two_state$alternative),
            if (!use_exact) ", exact = FALSE" else "", ")"
          )
        }
      }, error = function(e) {
        err <<- e$message
      })

      two_state$result <- res
      two_state$effect_size <- es
      two_state$power <- pwr
      two_state$error <- err
      two_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Test t / Comparaison : ", two_state$test),
          code = code_entry
        )
      }
      if (is.null(err)) {
        shiny::showNotification("Test statistique ex\u00e9cut\u00e9 avec succ\u00e8s !", type = "message")
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
            shiny::tags$span(style = "font-weight: 600;", "Param\u00e8tres : Test t & Wilcoxon")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_two"), "Ex\u00e9cuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("two_test_choice"),
              label = "Type de test :",
              choices = c(
                "\u00c9chantillon unique (comparaison \u00e0 une moyenne th\u00e9orique mu)" = "t_one_sample",
                "Deux \u00e9chantillons ind\u00e9pendants" = "t_indep",
                "\u00c9chantillons appari\u00e9s" = "t_paired",
                "Test non-param\u00e9trique : Wilcoxon sign\u00e9 (\u00e9chantillon unique)" = "wilcox_one_sample",
                "Test non-param\u00e9trique : Wilcoxon / Mann-Whitney (ind\u00e9pendant)" = "wilcox_indep",
                "Test non-param\u00e9trique : Wilcoxon sign\u00e9 (appari\u00e9)" = "wilcox_paired"
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
                label = "Moyenne th\u00e9orique (mu) :",
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
                  label = "Supposer l'\u00e9galit\u00e9 des variances (Student standard au lieu de Welch)",
                  value = two_state$var_equal
                )
              )
            ),
            shiny::conditionalPanel(
              condition = "input.two_test_choice == 't_paired' || input.two_test_choice == 'wilcox_paired'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("two_var_x2"),
                label = "Seconde variable quantitative appari\u00e9e (Y2) :",
                choices = num_cols,
                selected = two_state$var_x2
              )
            ),
            shiny::selectInput(
              inputId = ns("two_alternative"),
              label = "Hypoth\u00e8se alternative :",
              choices = c(
                "Bilat\u00e9rale (diff\u00e9rence != 0 / moyenne != mu)" = "two.sided",
                "Unilat\u00e9rale gauche (moyenne < mu / groupe 1 < groupe 2)" = "less",
                "Unilat\u00e9rale droite (moyenne > mu / groupe 1 > groupe 2)" = "greater"
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
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "S\u00e9lectionnez vos param\u00e8tres et cliquez sur 'Ex\u00e9cuter & Journaliser'."))
      }
      if (!is.null(two_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", two_state$error)))
      }
      p_val <- two_state$result$p.value
      sig <- p_val < two_state$alpha
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) "Diff\u00e9rence statistiquement significative (H0 rejet\u00e9e) !" else "Pas de diff\u00e9rence statistiquement significative (H0 conserv\u00e9e)"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 4, eps = 0.0001)))
      )
    })

    output$two_results_ui <- shiny::renderUI({
      shiny::req(two_state$calculated)
      if (!is.null(two_state$error)) {
        return(shiny::p(class = "text-danger small", two_state$error))
      }
      res <- two_state$result
      p_val <- res$p.value
      stat_val <- unname(res$statistic)
      stat_name <- names(res$statistic)
      sig <- p_val < two_state$alpha
      es <- two_state$effect_size
      pwr <- two_state$power

      is_one_sample <- two_state$test %in% c("t_one_sample", "wilcox_one_sample")
      is_paired <- two_state$test %in% c("t_paired", "wilcox_paired")

      decision_text <- if (is_one_sample) {
        obs_val <- if (!is.null(res$estimate)) round(res$estimate[1], 3) else "observ\u00e9e"
        if (sig) {
          paste0("Au risque alpha = ", two_state$alpha, ", la moyenne observ\u00e9e de '", two_state$var_y, "' (", obs_val, ") diff\u00e8re significativement de la moyenne th\u00e9orique mu = ", two_state$mu_val, " (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle H0 d'\u00e9galit\u00e9 est rejet\u00e9e.")
        } else {
          paste0("Au risque alpha = ", two_state$alpha, ", la moyenne observ\u00e9e de '", two_state$var_y, "' (", obs_val, ") ne diff\u00e8re pas significativement de la moyenne th\u00e9orique mu = ", two_state$mu_val, " (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle H0 est conserv\u00e9e.")
        }
      } else if (is_paired) {
        if (sig) {
          paste0("Au risque alpha = ", two_state$alpha, ", la diff\u00e9rence moyenne entre les s\u00e9ries appari\u00e9es '", two_state$var_y, "' et '", two_state$var_x2, "' est statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle H0 est rejet\u00e9e.")
        } else {
          paste0("Au risque alpha = ", two_state$alpha, ", la diff\u00e9rence moyenne entre les s\u00e9ries appari\u00e9es n'est pas statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle H0 est conserv\u00e9e.")
        }
      } else {
        if (sig) {
          paste0("Au risque alpha = ", two_state$alpha, ", la diff\u00e9rence observ\u00e9e entre les deux groupes de '", two_state$var_group, "' est statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'\u00e9galit\u00e9 des distributions / moyennes est rejet\u00e9e.")
        } else {
          paste0("Au risque alpha = ", two_state$alpha, ", la diff\u00e9rence observ\u00e9e entre les deux groupes n'est pas statistiquement significative (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'\u00e9galit\u00e9 est conserv\u00e9e.")
        }
      }

      estimate_display <- if (is_one_sample) {
        if (!is.null(res$estimate)) paste0("Moy. = ", round(res$estimate[1], 3), " (vs mu = ", two_state$mu_val, ")") else paste0("mu = ", two_state$mu_val)
      } else if (is_paired) {
        if (!is.null(res$estimate)) paste0("Diff. moy. = ", round(res$estimate[1], 3)) else "\u2014"
      } else {
        if (!is.null(res$estimate) && length(res$estimate) >= 2) {
          paste0(names(res$estimate)[1], ": ", round(res$estimate[1], 3), " | ", names(res$estimate)[2], ": ", round(res$estimate[2], 3))
        } else if (!is.null(res$estimate)) {
          paste0(round(res$estimate[1], 3))
        } else {
          "\u2014"
        }
      }

      es_display <- if (!is.null(es)) {
        paste0(es$symbol, " = ", es$formatted_value, " (", es$magnitude, ")")
      } else {
        "\u2014"
      }

      pwr_display <- if (!is.null(pwr)) {
        paste0(pwr$percentage, " (", pwr$magnitude, ")")
      } else {
        "\u2014"
      }

      tiles_two <- if (two_state$test == "t_one_sample") {
        obs_m <- if (!is.null(res$estimate)) round(res$estimate[1], 3) else "\u2014"
        list(
          list(label = "Moyenne observ\u00e9e", value = as.character(obs_m), subtext = two_state$var_y),
          list(label = "Moyenne th\u00e9orique", value = as.character(two_state$mu_val), subtext = "Valeur mu test\u00e9e"),
          list(label = "Statistique t", value = as.character(round(stat_val, 3)), subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else NULL),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "Taille d'effet", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) paste0(es$symbol, " (", es$magnitude, ")") else NULL)
        )
      } else if (two_state$test == "t_indep") {
        m1 <- if (!is.null(res$estimate) && length(res$estimate) >= 1) round(res$estimate[1], 3) else "\u2014"
        m2 <- if (!is.null(res$estimate) && length(res$estimate) >= 2) round(res$estimate[2], 3) else "\u2014"
        lbl1 <- if (!is.null(names(res$estimate)) && length(names(res$estimate)) >= 1) names(res$estimate)[1] else "Groupe 1"
        lbl2 <- if (!is.null(names(res$estimate)) && length(names(res$estimate)) >= 2) names(res$estimate)[2] else "Groupe 2"
        diff_m <- if (!is.null(res$estimate) && length(res$estimate) >= 2) round(res$estimate[1] - res$estimate[2], 3) else "\u2014"
        list(
          list(label = lbl1, value = as.character(m1), subtext = "Moyenne"),
          list(label = lbl2, value = as.character(m2), subtext = "Moyenne"),
          list(label = "Diff\u00e9rence", value = as.character(diff_m), subtext = "M1 - M2"),
          list(label = "Statistique t", value = as.character(round(stat_val, 3)), subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else NULL),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "d de Cohen", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) es$magnitude else NULL)
        )
      } else if (two_state$test == "t_paired") {
        diff_m <- if (!is.null(res$estimate)) round(res$estimate[1], 3) else "\u2014"
        list(
          list(label = "S\u00e9rie 1", value = two_state$var_y, subtext = "Avant / Var 1"),
          list(label = "S\u00e9rie 2", value = two_state$var_x2, subtext = "Apr\u00e8s / Var 2"),
          list(label = "Diff\u00e9rence moyenne", value = as.character(diff_m), subtext = "Moyenne diff."),
          list(label = "Statistique t", value = as.character(round(stat_val, 3)), subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else NULL),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "Taille d'effet", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) paste0(es$symbol, " (", es$magnitude, ")") else NULL)
        )
      } else if (two_state$test == "wilcox_indep") {
        list(
          list(label = "Variable", value = two_state$var_y, subtext = "Test de Mann-Whitney"),
          list(label = "Facteur", value = two_state$var_group, subtext = "Comparaison 2 groupes"),
          list(label = "Statistique W", value = as.character(round(stat_val, 2)), subtext = "Somme des rangs"),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "Taille d'effet (r)", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) es$magnitude else NULL)
        )
      } else if (two_state$test == "wilcox_paired") {
        pseudo_m <- if (!is.null(res$estimate)) round(res$estimate[1], 3) else "\u2014"
        list(
          list(label = "S\u00e9rie 1", value = two_state$var_y, subtext = "Avant / Var 1"),
          list(label = "S\u00e9rie 2", value = two_state$var_x2, subtext = "Apr\u00e8s / Var 2"),
          list(label = "Pseudo-m\u00e9diane", value = as.character(pseudo_m), subtext = "Diff\u00e9rence"),
          list(label = "Statistique V", value = as.character(round(stat_val, 2)), subtext = "Rangs sign\u00e9s"),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "Taille d'effet (r)", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) es$magnitude else NULL)
        )
      } else {
        pseudo_m <- if (!is.null(res$estimate)) round(res$estimate[1], 3) else "\u2014"
        list(
          list(label = "Variable", value = two_state$var_y, subtext = "1 \u00e9chantillon"),
          list(label = "Valeur mu test\u00e9e", value = as.character(two_state$mu_val), subtext = "Th\u00e9orique"),
          list(label = "Pseudo-m\u00e9diane", value = as.character(pseudo_m), subtext = "Observ\u00e9e"),
          list(label = "Statistique V", value = as.character(round(stat_val, 2)), subtext = "Rangs sign\u00e9s"),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark")
        )
      }

      shiny::tagList(
        ramses_result_tiles(tiles_two, title = "Indicateurs cl\u00e9s du test"),
        shiny::tags$div(
          class = "table-responsive",
          shiny::tags$table(
            class = "table table-sm table-bordered text-center align-middle mb-3",
            shiny::tags$thead(
              class = "table-light",
              shiny::tags$tr(
                shiny::tags$th("M\u00e9thode"),
                shiny::tags$th("Statistique"),
                shiny::tags$th("ddl"),
                shiny::tags$th("Estimation observ\u00e9e"),
                shiny::tags$th("p-value"),
                shiny::tags$th("Taille d'effet"),
                shiny::tags$th("Puissance a posteriori"),
                shiny::tags$th("D\u00e9cision")
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-start fw-medium", res$method),
                shiny::tags$td(paste0(stat_name, " = ", round(stat_val, 3))),
                shiny::tags$td(if (!is.null(res$parameter)) round(res$parameter, 2) else "\u2014"),
                shiny::tags$td(class = "small", estimate_display),
                shiny::tags$td(class = "fw-bold text-primary", format.pval(p_val, digits = 4, eps = 0.0001)),
                shiny::tags$td(class = "small", es_display),
                shiny::tags$td(class = "small", pwr_display),
                shiny::tags$td(
                  shiny::tags$span(
                    class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                    if (sig) "Significatif (Rejet H0)" else "Non significatif (Conservation H0)"
                  )
                )
              )
            )
          )
        ),
        if (!is.null(res$conf.int)) {
          shiny::div(
            class = "small text-muted mb-2",
            paste0("Intervalle de confiance \u00e0 ", round((1 - two_state$alpha)*100), "% : [",
                   round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
          )
        },
        if (!is.null(two_state$method_note)) {
          shiny::div(
            class = "alert alert-warning py-2 px-3 small mb-3 border-warning",
            shiny::tags$strong("Note de m\u00e9thode : "),
            two_state$method_note
          )
        },
        shiny::div(
          class = "p-3 rounded bg-light border text-secondary small mb-3",
          shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
          shiny::tags$ul(
            class = "mb-0 ps-3 space-y-1",
            shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), decision_text),
            if (!is.null(es)) shiny::tags$li(shiny::tags$strong(paste0("Taille d'effet (", es$name, ") : ")), es$description),
            if (!is.null(pwr)) shiny::tags$li(shiny::tags$strong("Puissance statistique : "), paste0("La puissance a posteriori calcul\u00e9e est de ", pwr$percentage, " (", pwr$magnitude, "). Le risque beta de faux n\u00e9gatif associ\u00e9 est d'environ ", round((1 - pwr$value)*100, 1), "%."))
          )
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher la sortie console R d\u00e9taill\u00e9e"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(utils::capture.output(print(res)), collapse = "\n"))
        )
      )
    })

    output$two_plot <- plotly::renderPlotly({
      df <- data_holder$df
      shiny::req(df, two_state$var_y)

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
            name = paste0("mu th\u00e9orique = ", mu_val),
            line = list(color = "#DC2626", dash = "dash", width = 2),
            inherit = FALSE
          ) %>%
          plotly::layout(font = list(family = "IBM Plex Sans"), 
            title = list(text = paste0("Distribution de ", two_state$var_y, " vs moyenne th\u00e9orique mu = ", mu_val), font = list(size = 13)),
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
          form_t <- ramses_formula(response = two_state$var_y, terms = grp_col)
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
            es_str <- if (!is.null(two_state$effect_size)) paste0(" | d = ", two_state$effect_size$formatted_value) else ""
            paste0("Test t : t = ", t_stat, ", ", p_str, es_str)
          } else {
            paste0("Test t : ", two_state$var_y, " selon ", grp_col)
          }

          # Calcul des moyennes pour affichage visuel sur le boxplot
          means_df <- tryCatch({
            stats::aggregate(clean_df[[two_state$var_y]] ~ clean_df[[grp_col]], data = clean_df, FUN = mean)
          }, error = function(e) NULL)
          if (!is.null(means_df)) {
            names(means_df) <- c(grp_col, two_state$var_y)
          }

          p <- ggplot2::ggplot(clean_df, ggplot2::aes(x = .data[[grp_col]], y = .data[[two_state$var_y]], group = .data[[grp_col]])) +
            ggplot2::geom_boxplot(fill = "#F3F4F6", color = "#1F2937", width = 0.4, outlier.shape = NA) +
            ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "#4B5563") +
            ggplot2::labs(
              title = t_title,
              x = grp_col,
              y = two_state$var_y
            ) +
            ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
            ggplot2::theme(
              text = ggplot2::element_text(family = "sans"),
              plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
              axis.title = ggplot2::element_text(size = 11, color = "#374151"),
              axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
              panel.grid.minor = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
            )

          if (!is.null(means_df) && nrow(means_df) > 0) {
            p <- p + ggplot2::geom_point(data = means_df, ggplot2::aes(x = .data[[grp_col]], y = .data[[two_state$var_y]]), color = "#DC2626", shape = 18, size = 4.5, inherit.aes = FALSE)
          }

          tryCatch({
            plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans"))
          }, error = function(e) {
            tryCatch({
              plotly::layout(plotly::plotly_build(p), font = list(family = "IBM Plex Sans"))
            }, error = function(e2) {
              plotly::plot_ly(
                data = clean_df,
                x = ramses_formula(response = NULL, terms = grp_col),
                y = ramses_formula(response = NULL, terms = two_state$var_y),
                type = "box",
                boxpoints = "all",
                jitter = 0.3
              )
            })
          })
        } else {
          w_res <- tryCatch({
            stats::wilcox.test(
              formula = ramses_formula(response = two_state$var_y, terms = grp_col),
              data = clean_df,
              alternative = two_state$alternative
            )
          }, error = function(e) NULL)

          w_title <- if (!is.null(w_res)) {
            w_stat <- round(unname(w_res$statistic), 2)
            p_val <- w_res$p.value
            p_str <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
            es_str <- if (!is.null(two_state$effect_size)) paste0(" | r = ", two_state$effect_size$formatted_value) else ""
            paste0("Test de Wilcoxon : W = ", w_stat, ", ", p_str, es_str)
          } else {
            paste0("Wilcoxon : ", two_state$var_y, " selon ", grp_col)
          }

          p <- ggplot2::ggplot(clean_df, ggplot2::aes(x = .data[[grp_col]], y = .data[[two_state$var_y]], group = .data[[grp_col]])) +
            ggplot2::geom_boxplot(fill = "#F3F4F6", color = "#1F2937", width = 0.4, outlier.shape = NA) +
            ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "#4B5563") +
            ggplot2::labs(
              title = w_title,
              x = grp_col,
              y = two_state$var_y
            ) +
            ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
            ggplot2::theme(
              text = ggplot2::element_text(family = "sans"),
              plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
              axis.title = ggplot2::element_text(size = 11, color = "#374151"),
              axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
              panel.grid.minor = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
            )

          tryCatch({
            plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans"))
          }, error = function(e) {
            tryCatch({
              plotly::layout(plotly::plotly_build(p), font = list(family = "IBM Plex Sans"))
            }, error = function(e2) {
              plotly::plot_ly(
                data = clean_df,
                x = ramses_formula(response = NULL, terms = grp_col),
                y = ramses_formula(response = NULL, terms = two_state$var_y),
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
          plotly::layout(font = list(family = "IBM Plex Sans"), 
            title = list(text = "Comparaison des s\u00e9ries appari\u00e9es", font = list(size = 13)),
            xaxis = list(title = "Variable"),
            yaxis = list(title = "Valeur")
          )
      }
    })

    output$two_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_two(two_state, data_holder$df)
    })

    # =========================================================================
    # 3. COMPARAISON DE 3+ GROUPES (ANOVA 1 FACTEUR / FACTORIELLE / ANCOVA / RM / KRUSKAL-WALLIS)
    # =========================================================================
    run_multi_analysis <- function(test_type, var_y, var_group = NULL, var_group2 = NULL, var_covar = NULL, var_subject = NULL, var_within = NULL, interaction_opt = TRUE, post_hoc_opt = TRUE, alpha_val = 0.05) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y)) {
        shiny::showNotification("Veuillez s\u00e9lectionner une variable d\u00e9pendante.", type = "warning")
        return()
      }

      if (identical(test_type, "anova_rm")) {
        if (is.null(var_subject) || !nzchar(var_subject)) {
          shiny::showNotification("Veuillez s\u00e9lectionner la variable identifiant du sujet.", type = "warning")
          return()
        }
        if (is.null(var_within) || !nzchar(var_within)) {
          shiny::showNotification("Veuillez s\u00e9lectionner le facteur intra-sujets.", type = "warning")
          return()
        }
        if (identical(var_y, var_subject) || identical(var_y, var_within) || identical(var_subject, var_within)) {
          shiny::showNotification("La variable d\u00e9pendante, l'identifiant sujet et le facteur intra-sujets doivent \u00eatre trois variables distinctes.", type = "warning")
          return()
        }
      } else {
        if (is.null(var_group) || !nzchar(var_group)) {
          shiny::showNotification("Veuillez s\u00e9lectionner au moins un facteur explicatif.", type = "warning")
          return()
        }
      }

      if (identical(test_type, "anova_twoway")) {
        if (is.null(var_group2) || !nzchar(var_group2)) {
          shiny::showNotification("Veuillez s\u00e9lectionner un second facteur pour l'ANOVA \u00e0 2 facteurs.", type = "warning")
          return()
        }
        if (identical(var_group, var_group2)) {
          shiny::showNotification("Les deux facteurs explicatifs doivent \u00eatre distincts.", type = "warning")
          return()
        }
      }

      if (identical(test_type, "ancova")) {
        if (is.null(var_covar) || !nzchar(var_covar)) {
          shiny::showNotification("Veuillez s\u00e9lectionner une covariable quantitative pour l'ANCOVA.", type = "warning")
          return()
        }
        if (identical(var_group, var_covar) || identical(var_y, var_covar)) {
          shiny::showNotification("La variable d\u00e9pendante, le facteur et la covariable doivent \u00eatre trois variables distinctes.", type = "warning")
          return()
        }
      }

      multi_state$test <- test_type
      multi_state$var_y <- var_y
      multi_state$var_group <- var_group
      multi_state$var_group2 <- var_group2
      multi_state$var_covar <- var_covar
      multi_state$var_subject <- var_subject
      multi_state$var_within <- var_within
      multi_state$interaction <- isTRUE(interaction_opt)
      multi_state$post_hoc <- isTRUE(post_hoc_opt)
      multi_state$alpha <- as.numeric(alpha_val)

      res <- NULL
      post_res <- NULL
      err <- NULL
      es <- NULL
      pwr <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        if (multi_state$test == "anova_rm") {
          # ANOVA a mesures repetees (1 facteur intra-sujets)
          rm_res <- ramses_anova_rm(
            data = df,
            response = multi_state$var_y,
            subject = multi_state$var_subject,
            within_factor = multi_state$var_within,
            alpha = multi_state$alpha
          )
          res <- rm_res
          post_res <- rm_res$post_hoc

          # R Markdown reproductible complet (Sections A a I)
          code_entry <- ramses_rmd_anova_rm(
            rm_res = rm_res,
            ds_name = ds_name,
            include_posthoc = isTRUE(multi_state$post_hoc)
          )

        } else if (multi_state$test == "ancova") {
          # ANCOVA a 1 facteur + 1 covariable quantitative
          ancova_res <- ramses_ancova(
            df = df,
            var_y = multi_state$var_y,
            var_factor = multi_state$var_group,
            var_covar = multi_state$var_covar,
            alpha = multi_state$alpha
          )
          res <- ancova_res

          # R Markdown reproductible
          fml_code_pentes <- ramses_formula_code(
            response = multi_state$var_y,
            terms = c(multi_state$var_group, multi_state$var_covar),
            op = "*"
          )
          fml_code_main <- ramses_formula_code(
            response = multi_state$var_y,
            terms = c(multi_state$var_group, multi_state$var_covar),
            op = "+"
          )

          code_entry <- paste0(
            "# Analyse de Covariance (ANCOVA : 1 facteur + 1 covariable quantitative)\n",
            "analysis_df <- ", ramses_code_symbol(ds_name), "[complete.cases(", ramses_code_symbol(ds_name), "[, c(",
            ramses_code_string(multi_state$var_y), ", ", ramses_code_string(multi_state$var_group), ", ", ramses_code_string(multi_state$var_covar), ")]), ]\n",
            "analysis_df[[", ramses_code_string(multi_state$var_group), "]] <- factor(analysis_df[[", ramses_code_string(multi_state$var_group), "]])\n",
            "\n# 1. Verification de l'homogeneite des pentes (modele complet avec interaction)\n",
            "mod_pentes <- lm(", fml_code_pentes, ", data = analysis_df)\n",
            "drop1(mod_pentes, ~ ., test = 'F')\n",
            "\n# 2. Modele ANCOVA principal (Sommes des carres de Type II)\n",
            "mod_ancova <- lm(", fml_code_main, ", data = analysis_df)\n",
            "drop1(mod_ancova, test = 'F')\n",
            "\n# 3. Moyennes ajustees (evaluees a la moyenne globale de la covariable)\n",
            "mean_cov <- mean(analysis_df[[", ramses_code_string(multi_state$var_covar), "]])\n",
            "grid_df <- data.frame(\n",
            "  ", multi_state$var_group, " = factor(levels(analysis_df[[", ramses_code_string(multi_state$var_group), "]])),\n",
            "  ", multi_state$var_covar, " = mean_cov\n",
            ")\n",
            "predict(mod_ancova, newdata = grid_df, se.fit = TRUE)\n",
            "\n# 4. Diagnostics des residus\n",
            "shapiro.test(residuals(mod_ancova))\n",
            "fligner.test(residuals(mod_ancova) ~ analysis_df[[", ramses_code_string(multi_state$var_group), "]])\n"
          )

        } else if (multi_state$test == "anova_twoway") {
          # ANOVA factorielle a 2 facteurs
          fact_res <- ramses_anova_factorial(
            df = df,
            var_y = multi_state$var_y,
            var_factor1 = multi_state$var_group,
            var_factor2 = multi_state$var_group2,
            interaction = multi_state$interaction,
            alpha = multi_state$alpha
          )
          res <- fact_res

          # R Markdown reproductible
          fml_code_fact <- ramses_formula_code(
            response = multi_state$var_y,
            terms = c(multi_state$var_group, multi_state$var_group2),
            op = if (multi_state$interaction) "*" else "+"
          )

          code_entry <- paste0(
            "# ANOVA factorielle a 2 facteurs (", if (multi_state$interaction) "Type III avec interaction" else "Type II sans interaction", ")\n",
            "analysis_df <- ", ramses_code_symbol(ds_name), "[complete.cases(", ramses_code_symbol(ds_name), "[, c(",
            ramses_code_string(multi_state$var_y), ", ", ramses_code_string(multi_state$var_group), ", ", ramses_code_string(multi_state$var_group2), ")]), ]\n",
            "analysis_df[[", ramses_code_string(multi_state$var_group), "]] <- factor(analysis_df[[", ramses_code_string(multi_state$var_group), "]])\n",
            "analysis_df[[", ramses_code_string(multi_state$var_group2), "]] <- factor(analysis_df[[", ramses_code_string(multi_state$var_group2), "]])\n",
            if (multi_state$interaction) {
              paste0(
                "contrasts(analysis_df[[", ramses_code_string(multi_state$var_group), "]]) <- contr.sum\n",
                "contrasts(analysis_df[[", ramses_code_string(multi_state$var_group2), "]]) <- contr.sum\n",
                "mod_fact <- lm(", fml_code_fact, ", data = analysis_df)\n",
                "drop1(mod_fact, ~ ., test = 'F') # Sommes des carres de Type III\n"
              )
            } else {
              paste0(
                "mod_fact <- lm(", fml_code_fact, ", data = analysis_df)\n",
                "drop1(mod_fact, test = 'F') # Sommes des carres de Type II\n"
              )
            },
            "\n# Diagnostics des hypotheses\n",
            "shapiro.test(residuals(mod_fact))\n",
            "fligner.test(", ramses_code_column("analysis_df", multi_state$var_y), " ~ interaction(",
            ramses_code_column("analysis_df", multi_state$var_group), ", ", ramses_code_column("analysis_df", multi_state$var_group2), "))\n"
          )

        } else if (multi_state$test == "anova") {
          # ANOVA 1 facteur
          fml <- ramses_formula(response = multi_state$var_y, terms = multi_state$var_group)
          fml_code <- ramses_formula_code(response = multi_state$var_y, terms = multi_state$var_group)
          aov_fit <- stats::aov(fml, data = df)
          res <- aov_fit
          es <- ramses_compute_effect_size("anova", stat_result = aov_fit)
          pwr <- ramses_compute_power("anova", stat_result = aov_fit, alpha = multi_state$alpha)
          if (multi_state$post_hoc) {
            post_res <- stats::TukeyHSD(aov_fit)
          }
          code_entry <- paste0(
            "# Analyse de variance a 1 facteur (ANOVA)\n",
            "mod_aov <- aov(", fml_code, ", data = ", ramses_code_symbol(ds_name), ")\n",
            "summary(mod_aov)\n",
            if (multi_state$post_hoc) "TukeyHSD(mod_aov)" else ""
          )
        } else {
          # Kruskal-Wallis
          fml <- ramses_formula(response = multi_state$var_y, terms = multi_state$var_group)
          fml_code <- ramses_formula_code(response = multi_state$var_y, terms = multi_state$var_group)
          res <- stats::kruskal.test(fml, data = df)
          es <- ramses_compute_effect_size("kruskal", stat_result = res, y1 = df[[multi_state$var_y]])
          pwr <- NULL
          if (multi_state$post_hoc) {
            post_res <- stats::pairwise.wilcox.test(df[[multi_state$var_y]], df[[multi_state$var_group]], p.adjust.method = "bonferroni", exact = FALSE)
          }
          code_entry <- paste0(
            "# Test non-parametrique de Kruskal-Wallis\n",
            "kruskal.test(", fml_code, ", data = ", ramses_code_symbol(ds_name), ")\n",
            if (multi_state$post_hoc) paste0("pairwise.wilcox.test(", ramses_code_column(ds_name, multi_state$var_y), ", ", ramses_code_column(ds_name, multi_state$var_group), ", p.adjust.method = 'bonferroni', exact = FALSE)") else ""
          )
        }
      }, error = function(e) {
        err <<- e$message
      })

      multi_state$result <- res
      multi_state$post_hoc_result <- post_res
      multi_state$effect_size <- es
      multi_state$power <- pwr
      multi_state$error <- err
      multi_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Comparaison 3+ Groupes : ", multi_state$test),
          code = code_entry
        )
      }
      if (is.null(err)) {
        shiny::showNotification("Analyse ex\u00e9cut\u00e9e avec succ\u00e8s !", type = "message")
      } else {
        shiny::showNotification(paste0("Erreur : ", err), type = "error")
      }
    }

    shiny::observeEvent(input$btn_run_multi, {
      post_opt <- if (identical(input$multi_test_direct, "kruskal")) {
        isTRUE(input$multi_posthoc_kruskal_direct)
      } else if (identical(input$multi_test_direct, "ancova")) {
        isTRUE(input$multi_posthoc_ancova_direct)
      } else if (identical(input$multi_test_direct, "anova_rm")) {
        isTRUE(input$multi_posthoc_rm_direct)
      } else {
        isTRUE(input$multi_posthoc_direct)
      }

      run_multi_analysis(
        test_type = input$multi_test_direct,
        var_y = input$multi_var_y_direct,
        var_group = input$multi_var_group_direct,
        var_group2 = input$multi_var_group2_direct,
        var_covar = input$multi_var_covar_direct,
        var_subject = input$multi_var_subject_direct,
        var_within = input$multi_var_within_direct,
        interaction_opt = isTRUE(input$multi_interaction_direct),
        post_hoc_opt = post_opt,
        alpha_val = input$multi_alpha_direct
      )
    })

    shiny::observeEvent(input$btn_open_multi_modal, {
      df <- data_holder$df
      num_cols <- get_num_vars()
      cat_cols <- get_cat_vars()
      all_cols <- if (!is.null(df) && is.data.frame(df)) names(df) else character(0)

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600;", "Param\u00e8tres : Comparaison de Groupes & ANOVA")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_multi"), "Ex\u00e9cuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("multi_test_choice"),
              label = "Test \u00e0 effectuer :",
              choices = c(
                "ANOVA \u00e0 1 facteur (aov) + Tukey HSD Post-hoc" = "anova",
                "ANOVA factorielle \u00e0 2 facteurs (inter-sujets)" = "anova_twoway",
                "ANCOVA (1 facteur + 1 covariable quantitative)" = "ancova",
                "ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es (1 facteur intra-sujets)" = "anova_rm",
                "Test de Kruskal-Wallis (non-param\u00e9trique)" = "kruskal"
              ),
              selected = multi_state$test
            ),
            shiny::selectInput(
              inputId = ns("multi_var_y"),
              label = "Variable quantitative d\u00e9pendante (Y) :",
              choices = num_cols,
              selected = multi_state$var_y
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice != 'anova_rm'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("multi_var_group"),
                label = "Facteur 1 (X1) / Groupe :",
                choices = cat_cols,
                selected = multi_state$var_group
              )
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice == 'anova_rm'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("multi_var_subject"),
                label = "Identifiant du sujet (Sujet) :",
                choices = all_cols,
                selected = multi_state$var_subject
              ),
              shiny::selectInput(
                inputId = ns("multi_var_within"),
                label = "Facteur intra-sujets (Temps / Condition) :",
                choices = all_cols,
                selected = multi_state$var_within
              )
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice == 'anova_twoway'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("multi_var_group2"),
                label = "Facteur 2 (X2) :",
                choices = cat_cols,
                selected = multi_state$var_group2
              ),
              shiny::checkboxInput(
                inputId = ns("multi_interaction_opt"),
                label = "Inclure l'interaction (Facteur 1 \u00d7 Facteur 2)",
                value = multi_state$interaction
              )
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice == 'ancova'",
              ns = ns,
              shiny::selectInput(
                inputId = ns("multi_var_covar"),
                label = "Covariable quantitative (C) :",
                choices = num_cols,
                selected = multi_state$var_covar
              ),
              shiny::checkboxInput(
                inputId = ns("multi_posthoc_ancova_opt"),
                label = "Calculer les comparaisons par paires (Holm)",
                value = multi_state$post_hoc
              )
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice == 'anova_rm'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("multi_posthoc_rm_opt"),
                label = "Calculer les comparaisons par paires (Holm)",
                value = multi_state$post_hoc
              )
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice == 'anova'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("multi_posthoc_opt"),
                label = "Calculer les tests Post-Hoc (Tukey HSD)",
                value = multi_state$post_hoc
              )
            ),
            shiny::conditionalPanel(
              condition = "input.multi_test_choice == 'kruskal'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("multi_posthoc_kruskal_opt"),
                label = "Calculer les comparaisons par paires (Wilcoxon)",
                value = multi_state$post_hoc
              )
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
      post_opt <- if (identical(input$multi_test_choice, "kruskal")) {
        isTRUE(input$multi_posthoc_kruskal_opt)
      } else if (identical(input$multi_test_choice, "ancova")) {
        isTRUE(input$multi_posthoc_ancova_opt)
      } else if (identical(input$multi_test_choice, "anova_rm")) {
        isTRUE(input$multi_posthoc_rm_opt)
      } else {
        isTRUE(input$multi_posthoc_opt)
      }

      run_multi_analysis(
        test_type = input$multi_test_choice,
        var_y = input$multi_var_y,
        var_group = input$multi_var_group,
        var_group2 = input$multi_var_group2,
        var_covar = input$multi_var_covar,
        var_subject = input$multi_var_subject,
        var_within = input$multi_var_within,
        interaction_opt = isTRUE(input$multi_interaction_opt),
        post_hoc_opt = post_opt,
        alpha_val = input$multi_alpha_select
      )
    })

    output$multi_status_badge <- shiny::renderUI({
      if (!multi_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ex\u00e9cuter' pour lancer l'analyse."))
      }
      if (!is.null(multi_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", multi_state$error)))
      }

      if (multi_state$test == "ancova") {
        anc_res <- multi_state$result
        a_tab <- anc_res$anova_table
        row_fact <- a_tab[a_tab$Term == multi_state$var_group, ]
        row_cov <- a_tab[a_tab$Term == multi_state$var_covar, ]
        fact_sig <- !is.null(row_fact) && nrow(row_fact) > 0 && !is.na(row_fact$p_value[1]) && row_fact$p_value[1] < multi_state$alpha
        cov_sig <- !is.null(row_cov) && nrow(row_cov) > 0 && !is.na(row_cov$p_value[1]) && row_cov$p_value[1] < multi_state$alpha

        msg <- if (fact_sig && cov_sig) {
          "Effet du facteur et effet de la covariable tous deux statistiquement significatifs"
        } else if (fact_sig) {
          "Effet du facteur significatif apr\u00e8s ajustement sur la covariable"
        } else if (cov_sig) {
          "Effet significatif de la covariable (ajustement pertinent), pas d'effet groupe d\u00e9tect\u00e9"
        } else {
          "Aucun effet significatif du facteur ni de la covariable d\u00e9tect\u00e9"
        }

        shiny::div(
          class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (fact_sig || cov_sig) "alert-success" else "alert-info"),
          shiny::tags$span(msg),
          shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", "ANCOVA Type II")
        )
      } else if (multi_state$test == "anova_twoway") {
        fact_res <- multi_state$result
        t_info <- fact_res$terms_info
        any_sig <- any(vapply(t_info[names(t_info) != "Residuals"], function(x) isTRUE(x$sig), logical(1)))
        
        shiny::div(
          class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (any_sig) "alert-success" else "alert-info"),
          shiny::tags$span(
            if (any_sig) "Au moins un effet principal ou l'interaction est statistiquement significatif" 
            else "Aucun effet principal ni interaction significatif d\u00e9tect\u00e9"
          ),
          shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", fact_res$type_ss)
        )
      } else if (multi_state$test == "anova_rm") {
        rm_res <- multi_state$result
        tab <- rm_res$anova_table
        row_w <- tab[tab$Source == rm_res$within_factor, ]
        p_val <- if (nrow(row_w) > 0) row_w$p_value[1] else NA_real_

        mauchly <- rm_res$mauchly
        use_gg <- isTRUE(mauchly$applicable) && !is.na(mauchly$p_value) && (mauchly$p_value < multi_state$alpha)
        p_eff <- if (use_gg && !is.null(rm_res$corrections)) rm_res$corrections$p_gg else p_val
        sig <- !is.na(p_eff) && (p_eff < multi_state$alpha)

        msg <- if (sig) {
          paste0("Effet du facteur intra-sujets '", rm_res$within_factor, "' statistiquement significatif (H0 rejet\u00e9e)")
        } else {
          paste0("Aucun effet significatif du facteur intra-sujets '", rm_res$within_factor, "' (H0 conserv\u00e9e)")
        }

        shiny::div(
          class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
          shiny::tags$span(msg),
          shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", if (use_gg) "ANOVA RM (p GG)" else "ANOVA RM")
        )
      } else if (multi_state$test == "anova") {
        smry <- summary(multi_state$result)
        p_val <- smry[[1]][["Pr(>F)"]][1]
        sig <- p_val < multi_state$alpha
        shiny::div(
          class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
          shiny::tags$span(if (sig) "Effet de groupe globalement significatif (au moins 2 groupes diff\u00e8rent)" else "Aucune diff\u00e9rence globale significative d\u00e9tect\u00e9e"),
          shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
        )
      } else {
        p_val <- multi_state$result$p.value
        sig <- p_val < multi_state$alpha
        shiny::div(
          class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
          shiny::tags$span(if (sig) "Diff\u00e9rence de distribution globalement significative" else "Aucune diff\u00e9rence globale significative d\u00e9tect\u00e9e"),
          shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
        )
      }
    })

    output$multi_results_ui <- shiny::renderUI({
      shiny::req(multi_state$calculated)
      if (!is.null(multi_state$error)) {
        return(shiny::p(class = "text-danger small", multi_state$error))
      }

      if (multi_state$test == "ancova") {
        anc_res <- multi_state$result
        a_tab <- anc_res$anova_table
        adj_m <- anc_res$adjusted_means
        ph <- anc_res$post_hoc
        var_f <- anc_res$var_factor
        var_c <- anc_res$var_covar
        slopes_info <- anc_res$slopes_test

        row_f <- a_tab[a_tab$Term == var_f, ]
        row_c <- a_tab[a_tab$Term == var_c, ]
        row_res <- a_tab[a_tab$Term == "R\u00e9sidus", ]

        fact_sig <- !is.null(row_f) && nrow(row_f) > 0 && !is.na(row_f$p_value[1]) && row_f$p_value[1] < multi_state$alpha
        cov_sig <- !is.null(row_c) && nrow(row_c) > 0 && !is.na(row_c$p_value[1]) && row_c$p_value[1] < multi_state$alpha

        # Lignes du tableau ANOVA Type II
        table_rows <- lapply(seq_len(nrow(a_tab)), function(i) {
          row_item <- a_tab[i, ]
          is_res <- identical(row_item$Term, "R\u00e9sidus")
          
          shiny::tags$tr(
            shiny::tags$td(class = if (is_res) "text-start text-muted" else "text-start fw-bold", row_item$Term),
            shiny::tags$td(row_item$Df),
            shiny::tags$td(round(row_item$Sum_Sq, 2)),
            shiny::tags$td(round(row_item$Mean_Sq, 2)),
            shiny::tags$td(class = if (!is_res) "fw-bold" else "", if (!is.na(row_item$F_value)) round(row_item$F_value, 3) else "\u2014"),
            shiny::tags$td(class = if (!is_res) "fw-bold text-primary" else "", if (!is.na(row_item$p_value)) format.pval(row_item$p_value, digits = 4, eps = 0.0001) else "\u2014"),
            shiny::tags$td(class = "small", if (!is.na(row_item$eta_p_sq)) paste0(round(row_item$eta_p_sq, 3), " (", row_item$eta_p_sq_mag, ")") else "\u2014")
          )
        })

        # Lignes du tableau des moyennes ajustees
        means_rows <- lapply(seq_len(nrow(adj_m)), function(i) {
          row_m <- adj_m[i, ]
          shiny::tags$tr(
            shiny::tags$td(class = "text-start fw-medium", row_m$Groupe),
            shiny::tags$td(row_m$N),
            shiny::tags$td(round(row_m$Moyenne_Brute, 3)),
            shiny::tags$td(class = "fw-bold text-dark", round(row_m$Moyenne_Ajustee, 3)),
            shiny::tags$td(round(row_m$SE, 3)),
            shiny::tags$td(paste0("[", round(row_m$CI_lower, 3), " ; ", round(row_m$CI_upper, 3), "]"))
          )
        })

        # Synthese textuelle
        synth_items <- list(
          shiny::tags$li(
            shiny::tags$strong(paste0("Effet du facteur '", var_f, "' (ajust\u00e9) : ")),
            if (fact_sig) {
              paste0("L'effet principal du facteur est statistiquement significatif apr\u00e8s contr\u00f4le de la covariable (F(", row_f$Df[1], ", ", row_res$Df[1], ") = ", round(row_f$F_value[1], 2), ", p = ", format.pval(row_f$p_value[1], digits = 4, eps = 0.0001), ", \u03b7p\u00b2 = ", round(row_f$eta_p_sq[1], 3), " - ", row_f$eta_p_sq_mag[1], ").")
            } else {
              paste0("Aucune diff\u00e9rence significative entre les groupes apr\u00e8s prise en compte de la covariable (F(", row_f$Df[1], ", ", row_res$Df[1], ") = ", round(row_f$F_value[1], 2), ", p = ", format.pval(row_f$p_value[1], digits = 4, eps = 0.0001), ").")
            }
          ),
          shiny::tags$li(
            shiny::tags$strong(paste0("Effet de la covariable '", var_c, "' : ")),
            if (cov_sig) {
              paste0("La covariable est significativement li\u00e9e \u00e0 la variable d\u00e9pendante (F(1, ", row_res$Df[1], ") = ", round(row_c$F_value[1], 2), ", p = ", format.pval(row_c$p_value[1], digits = 4, eps = 0.0001), ", \u03b7p\u00b2 = ", round(row_c$eta_p_sq[1], 3), " - ", row_c$eta_p_sq_mag[1], "). Son int\u00e9gration r\u00e9duit la variance r\u00e9siduelle et affine la comparaison des groupes.")
            } else {
              paste0("La covariable n'apporte pas d'ajustement lin\u00e9aire statistiquement significatif (F(1, ", row_res$Df[1], ") = ", round(row_c$F_value[1], 2), ", p = ", format.pval(row_c$p_value[1], digits = 4, eps = 0.0001), ").")
            }
          ),
          shiny::tags$li(
            shiny::tags$strong("Contr\u00f4le d'homog\u00e9n\u00e9it\u00e9 des pentes : "),
            if (!is.null(slopes_info) && !is.na(slopes_info$p_value)) {
              if (isTRUE(slopes_info$pentes_homogenes)) {
                paste0("L'interaction Facteur \u00d7 Covariable n'est pas significative (p = ", round(slopes_info$p_value, 4), " > ", multi_state$alpha, "). L'hypoth\u00e8se de pentes parall\u00e8les est valid\u00e9e.")
              } else {
                paste0("Attention : L'interaction Facteur \u00d7 Covariable est significative (p = ", round(slopes_info$p_value, 4), " \u2264 ", multi_state$alpha, "). Les pentes diff\u00e8rent selon les groupes ; les moyennes ajust\u00e9es du mod\u00e8le \u00e0 pente commune doivent \u00eatre interpr\u00e9t\u00e9es avec pr\u00e9caution.")
              }
            } else {
              "Test d'homog\u00e9n\u00e9it\u00e9 des pentes r\u00e9alis\u00e9."
            }
          )
        )

        slopes_alert <- if (!is.null(slopes_info) && !is.na(slopes_info$p_value) && !isTRUE(slopes_info$pentes_homogenes)) {
          shiny::div(
            class = "alert alert-warning py-2 px-3 small mt-3 mb-0",
            shiny::tags$strong("\u26a0 Alerte m\u00e9thodologique : "),
            "L'hypoth\u00e8se de parall\u00e9lisme des pentes n'\u00e9tant pas strictement respect\u00e9e, l'effet du traitement d\u00e9pend de la valeur de la covariable. Il convient d'analyser les droites de r\u00e9gression sp\u00e9cifiques \u00e0 chaque groupe dans l'onglet 'Graphiques d'\u00e9valuation'."
          )
        } else NULL

        tiles_ancova <- list(
          list(label = paste0("Facteur (", var_f, ")"), value = paste0("F = ", round(row_f$F_value[1], 2)), subtext = paste0("p = ", format.pval(row_f$p_value[1], digits = 4, eps = 0.0001)), status = if (fact_sig) "success" else "dark"),
          list(label = paste0("Covariable (", var_c, ")"), value = paste0("F = ", round(row_c$F_value[1], 2)), subtext = paste0("p = ", format.pval(row_c$p_value[1], digits = 4, eps = 0.0001)), status = if (cov_sig) "primary" else "dark"),
          list(label = "Taille d'effet (\u03b7p\u00b2)", value = as.character(round(row_f$eta_p_sq[1], 3)), subtext = row_f$eta_p_sq_mag[1]),
          list(label = "Homog\u00e9n\u00e9it\u00e9 des pentes", value = if (!is.null(slopes_info) && isTRUE(slopes_info$pentes_homogenes)) "Valid\u00e9e" else "Attention", status = if (!is.null(slopes_info) && isTRUE(slopes_info$pentes_homogenes)) "success" else "warning", subtext = if (!is.null(slopes_info)) paste0("p = ", round(slopes_info$p_value, 3)) else NULL),
          list(label = "D\u00e9cision globale", value = if (fact_sig) "Effet significatif" else "Non significatif", status = if (fact_sig) "success" else "dark", subtext = "Apr\u00e8s ajustement")
        )

        shiny::tagList(
          ramses_result_tiles(tiles_ancova, title = "Indicateurs cl\u00e9s de l'ANCOVA"),
          # Tableau ANOVA Type II
          shiny::tags$h6(class = "fw-bold text-dark mb-2", "Tableau de l'ANCOVA (Sommes des carr\u00e9s de Type II) :"),
          shiny::tags$div(
            class = "table-responsive mb-3",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-0",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Source"),
                  shiny::tags$th("ddl"),
                  shiny::tags$th("Somme des carr\u00e9s"),
                  shiny::tags$th("Carr\u00e9 moyen"),
                  shiny::tags$th("F value"),
                  shiny::tags$th("Pr(>F)"),
                  shiny::tags$th("Taille d'effet (\u03b7p\u00b2)")
                )
              ),
              shiny::tags$tbody(
                table_rows
              )
            )
          ),
          # Tableau des moyennes ajustees
          shiny::tags$h6(class = "fw-bold text-dark mb-2", paste0("Moyennes ajust\u00e9es (\u00e9valu\u00e9es \u00e0 la covariable moyenne = ", round(anc_res$mean_covar, 2), ") :")),
          shiny::tags$div(
            class = "table-responsive mb-3",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-0",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Groupe"),
                  shiny::tags$th("N"),
                  shiny::tags$th("Moyenne brute"),
                  shiny::tags$th("Moyenne ajust\u00e9e"),
                  shiny::tags$th("Erreur type (SE)"),
                  shiny::tags$th("IC 95% ajust\u00e9")
                )
              ),
              shiny::tags$tbody(
                means_rows
              )
            )
          ),
          # Synthese & Interpretation
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "d-flex justify-content-between align-items-center mb-2",
              shiny::tags$span(class = "fw-bold text-dark", "Synth\u00e8se & Interpr\u00e9tation :"),
              shiny::tags$span(class = "badge bg-white text-secondary border", "ANCOVA Type II")
            ),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              synth_items
            ),
            slopes_alert
          ),
          # Post-Hoc Holm
          if (!is.null(ph) && nrow(ph) > 0) {
            shiny::div(
              class = "p-3 rounded bg-white border text-secondary small",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Comparaisons deux \u00e0 deux des moyennes ajust\u00e9es (Correction de Holm) :"),
              shiny::tags$div(
                class = "table-responsive",
                shiny::tags$table(
                  class = "table table-sm table-striped table-hover small mb-0 text-center",
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
                      row_p <- ph[idx, ]
                      shiny::tags$tr(
                        shiny::tags$td(class = "text-start fw-medium", paste0(row_p$Groupe_1, " vs ", row_p$Groupe_2)),
                        shiny::tags$td(round(row_p$Difference, 3)),
                        shiny::tags$td(round(row_p$SE, 3)),
                        shiny::tags$td(round(row_p$t_value, 3)),
                        shiny::tags$td(format.pval(row_p$p_value_raw, digits = 3)),
                        shiny::tags$td(class = "fw-bold", format.pval(row_p$p_value_adj, digits = 3)),
                        shiny::tags$td(
                          if (isTRUE(row_p$sig)) {
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
        )

      } else if (multi_state$test == "anova_rm") {
        rm_res <- multi_state$result
        tab <- rm_res$anova_table
        mauchly <- rm_res$mauchly
        corrections <- rm_res$corrections
        es_res <- rm_res$effect_sizes
        ph <- rm_res$post_hoc
        w_factor <- rm_res$within_factor
        s_var <- rm_res$subject

        # Creation des lignes du tableau ANOVA
        anova_rows <- lapply(seq_len(nrow(tab)), function(i) {
          row_data <- tab[i, ]
          is_total <- identical(row_data$Source, "Total")
          is_within <- identical(row_data$Source, w_factor)
          is_error <- identical(row_data$Source, "R\u00e9sidus") || identical(row_data$Source, "Sujets")

          shiny::tags$tr(
            class = if (is_total) "table-light fw-bold" else if (is_within) "table-active fw-semibold" else "",
            shiny::tags$td(class = "text-start", row_data$Source),
            shiny::tags$td(row_data$Df),
            shiny::tags$td(round(row_data$Sum_Sq, 3)),
            shiny::tags$td(if (!is.na(row_data$Mean_Sq)) round(row_data$Mean_Sq, 3) else "\u2014"),
            shiny::tags$td(if (!is.na(row_data$F_value)) round(row_data$F_value, 3) else "\u2014"),
            shiny::tags$td(
              if (!is.na(row_data$p_value)) {
                if (row_data$p_value < multi_state$alpha) {
                  shiny::tags$span(class = "fw-bold text-success", format.pval(row_data$p_value, digits = 4, eps = 0.0001))
                } else {
                  format.pval(row_data$p_value, digits = 4, eps = 0.0001)
                }
              } else "\u2014"
            ),
            shiny::tags$td(
              if (is_within && !is.null(es_res)) {
                paste0(round(es_res$eta_p_sq, 4), " (", es_res$eta_p_sq_mag, ")")
              } else "\u2014"
            )
          )
        })

        # Mauchly & Corrections UI
        mauchly_ui <- if (isTRUE(mauchly$applicable)) {
          is_viol <- !is.na(mauchly$p_value) && (mauchly$p_value < multi_state$alpha)
          shiny::div(
            class = "p-3 rounded bg-light border mb-3 small",
            shiny::tags$div(
              class = "d-flex justify-content-between align-items-center mb-2",
              shiny::tags$span(class = "fw-bold text-dark", "Test de sph\u00e9ricit\u00e9 de Mauchly :"),
              if (is_viol) {
                shiny::tags$span(class = "badge bg-warning text-dark", "Non-sph\u00e9ricit\u00e9 d\u00e9tect\u00e9e (p < \u03b1)")
              } else {
                shiny::tags$span(class = "badge bg-success", "Sph\u00e9ricit\u00e9 respect\u00e9e")
              }
            ),
            shiny::tags$p(
              class = "mb-2",
              paste0("Statistique W = ", round(mauchly$w, 4), " ; ddl = ", mauchly$df, " ; p-value = ", format.pval(mauchly$p_value, digits = 4, eps = 0.0001), ".")
            ),
            if (is_viol) {
              shiny::tags$div(
                class = "alert alert-warning py-2 px-3 mb-2 small",
                shiny::tags$strong("\u26a0 Attention : "),
                "L'hypoth\u00e8se de sph\u00e9ricit\u00e9 est viol\u00e9e. Les degr\u00e9s de libert\u00e9 standards gonflent le risque d'erreur de Type I. Utilisez les p-values corrig\u00e9es ci-dessous (Greenhouse-Geisser ou Huynh-Feldt)."
              )
            } else {
              shiny::tags$div(
                class = "text-muted mb-2",
                "L'hypoth\u00e8se de sph\u00e9ricit\u00e9 est satisfaite. Les r\u00e9sultats de l'ANOVA non-corrig\u00e9e sont statistiquement valides."
              )
            },
            if (!is.null(corrections)) {
              shiny::tags$div(
                class = "table-responsive mt-2",
                shiny::tags$table(
                  class = "table table-sm table-bordered text-center align-middle mb-0 bg-white",
                  shiny::tags$thead(
                    class = "table-light",
                    shiny::tags$tr(
                      shiny::tags$th("Correction"),
                      shiny::tags$th("Epsilon (\u03b5)"),
                      shiny::tags$th("ddl corrig\u00e9s (num, den)"),
                      shiny::tags$th("p-value corrig\u00e9e"),
                      shiny::tags$th("D\u00e9cision (\u03b1 = ", multi_state$alpha, ")")
                    )
                  ),
                  shiny::tags$tbody(
                    shiny::tags$tr(
                      shiny::tags$td(class = "fw-medium text-start", "Greenhouse-Geisser (GG)"),
                      shiny::tags$td(round(corrections$eps_gg, 4)),
                      shiny::tags$td(paste0(round(corrections$df1_gg, 2), " ; ", round(corrections$df2_gg, 2))),
                      shiny::tags$td(class = "fw-bold", format.pval(corrections$p_gg, digits = 4, eps = 0.0001)),
                      shiny::tags$td(
                        if (corrections$p_gg < multi_state$alpha) {
                          shiny::tags$span(class = "badge bg-success", "Significatif")
                        } else {
                          shiny::tags$span(class = "badge bg-secondary", "Non sign.")
                        }
                      )
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(class = "fw-medium text-start", "Huynh-Feldt (HF)"),
                      shiny::tags$td(round(corrections$eps_hf, 4)),
                      shiny::tags$td(paste0(round(corrections$df1_hf, 2), " ; ", round(corrections$df2_hf, 2))),
                      shiny::tags$td(class = "fw-bold", format.pval(corrections$p_hf, digits = 4, eps = 0.0001)),
                      shiny::tags$td(
                        if (corrections$p_hf < multi_state$alpha) {
                          shiny::tags$span(class = "badge bg-success", "Significatif")
                        } else {
                          shiny::tags$span(class = "badge bg-secondary", "Non sign.")
                        }
                      )
                    )
                  )
                )
              )
            } else NULL
          )
        } else {
          shiny::div(
            class = "p-3 rounded bg-light border mb-3 small text-muted",
            shiny::tags$strong("Sph\u00e9ricit\u00e9 : "),
            "Le facteur intra-sujets comporte k = 2 niveaux. La condition de sph\u00e9ricit\u00e9 est automatiquement et exactement satisfaite (1 seule diff\u00e9rence de paires)."
          )
        }

        # Synthese
        row_w <- tab[tab$Source == w_factor, ]
        p_raw <- if (nrow(row_w) > 0) row_w$p_value[1] else NA_real_
        f_val_w <- if (nrow(row_w) > 0) row_w$F_value[1] else NA_real_
        df_num_w <- if (nrow(row_w) > 0) row_w$Df[1] else NA_integer_
        row_err <- tab[tab$Source == "R\u00e9sidus", ]
        df_den_w <- if (nrow(row_err) > 0) row_err$Df[1] else NA_integer_

        use_corr <- isTRUE(mauchly$applicable) && !is.na(mauchly$p_value) && (mauchly$p_value < multi_state$alpha)
        p_eval <- if (use_corr && !is.null(corrections)) corrections$p_gg else p_raw
        sig_eval <- !is.na(p_eval) && (p_eval < multi_state$alpha)

        synth_ui <- shiny::div(
          class = "p-3 rounded bg-light border text-secondary small mb-3",
          shiny::tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            shiny::tags$span(class = "fw-bold text-dark", "Synth\u00e8se & Interpr\u00e9tation :"),
            shiny::tags$span(class = "badge bg-white text-secondary border", paste0("N = ", rm_res$data_info$final_subjects, " sujets | k = ", rm_res$data_info$k_levels, " mesures"))
          ),
          shiny::tags$p(
            class = "mb-1",
            if (sig_eval) {
              paste0(
                "Au seuil alpha = ", multi_state$alpha, ", l'effet du facteur intra-sujets '", w_factor, "' est statistiquement significatif ",
                if (use_corr) {
                  paste0("(F(", round(corrections$df1_gg, 2), ", ", round(corrections$df2_gg, 2), ") = ", round(f_val_w, 2), ", p [GG] = ", format.pval(p_eval, digits = 4, eps = 0.0001), "). ")
                } else {
                  paste0("(F(", df_num_w, ", ", df_den_w, ") = ", round(f_val_w, 2), ", p = ", format.pval(p_eval, digits = 4, eps = 0.0001), "). ")
                },
                "L'hypoth\u00e8se nulle d'\u00e9galit\u00e9 des r\u00e9ponses entre les modalit\u00e9s temporelles / conditions est rejet\u00e9e."
              )
            } else {
              paste0(
                "Au seuil alpha = ", multi_state$alpha, ", aucun effet statistiquement significatif du facteur intra-sujets '", w_factor, "' n'est d\u00e9tect\u00e9 ",
                if (use_corr) {
                  paste0("(F(", round(corrections$df1_gg, 2), ", ", round(corrections$df2_gg, 2), ") = ", round(f_val_w, 2), ", p [GG] = ", format.pval(p_eval, digits = 4, eps = 0.0001), "). ")
                } else {
                  paste0("(F(", df_num_w, ", ", df_den_w, ") = ", round(f_val_w, 2), ", p = ", format.pval(p_eval, digits = 4, eps = 0.0001), "). ")
                },
                "L'hypoth\u00e8se nulle d'\u00e9galit\u00e9 des r\u00e9ponses est conserv\u00e9e."
              )
            }
          ),
          shiny::tags$p(
            class = "mb-0 text-muted",
            "Cette analyse mod\u00e9lise la corr\u00e9lation intra-sujet en isolant la variabilit\u00e9 inter-individuelle du terme d'erreur r\u00e9siduel."
          )
        )

        # Post-Hoc UI
        post_hoc_ui <- if (!is.null(ph) && nrow(ph) > 0) {
          shiny::div(
            class = "p-3 rounded bg-white border text-secondary small",
            shiny::tags$div(
              class = "d-flex justify-content-between align-items-center mb-2",
              shiny::tags$h6(class = "fw-bold text-dark mb-0", "Comparaisons par paires intra-sujets (Tests t appari\u00e9s avec correction de Holm) :"),
              shiny::tags$span(class = "badge bg-light text-dark border", paste0(nrow(ph), " comparaisons"))
            ),
            shiny::tags$div(
              class = "table-responsive",
              shiny::tags$table(
                class = "table table-sm table-bordered text-center align-middle mb-0",
                shiny::tags$thead(
                  class = "table-light",
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
                    row_p <- ph[idx, ]
                    shiny::tags$tr(
                      shiny::tags$td(class = "text-start fw-medium", paste0(row_p$Niveau_1, " vs ", row_p$Niveau_2)),
                      shiny::tags$td(round(row_p$Difference, 3)),
                      shiny::tags$td(round(row_p$SE, 3)),
                      shiny::tags$td(round(row_p$t_value, 3)),
                      shiny::tags$td(row_p$Df),
                      shiny::tags$td(format.pval(row_p$p_value_raw, digits = 4)),
                      shiny::tags$td(class = "fw-bold", format.pval(row_p$p_value_adj, digits = 4)),
                      shiny::tags$td(
                        if (isTRUE(row_p$sig)) {
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

        tiles_rm <- list(
          list(label = paste0("Facteur intra (", w_factor, ")"), value = if (!is.na(f_val_w)) paste0("F = ", round(f_val_w, 2)) else "\u2014", subtext = paste0("k = ", rm_res$data_info$k_levels, " modalit\u00e9s")),
          list(label = "p-value", value = if (!is.na(p_eval)) format.pval(p_eval, digits = 4, eps = 0.0001) else "\u2014", status = if (sig_eval) "success" else "dark", subtext = if (use_corr) "Corrig\u00e9e (GG)" else "Standard"),
          list(label = "Taille d'effet (\u03b7p\u00b2)", value = if (!is.null(es_res)) as.character(round(es_res$eta_p_sq, 3)) else "\u2014", subtext = if (!is.null(es_res)) es_res$eta_p_sq_mag else NULL),
          list(label = "Sph\u00e9ricit\u00e9 (Mauchly)", value = if (isTRUE(mauchly$applicable)) { if (!is.na(mauchly$p_value) && mauchly$p_value < multi_state$alpha) "Viol\u00e9e" else "Respect\u00e9e" } else "k = 2", status = if (isTRUE(mauchly$applicable) && !is.na(mauchly$p_value) && mauchly$p_value < multi_state$alpha) "warning" else "success", subtext = if (isTRUE(mauchly$applicable)) paste0("W = ", round(mauchly$w, 3)) else "Automatique"),
          list(label = "D\u00e9cision globale", value = if (sig_eval) "Effet significatif" else "Non significatif", status = if (sig_eval) "success" else "dark", subtext = paste0("N = ", rm_res$data_info$final_subjects, " sujets"))
        )

        shiny::tagList(
          ramses_result_tiles(tiles_rm, title = "Indicateurs cl\u00e9s de l'ANOVA \u00e0 mesures r\u00e9p\u00e9t\u00e9es"),
          # En-tete d'information sur le plan experimental
          shiny::div(
            class = "d-flex flex-wrap gap-2 mb-3",
            shiny::tags$span(class = "badge bg-light text-dark border p-2", paste0("Variable d\u00e9pendante : ", rm_res$response)),
            shiny::tags$span(class = "badge bg-light text-dark border p-2", paste0("Identifiant sujet : ", s_var, " (N = ", rm_res$data_info$final_subjects, ")")),
            shiny::tags$span(class = "badge bg-light text-dark border p-2", paste0("Facteur intra-sujets : ", w_factor, " (k = ", rm_res$data_info$k_levels, ")")),
            shiny::tags$span(class = "badge bg-light text-dark border p-2", paste0("Total observations : ", rm_res$data_info$n_obs))
          ),
          # Tableau ANOVA
          shiny::tags$div(
            class = "table-responsive",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-3",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Source de variation"),
                  shiny::tags$th("ddl"),
                  shiny::tags$th("Somme des carr\u00e9s"),
                  shiny::tags$th("Carr\u00e9 moyen"),
                  shiny::tags$th("F value"),
                  shiny::tags$th("Pr(>F)"),
                  shiny::tags$th("Taille d'effet (\u03b7p\u00b2)")
                )
              ),
              shiny::tags$tbody(
                anova_rows
              )
            )
          ),
          mauchly_ui,
          synth_ui,
          post_hoc_ui
        )

      } else if (multi_state$test == "anova_twoway") {
        fact_res <- multi_state$result
        t_info <- fact_res$terms_info
        var_a <- fact_res$var_factor1
        var_b <- fact_res$var_factor2
        term_ab <- paste0(var_a, ":", var_b)
        has_int <- fact_res$has_interaction

        # Creation dynamique des lignes du tableau ANOVA factoriel
        table_rows <- lapply(names(t_info), function(term_name) {
          info <- t_info[[term_name]]
          is_res <- identical(term_name, "Residuals")
          
          shiny::tags$tr(
            shiny::tags$td(class = if (is_res) "text-start text-muted" else "text-start fw-bold", info$term),
            shiny::tags$td(info$df),
            shiny::tags$td(round(info$sum_sq, 2)),
            shiny::tags$td(round(info$mean_sq, 2)),
            shiny::tags$td(class = if (!is_res) "fw-bold" else "", if (!is.na(info$f_value)) round(info$f_value, 3) else "\u2014"),
            shiny::tags$td(class = if (!is_res) "fw-bold text-primary" else "", if (!is.na(info$p_value)) format.pval(info$p_value, digits = 4, eps = 0.0001) else "\u2014"),
            shiny::tags$td(class = "small", if (!is.na(info$eta_p_sq)) paste0(round(info$eta_p_sq, 3), " (", info$eta_p_sq_mag, ")") else "\u2014")
          )
        })

        # Synthese et interpretation
        synth_items <- list()

        # Section Effet Facteur A
        if (!is.null(t_info[[var_a]])) {
          info_a <- t_info[[var_a]]
          dec_a <- if (info_a$sig) {
            paste0("L'effet principal du facteur '", var_a, "' est statistiquement significatif (F(", info_a$df, ", ", t_info$Residuals$df, ") = ", round(info_a$f_value, 2), ", p = ", format.pval(info_a$p_value, digits = 4, eps = 0.0001), ", \u03b7p\u00b2 = ", round(info_a$eta_p_sq, 3), " - ", info_a$eta_p_sq_mag, ").")
          } else {
            paste0("Aucun effet principal significatif du facteur '", var_a, "' (F(", info_a$df, ", ", t_info$Residuals$df, ") = ", round(info_a$f_value, 2), ", p = ", format.pval(info_a$p_value, digits = 4, eps = 0.0001), ").")
          }
          synth_items[[length(synth_items) + 1]] <- shiny::tags$li(
            shiny::tags$strong(paste0("Effet du facteur ", var_a, " : ")),
            dec_a
          )
        }

        # Section Effet Facteur B
        if (!is.null(t_info[[var_b]])) {
          info_b <- t_info[[var_b]]
          dec_b <- if (info_b$sig) {
            paste0("L'effet principal du facteur '", var_b, "' est statistiquement significatif (F(", info_b$df, ", ", t_info$Residuals$df, ") = ", round(info_b$f_value, 2), ", p = ", format.pval(info_b$p_value, digits = 4, eps = 0.0001), ", \u03b7p\u00b2 = ", round(info_b$eta_p_sq, 3), " - ", info_b$eta_p_sq_mag, ").")
          } else {
            paste0("Aucun effet principal significatif du facteur '", var_b, "' (F(", info_b$df, ", ", t_info$Residuals$df, ") = ", round(info_b$f_value, 2), ", p = ", format.pval(info_b$p_value, digits = 4, eps = 0.0001), ").")
          }
          synth_items[[length(synth_items) + 1]] <- shiny::tags$li(
            shiny::tags$strong(paste0("Effet du facteur ", var_b, " : ")),
            dec_b
          )
        }

        # Section Interaction A x B
        int_alert <- NULL
        if (has_int && !is.null(t_info[[term_ab]])) {
          info_ab <- t_info[[term_ab]]
          dec_ab <- if (info_ab$sig) {
            paste0("L'interaction entre '", var_a, "' et '", var_b, "' est statistiquement significative (F(", info_ab$df, ", ", t_info$Residuals$df, ") = ", round(info_ab$f_value, 2), ", p = ", format.pval(info_ab$p_value, digits = 4, eps = 0.0001), ", \u03b7p\u00b2 = ", round(info_ab$eta_p_sq, 3), " - ", info_ab$eta_p_sq_mag, ").")
          } else {
            paste0("L'interaction entre '", var_a, "' et '", var_b, "' n'est pas statistiquement significative (p = ", format.pval(info_ab$p_value, digits = 4, eps = 0.0001), "). Les effets principaux s'interpr\u00e8tent de mani\u00e8re ind\u00e9pendante.")
          }
          synth_items[[length(synth_items) + 1]] <- shiny::tags$li(
            shiny::tags$strong(paste0("Interaction ", var_a, " \u00d7 ", var_b, " : ")),
            dec_ab
          )

          if (info_ab$sig) {
            int_alert <- shiny::div(
              class = "alert alert-warning py-2 px-3 small mt-3 mb-0",
              shiny::tags$strong("\u26a0 Interaction significative d\u00e9tect\u00e9e : "),
              "Une interaction signifie que l'effet d'un facteur d\u00e9pend du niveau de l'autre facteur. Les effets principaux ne doivent pas \u00eatre g\u00e9n\u00e9ralis\u00e9s globalement sans examiner les profils par cellule (effets simples)."
            )
          }
        }

        sig_a <- !is.null(t_info[[var_a]]) && isTRUE(t_info[[var_a]]$sig)
        sig_b <- !is.null(t_info[[var_b]]) && isTRUE(t_info[[var_b]]$sig)
        sig_ab <- has_int && !is.null(t_info[[term_ab]]) && isTRUE(t_info[[term_ab]]$sig)

        tiles_twoway <- list(
          list(label = paste0("Effet ", var_a), value = if (!is.null(t_info[[var_a]])) paste0("F = ", round(t_info[[var_a]]$f_value, 2)) else "\u2014", subtext = if (!is.null(t_info[[var_a]])) paste0("p = ", format.pval(t_info[[var_a]]$p_value, digits = 3)) else NULL, status = if (sig_a) "success" else "dark"),
          list(label = paste0("Effet ", var_b), value = if (!is.null(t_info[[var_b]])) paste0("F = ", round(t_info[[var_b]]$f_value, 2)) else "\u2014", subtext = if (!is.null(t_info[[var_b]])) paste0("p = ", format.pval(t_info[[var_b]]$p_value, digits = 3)) else NULL, status = if (sig_b) "success" else "dark"),
          list(label = "Interaction", value = if (has_int && !is.null(t_info[[term_ab]])) paste0("F = ", round(t_info[[term_ab]]$f_value, 2)) else "Non test\u00e9e", subtext = if (has_int && !is.null(t_info[[term_ab]])) paste0("p = ", format.pval(t_info[[term_ab]]$p_value, digits = 3)) else NULL, status = if (sig_ab) "warning" else "dark"),
          list(label = "Mod\u00e8le ANOVA", value = fact_res$type_ss, subtext = "Type de somme des carr\u00e9s")
        )

        shiny::tagList(
          ramses_result_tiles(tiles_twoway, title = "Indicateurs cl\u00e9s de l'ANOVA factorielle"),
          shiny::tags$div(
            class = "table-responsive",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-3",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Source"),
                  shiny::tags$th("ddl"),
                  shiny::tags$th("Somme des carr\u00e9s"),
                  shiny::tags$th("Carr\u00e9 moyen"),
                  shiny::tags$th("F value"),
                  shiny::tags$th("Pr(>F)"),
                  shiny::tags$th("Taille d'effet (\u03b7p\u00b2)")
                )
              ),
              shiny::tags$tbody(
                table_rows
              )
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "d-flex justify-content-between align-items-center mb-2",
              shiny::tags$span(class = "fw-bold text-dark", "Synth\u00e8se & Interpr\u00e9tation :"),
              shiny::tags$span(class = "badge bg-white text-secondary border", fact_res$type_ss)
            ),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              synth_items
            ),
            int_alert
          ),
          shiny::div(
            class = "p-3 rounded bg-white border text-secondary small",
            shiny::tags$h6(class = "fw-bold text-dark mb-1", "Comparaisons multiples (Post-Hoc) :"),
            shiny::tags$p(class = "mb-0", "Comparaisons multiples pour les effets factoriels : fonctionnalit\u00e9 pr\u00e9vue dans une phase ult\u00e9rieure.")
          )
        )

      } else if (multi_state$test == "anova") {
        es <- multi_state$effect_size
        pwr <- multi_state$power
        smry <- summary(multi_state$result)[[1]]
        f_val <- smry[["F value"]][1]
        p_val <- smry[["Pr(>F)"]][1]
        df_group <- smry[["Df"]][1]
        df_res <- smry[["Df"]][2]
        sig <- p_val < multi_state$alpha

        decision_text <- if (sig) {
          paste0("Au risque alpha = ", multi_state$alpha, ", l'effet global du facteur '", multi_state$var_group, "' est statistiquement significatif (F(", df_group, ", ", df_res, ") = ", round(f_val, 2), ", p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'\u00e9galit\u00e9 de toutes les moyennes de groupes est rejet\u00e9e.")
        } else {
          paste0("Au risque alpha = ", multi_state$alpha, ", aucune diff\u00e9rence globale significative n'est mise en \u00e9vidence entre les groupes de '", multi_state$var_group, "' (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'\u00e9galit\u00e9 des moyennes est conserv\u00e9e.")
        }

        tiles_anova <- list(
          list(label = "Facteur", value = multi_state$var_group, subtext = "ANOVA \u00e0 1 facteur"),
          list(label = "Statistique F", value = as.character(round(f_val, 3)), subtext = paste0("ddl = ", df_group, " ; ", df_res)),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "Taille d'effet (\u03b7\u00b2)", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) es$magnitude else NULL),
          list(label = "Puissance", value = if (!is.null(pwr)) pwr$percentage else "\u2014", subtext = if (!is.null(pwr)) pwr$magnitude else NULL),
          list(label = "D\u00e9cision", value = if (sig) "Diff\u00e9rence significative" else "Non significatif", status = if (sig) "success" else "dark")
        )

        shiny::tagList(
          ramses_result_tiles(tiles_anova, title = "Indicateurs cl\u00e9s de l'ANOVA"),
          shiny::tags$div(
            class = "table-responsive",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-3",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Source"),
                  shiny::tags$th("ddl"),
                  shiny::tags$th("Somme des carr\u00e9s"),
                  shiny::tags$th("Carr\u00e9 moyen"),
                  shiny::tags$th("F value"),
                  shiny::tags$th("Pr(>F)"),
                  shiny::tags$th("Taille d'effet (\u03b7\u00b2)"),
                  shiny::tags$th("Puissance")
                )
              ),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td(class = "text-start fw-bold", multi_state$var_group),
                  shiny::tags$td(df_group),
                  shiny::tags$td(round(smry[["Sum Sq"]][1], 2)),
                  shiny::tags$td(round(smry[["Mean Sq"]][1], 2)),
                  shiny::tags$td(class = "fw-bold", round(f_val, 3)),
                  shiny::tags$td(class = "fw-bold text-primary", format.pval(p_val, digits = 4, eps = 0.0001)),
                  shiny::tags$td(class = "small", if (!is.null(es)) paste0(es$formatted_value, " (", es$magnitude, ")") else "\u2014"),
                  shiny::tags$td(class = "small", if (!is.null(pwr)) paste0(pwr$percentage, " (", pwr$magnitude, ")") else "\u2014")
                ),
                shiny::tags$tr(
                  shiny::tags$td(class = "text-start text-muted", "R\u00e9sidus"),
                  shiny::tags$td(df_res),
                  shiny::tags$td(round(smry[["Sum Sq"]][2], 2)),
                  shiny::tags$td(round(smry[["Mean Sq"]][2], 2)),
                  shiny::tags$td("\u2014"),
                  shiny::tags$td("\u2014"),
                  shiny::tags$td("\u2014"),
                  shiny::tags$td("\u2014")
                )
              )
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), decision_text),
              if (!is.null(es)) shiny::tags$li(shiny::tags$strong(paste0("Taille d'effet (", es$name, ") : ")), es$description),
              if (!is.null(pwr)) shiny::tags$li(shiny::tags$strong("Puissance statistique : "), paste0("La puissance a posteriori calcul\u00e9e est de ", pwr$percentage, " (", pwr$magnitude, ")."))
            )
          ),
          if (!is.null(multi_state$post_hoc_result)) {
            shiny::div(
              class = "mt-3",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Comparaisons par paires (Tukey HSD) :"),
              shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(utils::capture.output(print(multi_state$post_hoc_result)), collapse = "\n"))
            )
          }
        )
      } else {
        es <- multi_state$effect_size
        res <- multi_state$result
        p_val <- res$p.value
        stat_val <- unname(res$statistic)
        sig <- p_val < multi_state$alpha

        decision_text <- if (sig) {
          paste0("Au risque alpha = ", multi_state$alpha, ", les distributions de '", multi_state$var_y, "' diff\u00e8rent significativement entre les groupes de '", multi_state$var_group, "' (H = ", round(stat_val, 2), ", p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'homog\u00e9n\u00e9it\u00e9 est rejet\u00e9e.")
        } else {
          paste0("Au risque alpha = ", multi_state$alpha, ", aucune diff\u00e9rence significative de distribution n'est constat\u00e9e entre les groupes (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle est conserv\u00e9e.")
        }

        tiles_kruskal <- list(
          list(label = "Facteur", value = multi_state$var_group, subtext = "Kruskal-Wallis"),
          list(label = "Chi-deux (H)", value = as.character(round(stat_val, 3)), subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else NULL),
          list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
          list(label = "Taille d'effet (\u03b7\u00b2_H)", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) es$magnitude else NULL),
          list(label = "D\u00e9cision", value = if (sig) "Diff\u00e9rence significative" else "Non significatif", status = if (sig) "success" else "dark")
        )

        shiny::tagList(
          ramses_result_tiles(tiles_kruskal, title = "Indicateurs cl\u00e9s du test de Kruskal-Wallis"),
          shiny::tags$div(
            class = "table-responsive",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle mb-3",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("M\u00e9thode"),
                  shiny::tags$th("Chi-deux (H)"),
                  shiny::tags$th("ddl"),
                  shiny::tags$th("p-value"),
                  shiny::tags$th("Taille d'effet (\u03b7\u00b2_H)"),
                  shiny::tags$th("Conclusion")
                )
              ),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td(class = "text-start fw-medium", res$method),
                  shiny::tags$td(round(stat_val, 3)),
                  shiny::tags$td(round(res$parameter, 1)),
                  shiny::tags$td(class = "fw-bold text-primary", format.pval(p_val, digits = 4, eps = 0.0001)),
                  shiny::tags$td(class = "small", if (!is.null(es)) paste0(es$formatted_value, " (", es$magnitude, ")") else "\u2014"),
                  shiny::tags$td(
                    shiny::tags$span(
                      class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                      if (sig) "Diff\u00e9rence significative" else "Non significatif"
                    )
                  )
                )
              )
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), decision_text),
              if (!is.null(es)) shiny::tags$li(shiny::tags$strong(paste0("Taille d'effet (", es$name, ") : ")), es$description)
            )
          ),
          if (!is.null(multi_state$post_hoc_result)) {
            shiny::div(
              class = "mt-3",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Tests de Wilcoxon par paires (Bonferroni) :"),
              shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(utils::capture.output(print(multi_state$post_hoc_result)), collapse = "\n"))
            )
          }
        )
      }
    })

    output$multi_plot <- plotly::renderPlotly({
      df <- data_holder$df
      shiny::req(df, multi_state$var_y)
      y_col <- multi_state$var_y

      if (identical(multi_state$test, "anova_rm")) {
        shiny::req(multi_state$var_within, multi_state$var_subject)
        within_col <- multi_state$var_within
        subj_col <- multi_state$var_subject

        rm_res <- multi_state$result
        if (is.null(rm_res) || !is.list(rm_res) || is.null(rm_res$anova_table)) {
          rm_res <- tryCatch({
            ramses_anova_rm(
              data = df,
              response = y_col,
              subject = subj_col,
              within_factor = within_col,
              alpha = if (!is.null(multi_state$alpha)) multi_state$alpha else 0.05
            )
          }, error = function(e) NULL)
        }
        shiny::req(rm_res)
        plot_type <- if (!is.null(input$rm_plot_type_direct)) input$rm_plot_type_direct else "spaghetti"
        return(ramses_plot_anova_rm(rm_res, type = plot_type, interactive = TRUE))
      }

      shiny::req(multi_state$var_group)
      grp_col <- multi_state$var_group

      if (identical(multi_state$test, "ancova")) {
        shiny::req(multi_state$var_covar)
        cov_col <- multi_state$var_covar

        clean_df <- df[!is.na(df[[y_col]]) & !is.na(df[[grp_col]]) & !is.na(df[[cov_col]]), ]
        shiny::req(nrow(clean_df) > 0)
        clean_df[[grp_col]] <- droplevels(as.factor(clean_df[[grp_col]]))
        clean_df[[cov_col]] <- as.numeric(clean_df[[cov_col]])
        mean_c <- mean(clean_df[[cov_col]], na.rm = TRUE)

        title_text <- if (multi_state$calculated && !is.null(multi_state$result)) {
          paste0("ANCOVA : ", y_col, " en fonction de ", cov_col, " par ", grp_col)
        } else {
          paste0("R\u00e9gression et ajustement ANCOVA : ", y_col, " ~ ", grp_col, " + ", cov_col)
        }

        p <- ggplot2::ggplot(clean_df, ggplot2::aes(x = .data[[cov_col]], y = .data[[y_col]], color = .data[[grp_col]], group = .data[[grp_col]])) +
          ggplot2::geom_point(size = 2.8, alpha = 0.75) +
          ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
          ggplot2::geom_vline(xintercept = mean_c, linetype = "dashed", color = "#6B7280", linewidth = 0.6) +
          ggplot2::labs(
            title = title_text,
            subtitle = paste0("Droites par groupe (ajustement \u00e0 la covariable moyenne = ", round(mean_c, 2), ")"),
            x = cov_col,
            y = y_col,
            color = grp_col
          ) +
          ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
          ggplot2::theme(
            text = ggplot2::element_text(family = "sans"),
            plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
            axis.title = ggplot2::element_text(size = 11, color = "#374151"),
            axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5),
            legend.position = "right"
          )

        tryCatch({
          plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans"))
        }, error = function(e) {
          plotly::plot_ly(
            data = clean_df,
            x = clean_df[[cov_col]],
            y = clean_df[[y_col]],
            color = clean_df[[grp_col]],
            type = "scatter",
            mode = "markers"
          ) %>%
            plotly::layout(
              font = list(family = "IBM Plex Sans"),
              title = list(text = title_text, font = list(size = 13)),
              xaxis = list(title = cov_col),
              yaxis = list(title = y_col)
            )
        })

      } else if (identical(multi_state$test, "anova_twoway")) {
        shiny::req(multi_state$var_group2)
        grp2_col <- multi_state$var_group2
        
        clean_df <- df[!is.na(df[[y_col]]) & !is.na(df[[grp_col]]) & !is.na(df[[grp2_col]]), ]
        shiny::req(nrow(clean_df) > 0)
        clean_df[[grp_col]] <- droplevels(as.factor(clean_df[[grp_col]]))
        clean_df[[grp2_col]] <- droplevels(as.factor(clean_df[[grp2_col]]))

        # Calcul des moyennes et IC 95% par cellule
        stats_df <- clean_df %>%
          dplyr::group_by(.data[[grp_col]], .data[[grp2_col]]) %>%
          dplyr::summarise(
            n = dplyr::n(),
            mean_y = mean(.data[[y_col]], na.rm = TRUE),
            sd_y = stats::sd(.data[[y_col]], na.rm = TRUE),
            se_y = ifelse(dplyr::n() > 1, stats::sd(.data[[y_col]], na.rm = TRUE) / sqrt(dplyr::n()), 0),
            ci_low = ifelse(dplyr::n() > 1, mean(.data[[y_col]], na.rm = TRUE) - stats::qt(0.975, df = pmax(1, dplyr::n() - 1)) * (stats::sd(.data[[y_col]], na.rm = TRUE) / sqrt(dplyr::n())), mean(.data[[y_col]], na.rm = TRUE)),
            ci_high = ifelse(dplyr::n() > 1, mean(.data[[y_col]], na.rm = TRUE) + stats::qt(0.975, df = pmax(1, dplyr::n() - 1)) * (stats::sd(.data[[y_col]], na.rm = TRUE) / sqrt(dplyr::n())), mean(.data[[y_col]], na.rm = TRUE)),
            .groups = "drop"
          )

        title_text <- if (multi_state$calculated && !is.null(multi_state$result)) {
          paste0("Graphique d'interaction : ", y_col, " ~ ", grp_col, " \u00d7 ", grp2_col)
        } else {
          paste0("Profils d'interaction : ", grp_col, " et ", grp2_col)
        }

        p <- ggplot2::ggplot(stats_df, ggplot2::aes(x = .data[[grp_col]], y = mean_y, color = .data[[grp2_col]], group = .data[[grp2_col]])) +
          ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
          ggplot2::geom_point(size = 3.5) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low, ymax = ci_high), width = 0.12, linewidth = 0.7, alpha = 0.8) +
          ggplot2::labs(
            title = title_text,
            subtitle = "Moyennes et intervalles de confiance \u00e0 95%",
            x = grp_col,
            y = paste0("Moyenne de ", y_col),
            color = grp2_col
          ) +
          ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
          ggplot2::theme(
            text = ggplot2::element_text(family = "sans"),
            plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
            axis.title = ggplot2::element_text(size = 11, color = "#374151"),
            axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5),
            legend.position = "right"
          )

        tryCatch({
          plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans"))
        }, error = function(e) {
          plotly::plot_ly(
            data = stats_df,
            x = ramses_formula(response = NULL, terms = grp_col),
            y = stats_df$mean_y,
            color = stats_df[[grp2_col]],
            type = "scatter",
            mode = "lines+markers",
            error_y = list(
              type = "data",
              symmetric = FALSE,
              array = stats_df$ci_high - stats_df$mean_y,
              arrayminus = stats_df$mean_y - stats_df$ci_low
            )
          ) %>%
            plotly::layout(
              font = list(family = "IBM Plex Sans"),
              title = list(text = title_text, font = list(size = 13)),
              xaxis = list(title = grp_col),
              yaxis = list(title = paste0("Moyenne de ", y_col))
            )
        })

      } else {
        # ANOVA 1 facteur ou Kruskal-Wallis (Boxplot existant)
        clean_df <- df[!is.na(df[[y_col]]) & !is.na(df[[grp_col]]), ]
        shiny::req(nrow(clean_df) > 0)
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
          ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
          ggplot2::theme(
            text = ggplot2::element_text(family = "sans"),
            plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
            axis.title = ggplot2::element_text(size = 11, color = "#374151"),
            axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5)
          )

        tryCatch({
          plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans"))
        }, error = function(e) {
          tryCatch({
            plotly::layout(plotly::plotly_build(p), font = list(family = "IBM Plex Sans"))
          }, error = function(e2) {
            plotly::plot_ly(
              data = clean_df,
              x = ramses_formula(response = NULL, terms = grp_col),
              y = ramses_formula(response = NULL, terms = y_col),
              type = "box",
              boxpoints = "all",
              jitter = 0.3
            ) %>%
              plotly::layout(font = list(family = "IBM Plex Sans"), 
                title = list(text = title_text, font = list(size = 13)),
                xaxis = list(title = grp_col),
                yaxis = list(title = y_col)
              )
          })
        })
      }
    })

    output$multi_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_multi(multi_state, data_holder$df)
    })

    # =========================================================================
    # 4. ANALYSE DE CONTINGENCE & QUALITATIF
    # =========================================================================
    run_cont_analysis <- function(test_type, var_row, var_col, correct_opt, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_row) || is.null(var_col)) {
        shiny::showNotification("Veuillez s\u00e9lectionner deux variables qualitatives.", type = "warning")
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
      es <- NULL
      pwr <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        tab <- table(df[[cont_state$var_row]], df[[cont_state$var_col]])
        row_col_code <- ramses_code_column(ds_name, cont_state$var_row)
        col_col_code <- ramses_code_column(ds_name, cont_state$var_col)
        if (cont_state$test == "chisq") {
          res <- suppressWarnings(stats::chisq.test(tab, correct = cont_state$correct))
          es <- ramses_compute_effect_size("chisq", stat_result = res, tab = tab)
          pwr <- ramses_compute_power("chisq", stat_result = res, tab = tab, alpha = cont_state$alpha)

          exp_counts <- res$expected
          low_exp <- exp_counts < 5
          n_low <- sum(low_exp)
          tot_cells <- length(exp_counts)
          pct_low <- round((n_low / tot_cells) * 100, 1)
          min_exp <- min(exp_counts)

          exp_warn_msg <- NULL
          if (n_low > 0) {
            is_2x2 <- all(dim(tab) == c(2, 2))
            if (is_2x2) {
              exp_warn_msg <- paste0(
                "Avertissement statistique : Les conditions d'application du test du Chi-deux ne sont pas id\u00e9alement satisfaites. ",
                n_low, " cellule(s) sur ", tot_cells, " (", pct_low, "%) ont un effectif th\u00e9orique attendu inf\u00e9rieur \u00e0 5 (effectif minimal attendu : ", round(min_exp, 2), "). ",
                "S'agissant d'un tableau 2x2, l'utilisation du test exact de Fisher est fortement recommand\u00e9e."
              )
            } else {
              exp_warn_msg <- paste0(
                "Avertissement statistique : Les conditions d'application du test du Chi-deux ne sont pas id\u00e9alement satisfaites. ",
                n_low, " cellule(s) sur ", tot_cells, " (", pct_low, "%) ont un effectif th\u00e9orique attendu inf\u00e9rieur \u00e0 5 (effectif minimal attendu : ", round(min_exp, 2), "). ",
                "Il est conseill\u00e9 de regrouper certaines modalit\u00e9s ou d'utiliser le test exact de Fisher."
              )
            }
          }
          cont_state$exp_warn_msg <- exp_warn_msg

          code_entry <- paste0(
            "# Tableau de contingence et Test du Chi-deux\n",
            "tab <- table(", row_col_code, ", ", col_col_code, ")\n",
            "tab\n",
            "chisq.test(tab, correct = ", cont_state$correct, ")"
          )
        } else if (cont_state$test == "fisher") {
          cont_state$exp_warn_msg <- NULL
          res <- stats::fisher.test(tab)
          es <- ramses_compute_effect_size("fisher", stat_result = res, tab = tab)
          pwr <- NULL
          code_entry <- paste0(
            "# Test Exact de Fisher\n",
            "tab <- table(", row_col_code, ", ", col_col_code, ")\n",
            "fisher.test(tab)"
          )
        } else if (cont_state$test == "mcnemar") {
          cont_state$exp_warn_msg <- NULL
          res <- stats::mcnemar.test(tab)
          es <- ramses_compute_effect_size("mcnemar", stat_result = res, tab = tab)
          pwr <- NULL
          code_entry <- paste0(
            "# Test de McNemar\n",
            "tab <- table(", row_col_code, ", ", col_col_code, ")\n",
            "mcnemar.test(tab)"
          )
        }
      }, error = function(e) {
        err <- e$message
      })

      cont_state$result <- res
      cont_state$tab <- tab
      cont_state$effect_size <- es
      cont_state$power <- pwr
      cont_state$error <- err
      cont_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Contingence : ", cont_state$test, " (", cont_state$var_row, " x ", cont_state$var_col, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Test de contingence ex\u00e9cut\u00e9 !", type = "message")
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
            shiny::tags$span(style = "font-weight: 600;", "Param\u00e8tres : Analyse de Contingence")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_cont"), "Ex\u00e9cuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("cont_test_choice"),
              label = "Test d'ind\u00e9pendance / association :",
              choices = c(
                "Test du Chi-deux (chisq.test)" = "chisq",
                "Test exact de Fisher (fisher.test)" = "fisher",
                "Test de McNemar (donn\u00e9es appari\u00e9es)" = "mcnemar"
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
                label = "Correction de continuit\u00e9 de Yates",
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
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ex\u00e9cuter' pour lancer le Chi-deux ou le test de Fisher."))
      }
      if (!is.null(cont_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", cont_state$error)))
      }
      if (is.null(cont_state$result) || is.null(cont_state$result$p.value)) {
        return(shiny::div(class = "alert alert-warning py-2 px-3 small", "Aucun r\u00e9sultat disponible."))
      }
      p_val <- cont_state$result$p.value
      sig <- isTRUE(p_val < cont_state$alpha)
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) "Association statistiquement significative entre les deux variables !" else "Variables ind\u00e9pendantes (aucune liaison significative)"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
      )
    })

    output$cont_results_ui <- shiny::renderUI({
      shiny::req(cont_state$calculated)
      if (!is.null(cont_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", cont_state$error)))
      }
      if (is.null(cont_state$result) || is.null(cont_state$result$p.value)) {
        return(shiny::p(class = "text-muted small", "Aucun r\u00e9sultat disponible."))
      }
      res <- cont_state$result
      p_val <- res$p.value
      stat_val <- unname(res$statistic)
      stat_name <- names(res$statistic)
      sig <- isTRUE(p_val < cont_state$alpha)
      es <- cont_state$effect_size
      pwr <- cont_state$power

      decision_text <- if (sig) {
        paste0("Au risque alpha = ", cont_state$alpha, ", il existe une association statistiquement significative entre '", cont_state$var_row, "' et '", cont_state$var_col, "' (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'ind\u00e9pendance est rejet\u00e9e.")
      } else {
        paste0("Au risque alpha = ", cont_state$alpha, ", aucune association statistiquement significative n'est mise en \u00e9vidence (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle d'ind\u00e9pendance est conserv\u00e9e.")
      }

      tiles_cont <- list(
        list(label = if (!is.null(stat_name) && nzchar(stat_name)) paste0("Statistique (", stat_name, ")") else "Statistique", value = if (!is.null(stat_val)) as.character(round(stat_val, 3)) else "\u2014", subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else NULL),
        list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
        list(label = "Taille d'effet", value = if (!is.null(es)) es$formatted_value else "\u2014", subtext = if (!is.null(es)) paste0(es$symbol, " (", es$magnitude, ")") else NULL),
        list(label = "Puissance", value = if (!is.null(pwr)) pwr$percentage else "\u2014", subtext = if (!is.null(pwr)) pwr$magnitude else NULL),
        list(label = "D\u00e9cision", value = if (sig) "Association" else "Ind\u00e9pendance", status = if (sig) "success" else "dark", subtext = paste0("\u03b1 = ", cont_state$alpha))
      )

      shiny::tagList(
        ramses_result_tiles(tiles_cont, title = "Indicateurs cl\u00e9s du test de contingence"),
        if (!is.null(cont_state$exp_warn_msg)) {
          shiny::div(
            class = "alert alert-warning py-2 px-3 small mb-3 border-warning",
            shiny::tags$strong("Avertissement sur les effectifs th\u00e9oriques : "),
            cont_state$exp_warn_msg
          )
        },
        shiny::tags$div(
          class = "table-responsive",
          shiny::tags$table(
            class = "table table-sm table-bordered text-center align-middle mb-3",
            shiny::tags$thead(
              class = "table-light",
              shiny::tags$tr(
                shiny::tags$th("M\u00e9thode"),
                shiny::tags$th("Statistique"),
                shiny::tags$th("ddl"),
                shiny::tags$th("p-value"),
                shiny::tags$th("Taille d'effet"),
                shiny::tags$th("Puissance"),
                shiny::tags$th("Conclusion")
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-start fw-medium", res$method),
                shiny::tags$td(if (!is.null(stat_val)) paste0(stat_name, " = ", round(stat_val, 3)) else "\u2014"),
                shiny::tags$td(if (!is.null(res$parameter)) round(res$parameter, 1) else "\u2014"),
                shiny::tags$td(class = "fw-bold text-primary", format.pval(p_val, digits = 4, eps = 0.0001)),
                shiny::tags$td(class = "small", if (!is.null(es)) paste0(es$symbol, " = ", es$formatted_value, " (", es$magnitude, ")") else "\u2014"),
                shiny::tags$td(class = "small", if (!is.null(pwr)) paste0(pwr$percentage, " (", pwr$magnitude, ")") else "\u2014"),
                shiny::tags$td(
                  shiny::tags$span(
                    class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                    if (sig) "Liaison significative" else "Ind\u00e9pendance"
                  )
                )
              )
            )
          )
        ),
        shiny::div(
          class = "p-3 rounded bg-light border text-secondary small mb-3",
          shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
          shiny::tags$ul(
            class = "mb-0 ps-3 space-y-1",
            shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), decision_text),
            if (!is.null(es)) shiny::tags$li(shiny::tags$strong(paste0("Taille d'effet (", es$name, ") : ")), es$description),
            if (!is.null(pwr)) shiny::tags$li(shiny::tags$strong("Puissance statistique : "), paste0("La puissance a posteriori calcul\u00e9e est de ", pwr$percentage, " (", pwr$magnitude, ")."))
          )
        ),
        shiny::div(
          class = "mt-3",
          shiny::tags$h6(class = "fw-bold text-dark mb-1", "Tableau des effectifs observ\u00e9s :"),
          shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(utils::capture.output(print(cont_state$tab)), collapse = "\n"))
        )
      )
    })

    output$cont_plot <- plotly::renderPlotly({
      shiny::req(cont_state$calculated, cont_state$tab)
      tab_df <- as.data.frame(cont_state$tab)
      names(tab_df) <- c("Ligne", "Colonne", "Freq")

      plotly::plot_ly(
        data = tab_df,
        x = ~Ligne,
        y = ~Freq,
        color = ~Colonne,
        type = "bar"
      ) %>%
        plotly::layout(font = list(family = "IBM Plex Sans"), 
          barmode = "stack",
          title = list(text = paste0("Contingence : ", cont_state$var_row, " x ", cont_state$var_col), font = list(size = 13)),
          xaxis = list(title = cont_state$var_row),
          yaxis = list(title = "Effectifs (Fr\u00e9quence)")
        )
    })

    output$cont_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_cont(cont_state, data_holder$df)
    })

    # =========================================================================
    # 4bis. TESTS DE PROPORTIONS (1 ET 2 \u00c9CHANTILLONS)
    # =========================================================================
    shiny::observeEvent(input$prop_one_var_direct, {
      df <- data_holder$df
      var_o <- input$prop_one_var_direct
      if (!is.null(df) && !is.null(var_o) && (var_o %in% names(df))) {
        u_vals <- unique(as.character(df[[var_o]][!is.na(df[[var_o]])]))
        shiny::updateSelectInput(session, "prop_one_success_direct", choices = u_vals, selected = if (length(u_vals) > 0) u_vals[1] else NULL)
      }
    })

    shiny::observeEvent(input$prop_two_var_outcome_direct, {
      df <- data_holder$df
      var_o <- input$prop_two_var_outcome_direct
      if (!is.null(df) && !is.null(var_o) && (var_o %in% names(df))) {
        u_vals <- unique(as.character(df[[var_o]][!is.na(df[[var_o]])]))
        shiny::updateSelectInput(session, "prop_two_success_direct", choices = u_vals, selected = if (length(u_vals) > 0) u_vals[1] else NULL)
      }
    })

    prop_group_modalities_direct <- shiny::reactive({
      df <- data_holder$df
      var_g <- input$prop_two_var_group_direct
      if (is.null(df) || is.null(var_g) || !(var_g %in% names(df))) return(character(0))
      col_data <- df[[var_g]]
      col_data <- col_data[!is.na(col_data)]
      unique(as.character(col_data))
    })

    output$prop_two_group_mods_direct_ui <- shiny::renderUI({
      mods <- prop_group_modalities_direct()
      if (length(mods) > 2) {
        shiny::tagList(
          shiny::div(
            class = "alert alert-warning py-1 px-2 small mb-2",
            paste0("Variable \u00e0 ", length(mods), " modalit\u00e9s : veuillez en s\u00e9lectionner exactement 2 pour la comparaison.")
          ),
          shiny::selectizeInput(
            inputId = ns("prop_two_selected_groups_direct"),
            label = "Modalit\u00e9s \u00e0 comparer (exactement 2) :",
            choices = mods,
            selected = mods[1:2],
            multiple = TRUE,
            options = list(maxItems = 2, plugins = list("remove_button"))
          )
        )
      } else if (length(mods) == 2) {
        shiny::div(
          class = "small text-muted mb-2",
          paste0("2 modalit\u00e9s d\u00e9tect\u00e9es : ", mods[1], " vs ", mods[2])
        )
      } else if (length(mods) < 2 && length(mods) > 0) {
        shiny::div(
          class = "alert alert-danger py-1 px-2 small mb-2",
          "La variable de regroupement poss\u00e8de moins de 2 modalit\u00e9s valides."
        )
      }
    })

    # Modale pour Tests de Proportions
    shiny::observeEvent(input$btn_open_prop_modal, {
      cat_cols <- get_cat_vars()
      shiny::showModal(
        shiny::modalDialog(
          title = "Configuration du Test de Proportions",
          size = "l",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(
              inputId = ns("btn_confirm_prop"),
              label = "Ex\u00e9cuter & Journaliser",
              class = "btn-dark shadow-sm"
            )
          ),
          shiny::radioButtons(
            inputId = ns("prop_analysis_type_modal"),
            label = "Que voulez-vous tester ?",
            choices = c(
              "Une proportion (\u00e9chantillon unique vs p0)" = "one_sample",
              "Deux proportions (comparaison 2 groupes)" = "two_samples"
            ),
            selected = input$prop_analysis_type_direct %||% "one_sample"
          ),
          shiny::conditionalPanel(
            condition = "input.prop_analysis_type_modal == 'one_sample'",
            ns = ns,
            shiny::div(
              class = "alert alert-info py-1 px-2 small mb-2",
              "Compare la proportion observ\u00e9e d'un \u00e9v\u00e9nement \u00e0 une proportion th\u00e9orique de r\u00e9f\u00e9rence (p0)."
            ),
            shiny::selectInput(
              inputId = ns("prop_one_var_modal"),
              label = "Variable d'int\u00e9r\u00eat (binaire / qualitative) :",
              choices = cat_cols,
              selected = input$prop_one_var_direct %||% (if (length(cat_cols) > 0) cat_cols[1] else NULL)
            ),
            shiny::selectInput(
              inputId = ns("prop_one_success_modal"),
              label = "Modalit\u00e9 consid\u00e9r\u00e9e comme \u00ab succ\u00e8s \u00bb :",
              choices = NULL
            ),
            shiny::numericInput(
              inputId = ns("prop_one_p0_modal"),
              label = "Proportion th\u00e9orique de r\u00e9f\u00e9rence (p0) :",
              value = input$prop_one_p0_direct %||% 0.5,
              min = 0.001,
              max = 0.999,
              step = 0.05
            ),
            shiny::selectInput(
              inputId = ns("prop_one_method_modal"),
              label = "M\u00e9thode de test :",
              choices = c(
                "Test asymptotique du Chi-deux (prop.test)" = "prop",
                "Test exact binomial (binom.test)" = "binom"
              ),
              selected = input$prop_one_method_direct %||% "prop"
            ),
            shiny::conditionalPanel(
              condition = "input.prop_one_method_modal == 'prop'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("prop_one_correct_modal"),
                label = "Correction de continuit\u00e9 de Yates",
                value = if (!is.null(input$prop_one_correct_direct)) input$prop_one_correct_direct else TRUE
              )
            ),
            shiny::selectInput(
              inputId = ns("prop_one_alt_modal"),
              label = "Hypoth\u00e8se alternative (H1) :",
              choices = c(
                "Bilat\u00e9rale (p \u2260 p0)" = "two.sided",
                "Unilat\u00e9rale gauche (p < p0)" = "less",
                "Unilat\u00e9rale droite (p > p0)" = "greater"
              ),
              selected = input$prop_one_alt_direct %||% "two.sided"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.prop_analysis_type_modal == 'two_samples'",
            ns = ns,
            shiny::div(
              class = "alert alert-info py-1 px-2 small mb-2",
              "Compare les proportions d'un m\u00eame \u00e9v\u00e9nement entre deux groupes ind\u00e9pendants."
            ),
            shiny::selectInput(
              inputId = ns("prop_two_var_outcome_modal"),
              label = "Variable crit\u00e8re (succ\u00e8s / \u00e9chec) :",
              choices = cat_cols,
              selected = input$prop_two_var_outcome_direct %||% (if (length(cat_cols) > 0) cat_cols[1] else NULL)
            ),
            shiny::selectInput(
              inputId = ns("prop_two_success_modal"),
              label = "Modalit\u00e9 consid\u00e9r\u00e9e comme \u00ab succ\u00e8s \u00bb :",
              choices = NULL
            ),
            shiny::selectInput(
              inputId = ns("prop_two_var_group_modal"),
              label = "Variable qualitative de regroupement (X) :",
              choices = cat_cols,
              selected = input$prop_two_var_group_direct %||% (if (length(cat_cols) > 1) cat_cols[2] else if (length(cat_cols) > 0) cat_cols[1] else NULL)
            ),
            shiny::uiOutput(ns("prop_two_group_mods_modal_ui")),
            shiny::selectInput(
              inputId = ns("prop_two_method_modal"),
              label = "M\u00e9thode de test :",
              choices = c(
                "Test asymptotique de comparaison (prop.test)" = "prop",
                "Test exact de Fisher (fisher.test)" = "fisher"
              ),
              selected = input$prop_two_method_direct %||% "prop"
            ),
            shiny::conditionalPanel(
              condition = "input.prop_two_method_modal == 'prop'",
              ns = ns,
              shiny::checkboxInput(
                inputId = ns("prop_two_correct_modal"),
                label = "Correction de continuit\u00e9 de Yates",
                value = if (!is.null(input$prop_two_correct_direct)) input$prop_two_correct_direct else TRUE
              )
            ),
            shiny::selectInput(
              inputId = ns("prop_two_alt_modal"),
              label = "Hypoth\u00e8se alternative (H1) :",
              choices = c(
                "Bilat\u00e9rale (p1 \u2260 p2)" = "two.sided",
                "Unilat\u00e9rale gauche (p1 < p2)" = "less",
                "Unilat\u00e9rale droite (p1 > p2)" = "greater"
              ),
              selected = input$prop_two_alt_direct %||% "two.sided"
            )
          ),
          shiny::selectInput(
            inputId = ns("prop_alpha_modal"),
            label = "Seuil de significativit\u00e9 (alpha) :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = input$prop_alpha_direct %||% "0.05"
          )
        )
      )
    })

    shiny::observeEvent(input$prop_one_var_modal, {
      df <- data_holder$df
      var_o <- input$prop_one_var_modal
      if (!is.null(df) && !is.null(var_o) && (var_o %in% names(df))) {
        u_vals <- unique(as.character(df[[var_o]][!is.na(df[[var_o]])]))
        shiny::updateSelectInput(session, "prop_one_success_modal", choices = u_vals, selected = if (length(u_vals) > 0) u_vals[1] else NULL)
      }
    })

    shiny::observeEvent(input$prop_two_var_outcome_modal, {
      df <- data_holder$df
      var_o <- input$prop_two_var_outcome_modal
      if (!is.null(df) && !is.null(var_o) && (var_o %in% names(df))) {
        u_vals <- unique(as.character(df[[var_o]][!is.na(df[[var_o]])]))
        shiny::updateSelectInput(session, "prop_two_success_modal", choices = u_vals, selected = if (length(u_vals) > 0) u_vals[1] else NULL)
      }
    })

    prop_group_modalities_modal <- shiny::reactive({
      df <- data_holder$df
      var_g <- input$prop_two_var_group_modal
      if (is.null(df) || is.null(var_g) || !(var_g %in% names(df))) return(character(0))
      col_data <- df[[var_g]]
      col_data <- col_data[!is.na(col_data)]
      unique(as.character(col_data))
    })

    output$prop_two_group_mods_modal_ui <- shiny::renderUI({
      mods <- prop_group_modalities_modal()
      if (length(mods) > 2) {
        shiny::tagList(
          shiny::div(
            class = "alert alert-warning py-1 px-2 small mb-2",
            paste0("Variable \u00e0 ", length(mods), " modalit\u00e9s : veuillez en s\u00e9lectionner exactement 2 pour la comparaison.")
          ),
          shiny::selectizeInput(
            inputId = ns("prop_two_selected_groups_modal"),
            label = "Modalit\u00e9s \u00e0 comparer (exactement 2) :",
            choices = mods,
            selected = mods[1:2],
            multiple = TRUE,
            options = list(maxItems = 2, plugins = list("remove_button"))
          )
        )
      } else if (length(mods) == 2) {
        shiny::div(
          class = "small text-muted mb-2",
          paste0("2 modalit\u00e9s d\u00e9tect\u00e9es : ", mods[1], " vs ", mods[2])
        )
      } else if (length(mods) < 2 && length(mods) > 0) {
        shiny::div(
          class = "alert alert-danger py-1 px-2 small mb-2",
          "La variable de regroupement poss\u00e8de moins de 2 modalit\u00e9s valides."
        )
      }
    })

    # Moteur d'analyse des proportions
    run_prop_analysis <- function(analysis_type,
                                  var_outcome,
                                  success_modality,
                                  p0_val,
                                  method_choice,
                                  correct_opt,
                                  alt_choice,
                                  alpha_val,
                                  var_group = NULL,
                                  selected_groups = NULL) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_outcome) || !nzchar(var_outcome)) {
        shiny::showNotification("Veuillez s\u00e9lectionner une variable valide.", type = "warning")
        return()
      }

      prop_state$analysis_type <- analysis_type
      prop_state$var_outcome <- var_outcome
      prop_state$success_modality <- success_modality
      prop_state$p0 <- as.numeric(p0_val)
      prop_state$method <- method_choice
      prop_state$correct <- isTRUE(correct_opt)
      prop_state$alternative <- alt_choice
      prop_state$alpha <- as.numeric(alpha_val)
      prop_state$var_group <- var_group
      prop_state$selected_groups <- selected_groups

      res <- NULL
      err <- NULL
      code_entry <- ""
      ds_name <- data_holder$name

      tryCatch({
        conf_lvl <- 1 - prop_state$alpha
        if (prop_state$analysis_type == "one_sample") {
          res <- ramses_test_one_prop(
            df = df,
            var_outcome = prop_state$var_outcome,
            success_modality = prop_state$success_modality,
            p0 = prop_state$p0,
            alternative = prop_state$alternative,
            conf_level = conf_lvl,
            method = prop_state$method,
            correct = prop_state$correct
          )
          out_col_code <- ramses_code_column(ds_name, prop_state$var_outcome)
          succ_lit <- ramses_code_string(as.character(prop_state$success_modality))
          code_entry <- paste0(
            "# ==============================================================================\n",
            "# Test d'une proportion : ", prop_state$var_outcome, " (Succ\u00e8s : ", succ_lit, ")\n",
            "# ==============================================================================\n",
            "x_vec <- ", out_col_code, "\n",
            "x_clean <- x_vec[!is.na(x_vec)]\n",
            "n_total <- length(x_clean)\n",
            "n_success <- sum(as.character(x_clean) == ", succ_lit, ")\n",
            if (prop_state$method == "prop") {
              paste0("stats::prop.test(x = n_success, n = n_total, p = ", prop_state$p0, ", alternative = ", ramses_code_string(prop_state$alternative), ", conf.level = ", conf_lvl, ", correct = ", prop_state$correct, ")")
            } else {
              paste0("stats::binom.test(x = n_success, n = n_total, p = ", prop_state$p0, ", alternative = ", ramses_code_string(prop_state$alternative), ", conf.level = ", conf_lvl, ")")
            }
          )
        } else {
          if (is.null(var_group) || !nzchar(var_group)) {
            stop("Veuillez s\u00e9lectionner une variable de regroupement.")
          }
          res <- ramses_test_two_props(
            df = df,
            var_outcome = prop_state$var_outcome,
            var_group = prop_state$var_group,
            success_modality = prop_state$success_modality,
            group_levels = prop_state$selected_groups,
            alternative = prop_state$alternative,
            conf_level = conf_lvl,
            method = prop_state$method,
            correct = prop_state$correct
          )
          grp1_lit <- ramses_code_string(res$group_levels[1])
          grp2_lit <- ramses_code_string(res$group_levels[2])
          out_col_code <- ramses_code_column(ds_name, prop_state$var_outcome)
          grp_col_code <- ramses_code_column(ds_name, prop_state$var_group)
          succ_lit <- ramses_code_string(as.character(prop_state$success_modality))
          code_entry <- paste0(
            "# ==============================================================================\n",
            "# Comparaison de deux proportions : ", prop_state$var_outcome, " par ", prop_state$var_group, "\n",
            "# ==============================================================================\n",
            "sub_df <- ", ds_name, "[!is.na(", out_col_code, ") & !is.na(", grp_col_code, ") & (as.character(", grp_col_code, ") %in% c(", grp1_lit, ", ", grp2_lit, ")), ]\n",
            "n1 <- sum(as.character(sub_df[[", ramses_code_string(prop_state$var_group), "]]) == ", grp1_lit, ")\n",
            "n2 <- sum(as.character(sub_df[[", ramses_code_string(prop_state$var_group), "]]) == ", grp2_lit, ")\n",
            "x1 <- sum(as.character(sub_df[[", ramses_code_string(prop_state$var_group), "]]) == ", grp1_lit, " & as.character(sub_df[[", ramses_code_string(prop_state$var_outcome), "]]) == ", succ_lit, ")\n",
            "x2 <- sum(as.character(sub_df[[", ramses_code_string(prop_state$var_group), "]]) == ", grp2_lit, " & as.character(sub_df[[", ramses_code_string(prop_state$var_outcome), "]]) == ", succ_lit, ")\n",
            "tab_props <- data.frame(\n",
            "  Groupe = c(", grp1_lit, ", ", grp2_lit, "),\n",
            "  Succes = c(x1, x2),\n",
            "  Total = c(n1, n2),\n",
            "  Proportion = c(x1 / n1, x2 / n2)\n",
            ")\n",
            "print(tab_props)\n",
            if (prop_state$method == "prop") {
              paste0("stats::prop.test(x = c(x1, x2), n = c(n1, n2), alternative = ", ramses_code_string(prop_state$alternative), ", conf.level = ", conf_lvl, ", correct = ", prop_state$correct, ")")
            } else {
              paste0("mat_2x2 <- matrix(c(x1, n1 - x1, x2, n2 - x2), nrow = 2, byrow = TRUE)\nstats::fisher.test(mat_2x2, alternative = ", ramses_code_string(prop_state$alternative), ", conf.level = ", conf_lvl, ")")
            }
          )
        }
      }, error = function(e) {
        err <<- e$message
      })

      prop_state$result <- res
      prop_state$error <- err
      prop_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Test de proportions : ", if (prop_state$analysis_type == "one_sample") "Echantillon unique" else "Deux echantillons"),
          code = code_entry
        )
        shiny::showNotification("Test de proportion ex\u00e9cut\u00e9 et journalis\u00e9 !", type = "message")
      } else if (!is.null(err)) {
        shiny::showNotification(paste0("Erreur : ", err), type = "error")
      }
    }

    shiny::observeEvent(input$btn_run_prop, {
      atype <- input$prop_analysis_type_direct %||% "one_sample"
      if (atype == "one_sample") {
        run_prop_analysis(
          analysis_type = "one_sample",
          var_outcome = input$prop_one_var_direct,
          success_modality = input$prop_one_success_direct,
          p0_val = input$prop_one_p0_direct,
          method_choice = input$prop_one_method_direct,
          correct_opt = input$prop_one_correct_direct,
          alt_choice = input$prop_one_alt_direct,
          alpha_val = input$prop_alpha_direct
        )
      } else {
        mods <- prop_group_modalities_direct()
        sel_groups <- if (length(mods) > 2) input$prop_two_selected_groups_direct else mods[1:2]
        run_prop_analysis(
          analysis_type = "two_samples",
          var_outcome = input$prop_two_var_outcome_direct,
          success_modality = input$prop_two_success_direct,
          p0_val = 0.5,
          method_choice = input$prop_two_method_direct,
          correct_opt = input$prop_two_correct_direct,
          alt_choice = input$prop_two_alt_direct,
          alpha_val = input$prop_alpha_direct,
          var_group = input$prop_two_var_group_direct,
          selected_groups = sel_groups
        )
      }
    })

    shiny::observeEvent(input$btn_confirm_prop, {
      shiny::removeModal()
      atype <- input$prop_analysis_type_modal %||% "one_sample"
      if (atype == "one_sample") {
        run_prop_analysis(
          analysis_type = "one_sample",
          var_outcome = input$prop_one_var_modal,
          success_modality = input$prop_one_success_modal,
          p0_val = input$prop_one_p0_modal,
          method_choice = input$prop_one_method_modal,
          correct_opt = input$prop_one_correct_modal,
          alt_choice = input$prop_one_alt_modal,
          alpha_val = input$prop_alpha_modal
        )
      } else {
        mods <- prop_group_modalities_modal()
        sel_groups <- if (length(mods) > 2) input$prop_two_selected_groups_modal else mods[1:2]
        run_prop_analysis(
          analysis_type = "two_samples",
          var_outcome = input$prop_two_var_outcome_modal,
          success_modality = input$prop_two_success_modal,
          p0_val = 0.5,
          method_choice = input$prop_two_method_modal,
          correct_opt = input$prop_two_correct_modal,
          alt_choice = input$prop_two_alt_modal,
          alpha_val = input$prop_alpha_modal,
          var_group = input$prop_two_var_group_modal,
          selected_groups = sel_groups
        )
      }
    })

    output$prop_status_badge <- shiny::renderUI({
      if (!prop_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ex\u00e9cuter & Journaliser' pour lancer le test de proportion."))
      }
      if (!is.null(prop_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", prop_state$error)))
      }
      res <- prop_state$result
      p_val <- res$p_value
      sig <- p_val < prop_state$alpha
      msg <- if (prop_state$analysis_type == "one_sample") {
        if (sig) paste0("Diff\u00e9rence significative avec p0 = ", res$p0, " !") else paste0("Aucune diff\u00e9rence significative d\u00e9tect\u00e9e avec p0 = ", res$p0)
      } else {
        if (sig) "Diff\u00e9rence significative de proportion entre les deux groupes !" else "Aucune diff\u00e9rence significative d\u00e9tect\u00e9e entre les deux groupes"
      }
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(msg),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
      )
    })

    output$prop_results_ui <- shiny::renderUI({
      shiny::req(prop_state$calculated)
      if (!is.null(prop_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", prop_state$error)))
      }
      res <- prop_state$result
      shiny::req(res)

      sig <- res$p_value < prop_state$alpha

      if (prop_state$analysis_type == "one_sample") {
        stat_name <- if (prop_state$method == "prop") "Chi-deux" else "Succ\u00e8s observ\u00e9s (x)"
        stat_val <- if (prop_state$method == "prop") round(unname(res$htest$statistic), 3) else res$x
        p_pct <- paste0(round(res$p_obs * 100, 2), " %")
        p0_pct <- paste0(round(res$p0 * 100, 2), " %")
        ci_str <- paste0("[", round(res$conf_int[1] * 100, 2), "% ; ", round(res$conf_int[2] * 100, 2), "%]")
        p_val_str <- if (res$p_value < 0.001) "< 0.001" else round(res$p_value, 4)

        tiles_prop_one <- list(
          list(label = "Effectif total (N)", value = as.character(res$n), subtext = paste0(res$x, " succ\u00e8s observ\u00e9s")),
          list(label = "Proportion observ\u00e9e", value = p_pct, subtext = paste0("p0 = ", p0_pct), status = "primary"),
          list(label = if (prop_state$method == "prop") "Chi-deux" else "Succ\u00e8s (x)", value = as.character(stat_val), subtext = res$method),
          list(label = "p-value", value = p_val_str, subtext = paste0("Seuil \u03b1 = ", prop_state$alpha), status = if (sig) "success" else "dark"),
          list(label = "D\u00e9cision", value = if (sig) "Diff\u00e9rence significative" else "Non significatif", status = if (sig) "success" else "dark", subtext = paste0("IC : ", ci_str))
        )

        shiny::tagList(
          ramses_result_tiles(tiles_prop_one, title = "Indicateurs cl\u00e9s du test de proportion"),
          if (!is.null(res$warning_msg)) {
            shiny::div(class = "alert alert-warning py-2 px-3 small mb-3", shiny::tags$strong("Avertissement m\u00e9thodologique : "), res$warning_msg)
          },
          shiny::div(
            class = "table-responsive mb-3",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Effectif total (N)"),
                  shiny::tags$th(paste0("Succ\u00e8s '", res$success_modality, "' (x)")),
                  shiny::tags$th("\u00c9checs"),
                  shiny::tags$th("Proportion observ\u00e9e"),
                  shiny::tags$th("Proportion th\u00e9orique (p0)"),
                  shiny::tags$th(stat_name),
                  shiny::tags$th("p-value"),
                  shiny::tags$th(paste0("IC \u00e0 ", round(res$conf_level * 100), "%"))
                )
              ),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td(res$n),
                  shiny::tags$td(class = "fw-bold", res$x),
                  shiny::tags$td(res$n - res$x),
                  shiny::tags$td(class = "fw-bold text-primary", p_pct),
                  shiny::tags$td(p0_pct),
                  shiny::tags$td(stat_val),
                  shiny::tags$td(class = "fw-bold", p_val_str),
                  shiny::tags$td(ci_str)
                )
              )
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), res$decision),
              if (!is.null(res$effect_size)) shiny::tags$li(shiny::tags$strong("Taille d'effet (h de Cohen) : "), res$effect_size$description),
              if (!is.null(res$power)) shiny::tags$li(shiny::tags$strong("Puissance statistique : "), paste0("La puissance a posteriori calcul\u00e9e est de ", res$power$percentage, " (", res$power$magnitude, ")."))
            )
          ),
          shiny::div(
            class = "mt-3",
            shiny::tags$h6(class = "fw-bold text-dark mb-1", "Sortie brute R (htest) :"),
            shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(utils::capture.output(print(res$htest)), collapse = "\n"))
          )
        )
      } else {
        # 2 groupes
        p1_pct <- paste0(round(res$groups_summary$Proportion[1] * 100, 2), " %")
        p2_pct <- paste0(round(res$groups_summary$Proportion[2] * 100, 2), " %")
        diff_pct <- paste0(round(res$diff * 100, 2), " %")
        ci_diff_str <- paste0("[", round(res$conf_int_diff[1] * 100, 2), "% ; ", round(res$conf_int_diff[2] * 100, 2), "%]")
        p_val_str <- if (res$p_value < 0.001) "< 0.001" else round(res$p_value, 4)
        stat_str <- if (prop_state$method == "prop") paste0("Chi2 = ", round(unname(res$htest$statistic), 3)) else "Test Exact de Fisher"

        tiles_prop_two <- list(
          list(label = paste0("Prop. ", res$group_levels[1]), value = p1_pct, subtext = paste0("n = ", res$groups_summary$Total[1])),
          list(label = paste0("Prop. ", res$group_levels[2]), value = p2_pct, subtext = paste0("n = ", res$groups_summary$Total[2])),
          list(label = "Diff\u00e9rence (p1 - p2)", value = diff_pct, subtext = paste0("IC : ", ci_diff_str)),
          list(label = "Risque Relatif (RR)", value = if (!is.na(res$relative_risk)) as.character(round(res$relative_risk, 3)) else "\u2014", subtext = if (!is.na(res$odds_ratio)) paste0("OR = ", round(res$odds_ratio, 3)) else NULL),
          list(label = "p-value", value = p_val_str, subtext = stat_str, status = if (sig) "success" else "dark"),
          list(label = "D\u00e9cision", value = if (sig) "Diff\u00e9rence significative" else "Non significatif", status = if (sig) "success" else "dark")
        )

        shiny::tagList(
          ramses_result_tiles(tiles_prop_two, title = "Indicateurs cl\u00e9s de la comparaison de proportions"),
          if (!is.null(res$warning_msg)) {
            shiny::div(class = "alert alert-warning py-2 px-3 small mb-3", shiny::tags$strong("Avertissement m\u00e9thodologique : "), res$warning_msg)
          },
          shiny::div(
            class = "table-responsive mb-3",
            shiny::tags$table(
              class = "table table-sm table-bordered text-center align-middle",
              shiny::tags$thead(
                class = "table-light",
                shiny::tags$tr(
                  shiny::tags$th("Groupe"),
                  shiny::tags$th(paste0("Succ\u00e8s '", res$success_modality, "'")),
                  shiny::tags$th("Total (N)"),
                  shiny::tags$th("Proportion observ\u00e9e"),
                  shiny::tags$th(paste0("IC individuel \u00e0 ", round(res$conf_level * 100), "%"))
                )
              ),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td(class = "fw-bold", res$group_levels[1]),
                  shiny::tags$td(res$groups_summary$Succes[1]),
                  shiny::tags$td(res$groups_summary$Total[1]),
                  shiny::tags$td(class = "fw-bold text-primary", p1_pct),
                  shiny::tags$td(paste0("[", round(res$conf_int_grp1[1] * 100, 2), "% ; ", round(res$conf_int_grp1[2] * 100, 2), "%]"))
                ),
                shiny::tags$tr(
                  shiny::tags$td(class = "fw-bold", res$group_levels[2]),
                  shiny::tags$td(res$groups_summary$Succes[2]),
                  shiny::tags$td(res$groups_summary$Total[2]),
                  shiny::tags$td(class = "fw-bold text-primary", p2_pct),
                  shiny::tags$td(paste0("[", round(res$conf_int_grp2[1] * 100, 2), "% ; ", round(res$conf_int_grp2[2] * 100, 2), "%]"))
                )
              )
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border text-secondary small mb-3",
            shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 space-y-1",
              shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), res$decision),
              if (!is.null(res$effect_size)) shiny::tags$li(shiny::tags$strong("Taille d'effet (h de Cohen) : "), res$effect_size$description),
              if (!is.null(res$power)) shiny::tags$li(shiny::tags$strong("Puissance statistique : "), paste0("La puissance a posteriori calcul\u00e9e est de ", res$power$percentage, " (", res$power$magnitude, ")."))
            )
          ),
          shiny::div(
            class = "mt-3",
            shiny::tags$h6(class = "fw-bold text-dark mb-1", "Tableau de contingence 2x2 :"),
            shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(utils::capture.output(print(res$table_2x2)), collapse = "\n"))
          ),
          shiny::div(
            class = "mt-3",
            shiny::tags$h6(class = "fw-bold text-dark mb-1", "Sortie brute R (htest) :"),
            shiny::tags$pre(class = "p-2 bg-light text-dark rounded small font-monospace border", paste(utils::capture.output(print(res$htest)), collapse = "\n"))
          )
        )
      }
    })

    output$prop_plot <- plotly::renderPlotly({
      shiny::req(prop_state$calculated, prop_state$result)
      res <- prop_state$result

      if (prop_state$analysis_type == "one_sample") {
        p_pct <- res$p_obs * 100
        p0_pct <- res$p0 * 100
        ci_low <- res$conf_int[1] * 100
        ci_high <- res$conf_int[2] * 100

        plot_df <- data.frame(
          Type = c("Observ\u00e9e", "Th\u00e9orique (p0)"),
          Proportion = c(p_pct, p0_pct),
          stringsAsFactors = FALSE
        )

        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Type, y = Proportion, fill = Type)) +
          ggplot2::geom_col(width = 0.45, alpha = 0.85) +
          ggplot2::geom_errorbar(
            data = data.frame(Type = "Observ\u00e9e", ymin = ci_low, ymax = ci_high),
            ggplot2::aes(x = Type, ymin = ymin, ymax = ymax),
            width = 0.15,
            color = "#111827",
            linewidth = 0.8,
            inherit.aes = FALSE
          ) +
          ggplot2::geom_hline(yintercept = p0_pct, linetype = "dashed", color = "#DC2626", linewidth = 0.7) +
          ggplot2::scale_fill_manual(values = c("Observ\u00e9e" = "#2563EB", "Th\u00e9orique (p0)" = "#9CA3AF")) +
          ggplot2::scale_y_continuous(limits = c(0, min(100, max(100, ci_high + 10))), labels = function(x) paste0(x, "%")) +
          ggplot2::labs(
            title = paste0("Proportion de '", res$success_modality, "' : ", round(p_pct, 1), "% (IC : [", round(ci_low, 1), "% ; ", round(ci_high, 1), "%])"),
            x = "",
            y = "Proportion (%)"
          ) +
          ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
          ggplot2::theme(
            text = ggplot2::element_text(family = "sans"),
            plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
            axis.title = ggplot2::element_text(size = 11, color = "#374151"),
            axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5),
            legend.position = "none"
          )

        tryCatch(plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans")), error = function(e) plotly::layout(plotly::plotly_build(p), font = list(family = "IBM Plex Sans")))
      } else {
        grp1 <- res$group_levels[1]
        grp2 <- res$group_levels[2]
        p1_pct <- res$groups_summary$Proportion[1] * 100
        p2_pct <- res$groups_summary$Proportion[2] * 100
        ci1_low <- res$conf_int_grp1[1] * 100
        ci1_high <- res$conf_int_grp1[2] * 100
        ci2_low <- res$conf_int_grp2[1] * 100
        ci2_high <- res$conf_int_grp2[2] * 100

        plot_df <- data.frame(
          Groupe = c(grp1, grp2),
          Proportion = c(p1_pct, p2_pct),
          ymin = c(ci1_low, ci2_low),
          ymax = c(ci1_high, ci2_high),
          stringsAsFactors = FALSE
        )

        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Groupe, y = Proportion, fill = Groupe)) +
          ggplot2::geom_col(width = 0.45, alpha = 0.85) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.15, color = "#111827", linewidth = 0.8) +
          ggplot2::scale_fill_manual(values = c("#2563EB", "#059669")) +
          ggplot2::scale_y_continuous(limits = c(0, min(100, max(100, max(ci1_high, ci2_high) + 10))), labels = function(x) paste0(x, "%")) +
          ggplot2::labs(
            title = paste0("Comparaison des proportions de '", res$success_modality, "'"),
            x = res$var_group,
            y = "Proportion (%)"
          ) +
          ggplot2::theme_minimal(base_family = "IBM Plex Sans") +
          ggplot2::theme(
            text = ggplot2::element_text(family = "sans"),
            plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#111827"),
            axis.title = ggplot2::element_text(size = 11, color = "#374151"),
            axis.text = ggplot2::element_text(size = 10, color = "#4B5563"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.5),
            legend.position = "none"
          )

        tryCatch(plotly::layout(plotly::ggplotly(p), font = list(family = "IBM Plex Sans")), error = function(e) plotly::layout(plotly::plotly_build(p), font = list(family = "IBM Plex Sans")))
      }
    })

    output$prop_pedagogy_ui <- shiny::renderUI({
      shiny::req(prop_state$calculated, prop_state$result)
      res <- prop_state$result

      if (prop_state$analysis_type == "one_sample") {
        h0_txt <- paste0("H0 : La vraie proportion d'\u00e9v\u00e9nements dans la population est \u00e9gale \u00e0 ", res$p0, " (p = ", res$p0, ").")
        h1_txt <- switch(
          prop_state$alternative,
          "two.sided" = paste0("H1 (bilat\u00e9rale) : La vraie proportion est diff\u00e9rente de ", res$p0, " (p \u2260 ", res$p0, ")."),
          "less" = paste0("H1 (unilat\u00e9rale \u00e0 gauche) : La vraie proportion est strictement inf\u00e9rieure \u00e0 ", res$p0, " (p < ", res$p0, ")."),
          "greater" = paste0("H1 (unilat\u00e9rale \u00e0 droite) : La vraie proportion est strictement sup\u00e9rieure \u00e0 ", res$p0, " (p > ", res$p0, ").")
        )

        n_p0 <- res$n * res$p0
        n_q0 <- res$n * (1 - res$p0)
        cond_ok <- (n_p0 >= 5) && (n_q0 >= 5)

        shiny::div(
          class = "space-y-3",
          shiny::div(
            class = "p-3 rounded bg-light border",
            shiny::tags$h6(class = "fw-bold text-dark mb-2", "1. Hypoth\u00e8ses statistiques :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 small",
              shiny::tags$li(shiny::tags$strong("Hypoth\u00e8se nulle : "), h0_txt),
              shiny::tags$li(shiny::tags$strong("Hypoth\u00e8se alternative : "), h1_txt)
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border",
            shiny::tags$h6(class = "fw-bold text-dark mb-2", "2. Conditions d'application m\u00e9thodologiques :"),
            shiny::tags$p(class = "small text-muted mb-2", "Pour la validit\u00e9 de l'approximation normale (prop.test), les effectifs th\u00e9oriques attendus sous H0 doivent \u00eatre au moins \u00e9gaux \u00e0 5 :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 small",
              shiny::tags$li(paste0("n \u00d7 p0 = ", round(n_p0, 2), if (n_p0 >= 5) " \u2265 5 (valid\u00e9)" else " < 5 (insuffisant)")),
              shiny::tags$li(paste0("n \u00d7 (1 - p0) = ", round(n_q0, 2), if (n_q0 >= 5) " \u2265 5 (valid\u00e9)" else " < 5 (insuffisant)")),
              shiny::tags$li(
                if (cond_ok) {
                  shiny::tags$span(class = "text-success fw-semibold", "\u2714 Les conditions d'application asymptotiques sont satisfaites.")
                } else {
                  shiny::tags$span(class = "text-danger fw-semibold", "\u26a0 Les conditions d'approximation ne sont pas remplies : l'utilisation du test exact binomial (binom.test) est fortement conseill\u00e9e.")
                }
              )
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border",
            shiny::tags$h6(class = "fw-bold text-dark mb-2", "3. Guide d'interpr\u00e9tation :"),
            shiny::tags$p(class = "small text-secondary mb-0",
              "Le test de proportion examine si l'\u00e9cart constat\u00e9 entre la fr\u00e9quence observ\u00e9e dans vos donn\u00e9es et la valeur de r\u00e9f\u00e9rence peut raisonnablement \u00eatre attribu\u00e9 aux seules fluctuations d'\u00e9chantillonnage. Lorsque la p-value est inf\u00e9rieure \u00e0 votre seuil alpha (ex: 0.05), on rejette l'hypoth\u00e8se nulle en faveur de l'hypoth\u00e8se alternative."
            )
          )
        )
      } else {
        # 2 groupes
        h0_txt <- "H0 : Les deux groupes ont la m\u00eame proportion sous-jacente dans la population (p1 = p2)."
        h1_txt <- switch(
          prop_state$alternative,
          "two.sided" = "H1 (bilat\u00e9rale) : Les deux proportions sont diff\u00e9rentes (p1 \u2260 p2).",
          "less" = paste0("H1 (unilat\u00e9rale \u00e0 gauche) : La proportion dans '", res$group_levels[1], "' est strictement inf\u00e9rieure \u00e0 celle de '", res$group_levels[2], "' (p1 < p2)."),
          "greater" = paste0("H1 (unilat\u00e9rale \u00e0 droite) : La proportion dans '", res$group_levels[1], "' est strictement sup\u00e9rieure \u00e0 celle de '", res$group_levels[2], "' (p1 > p2).")
        )

        shiny::div(
          class = "space-y-3",
          shiny::div(
            class = "p-3 rounded bg-light border",
            shiny::tags$h6(class = "fw-bold text-dark mb-2", "1. Hypoth\u00e8ses statistiques formul\u00e9es :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 small",
              shiny::tags$li(shiny::tags$strong("Hypoth\u00e8se nulle : "), h0_txt),
              shiny::tags$li(shiny::tags$strong("Hypoth\u00e8se alternative : "), h1_txt)
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border",
            shiny::tags$h6(class = "fw-bold text-dark mb-2", "2. Choix de la m\u00e9thode & Conditions :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 small",
              shiny::tags$li(shiny::tags$strong("Test asymptotique (prop.test) : "), "Adapt\u00e9 aux effectifs mod\u00e9r\u00e9s \u00e0 grands. Il applique par d\u00e9faut la correction de continuit\u00e9 de Yates pour am\u00e9liorer la pr\u00e9cision."),
              shiny::tags$li(shiny::tags$strong("Test exact de Fisher (fisher.test) : "), "Pr\u00e9f\u00e9rable lorsque l'un des effectifs observ\u00e9s ou th\u00e9oriques dans le tableau 2x2 est inf\u00e9rieur \u00e0 5.")
            )
          ),
          shiny::div(
            class = "p-3 rounded bg-light border",
            shiny::tags$h6(class = "fw-bold text-dark mb-2", "3. Mesures d'association \u00e9pid\u00e9miologique :"),
            shiny::tags$ul(
              class = "mb-0 ps-3 small",
              shiny::tags$li(shiny::tags$strong("Diff\u00e9rence de risques : "), "Diff\u00e9rence brute p1 - p2, mesur\u00e9e en points de pourcentage."),
              shiny::tags$li(shiny::tags$strong("Risque Relatif (RR) : "), "Rapport p1 / p2. Un RR > 1 indique un exc\u00e8s de risque dans le premier groupe par rapport au second."),
              shiny::tags$li(shiny::tags$strong("Odds Ratio (OR) : "), "Rapport des cotes de succ\u00e8s. Mesure cl\u00e9 en \u00e9tudes cas-t\u00e9moins.")
            )
          )
        )
      }
    })

    # =========================================================================
    # 5. TESTS DE CORR\u00c9LATION (PEARSON, SPEARMAN, KENDALL)
    # =========================================================================
    run_cor_analysis <- function(method_choice, var_x, var_y, alt, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_x) || is.null(var_y)) {
        shiny::showNotification("Veuillez choisir 2 variables num\u00e9riques.", type = "warning")
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
        es <- ramses_compute_effect_size(paste0("cor_", cor_state$method), stat_result = res, y1 = df[[cor_state$var_x]], y2 = df[[cor_state$var_y]])
        pwr <- ramses_compute_power(paste0("cor_", cor_state$method), stat_result = res, y1 = df[[cor_state$var_x]], y2 = df[[cor_state$var_y]], alpha = cor_state$alpha, alternative = cor_state$alternative)
        code_entry <- paste0(
          "# Test de correlation bivariee (", cor_state$method, ")\n",
          "cor.test(", ramses_code_column(ds_name, cor_state$var_x), ", ", ramses_code_column(ds_name, cor_state$var_y),
          ", method = ", ramses_code_string(cor_state$method), ", alternative = ", ramses_code_string(cor_state$alternative), ")"
        )
      }, error = function(e) {
        err <- e$message
      })

      cor_state$result <- res
      cor_state$effect_size <- es
      cor_state$power <- pwr
      cor_state$error <- err
      cor_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Corr\u00e9lation : ", cor_state$method, " (", cor_state$var_x, " & ", cor_state$var_y, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Test de corr\u00e9lation calcul\u00e9 !", type = "message")
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
            shiny::tags$span(style = "font-weight: 600;", "Param\u00e8tres : Test de Corr\u00e9lation")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_cor"), "Ex\u00e9cuter & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("cor_method_choice"),
              label = "Coefficient de corr\u00e9lation :",
              choices = c(
                "Pearson (param\u00e9trique - relation lin\u00e9aire)" = "pearson",
                "Spearman (non-param\u00e9trique - rangs)" = "spearman",
                "Kendall (tau - concordance des paires)" = "kendall"
              ),
              selected = cor_state$method
            ),
            shiny::selectInput(
              inputId = ns("cor_var_x"),
              label = "Premi\u00e8re variable (X) :",
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
              label = "Hypoth\u00e8se alternative :",
              choices = c(
                "Bilat\u00e9rale (corr\u00e9lation != 0)" = "two.sided",
                "Unilat\u00e9rale positive (r > 0)" = "greater",
                "Unilat\u00e9rale n\u00e9gative (r < 0)" = "less"
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
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ex\u00e9cuter' pour calculer le test de corr\u00e9lation."))
      }
      if (!is.null(cor_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur : ", cor_state$error)))
      }
      p_val <- cor_state$result$p.value
      r_val <- unname(cor_state$result$estimate)
      sig <- p_val < cor_state$alpha
      shiny::div(
        class = paste0("alert py-2 px-3 small d-flex justify-content-between align-items-center ", if (sig) "alert-success" else "alert-info"),
        shiny::tags$span(if (sig) paste0("Corr\u00e9lation significative (r = ", round(r_val, 3), ")") else "Aucune corr\u00e9lation statistiquement significative"),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", paste0("p = ", format.pval(p_val, digits = 3)))
      )
    })

    output$cor_results_ui <- shiny::renderUI({
      shiny::req(cor_state$calculated)
      if (!is.null(cor_state$error)) {
        return(shiny::p(class = "text-danger small", cor_state$error))
      }
      res <- cor_state$result
      p_val <- res$p.value
      r_val <- unname(res$estimate)
      r_name <- names(res$estimate)
      sig <- p_val < cor_state$alpha
      es <- cor_state$effect_size
      pwr <- cor_state$power

      decision_text <- if (sig) {
        sens <- if (r_val > 0) "positive (les deux variables varient dans le m\u00eame sens)" else "n\u00e9gative (les variables varient en sens inverse)"
        force <- if (abs(r_val) > 0.7) "forte" else if (abs(r_val) > 0.4) "mod\u00e9r\u00e9e" else "faible"
        paste0("Au risque alpha = ", cor_state$alpha, ", il existe une corr\u00e9lation ", force, " et ", sens, " statistiquement significative entre '", cor_state$var_x, "' et '", cor_state$var_y, "' (", r_name, " = ", round(r_val, 3), ", p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle de non-corr\u00e9lation est rejet\u00e9e.")
      } else {
        paste0("Au risque alpha = ", cor_state$alpha, ", le coefficient de corr\u00e9lation observ\u00e9 (", r_name, " = ", round(r_val, 3), ") n'est pas statistiquement significatif (p = ", format.pval(p_val, digits = 4, eps = 0.0001), "). L'hypoth\u00e8se nulle est conserv\u00e9e.")
      }

      tiles_cor <- list(
        list(label = paste0("Coefficient (", r_name, ")"), value = as.character(round(r_val, 3)), subtext = res$method, status = "primary"),
        list(label = "Statistique", value = as.character(round(unname(res$statistic), 3)), subtext = if (!is.null(res$parameter)) paste0("ddl = ", round(res$parameter, 1)) else names(res$statistic)),
        list(label = "p-value", value = format.pval(p_val, digits = 4, eps = 0.0001), subtext = if (sig) "Rejet H0" else "Conservation H0", status = if (sig) "success" else "dark"),
        list(label = "IC de corr\u00e9lation", value = if (!is.null(res$conf.int)) paste0("[", round(res$conf.int[1], 2), " ; ", round(res$conf.int[2], 2), "]") else "\u2014", subtext = paste0("Niveau ", round((1 - cor_state$alpha)*100), " %")),
        list(label = "Puissance", value = if (!is.null(pwr)) pwr$percentage else "\u2014", subtext = if (!is.null(pwr)) pwr$magnitude else NULL),
        list(label = "D\u00e9cision", value = if (sig) "Significatif" else "Non significatif", status = if (sig) "success" else "dark")
      )

      shiny::tagList(
        ramses_result_tiles(tiles_cor, title = "Indicateurs cl\u00e9s de la corr\u00e9lation"),
        shiny::tags$div(
          class = "table-responsive",
          shiny::tags$table(
            class = "table table-sm table-bordered text-center align-middle mb-3",
            shiny::tags$thead(
              class = "table-light",
              shiny::tags$tr(
                shiny::tags$th("M\u00e9thode"),
                shiny::tags$th("Coefficient"),
                shiny::tags$th("Statistique (t/z/S)"),
                shiny::tags$th("p-value"),
                shiny::tags$th("Taille d'effet"),
                shiny::tags$th("Puissance"),
                shiny::tags$th("Conclusion")
              )
            ),
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-start fw-medium", res$method),
                shiny::tags$td(class = "fw-bold text-primary", paste0(r_name, " = ", round(r_val, 3))),
                shiny::tags$td(round(unname(res$statistic), 3)),
                shiny::tags$td(class = "fw-bold", format.pval(p_val, digits = 4, eps = 0.0001)),
                shiny::tags$td(class = "small", if (!is.null(es)) paste0(es$symbol, " = ", es$formatted_value, " (", es$magnitude, ")") else "\u2014"),
                shiny::tags$td(class = "small", if (!is.null(pwr)) paste0(pwr$percentage, " (", pwr$magnitude, ")") else "\u2014"),
                shiny::tags$td(
                  shiny::tags$span(
                    class = paste0("badge ", if (sig) "bg-success" else "bg-secondary"),
                    if (sig) "Significatif" else "Non significatif"
                  )
                )
              )
            )
          )
        ),
        if (!is.null(res$conf.int)) {
          shiny::div(
            class = "small text-muted mb-2",
            paste0("Intervalle de confiance \u00e0 ", round((1 - cor_state$alpha)*100), "% : [",
                   round(res$conf.int[1], 3), " ; ", round(res$conf.int[2], 3), "]")
          )
        },
        shiny::div(
          class = "p-3 rounded bg-light border text-secondary small mb-3",
          shiny::tags$div(class = "fw-bold text-dark mb-1", "Synth\u00e8se p\u00e9dagogique :"),
          shiny::tags$ul(
            class = "mb-0 ps-3 space-y-1",
            shiny::tags$li(shiny::tags$strong("D\u00e9cision statistique : "), decision_text),
            if (!is.null(es)) shiny::tags$li(shiny::tags$strong(paste0("Taille d'effet (", es$name, ") : ")), es$description),
            if (!is.null(pwr)) shiny::tags$li(shiny::tags$strong("Puissance statistique : "), paste0("La puissance a posteriori calcul\u00e9e est de ", pwr$percentage, " (", pwr$magnitude, ")."))
          )
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher la sortie console R"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(utils::capture.output(print(res)), collapse = "\n"))
        )
      )
    })

    output$cor_plot <- plotly::renderPlotly({
      df <- data_holder$df
      shiny::req(df, cor_state$var_x, cor_state$var_y)
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
          name = "Droite de r\u00e9gression",
          line = list(color = "#ef4444", width = 2)
        ) %>%
        plotly::layout(font = list(family = "IBM Plex Sans"), 
          title = list(text = paste0("Corr\u00e9lation : ", cor_state$var_x, " vs ", cor_state$var_y), font = list(size = 13)),
          xaxis = list(title = cor_state$var_x),
          yaxis = list(title = cor_state$var_y)
        )
    })

    output$cor_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_cor(cor_state, data_holder$df)
    })

    # =========================================================================
    # 6. MOD\u00c9LISATION (R\u00c9GRESSION LIN\u00c9AIRE & LOGISTIQUE)
    # =========================================================================
    run_reg_analysis <- function(model_choice, var_y, vars_x, alpha_val) {
      df <- data_holder$df
      if (is.null(df) || is.null(var_y) || length(vars_x) == 0) {
        shiny::showNotification("Veuillez s\u00e9lectionner la variable d\u00e9pendante Y et au moins une variable explicative X.", type = "warning")
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
        fml <- ramses_formula(response = reg_state$var_y, terms = reg_state$vars_x)
        fml_code <- ramses_formula_code(response = reg_state$var_y, terms = reg_state$vars_x)

        if (reg_state$model_type == "linear") {
          fit <- stats::lm(fml, data = df)
          code_entry <- paste0(
            "# Regression lineaire (lm)\n",
            "mod_lm <- lm(", fml_code, ", data = ", ramses_code_symbol(ds_name), ")\n",
            "summary(mod_lm)\n",
            "confint(mod_lm)\n",
            "rmse_val <- sqrt(mean(residuals(mod_lm)^2))\n",
            "cat('RMSE =', round(rmse_val, 4), '\\n')"
          )
        } else {
          sub_df <- df
          has_filtering <- FALSE
          target_mods <- NULL
          if (is.factor(sub_df[[reg_state$var_y]]) || is.character(sub_df[[reg_state$var_y]])) {
            levs <- unique(as.character(sub_df[[reg_state$var_y]]))
            levs <- levs[!is.na(levs)]
            if (length(levs) > 2) {
              has_filtering <- TRUE
              target_mods <- levs[1:2]
              sub_df <- sub_df[sub_df[[reg_state$var_y]] %in% target_mods, , drop = FALSE]
            }
            sub_df[[reg_state$var_y]] <- as.factor(sub_df[[reg_state$var_y]])
          }
          fit <- stats::glm(fml, data = sub_df, family = stats::binomial(link = "logit"))

          if (has_filtering) {
            mods_code <- ramses_code_string(target_mods)
            code_entry <- paste0(
              "# Sous-ensemble filtre sur les 2 modalites retenues pour la regression logistique\n",
              "data_sub <- ", ramses_code_symbol(ds_name), "[", ramses_code_column(ds_name, reg_state$var_y), " %in% ", mods_code, ", , drop = FALSE]\n",
              ramses_code_column("data_sub", reg_state$var_y), " <- as.factor(", ramses_code_column("data_sub", reg_state$var_y), ")\n",
              "# Regression logistique binaire (glm binomial)\n",
              "mod_glm <- glm(", fml_code, ", data = data_sub, family = binomial(link = 'logit'))\n",
              "summary(mod_glm)"
            )
          } else if (is.character(df[[reg_state$var_y]])) {
            code_entry <- paste0(
              "# Conversion de la variable cible binaire en facteur\n",
              "data_sub <- ", ramses_code_symbol(ds_name), "\n",
              ramses_code_column("data_sub", reg_state$var_y), " <- as.factor(", ramses_code_column("data_sub", reg_state$var_y), ")\n",
              "# Regression logistique binaire (glm binomial)\n",
              "mod_glm <- glm(", fml_code, ", data = data_sub, family = binomial(link = 'logit'))\n",
              "summary(mod_glm)"
            )
          } else {
            code_entry <- paste0(
              "# Regression logistique binaire (glm binomial)\n",
              "mod_glm <- glm(", fml_code, ", data = ", ramses_code_symbol(ds_name), ", family = binomial(link = 'logit'))\n",
              "summary(mod_glm)"
            )
          }
        }
      }, error = function(e) {
        err <- e$message
      })

      reg_state$model <- fit
      reg_state$error <- err
      reg_state$calculated <- TRUE

      if (is.null(err) && nzchar(code_entry)) {
        append_to_rmd(
          title = paste0("Mod\u00e9lisation : ", reg_state$model_type, " (", reg_state$var_y, ")"),
          code = code_entry
        )
      }
      shiny::showNotification("Mod\u00e8le ajust\u00e9 et consign\u00e9 avec succ\u00e8s !", type = "message")
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
            shiny::tags$span(style = "font-weight: 600;", "Param\u00e8tres : Mod\u00e9lisation Statistique")
          ),
          size = "m",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("btn_confirm_reg"), "Ajuster le mod\u00e8le & Journaliser", class = "btn-dark")
          ),
          shiny::div(
            class = "space-y-3",
            shiny::selectInput(
              inputId = ns("reg_model_choice"),
              label = "Type de r\u00e9gression :",
              choices = c(
                "R\u00e9gression Lin\u00e9aire Simple / Multiple (lm)" = "linear",
                "R\u00e9gression Logistique Binaire (glm binomial)" = "logistic"
              ),
              selected = reg_state$model_type
            ),
            shiny::selectInput(
              inputId = ns("reg_var_y"),
              label = "Variable D\u00e9pendante (Y) :",
              choices = if (reg_state$model_type == "linear") num_cols else all_cols,
              selected = reg_state$var_y
            ),
            shiny::selectizeInput(
              inputId = ns("reg_vars_x"),
              label = "Variables Explicatives / Pr\u00e9dictives (X) :",
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
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ajuster le mod\u00e8le' pour lancer l'estimation."))
      }
      if (!is.null(reg_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur d'ajustement : ", reg_state$error)))
      }
      shiny::div(
        class = "alert alert-success py-2 px-3 small d-flex justify-content-between align-items-center",
        shiny::tags$span("Mod\u00e8le ajust\u00e9 avec succ\u00e8s : ", paste(reg_state$var_y, "~", paste(reg_state$vars_x, collapse = " + "))),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", if (reg_state$model_type == "linear") "R\u00e9gression Lin\u00e9aire" else "R\u00e9gression Logistique")
      )
    })

    output$reg_results_ui <- shiny::renderUI({
      shiny::req(reg_state$calculated)
      if (!is.null(reg_state$error)) {
        return(shiny::p(class = "text-danger small", reg_state$error))
      }
      if (reg_state$model_type == "linear") {
        return(ramses_render_lm_results(reg_state))
      }
      smry <- summary(reg_state$model)
      coef_table <- as.data.frame(smry$coefficients)

      shiny::tagList(
        shiny::tags$h6(class = "fw-bold text-dark mb-2", "Tableau des coefficients du mod\u00e8le :"),
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
              shiny::tags$th("Significativit\u00e9")
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
            shiny::tags$strong("Qualit\u00e9 globale : "),
            paste0("R\u00b2 = ", round(smry$r.squared, 3), " | R\u00b2 ajust\u00e9 = ", round(smry$adj.r.squared, 3),
                   " | Erreur r\u00e9siduelle type = ", round(smry$sigma, 3))
          )
        } else {
          shiny::div(
            class = "p-2 rounded bg-light border text-secondary small mb-3",
            shiny::tags$strong("Qualit\u00e9 globale : "),
            paste0("D\u00e9viance r\u00e9siduelle = ", round(smry$deviance, 2), " (sur ", smry$df.residual, " ddl) | AIC = ", round(smry$aic, 1))
          )
        },
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small cursor-pointer", "Afficher le r\u00e9sum\u00e9 R complet"),
          shiny::tags$pre(class = "p-2 bg-light text-dark border rounded small mt-1 font-monospace", paste(utils::capture.output(print(smry)), collapse = "\n"))
        )
      )
    })

    output$reg_plot <- plotly::renderPlotly({
      shiny::req(reg_state$calculated, reg_state$model)
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
            name = "R\u00e9sidus"
          ) %>%
          plotly::add_lines(
            x = range(fitted_vals),
            y = c(0, 0),
            line = list(color = "#ef4444", dash = "dash"),
            name = "Ligne 0"
          ) %>%
          plotly::layout(font = list(family = "IBM Plex Sans"), 
            title = list(text = "R\u00e9sidus vs Valeurs ajust\u00e9es", font = list(size = 13)),
            xaxis = list(title = "Valeurs ajust\u00e9es (Fitted values)"),
            yaxis = list(title = "R\u00e9sidus")
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
            name = "R\u00e9sidus observ\u00e9s"
          ) %>%
          plotly::add_lines(
            x = range(theo),
            y = range(theo),
            line = list(color = "#ef4444", dash = "dash"),
            name = "Normale th\u00e9orique"
          ) %>%
          plotly::layout(font = list(family = "IBM Plex Sans"), 
            title = list(text = "Normal Q-Q Plot des R\u00e9sidus", font = list(size = 13)),
            xaxis = list(title = "Quantiles th\u00e9oriques"),
            yaxis = list(title = "R\u00e9sidus")
          )
      }
    })

    output$reg_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_reg(reg_state, data_holder$df)
    })

  })
}
