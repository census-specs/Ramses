#' Sous-interface : Variables quantitatives
#'
#' @param id Identifiant de module Shiny
#' @return Interface utilisateur du sous-module (tagList)
#' @noRd
mod_descriptives_quanti_ui <- function(id) {
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
            shiny::tags$strong("Variables Quantitatives")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_quanti_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectizeInput(
            inputId = ns("quanti_vars_direct"),
            label = "Variable(s) quantitative(s) :",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("quanti_group_direct"),
            label = "Groupe (optionnel) :",
            choices = c("Aucun (analyse globale)" = "")
          ),
          shiny::checkboxGroupInput(
            inputId = ns("quanti_stats_direct"),
            label = "Indicateurs statistiques :",
            choices = c(
              "Effectif (N)" = "n",
              "Valeurs manquantes (NA)" = "na",
              "Moyenne" = "mean",
              "Médiane" = "median",
              "Variance" = "var",
              "Écart-type (SD)" = "sd",
              "CV (%)" = "cv",
              "Minimum" = "min",
              "Maximum" = "max",
              "IQR" = "iqr",
              "Asymétrie" = "skewness",
              "Aplatissement" = "kurtosis"
            ),
            selected = c("n", "na", "mean", "median", "sd", "min", "max", "iqr"),
            inline = TRUE
          ),
          shiny::radioButtons(
            inputId = ns("quanti_plot_type_direct"),
            label = "Graphique associé :",
            choices = c("Boîte à moustaches (Boxplot)" = "box", "Histogramme" = "hist"),
            selected = "box",
            inline = TRUE
          ),
          shiny::actionButton(
            inputId = ns("btn_run_quanti"),
            label = "Calculer & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("quanti_status_badge")),
      bslib::navset_card_tab(
        id = ns("quanti_results_tabs"),
        bslib::nav_panel(
          title = "Tableau des statistiques",
          bslib::card_body(
            padding = 0,
            DT::dataTableOutput(ns("table_quanti"))
          )
        ),
        bslib::nav_panel(
          title = "Graphique de distribution",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("plot_quanti"), height = "440px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Variables qualitatives
#'
#' @noRd
mod_descriptives_quali_ui <- function(id) {
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
            shiny::tags$strong("Variables Qualitatives")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_quali_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("quali_var_direct"),
            label = "Variable qualitative :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("quali_group_direct"),
            label = "Variable de groupe (optionnelle) :",
            choices = c("Aucune (distribution simple)" = "")
          ),
          shiny::checkboxGroupInput(
            inputId = ns("quali_options_direct"),
            label = "Options d'affichage :",
            choices = c(
              "Effectifs (N)" = "n",
              "Pourcentages (%)" = "pct",
              "% cumulés" = "cum_pct"
            ),
            selected = c("n", "pct", "cum_pct"),
            inline = TRUE
          ),
          shiny::radioButtons(
            inputId = ns("quali_plot_type_direct"),
            label = "Graphique associé :",
            choices = c("Diagramme en barres" = "bar", "Diagramme circulaire (Camembert)" = "pie"),
            selected = "bar",
            inline = TRUE
          ),
          shiny::actionButton(
            inputId = ns("btn_run_quali"),
            label = "Calculer & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("quali_status_badge")),
      bslib::navset_card_tab(
        id = ns("quali_results_tabs"),
        bslib::nav_panel(
          title = "Table des effectifs & pourcentages",
          bslib::card_body(
            padding = 0,
            DT::dataTableOutput(ns("table_quali"))
          )
        ),
        bslib::nav_panel(
          title = "Diagramme de distribution",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("plot_quali"), height = "440px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Tableaux croisés
#'
#' @noRd
mod_descriptives_cross_ui <- function(id) {
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
            shiny::tags$strong("Tableau Croisé (Contingence)")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_crosstab_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("crosstab_row_direct"),
            label = "Variable en Ligne (Row) :",
            choices = NULL
          ),
          shiny::selectInput(
            inputId = ns("crosstab_col_direct"),
            label = "Variable en Colonne (Column) :",
            choices = NULL
          ),
          shiny::radioButtons(
            inputId = ns("crosstab_display_direct"),
            label = "Métriques affichées dans la table :",
            choices = c(
              "Effectifs observés (N)" = "count",
              "Pourcentages ligne (% Ligne)" = "row_pct",
              "Pourcentages colonne (% Col)" = "col_pct",
              "Pourcentages totaux (% Total)" = "total_pct"
            ),
            selected = "count"
          ),
          shiny::checkboxInput(
            inputId = ns("crosstab_totals_direct"),
            label = "Afficher les totaux marginaux (Ligne & Colonne)",
            value = TRUE
          ),
          shiny::actionButton(
            inputId = ns("btn_run_crosstab"),
            label = "Calculer & Journaliser",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("crosstab_status_badge")),
      bslib::navset_card_tab(
        id = ns("crosstab_results_tabs"),
        bslib::nav_panel(
          title = "Table de contingence",
          bslib::card_body(
            padding = 0,
            DT::dataTableOutput(ns("table_crosstab"))
          )
        ),
        bslib::nav_panel(
          title = "Répartition bivariée",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("plot_crosstab"), height = "440px")
          )
        )
      )
    )
  )
}

#' Sous-interface : Matrice de corrélation
#'
#' @noRd
mod_descriptives_cor_ui <- function(id) {
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
            shiny::tags$strong("Matrice de Corrélation")
          ),
          shiny::actionButton(
            inputId = ns("btn_open_cor_modal"),
            label = "Modale",
            class = "btn-outline-secondary btn-sm py-0 px-2"
          )
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectizeInput(
            inputId = ns("cor_vars_direct"),
            label = "Variables numériques (>= 2) :",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::radioButtons(
            inputId = ns("cor_method_direct"),
            label = "Méthode de corrélation :",
            choices = c(
              "Pearson (linéaire)" = "pearson",
              "Spearman (rangs / monotone)" = "spearman"
            ),
            selected = "pearson",
            inline = TRUE
          ),
          shiny::checkboxInput(
            inputId = ns("cor_pvalues_direct"),
            label = "Calculer et afficher les p-values",
            value = TRUE
          ),
          shiny::actionButton(
            inputId = ns("btn_run_cor"),
            label = "Calculer & Journaliser",
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
          title = "Coefficients de corrélation",
          bslib::card_body(
            padding = 0,
            DT::dataTableOutput(ns("table_cor"))
          )
        ),
        bslib::nav_panel(
          title = "Heatmap de corrélation",
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("plot_cor"), height = "440px")
          )
        )
      )
    )
  )
}

#' @title Interface utilisateur pour le module de statistiques descriptives
#'
#' @description Construit l'interface du module de statistiques descriptives de Ramses,
#'   comprenant l'analyse univariée quantitative, l'analyse univariée qualitative,
#'   les tableaux de contingence croisés et les matrices de corrélation.
#'
#' @param id Identifiant de namespace Shiny.
#' @return Un objet tagList d'interface Shiny (\code{shiny.tag}).
#' @export
mod_descriptives_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    id = ns("tabs_descriptives"),
    title = shiny::div(
      class = "d-flex align-items-center gap-2",
      shiny::tags$span(style = "font-weight: 600;", "Analyses Descriptives")
    ),
    bslib::nav_panel(
      title = "Variables Quantitatives",
      mod_descriptives_quanti_ui(id)
    ),
    bslib::nav_panel(
      title = "Variables Qualitatives",
      mod_descriptives_quali_ui(id)
    ),
    bslib::nav_panel(
      title = "Tableau Croisé",
      mod_descriptives_cross_ui(id)
    ),
    bslib::nav_panel(
      title = "Matrice de Corrélation",
      mod_descriptives_cor_ui(id)
    )
  )
}

#' @title Logique serveur pour le module de statistiques descriptives
#'
#' @description Gère la logique interactive, les calculs de statistiques descriptives,
#'   le rendu des tableaux DT, la génération des graphiques Plotly et l'enregistrement
#'   reproductible dans le journal R Markdown.
#'
#' @param id Identifiant de namespace Shiny.
#' @param data_holder Environnement ou liste réactive contenant \code{df} et \code{name}.
#' @param append_to_rmd Fonction de callback pour enregistrer le code dans le journal Rmd.
#' @return Un module serveur Shiny.
#' @export
mod_descriptives_server <- function(id, data_holder, append_to_rmd) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers de détection des types de variables dans le dataset actif
    numeric_vars <- shiny::reactive({
      df <- data_holder$df
      if (!is.data.frame(df)) return(character(0))
      names(df)[vapply(df, is.numeric, logical(1))]
    })

    categorical_vars <- shiny::reactive({
      df <- data_holder$df
      if (!is.data.frame(df)) return(character(0))
      names(df)[vapply(df, function(x) is.factor(x) || is.character(x) || length(unique(x)) <= 15, logical(1))]
    })

    # Mise à jour automatique des sélecteurs directs (layout 2 colonnes)
    shiny::observe({
      num_cols <- numeric_vars()
      cat_cols <- categorical_vars()
      all_cols <- if (is.data.frame(data_holder$df)) names(data_holder$df) else character(0)
      
      shiny::updateSelectizeInput(session, "quanti_vars_direct", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      shiny::updateSelectInput(session, "quanti_group_direct", choices = c("Aucun (analyse globale)" = "", cat_cols))
      
      shiny::updateSelectInput(session, "quali_var_direct", choices = if (length(cat_cols) > 0) cat_cols else all_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else if (length(all_cols) > 0) all_cols[1] else NULL)
      shiny::updateSelectInput(session, "quali_group_direct", choices = c("Aucune (distribution simple)" = "", cat_cols))
      
      shiny::updateSelectInput(session, "crosstab_row_direct", choices = if (length(cat_cols) >= 2) cat_cols else all_cols, selected = if (length(cat_cols) >= 1) cat_cols[1] else if (length(all_cols) >= 1) all_cols[1] else NULL)
      shiny::updateSelectInput(session, "crosstab_col_direct", choices = if (length(cat_cols) >= 2) cat_cols else all_cols, selected = if (length(cat_cols) >= 2) cat_cols[2] else if (length(all_cols) >= 2) all_cols[2] else NULL)
      
      shiny::updateSelectizeInput(session, "cor_vars_direct", choices = num_cols, selected = if (length(num_cols) >= 2) num_cols[1:min(length(num_cols), 4)] else num_cols)
    })

    # Helper interne pour la génération reproductible du code R dplyr::summarise
    build_quanti_rmd_code <- function(target_df, vars, group_var, selected_stats) {
      if (is.null(selected_stats) || length(selected_stats) == 0) {
        selected_stats <- c("n", "mean", "sd")
      }
      group_clause <- if (nzchar(group_var)) paste0('group_by(', group_var, ') %>%\n  ') else ''

      summarise_terms <- character(0)
      for (v in vars) {
        if ("n" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (N)` = sum(!is.na(', v, '))'))
        }
        if ("na" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (NA)` = sum(is.na(', v, '))'))
        }
        if ("mean" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Moyenne)` = round(mean(', v, ', na.rm = TRUE), 3)'))
        }
        if ("median" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Médiane)` = round(median(', v, ', na.rm = TRUE), 3)'))
        }
        if ("var" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Variance)` = round(var(', v, ', na.rm = TRUE), 3)'))
        }
        if ("sd" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Écart-type)` = round(sd(', v, ', na.rm = TRUE), 3)'))
        }
        if ("cv" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (CV %)` = round((sd(', v, ', na.rm = TRUE) / mean(', v, ', na.rm = TRUE)) * 100, 2)'))
        }
        if ("min" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Min)` = round(min(', v, ', na.rm = TRUE), 3)'))
        }
        if ("max" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Max)` = round(max(', v, ', na.rm = TRUE), 3)'))
        }
        if ("iqr" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (IQR)` = round(IQR(', v, ', na.rm = TRUE), 3)'))
        }
        if ("skewness" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Asymétrie)` = round(sum((', v, ' - mean(', v, ', na.rm = TRUE))^3, na.rm = TRUE) / (sum(!is.na(', v, ')) * sd(', v, ', na.rm = TRUE)^3), 3)'))
        }
        if ("kurtosis" %in% selected_stats) {
          summarise_terms <- c(summarise_terms, paste0('  `', v, ' (Aplatissement)` = round(sum((', v, ' - mean(', v, ', na.rm = TRUE))^4, na.rm = TRUE) / (sum(!is.na(', v, ')) * sd(', v, ', na.rm = TRUE)^4) - 3, 3)'))
        }
      }

      paste0(
        "# Statistiques descriptives pour variables quantitatives\n",
        "library(dplyr)\n\n",
        "res_quanti <- ", target_df, " %>%\n  ",
        group_clause,
        "summarise(\n",
        paste(summarise_terms, collapse = ",\n"),
        "\n)\nprint(res_quanti)"
      )
    }

    # Observateur direct pour Quantitatif
    shiny::observeEvent(input$btn_run_quanti, {
      vars <- input$quanti_vars_direct
      if (length(vars) == 0) {
        num_cols <- numeric_vars()
        if (length(num_cols) > 0) vars <- num_cols[1]
      }
      if (length(vars) == 0) {
        shiny::showNotification("Veuillez sélectionner au moins une variable quantitative.", type = "warning")
        return()
      }
      quanti_state$vars <- vars
      quanti_state$group <- input$quanti_group_direct
      quanti_state$stats <- if (!is.null(input$quanti_stats_direct)) input$quanti_stats_direct else c("n", "na", "mean", "median", "sd", "min", "max", "iqr")
      quanti_state$plot_type <- input$quanti_plot_type_direct
      quanti_state$ready <- TRUE

      target_df <- data_holder$name
      rmd_code <- build_quanti_rmd_code(
        target_df = target_df,
        vars = quanti_state$vars,
        group_var = quanti_state$group,
        selected_stats = quanti_state$stats
      )
      append_to_rmd(title = paste0("Statistiques quantitatives (", paste(quanti_state$vars, collapse = ", "), ")"), code = rmd_code)
      shiny::showNotification("Résumé quantitatif calculé et journalisé !", type = "message")
    })

    # Observateur direct pour Qualitatif
    shiny::observeEvent(input$btn_run_quali, {
      v <- input$quali_var_direct
      if (is.null(v) || !nzchar(v)) {
        shiny::showNotification("Veuillez choisir une variable qualitative.", type = "warning")
        return()
      }
      quali_state$var <- v
      quali_state$ready <- TRUE
      
      target_df <- data_holder$name
      rmd_code <- paste0(
        "# Tableau de fréquences\n",
        sprintf("tab <- table(%s$%s)\n", target_df, v),
        "print(tab)\nprint(round(prop.table(tab)*100, 2))"
      )
      append_to_rmd(title = paste0("Fréquences pour '", v, "'"), code = rmd_code)
      shiny::showNotification("Table de fréquences calculée et journalisée !", type = "message")
    })

    # Observateur direct pour Tableau Croisé
    shiny::observeEvent(input$btn_run_crosstab, {
      r_v <- input$crosstab_row_direct
      c_v <- input$crosstab_col_direct
      if (is.null(r_v) || is.null(c_v) || r_v == c_v) {
        shiny::showNotification("Veuillez sélectionner deux variables distinctes pour le croisement.", type = "warning")
        return()
      }
      crosstab_state$row_var <- r_v
      crosstab_state$col_var <- c_v
      crosstab_state$display_type <- switch(input$crosstab_display_direct,
        "count" = "counts",
        "row_pct" = "row_pct",
        "col_pct" = "col_pct",
        "total_pct" = "tot_pct",
        "counts"
      )
      crosstab_state$ready <- TRUE
      
      target_df <- data_holder$name
      rmd_code <- paste0(
        "# Tableau croisé de contingence\n",
        sprintf("tab_croise <- table(%s$%s, %s$%s)\nprint(tab_croise)\n", target_df, r_v, target_df, c_v),
        "print(chisq.test(tab_croise))"
      )
      append_to_rmd(title = paste0("Tableau croisé : ", r_v, " × ", c_v), code = rmd_code)
      shiny::showNotification("Tableau croisé calculé et consigné !", type = "message")
    })

    # Observateur direct pour Corrélation
    shiny::observeEvent(input$btn_run_cor, {
      vars <- input$cor_vars_direct
      if (length(vars) < 2) {
        num_cols <- numeric_vars()
        if (length(num_cols) >= 2) vars <- num_cols[1:min(length(num_cols), 4)]
      }
      if (length(vars) < 2) {
        shiny::showNotification("Il faut au moins 2 variables numériques pour la corrélation.", type = "warning")
        return()
      }
      cor_state$vars <- vars
      cor_state$method <- if (!is.null(input$cor_method_direct)) input$cor_method_direct else "pearson"
      cor_state$ready <- TRUE
      
      target_df <- data_holder$name
      vars_str <- paste0('c("', paste(cor_state$vars, collapse = '", "'), '")')
      rmd_code <- paste0(
        "# Matrice de corrélation\n",
        sprintf("mat_cor <- cor(%s[, %s], method = '%s', use = 'pairwise.complete.obs')\nprint(round(mat_cor, 3))\n", target_df, vars_str, cor_state$method)
      )
      append_to_rmd(title = paste0("Matrice de corrélation (", toupper(cor_state$method), ")"), code = rmd_code)
      shiny::showNotification("Matrice de corrélation calculée !", type = "message")
    })

    # =========================================================================
    # 1. LOGIQUE : VARIABLES QUANTITATIVES
    # =========================================================================
    quanti_state <- shiny::reactiveValues(
      vars = NULL,
      group = "",
      stats = c("n", "na", "mean", "median", "sd", "min", "max", "iqr"),
      plot_type = "box",
      ready = FALSE
    )

    # Réactivité instantanée sur la sélection des métriques dans la barre latérale
    shiny::observeEvent(input$quanti_stats_direct, {
      quanti_state$stats <- input$quanti_stats_direct
    }, ignoreNULL = FALSE)

    # Initialisation automatique dès qu'un dataset est disponible
    shiny::observe({
      num_cols <- numeric_vars()
      if (length(num_cols) > 0 && !quanti_state$ready) {
        quanti_state$vars <- num_cols[1]
        cat_cols <- categorical_vars()
        quanti_state$group <- if (length(cat_cols) > 0) cat_cols[1] else ""
        quanti_state$ready <- TRUE
      }
    })

    shiny::observeEvent(input$btn_open_quanti_modal, {
      num_cols <- numeric_vars()
      cat_cols <- categorical_vars()

      if (length(num_cols) == 0) {
        shiny::showNotification("Aucune variable numérique détectée dans le jeu de données.", type = "warning")
        return()
      }

      current_selected <- if (!is.null(quanti_state$vars)) quanti_state$vars else num_cols[1]
      current_group <- if (!is.null(quanti_state$group)) quanti_state$group else ""

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::strong("Paramètres - Résumé Statistique Quantitatif")
          ),
          size = "l",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(
              inputId = ns("btn_confirm_quanti"),
              label = "Valider et Calculer",
              class = "btn-primary"
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::selectizeInput(
              inputId = ns("modal_quanti_vars"),
              label = shiny::strong("Variable(s) numérique(s) à analyser :"),
              choices = num_cols,
              selected = current_selected,
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::selectInput(
              inputId = ns("modal_quanti_group"),
              label = shiny::strong("Variable de groupement (optionnelle) :"),
              choices = c("Aucune (analyse globale)" = "", cat_cols),
              selected = current_group
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::strong("Indicateurs statistiques à calculer :"),
            shiny::checkboxGroupInput(
              inputId = ns("modal_quanti_stats"),
              label = NULL,
              choices = c(
                "Effectif (N)" = "n",
                "Valeurs manquantes (NA)" = "na",
                "Moyenne" = "mean",
                "Médiane" = "median",
                "Variance" = "var",
                "Écart-type (SD)" = "sd",
                "CV (%)" = "cv",
                "Minimum" = "min",
                "Maximum" = "max",
                "IQR" = "iqr",
                "Asymétrie" = "skewness",
                "Aplatissement" = "kurtosis"
              ),
              selected = if (!is.null(quanti_state$stats)) quanti_state$stats else c("n", "na", "mean", "median", "sd", "min", "max", "iqr"),
              inline = TRUE
            )
          ),

          shiny::div(
            class = "mb-2",
            shiny::radioButtons(
              inputId = ns("modal_quanti_plot_type"),
              label = shiny::strong("Graphique interactif Plotly associé :"),
              choices = c(
                "Boîte à moustaches (Boxplot)" = "box",
                "Histogramme de distribution" = "hist"
              ),
              selected = quanti_state$plot_type,
              inline = TRUE
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_quanti, {
      if (length(input$modal_quanti_vars) == 0) {
        shiny::showNotification("Veuillez sélectionner au moins une variable numérique.", type = "warning")
        return()
      }

      quanti_state$vars <- input$modal_quanti_vars
      quanti_state$group <- input$modal_quanti_group
      quanti_state$stats <- if (!is.null(input$modal_quanti_stats)) input$modal_quanti_stats else c("n", "na", "mean", "median", "sd", "min", "max", "iqr")
      quanti_state$plot_type <- input$modal_quanti_plot_type
      quanti_state$ready <- TRUE

      shiny::removeModal()

      # Mise à jour synchronisée des sélecteurs UI directs
      shiny::updateCheckboxGroupInput(session, "quanti_stats_direct", selected = quanti_state$stats)

      # Construction du code R reproductible pour rmd_log
      target_df <- data_holder$name
      rmd_code <- build_quanti_rmd_code(
        target_df = target_df,
        vars = quanti_state$vars,
        group_var = quanti_state$group,
        selected_stats = quanti_state$stats
      )

      append_to_rmd(
        title = paste0("Statistiques quantitatives (", paste(quanti_state$vars, collapse = ", "), ")"),
        code = rmd_code
      )

      shiny::showNotification("Résumé quantitatif calculé et journalisé avec succès !", type = "message")
    })

    output$quanti_status_badge <- shiny::renderUI({
      if (!quanti_state$ready || length(quanti_state$vars) == 0) return(NULL)
      group_txt <- if (nzchar(quanti_state$group)) paste0(" | Groupé par : ", quanti_state$group) else " | Global"
      shiny::div(
        class = "alert alert-info py-1 px-2 small mb-2 d-flex justify-content-between align-items-center",
        shiny::span(
          shiny::strong("Variables : "), paste(quanti_state$vars, collapse = ", "),
          group_txt
        ),
        shiny::tags$span(class = "badge bg-primary", paste0(length(quanti_state$vars), " variable(s)"))
      )
    })

    output$quanti_table_info <- shiny::renderUI({
      shiny::tags$span(class = "text-muted small", paste0("Dataset : ", data_holder$name))
    })

    output$table_quanti <- DT::renderDataTable({
      shiny::req(quanti_state$ready, quanti_state$vars, data_holder$df)
      df <- data_holder$df
      vars <- quanti_state$vars[quanti_state$vars %in% names(df)]
      if (length(vars) == 0) return(NULL)

      active_stats <- if (!is.null(input$quanti_stats_direct)) input$quanti_stats_direct else quanti_state$stats
      if (is.null(active_stats) || length(active_stats) == 0) active_stats <- character(0)

      has_group <- nzchar(quanti_state$group) && (quanti_state$group %in% names(df))

      if (!has_group && length(vars) == 1) {
        # Formatage vertical univarié à 2 colonnes (Statistique / Valeur) type SPSS
        v <- vars[1]
        sub_vec <- df[[v]]
        vals <- sub_vec[!is.na(sub_vec)]
        n_val <- length(vals)
        na_count <- sum(is.na(sub_vec))
        m_val <- if (n_val > 0) mean(vals) else NA_real_
        s_val <- if (n_val > 1) stats::sd(vals) else NA_real_
        var_val <- if (n_val > 1) stats::var(vals) else NA_real_
        med_val <- if (n_val > 0) stats::median(vals) else NA_real_
        iqr_val <- if (n_val > 0) stats::IQR(vals) else NA_real_
        min_val <- if (n_val > 0) min(vals) else NA_real_
        max_val <- if (n_val > 0) max(vals) else NA_real_
        cv_val <- if (n_val > 1 && !is.na(m_val) && m_val != 0 && !is.na(s_val)) round((s_val / m_val) * 100, 2) else NA_real_
        skew_val <- if (n_val >= 3 && !is.na(s_val) && s_val > 0) round(sum((vals - m_val)^3) / (n_val * s_val^3), 3) else NA_real_
        kurt_val <- if (n_val >= 4 && !is.na(s_val) && s_val > 0) round(sum((vals - m_val)^4) / (n_val * s_val^4) - 3, 3) else NA_real_

        stats_defs <- list(
          list(id = "n", label = "Effectif valide (N)", val = as.character(n_val)),
          list(id = "na", label = "Valeurs manquantes (NA)", val = as.character(na_count)),
          list(id = "mean", label = "Moyenne", val = if (!is.na(m_val)) sprintf("%.3f", m_val) else "NA"),
          list(id = "median", label = "Médiane", val = if (!is.na(med_val)) sprintf("%.3f", med_val) else "NA"),
          list(id = "var", label = "Variance", val = if (!is.na(var_val)) sprintf("%.3f", var_val) else "NA"),
          list(id = "sd", label = "Écart-type (SD)", val = if (!is.na(s_val)) sprintf("%.3f", s_val) else "NA"),
          list(id = "cv", label = "Coefficient de variation (CV %)", val = if (!is.na(cv_val)) paste0(sprintf("%.2f", cv_val), " %") else "NA"),
          list(id = "min", label = "Minimum", val = if (!is.na(min_val)) sprintf("%.3f", min_val) else "NA"),
          list(id = "max", label = "Maximum", val = if (!is.na(max_val)) sprintf("%.3f", max_val) else "NA"),
          list(id = "iqr", label = "Écart interquartile (IQR)", val = if (!is.na(iqr_val)) sprintf("%.3f", iqr_val) else "NA"),
          list(id = "skewness", label = "Asymétrie (Skewness)", val = if (!is.na(skew_val)) sprintf("%.3f", skew_val) else "NA"),
          list(id = "kurtosis", label = "Aplatissement (Kurtosis)", val = if (!is.na(kurt_val)) sprintf("%.3f", kurt_val) else "NA")
        )

        filtered_defs <- stats_defs[vapply(stats_defs, function(x) x$id %in% active_stats, logical(1))]

        res_df <- data.frame(
          Statistique = vapply(filtered_defs, function(x) x$label, character(1)),
          Valeur = vapply(filtered_defs, function(x) x$val, character(1)),
          stringsAsFactors = FALSE
        )

        DT::datatable(
          res_df,
          options = list(dom = "t", scrollX = TRUE, pageLength = 15),
          rownames = FALSE,
          class = "compact stripe hover border"
        )
      } else {
        # Tableau multivarié ou groupé avec indicateurs filtrés
        rows_list <- list()
        if (has_group) {
          grp_col <- df[[quanti_state$group]]
          modalities <- levels(as.factor(grp_col))

          for (v in vars) {
            for (m in modalities) {
              sub_vec <- df[[v]][grp_col == m & !is.na(grp_col)]
              vals <- sub_vec[!is.na(sub_vec)]
              n_val <- length(vals)
              m_val <- if (n_val > 0) mean(vals) else NA_real_
              s_val <- if (n_val > 1) stats::sd(vals) else NA_real_
              var_val <- if (n_val > 1) stats::var(vals) else NA_real_
              med_val <- if (n_val > 0) stats::median(vals) else NA_real_
              iqr_val <- if (n_val > 0) stats::IQR(vals) else NA_real_
              min_val <- if (n_val > 0) min(vals) else NA_real_
              max_val <- if (n_val > 0) max(vals) else NA_real_
              cv_val <- if (n_val > 1 && !is.na(m_val) && m_val != 0 && !is.na(s_val)) round((s_val / m_val) * 100, 2) else NA_real_
              skew_val <- if (n_val >= 3 && !is.na(s_val) && s_val > 0) round(sum((vals - m_val)^3) / (n_val * s_val^3), 3) else NA_real_
              kurt_val <- if (n_val >= 4 && !is.na(s_val) && s_val > 0) round(sum((vals - m_val)^4) / (n_val * s_val^4) - 3, 3) else NA_real_

              row_data <- list(
                "Variable" = v,
                "Groupe" = as.character(m),
                "N" = n_val,
                "NA" = sum(is.na(sub_vec)),
                "Moyenne" = if (!is.na(m_val)) round(m_val, 3) else NA,
                "Médiane" = if (!is.na(med_val)) round(med_val, 3) else NA,
                "Variance" = if (!is.na(var_val)) round(var_val, 3) else NA,
                "Écart-type" = if (!is.na(s_val)) round(s_val, 3) else NA,
                "CV (%)" = cv_val,
                "Min" = if (!is.na(min_val)) round(min_val, 3) else NA,
                "Max" = if (!is.na(max_val)) round(max_val, 3) else NA,
                "IQR" = if (!is.na(iqr_val)) round(iqr_val, 3) else NA,
                "Asymétrie" = skew_val,
                "Aplatissement" = kurt_val
              )
              rows_list[[length(rows_list) + 1]] <- row_data
            }
          }
        } else {
          for (v in vars) {
            sub_vec <- df[[v]]
            vals <- sub_vec[!is.na(sub_vec)]
            n_val <- length(vals)
            m_val <- if (n_val > 0) mean(vals) else NA_real_
            s_val <- if (n_val > 1) stats::sd(vals) else NA_real_
            var_val <- if (n_val > 1) stats::var(vals) else NA_real_
            med_val <- if (n_val > 0) stats::median(vals) else NA_real_
            iqr_val <- if (n_val > 0) stats::IQR(vals) else NA_real_
            min_val <- if (n_val > 0) min(vals) else NA_real_
            max_val <- if (n_val > 0) max(vals) else NA_real_
            cv_val <- if (n_val > 1 && !is.na(m_val) && m_val != 0 && !is.na(s_val)) round((s_val / m_val) * 100, 2) else NA_real_
            skew_val <- if (n_val >= 3 && !is.na(s_val) && s_val > 0) round(sum((vals - m_val)^3) / (n_val * s_val^3), 3) else NA_real_
            kurt_val <- if (n_val >= 4 && !is.na(s_val) && s_val > 0) round(sum((vals - m_val)^4) / (n_val * s_val^4) - 3, 3) else NA_real_

            row_data <- list(
              "Variable" = v,
              "N" = n_val,
              "NA" = sum(is.na(sub_vec)),
              "Moyenne" = if (!is.na(m_val)) round(m_val, 3) else NA,
              "Médiane" = if (!is.na(med_val)) round(med_val, 3) else NA,
              "Variance" = if (!is.na(var_val)) round(var_val, 3) else NA,
              "Écart-type" = if (!is.na(s_val)) round(s_val, 3) else NA,
              "CV (%)" = cv_val,
              "Min" = if (!is.na(min_val)) round(min_val, 3) else NA,
              "Max" = if (!is.na(max_val)) round(max_val, 3) else NA,
              "IQR" = if (!is.na(iqr_val)) round(iqr_val, 3) else NA,
              "Asymétrie" = skew_val,
              "Aplatissement" = kurt_val
            )
            rows_list[[length(rows_list) + 1]] <- row_data
          }
        }

        res_df <- do.call(rbind, lapply(rows_list, as.data.frame, stringsAsFactors = FALSE))

        # Filtrage dynamique strict selon les indicateurs cochés
        selected_cols <- c("Variable")
        if (has_group) selected_cols <- c(selected_cols, "Groupe")
        if ("n" %in% active_stats) selected_cols <- c(selected_cols, "N")
        if ("na" %in% active_stats) selected_cols <- c(selected_cols, "NA")
        if ("mean" %in% active_stats) selected_cols <- c(selected_cols, "Moyenne")
        if ("median" %in% active_stats) selected_cols <- c(selected_cols, "Médiane")
        if ("var" %in% active_stats) selected_cols <- c(selected_cols, "Variance")
        if ("sd" %in% active_stats) selected_cols <- c(selected_cols, "Écart-type")
        if ("cv" %in% active_stats) selected_cols <- c(selected_cols, "CV (%)")
        if ("min" %in% active_stats) selected_cols <- c(selected_cols, "Min")
        if ("max" %in% active_stats) selected_cols <- c(selected_cols, "Max")
        if ("iqr" %in% active_stats) selected_cols <- c(selected_cols, "IQR")
        if ("skewness" %in% active_stats) selected_cols <- c(selected_cols, "Asymétrie")
        if ("kurtosis" %in% active_stats) selected_cols <- c(selected_cols, "Aplatissement")

        res_df <- res_df[, intersect(selected_cols, names(res_df)), drop = FALSE]

        DT::datatable(
          res_df,
          options = list(dom = "t", scrollX = TRUE, pageLength = 15),
          rownames = FALSE,
          class = "compact stripe hover border"
        )
      }
    })

    output$plot_quanti <- plotly::renderPlotly({
      shiny::req(quanti_state$ready, quanti_state$vars, data_holder$df)
      df <- data_holder$df
      target_var <- quanti_state$vars[1]
      if (!(target_var %in% names(df))) return(NULL)

      has_group <- nzchar(quanti_state$group) && (quanti_state$group %in% names(df))

      # Filtrage des NA pour un tracé propre sans warnings Plotly
      plot_df <- df[!is.na(df[[target_var]]), ]
      if (has_group) {
        plot_df <- plot_df[!is.na(plot_df[[quanti_state$group]]), ]
      }

      if (quanti_state$plot_type == "box") {
        p <- if (has_group) {
          plotly::plot_ly(
            data = plot_df,
            x = as.formula(paste0("~", quanti_state$group)),
            y = as.formula(paste0("~", target_var)),
            color = as.formula(paste0("~", quanti_state$group)),
            type = "box"
          )
        } else {
          plotly::plot_ly(
            data = plot_df,
            y = as.formula(paste0("~", target_var)),
            type = "box",
            name = target_var,
            marker = list(color = "#3498db")
          )
        }
        p <- plotly::layout(
          p,
          title = list(text = paste0("Boxplot de ", target_var, if (has_group) paste0(" selon ", quanti_state$group) else "")),
          yaxis = list(title = target_var),
          showlegend = has_group
        )
      } else {
        # Histogramme
        p <- if (has_group) {
          plotly::plot_ly(
            data = plot_df,
            x = as.formula(paste0("~", target_var)),
            color = as.formula(paste0("~", quanti_state$group)),
            type = "histogram",
            opacity = 0.75
          ) %>% plotly::layout(barmode = "overlay")
        } else {
          plotly::plot_ly(
            data = plot_df,
            x = as.formula(paste0("~", target_var)),
            type = "histogram",
            marker = list(color = "#2980b9", line = list(color = "#ffffff", width = 1))
          )
        }
        p <- plotly::layout(
          p,
          title = list(text = paste0("Distribution de ", target_var)),
          xaxis = list(title = target_var),
          yaxis = list(title = "Effectif (Fréquence)")
        )
      }
      p
    })

    # =========================================================================
    # 2. LOGIQUE : VARIABLES QUALITATIVES (FREQUENCES)
    # =========================================================================
    quali_state <- shiny::reactiveValues(
      var = NULL,
      include_na = TRUE,
      sort_mode = "none",
      ready = FALSE
    )

    shiny::observe({
      cat_cols <- categorical_vars()
      if (length(cat_cols) > 0 && !quali_state$ready) {
        quali_state$var <- cat_cols[1]
        quali_state$ready <- TRUE
      }
    })

    shiny::observeEvent(input$btn_open_quali_modal, {
      cat_cols <- categorical_vars()
      all_cols <- names(data_holder$df)

      choices_list <- if (length(cat_cols) > 0) cat_cols else all_cols
      current_selected <- if (!is.null(quali_state$var)) quali_state$var else choices_list[1]

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::strong("Paramètres - Table de Fréquences (Qualitative)")
          ),
          size = "m",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(
              inputId = ns("btn_confirm_quali"),
              label = "Valider et Calculer",
              class = "btn-primary"
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::selectInput(
              inputId = ns("modal_quali_var"),
              label = shiny::strong("Variable qualitative / catégorielle :"),
              choices = choices_list,
              selected = current_selected
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::checkboxInput(
              inputId = ns("modal_quali_include_na"),
              label = "Inclure les valeurs manquantes (NA)",
              value = quali_state$include_na
            )
          ),

          shiny::div(
            class = "mb-2",
            shiny::radioButtons(
              inputId = ns("modal_quali_sort"),
              label = shiny::strong("Ordre d'affichage des modalités :"),
              choices = c(
                "Ordre d'origine / alphabétique" = "none",
                "Effectif décroissant (du + fréquent au - fréquent)" = "desc",
                "Effectif croissant" = "asc"
              ),
              selected = quali_state$sort_mode
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_quali, {
      shiny::req(input$modal_quali_var)
      quali_state$var <- input$modal_quali_var
      quali_state$include_na <- input$modal_quali_include_na
      quali_state$sort_mode <- input$modal_quali_sort
      quali_state$ready <- TRUE

      shiny::removeModal()

      target_df <- data_holder$name
      v <- quali_state$var
      na_arg <- if (quali_state$include_na) 'useNA = "ifany"' else 'useNA = "no"'

      rmd_code <- paste0(
        "# Tableau de fréquences et pourcentages\n",
        sprintf("tab <- table(%s$%s, %s)\n", target_df, v, na_arg),
        "prop <- prop.table(tab) * 100\n",
        "res_freq <- data.frame(\n",
        "  Modalite = names(tab),\n",
        "  Effectif = as.numeric(tab),\n",
        "  Pourcentage = round(as.numeric(prop), 2),\n",
        "  Pourcentage_Cumule = round(cumsum(as.numeric(prop)), 2)\n",
        ")\nprint(res_freq)\n\n",
        "# Graphique en barres\n",
        "library(ggplot2)\n",
        sprintf("ggplot(%s, aes(x = %s)) +\n", target_df, v),
        '  geom_bar(fill = "#18bc9c", color = "#2c3e50") +\n',
        "  theme_minimal() +\n",
        sprintf('  labs(title = "Distribution de %s", x = "%s", y = "Effectif")', v, v)
      )

      append_to_rmd(
        title = paste0("Fréquences pour la variable '", v, "'"),
        code = rmd_code
      )

      shiny::showNotification("Table de fréquences mise à jour avec succès !", type = "message")
    })

    output$quali_status_badge <- shiny::renderUI({
      if (!quali_state$ready || is.null(quali_state$var)) return(NULL)
      shiny::div(
        class = "alert alert-success py-1 px-2 small mb-2 d-flex justify-content-between align-items-center",
        shiny::span(
          shiny::strong("Variable analysée : "), quali_state$var
        ),
        shiny::tags$span(class = "badge bg-success", "Fréquences & Pourcentages")
      )
    })

    output$table_quali <- DT::renderDataTable({
      shiny::req(quali_state$ready, quali_state$var, data_holder$df)
      df <- data_holder$df
      v <- quali_state$var
      if (!(v %in% names(df))) return(NULL)

      raw_vec <- df[[v]]
      tab <- if (quali_state$include_na) {
        table(raw_vec, useNA = "ifany")
      } else {
        table(raw_vec, useNA = "no")
      }

      prop <- prop.table(tab) * 100

      res_df <- data.frame(
        Modalité = names(tab),
        Effectif = as.numeric(tab),
        `Pourcentage (%)` = round(as.numeric(prop), 2),
        `% Cumulé` = round(cumsum(as.numeric(prop)), 2),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (quali_state$sort_mode == "desc") {
        res_df <- res_df[order(-res_df$Effectif), ]
        res_df$`% Cumulé` <- round(cumsum(res_df$`Pourcentage (%)`), 2)
      } else if (quali_state$sort_mode == "asc") {
        res_df <- res_df[order(res_df$Effectif), ]
        res_df$`% Cumulé` <- round(cumsum(res_df$`Pourcentage (%)`), 2)
      }

      DT::datatable(
        res_df,
        options = list(pageLength = 10, dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe hover border"
      )
    })

    output$plot_quali <- plotly::renderPlotly({
      shiny::req(quali_state$ready, quali_state$var, data_holder$df)
      df <- data_holder$df
      v <- quali_state$var
      if (!(v %in% names(df))) return(NULL)

      tab <- table(df[[v]], useNA = if (quali_state$include_na) "ifany" else "no")
      mod_names <- names(tab)
      counts <- as.numeric(tab)
      pcts <- round((counts / sum(counts)) * 100, 1)

      plotly::plot_ly(
        x = mod_names,
        y = counts,
        type = "bar",
        text = paste0(counts, " (", pcts, "%)"),
        textposition = "auto",
        marker = list(
          color = "#18bc9c",
          line = list(color = "#128f76", width = 1)
        )
      ) %>%
        plotly::layout(
          title = list(text = paste0("Effectifs par modalité de '", v, "'")),
          xaxis = list(title = v),
          yaxis = list(title = "Effectif (N)")
        )
    })

    # =========================================================================
    # 3. LOGIQUE : TABLEAU CROISÉ (BIVARIÉ)
    # =========================================================================
    crosstab_state <- shiny::reactiveValues(
      row_var = NULL,
      col_var = NULL,
      display_type = "counts",
      plot_mode = "group",
      ready = FALSE
    )

    shiny::observe({
      cat_cols <- categorical_vars()
      if (length(cat_cols) >= 2 && !crosstab_state$ready) {
        crosstab_state$row_var <- cat_cols[1]
        crosstab_state$col_var <- cat_cols[2]
        crosstab_state$ready <- TRUE
      } else if (length(names(data_holder$df)) >= 2 && !crosstab_state$ready) {
        crosstab_state$row_var <- names(data_holder$df)[1]
        crosstab_state$col_var <- names(data_holder$df)[2]
        crosstab_state$ready <- TRUE
      }
    })

    shiny::observeEvent(input$btn_open_crosstab_modal, {
      cat_cols <- categorical_vars()
      all_cols <- names(data_holder$df)
      choices_list <- if (length(cat_cols) >= 2) cat_cols else all_cols

      current_row <- if (!is.null(crosstab_state$row_var)) crosstab_state$row_var else choices_list[1]
      current_col <- if (!is.null(crosstab_state$col_var)) crosstab_state$col_var else choices_list[min(2, length(choices_list))]

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::strong("Paramètres - Tableau Croisé Bivarié (Contingence)")
          ),
          size = "m",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(
              inputId = ns("btn_confirm_crosstab"),
              label = "Valider et Calculer",
              class = "btn-primary"
            )
          ),

          shiny::div(
            class = "row g-2 mb-3",
            shiny::div(
              class = "col-md-6",
              shiny::selectInput(
                inputId = ns("modal_cross_row"),
                label = shiny::strong("Variable en Ligne (X) :"),
                choices = choices_list,
                selected = current_row
              )
            ),
            shiny::div(
              class = "col-md-6",
              shiny::selectInput(
                inputId = ns("modal_cross_col"),
                label = shiny::strong("Variable en Colonne (Y) :"),
                choices = choices_list,
                selected = current_col
              )
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::radioButtons(
              inputId = ns("modal_cross_display"),
              label = shiny::strong("Type de valeurs à présenter dans le tableau :"),
              choices = c(
                "Effectifs observés (N)" = "counts",
                "Pourcentages en ligne (% ligne)" = "row_pct",
                "Pourcentages en colonne (% colonne)" = "col_pct",
                "Pourcentages sur le total (% total)" = "tot_pct"
              ),
              selected = crosstab_state$display_type
            )
          ),

          shiny::div(
            class = "mb-2",
            shiny::radioButtons(
              inputId = ns("modal_cross_plot_mode"),
              label = shiny::strong("Disposition du graphique en barres Plotly :"),
              choices = c(
                "Barres groupées (côte à côte)" = "group",
                "Barres empilées (structure proportionnelle)" = "stack"
              ),
              selected = crosstab_state$plot_mode,
              inline = TRUE
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_crosstab, {
      if (input$modal_cross_row == input$modal_cross_col) {
        shiny::showNotification("Veuillez choisir deux variables distinctes pour le croisement.", type = "warning")
        return()
      }

      crosstab_state$row_var <- input$modal_cross_row
      crosstab_state$col_var <- input$modal_cross_col
      crosstab_state$display_type <- input$modal_cross_display
      crosstab_state$plot_mode <- input$modal_cross_plot_mode
      crosstab_state$ready <- TRUE

      shiny::removeModal()

      target_df <- data_holder$name
      r_v <- crosstab_state$row_var
      c_v <- crosstab_state$col_var

      rmd_code <- paste0(
        "# Tableau croisé bivarié (Table de contingence)\n",
        sprintf("tab_croise <- table(%s$%s, %s$%s)\n", target_df, r_v, target_df, c_v),
        "print('--- Effectifs observés ---')\n",
        "print(tab_croise)\n\n",
        "print('--- Pourcentages en ligne (%) ---')\n",
        "print(round(prop.table(tab_croise, margin = 1) * 100, 2))\n\n",
        "print('--- Pourcentages en colonne (%) ---')\n",
        "print(round(prop.table(tab_croise, margin = 2) * 100, 2))\n\n",
        "# Test du Chi-deux d'indépendance\n",
        "test_chi2 <- chisq.test(tab_croise)\n",
        "print(test_chi2)"
      )

      append_to_rmd(
        title = paste0("Tableau croisé : ", r_v, " × ", c_v),
        code = rmd_code
      )

      shiny::showNotification("Tableau croisé généré et consigné dans le journal R Markdown !", type = "message")
    })

    output$crosstab_status_badge <- shiny::renderUI({
      if (!crosstab_state$ready || is.null(crosstab_state$row_var)) return(NULL)
      shiny::div(
        class = "alert alert-warning py-1 px-2 small mb-2 d-flex justify-content-between align-items-center",
        shiny::span(
          shiny::strong("Croisement : "), crosstab_state$row_var, " × ", crosstab_state$col_var
        ),
        shiny::tags$span(class = "badge bg-warning text-dark", "Bivarié Quali x Quali")
      )
    })

    output$table_crosstab <- DT::renderDataTable({
      shiny::req(crosstab_state$ready, crosstab_state$row_var, crosstab_state$col_var, data_holder$df)
      df <- data_holder$df
      r_v <- crosstab_state$row_var
      c_v <- crosstab_state$col_var
      if (!(r_v %in% names(df)) || !(c_v %in% names(df))) return(NULL)

      tab <- table(df[[r_v]], df[[c_v]])

      mat_display <- switch(
        crosstab_state$display_type,
        "counts" = tab,
        "row_pct" = round(prop.table(tab, margin = 1) * 100, 2),
        "col_pct" = round(prop.table(tab, margin = 2) * 100, 2),
        "tot_pct" = round(prop.table(tab) * 100, 2)
      )

      res_df <- as.data.frame.matrix(mat_display)
      res_df <- cbind(setNames(data.frame(rownames(res_df), stringsAsFactors = FALSE), r_v), res_df)

      DT::datatable(
        res_df,
        options = list(pageLength = 10, dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe hover border"
      )
    })

    output$plot_crosstab <- plotly::renderPlotly({
      shiny::req(crosstab_state$ready, crosstab_state$row_var, crosstab_state$col_var, data_holder$df)
      df <- data_holder$df
      r_v <- crosstab_state$row_var
      c_v <- crosstab_state$col_var
      if (!(r_v %in% names(df)) || !(c_v %in% names(df))) return(NULL)

      tab <- table(df[[r_v]], df[[c_v]])
      row_names <- rownames(tab)
      col_names <- colnames(tab)

      p <- plotly::plot_ly()
      colors_palette <- c("#3498db", "#2ecc71", "#e74c3c", "#f39c12", "#9b59b6", "#1abc9c", "#34495e")

      for (j in seq_along(col_names)) {
        col_m <- col_names[j]
        color_chosen <- colors_palette[((j - 1) %% length(colors_palette)) + 1]
        p <- plotly::add_bars(
          p,
          x = row_names,
          y = as.numeric(tab[, j]),
          name = as.character(col_m),
          marker = list(color = color_chosen)
        )
      }

      plotly::layout(
        p,
        barmode = crosstab_state$plot_mode,
        title = list(text = paste0("Répartition de '", c_v, "' selon '", r_v, "'")),
        xaxis = list(title = r_v),
        yaxis = list(title = "Effectif (N)")
      )
    })

    # =========================================================================
    # 4. LOGIQUE : MATRICE DE CORRELATION
    # =========================================================================
    cor_state <- shiny::reactiveValues(
      vars = NULL,
      method = "pearson",
      use = "pairwise.complete.obs",
      ready = FALSE
    )

    shiny::observe({
      num_cols <- numeric_vars()
      if (length(num_cols) >= 2 && !cor_state$ready) {
        cor_state$vars <- num_cols[seq_len(min(4, length(num_cols)))]
        cor_state$ready <- TRUE
      }
    })

    shiny::observeEvent(input$btn_open_cor_modal, {
      num_cols <- numeric_vars()
      if (length(num_cols) < 2) {
        shiny::showNotification("Il faut au moins 2 variables numériques pour calculer une corrélation.", type = "warning")
        return()
      }

      current_selected <- if (!is.null(cor_state$vars)) cor_state$vars else num_cols[seq_len(min(4, length(num_cols)))]

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::strong("Paramètres - Matrice de Corrélation")
          ),
          size = "m",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(
              inputId = ns("btn_confirm_cor"),
              label = "Valider et Calculer",
              class = "btn-primary"
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::selectizeInput(
              inputId = ns("modal_cor_vars"),
              label = shiny::strong("Variables numériques (sélectionner 2 ou plus) :"),
              choices = num_cols,
              selected = current_selected,
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          ),

          shiny::div(
            class = "mb-3",
            shiny::radioButtons(
              inputId = ns("modal_cor_method"),
              label = shiny::strong("Méthode de corrélation :"),
              choices = c(
                "Pearson (paramétrique, relation linéaire)" = "pearson",
                "Spearman (non-paramétrique, monotonie de rangs)" = "spearman"
              ),
              selected = cor_state$method
            )
          ),

          shiny::div(
            class = "mb-2",
            shiny::radioButtons(
              inputId = ns("modal_cor_use"),
              label = shiny::strong("Traitement des valeurs manquantes :"),
              choices = c(
                "Paires complètes (pairwise.complete.obs)" = "pairwise.complete.obs",
                "Observations complètes (complete.obs)" = "complete.obs"
              ),
              selected = cor_state$use
            )
          )
        )
      )
    })

    shiny::observeEvent(input$btn_confirm_cor, {
      if (length(input$modal_cor_vars) < 2) {
        shiny::showNotification("Veuillez sélectionner au moins deux variables numériques.", type = "warning")
        return()
      }

      cor_state$vars <- input$modal_cor_vars
      cor_state$method <- input$modal_cor_method
      cor_state$use <- input$modal_cor_use
      cor_state$ready <- TRUE

      shiny::removeModal()

      target_df <- data_holder$name
      vars_str <- paste0('c("', paste(cor_state$vars, collapse = '", "'), '")')

      rmd_code <- paste0(
        "# Matrice de corrélation linéaire / monotone\n",
        sprintf("vars_num <- %s[, %s]\n", target_df, vars_str),
        sprintf('mat_cor <- cor(vars_num, method = "%s", use = "%s")\n', cor_state$method, cor_state$use),
        "print(round(mat_cor, 3))\n\n",
        "# Visualisation de la matrice de corrélation\n",
        "if (requireNamespace('corrplot', quietly = TRUE)) {\n",
        "  corrplot::corrplot(mat_cor, method = 'color', type = 'upper', tl.col = 'black', tl.srt = 45)\n",
        "}"
      )

      append_to_rmd(
        title = paste0("Matrice de corrélation (", toupper(cor_state$method), ")"),
        code = rmd_code
      )

      shiny::showNotification("Matrice de corrélation calculée et enregistrée !", type = "message")
    })

    output$cor_status_badge <- shiny::renderUI({
      if (!cor_state$ready || length(cor_state$vars) < 2) return(NULL)
      shiny::div(
        class = "alert alert-danger py-1 px-2 small mb-2 d-flex justify-content-between align-items-center",
        shiny::span(
          shiny::strong("Variables : "), paste(cor_state$vars, collapse = ", "),
          paste0(" | Méthode : ", toupper(cor_state$method))
        ),
        shiny::tags$span(class = "badge bg-danger", paste0(length(cor_state$vars), " variables"))
      )
    })

    output$table_cor <- DT::renderDataTable({
      shiny::req(cor_state$ready, cor_state$vars, data_holder$df)
      df <- data_holder$df
      vars <- cor_state$vars[cor_state$vars %in% names(df)]
      if (length(vars) < 2) return(NULL)

      sub_df <- df[, vars, drop = FALSE]
      c_mat <- stats::cor(sub_df, method = cor_state$method, use = cor_state$use)
      round_mat <- round(c_mat, 3)

      res_df <- as.data.frame(round_mat)
      res_df <- cbind(Variable = rownames(res_df), res_df)

      DT::datatable(
        res_df,
        options = list(pageLength = 10, dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe hover border"
      )
    })

    output$plot_cor <- plotly::renderPlotly({
      shiny::req(cor_state$ready, cor_state$vars, data_holder$df)
      df <- data_holder$df
      vars <- cor_state$vars[cor_state$vars %in% names(df)]
      if (length(vars) < 2) return(NULL)

      sub_df <- df[, vars, drop = FALSE]
      c_mat <- stats::cor(sub_df, method = cor_state$method, use = cor_state$use)
      c_mat <- round(c_mat, 3)

      plotly::plot_ly(
        x = colnames(c_mat),
        y = rownames(c_mat),
        z = c_mat,
        type = "heatmap",
        zmin = -1,
        zmax = 1,
        colorscale = list(
          list(0, "#2980b9"),
          list(0.5, "#ffffff"),
          list(1, "#c0392b")
        )
      ) %>%
        plotly::layout(
          title = list(text = paste0("Heatmap de corrélation (", toupper(cor_state$method), ")")),
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })

  })
}
