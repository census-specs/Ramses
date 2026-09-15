#' @title Module Shiny pour les Mod\u00e8les de R\u00e9gression (Ramses)
#'
#' @description Fournit les interfaces graphiques et les calculs statistiques pour
#'   la R\u00e9gression Lin\u00e9aire (Simple/Multiple via stats::lm) et la R\u00e9gression Logistique
#'   Binaire (via stats::glm binomial logit) avec restitutions p\u00e9dagogiques compl\u00e8tes,
#'   diagnostics sp\u00e9cialis\u00e9s et journalisation R Markdown reproductible.
#'
#' @name mod_regression
#' @noRd
NULL

#' Interface utilisateur du sous-panneau R\u00e9gression Lin\u00e9aire
#'
#' @param id Identifiant du module
#' @noRd
mod_regression_linear_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm mb-3 border-0",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center py-2 bg-light",
          shiny::tags$span(shiny::tags$strong("R\u00e9gression Lin\u00e9aire (lm)")),
          shiny::tags$span(class = "badge bg-secondary text-white", "OLS")
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("lm_var_y"),
            label = "Variable D\u00e9pendante Quantitative (Y) :",
            choices = NULL
          ),
          shiny::selectizeInput(
            inputId = ns("lm_vars_x"),
            label = "Variables Explicatives (X) :",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("lm_alpha"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_lm"),
            label = "Ajuster la r\u00e9gression lin\u00e9aire",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("lm_status_badge")),
      bslib::navset_card_tab(
        id = ns("lm_results_tabs"),
        bslib::nav_panel(
          title = "R\u00e9sultats & Interpr\u00e9tation",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("lm_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Diagnostic des r\u00e9sidus",
          bslib::card_header(
            class = "py-2 bg-light d-flex justify-content-between align-items-center",
            shiny::tags$span(shiny::tags$strong("Graphiques des r\u00e9sidus")),
            shiny::radioButtons(
              inputId = ns("lm_plot_choice"),
              label = NULL,
              choices = c("R\u00e9sidus vs Valeurs ajust\u00e9es" = "rvf", "Normal Q-Q Plot r\u00e9sidus" = "qq"),
              inline = TRUE
            )
          ),
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("lm_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("lm_pedagogy_ui"))
          )
        )
      )
    )
  )
}

#' Interface utilisateur du sous-panneau R\u00e9gression Logistique
#'
#' @param id Identifiant du module
#' @noRd
mod_regression_logistic_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::div(
      class = "col-lg-4 col-md-5",
      bslib::card(
        class = "shadow-sm mb-3 border-0",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center py-2 bg-light",
          shiny::tags$span(shiny::tags$strong("R\u00e9gression Logistique Binaire")),
          shiny::tags$span(class = "badge bg-primary text-white", "Logit")
        ),
        bslib::card_body(
          class = "p-3",
          shiny::selectInput(
            inputId = ns("glm_var_y"),
            label = "Variable R\u00e9ponse Binaire (Y) :",
            choices = NULL
          ),
          shiny::uiOutput(ns("glm_event_ui")),
          shiny::selectizeInput(
            inputId = ns("glm_vars_x"),
            label = "Variables Explicatives (X) :",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("glm_alpha"),
            label = "Seuil alpha :",
            choices = c("1% (0.01)" = "0.01", "5% (0.05)" = "0.05", "10% (0.10)" = "0.10"),
            selected = "0.05"
          ),
          shiny::actionButton(
            inputId = ns("btn_run_glm"),
            label = "Ajuster le mod\u00e8le logistique",
            class = "btn-dark btn-sm w-100 mt-2 shadow-sm"
          )
        )
      )
    ),
    shiny::div(
      class = "col-lg-8 col-md-7",
      shiny::uiOutput(ns("glm_status_badge")),
      bslib::navset_card_tab(
        id = ns("glm_results_tabs"),
        bslib::nav_panel(
          title = "R\u00e9sultats & Interpr\u00e9tation",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("glm_results_ui"))
          )
        ),
        bslib::nav_panel(
          title = "Diagnostics adapt\u00e9s",
          bslib::card_header(
            class = "py-2 bg-light d-flex justify-content-between align-items-center",
            shiny::tags$span(shiny::tags$strong("Graphique diagnostique du mod\u00e8le logistique")),
            shiny::radioButtons(
              inputId = ns("glm_plot_choice"),
              label = NULL,
              choices = c("R\u00e9sidus de d\u00e9viance vs Probabilit\u00e9s pr\u00e9dites" = "rvf", "Distance de Cook (Levier & Influence)" = "cook"),
              inline = TRUE
            )
          ),
          bslib::card_body(
            padding = 1,
            plotly::plotlyOutput(ns("glm_plot"), height = "400px")
          )
        ),
        bslib::nav_panel(
          title = "Hypoth\u00e8ses & P\u00e9dagogie",
          bslib::card_body(
            class = "p-3",
            shiny::uiOutput(ns("glm_pedagogy_ui"))
          )
        )
      )
    )
  )
}

#' Interface principale globale du module R\u00e9gression (navset)
#'
#' @param id Identifiant de namespace
#' @noRd
mod_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    id = ns("regression_main_tabs"),
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("chart-line", fill = "#6B7280", height = "0.85em"), "R\u00e9gression Lin\u00e9aire")),
      value = "tab_linear",
      mod_regression_linear_ui(id)
    ),
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("sliders", fill = "#6B7280", height = "0.85em"), "R\u00e9gression Logistique")),
      value = "tab_logistic",
      mod_regression_logistic_ui(id)
    )
  )
}

#' Logique serveur pour le module Mod\u00e8les de R\u00e9gression
#'
#' @param id Identifiant Shiny
#' @param data_holder R\u00e9actif contenant les donn\u00e9es actives
#' @param append_to_rmd Fonction d'ajout au journal Rmd
#' @noRd
mod_regression_server <- function(id, data_holder, append_to_rmd) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers d'extraction des colonnes
    get_num_vars <- function() {
      df <- data_holder$df
      if (!is.data.frame(df) || ncol(df) == 0) return(character(0))
      names(df)[vapply(df, is.numeric, logical(1))]
    }

    get_all_vars <- function() {
      df <- data_holder$df
      if (!is.data.frame(df) || ncol(df) == 0) return(character(0))
      names(df)
    }

    # Mise \u00e0 jour r\u00e9active des s\u00e9lecteurs pour les deux sous-mod\u00e8les
    shiny::observe({
      df <- data_holder$df
      if (!is.data.frame(df)) return()
      num_cols <- get_num_vars()
      all_cols <- get_all_vars()

      # 1. R\u00e9gression Lin\u00e9aire
      shiny::updateSelectInput(
        session, "lm_var_y",
        choices = num_cols,
        selected = if (length(num_cols) > 0) num_cols[1] else NULL
      )
      shiny::updateSelectizeInput(
        session, "lm_vars_x",
        choices = all_cols,
        selected = if (length(num_cols) > 1) num_cols[2] else NULL,
        server = TRUE
      )

      # 2. R\u00e9gression Logistique
      shiny::updateSelectInput(
        session, "glm_var_y",
        choices = all_cols,
        selected = if (length(all_cols) > 0) all_cols[1] else NULL
      )
      shiny::updateSelectizeInput(
        session, "glm_vars_x",
        choices = all_cols,
        selected = if (length(all_cols) > 1) all_cols[2] else NULL,
        server = TRUE
      )
    })

    # Affichage dynamique de l'\u00e9v\u00e9nement mod\u00e9lis\u00e9 et r\u00e9f\u00e9rence pour Y s\u00e9lectionn\u00e9 dans glm
    output$glm_event_ui <- shiny::renderUI({
      shiny::req(input$glm_var_y, data_holder$df)
      df <- data_holder$df
      y_col <- input$glm_var_y
      if (!y_col %in% names(df)) return(NULL)

      raw_y <- df[[y_col]]
      non_na_y <- raw_y[!is.na(raw_y)]
      unique_vals <- unique(non_na_y)
      n_mods <- length(unique_vals)

      if (n_mods != 2) {
        return(shiny::div(
          class = "alert alert-warning py-1 px-2 small mb-2",
          shiny::tags$strong("Attention : "),
          paste0("Cette variable comporte ", n_mods, " modalit\u00e9(s). Une r\u00e9ponse exactement binaire (2 modalit\u00e9s) est requise.")
        ))
      }

      # Identifier les deux modalit\u00e9s selon la logique de R
      if (is.factor(raw_y)) {
        ref_mod <- levels(raw_y)[1]
        evt_mod <- levels(raw_y)[2]
      } else if (is.numeric(raw_y) && all(unique_vals %in% c(0, 1))) {
        ref_mod <- "0"
        evt_mod <- "1"
      } else {
        ch_vals <- sort(as.character(unique_vals))
        ref_mod <- ch_vals[1]
        evt_mod <- ch_vals[2]
      }

      shiny::div(
        class = "p-2 rounded bg-light border small mb-2",
        shiny::div(class = "d-flex justify-content-between",
          shiny::tags$span(shiny::tags$strong("\u00c9v\u00e9nement mod\u00e9lis\u00e9 (1) : "), shiny::tags$span(class = "badge bg-primary", evt_mod)),
          shiny::tags$span(shiny::tags$strong("R\u00e9f\u00e9rence (0) : "), shiny::tags$span(class = "badge bg-secondary", ref_mod))
        )
      )
    })

    # =========================================================================
    # PARTIE 1 : R\u00c9GRESSION LIN\u00c9AIRE
    # =========================================================================
    lm_state <- shiny::reactiveValues(
      model = NULL,
      var_y = NULL,
      vars_x = NULL,
      alpha = 0.05,
      calculated = FALSE,
      error = NULL,
      model_type = "linear"
    )

    shiny::observeEvent(input$btn_run_lm, {
      df <- data_holder$df
      ds_name <- data_holder$name
      var_y <- input$lm_var_y
      vars_x <- input$lm_vars_x
      alpha_val <- as.numeric(input$lm_alpha)

      if (is.null(var_y) || !nzchar(var_y) || is.null(vars_x) || length(vars_x) == 0) {
        lm_state$error <- "Veuillez s\u00e9lectionner une variable d\u00e9pendante quantitative (Y) et au moins une variable explicative (X)."
        lm_state$calculated <- TRUE
        return()
      }

      if (var_y %in% vars_x) {
        lm_state$error <- "La variable d\u00e9pendante (Y) ne peut pas figurer parmi les variables explicatives (X)."
        lm_state$calculated <- TRUE
        return()
      }

      lm_state$var_y <- var_y
      lm_state$vars_x <- vars_x
      lm_state$alpha <- alpha_val

      tryCatch({
        fml <- ramses_formula(response = var_y, terms = vars_x)
        fml_code <- ramses_formula_code(response = var_y, terms = vars_x)

        fit <- stats::lm(fml, data = df)
        lm_state$model <- fit
        lm_state$error <- NULL
        lm_state$calculated <- TRUE

        code_entry <- paste0(
          "# Regression lineaire (lm)\n",
          "mod_lm <- lm(", fml_code, ", data = ", ramses_code_symbol(ds_name), ")\n",
          "summary(mod_lm)\n",
          "confint(mod_lm)\n",
          "rmse_val <- sqrt(mean(residuals(mod_lm)^2))\n",
          "cat('RMSE =', round(rmse_val, 4), '\\n')"
        )

        append_to_rmd(
          title = paste0("R\u00e9gression Lin\u00e9aire : ", var_y, " ~ ", paste(vars_x, collapse = " + ")),
          code = code_entry
        )
        shiny::showNotification("Mod\u00e8le de r\u00e9gression lin\u00e9aire ajust\u00e9 et consign\u00e9 avec succ\u00e8s !", type = "message")
      }, error = function(e) {
        lm_state$error <- paste0("Erreur lors de l'ajustement du mod\u00e8le lin\u00e9aire : ", e$message)
        lm_state$calculated <- TRUE
      })
    })

    output$lm_status_badge <- shiny::renderUI({
      if (!lm_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ajuster la r\u00e9gression lin\u00e9aire' pour lancer l'estimation."))
      }
      if (!is.null(lm_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur d'ajustement : ", lm_state$error)))
      }
      shiny::div(
        class = "alert alert-success py-2 px-3 small d-flex justify-content-between align-items-center",
        shiny::tags$span("Mod\u00e8le lin\u00e9aire ajust\u00e9 : ", paste(lm_state$var_y, "~", paste(lm_state$vars_x, collapse = " + "))),
        shiny::tags$span(class = "badge text-dark border", style = "background-color: #F3F4F6; border-color: #D1D5DB !important;", "R\u00e9gression Lin\u00e9aire OLS")
      )
    })

    output$lm_results_ui <- shiny::renderUI({
      shiny::req(lm_state$calculated)
      if (!is.null(lm_state$error)) {
        return(shiny::p(class = "text-danger small", lm_state$error))
      }
      # Appel direct de la fonction p\u00e9dagogique lin\u00e9aire valid\u00e9e (5 \u00e9tapes)
      ramses_render_lm_results(lm_state, lm_state$alpha)
    })

    output$lm_plot <- plotly::renderPlotly({
      shiny::req(lm_state$calculated, lm_state$model)
      mod <- lm_state$model
      resids <- stats::residuals(mod)
      fitted_vals <- stats::fitted(mod)

      if (input$lm_plot_choice == "rvf") {
        plotly::plot_ly() %>%
          plotly::add_trace(
            x = fitted_vals,
            y = resids,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#1F2937", size = 6, opacity = 0.8),
            name = "R\u00e9sidus"
          ) %>%
          plotly::add_lines(
            x = range(fitted_vals),
            y = c(0, 0),
            line = list(color = "#991B1B", dash = "dash"),
            name = "Ligne 0"
          ) %>%
          plotly::layout(
            font = list(family = "IBM Plex Sans"),
            title = list(text = "R\u00e9sidus vs Valeurs ajust\u00e9es (Lin\u00e9arit\u00e9 & Homosc\u00e9dasticit\u00e9)", font = list(size = 13)),
            xaxis = list(title = "Valeurs ajust\u00e9es"),
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
            marker = list(color = "#374151", size = 6),
            name = "R\u00e9sidus observ\u00e9s"
          ) %>%
          plotly::add_lines(
            x = range(theo),
            y = range(theo),
            line = list(color = "#991B1B", dash = "dash"),
            name = "Normale th\u00e9orique"
          ) %>%
          plotly::layout(
            font = list(family = "IBM Plex Sans"),
            title = list(text = "Normal Q-Q Plot des R\u00e9sidus (Normalit\u00e9)", font = list(size = 13)),
            xaxis = list(title = "Quantiles th\u00e9oriques"),
            yaxis = list(title = "R\u00e9sidus observ\u00e9s")
          )
      }
    })

    output$lm_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_reg(lm_state, data_holder$df)
    })


    # =========================================================================
    # PARTIE 2 : R\u00c9GRESSION LOGISTIQUE BINAIRE
    # =========================================================================
    glm_state <- shiny::reactiveValues(
      model = NULL,
      model_null = NULL,
      var_y = NULL,
      vars_x = NULL,
      alpha = 0.05,
      calculated = FALSE,
      error = NULL,
      n_initial = 0,
      n_used = 0,
      n_excluded = 0,
      event_label = NULL,
      ref_label = NULL,
      df_model = NULL
    )

    shiny::observeEvent(input$btn_run_glm, {
      df <- data_holder$df
      ds_name <- data_holder$name
      var_y <- input$glm_var_y
      vars_x <- input$glm_vars_x
      alpha_val <- as.numeric(input$glm_alpha)

      if (is.null(var_y) || !nzchar(var_y) || is.null(vars_x) || length(vars_x) == 0) {
        glm_state$error <- "Veuillez s\u00e9lectionner une variable d\u00e9pendante binaire (Y) et au moins une variable explicative (X)."
        glm_state$calculated <- TRUE
        return()
      }

      if (var_y %in% vars_x) {
        glm_state$error <- "La variable d\u00e9pendante (Y) ne peut pas figurer parmi les variables explicatives (X)."
        glm_state$calculated <- TRUE
        return()
      }

      # 1. Validation stricte de la binarit\u00e9 de la variable r\u00e9ponse
      raw_y <- df[[var_y]]
      valid_y_idx <- !is.na(raw_y)
      unique_y <- unique(raw_y[valid_y_idx])

      if (length(unique_y) < 2) {
        glm_state$error <- paste0("La variable d\u00e9pendante '", var_y, "' ne comporte qu'une seule valeur observable (", unique_y[1], "). Deux cat\u00e9gories distinctes sont requises.")
        glm_state$calculated <- TRUE
        return()
      }

      if (length(unique_y) > 2) {
        glm_state$error <- paste0("La r\u00e9gression logistique binaire n\u00e9cessite une variable r\u00e9ponse comportant exactement deux modalit\u00e9s. La variable '", var_y, "' en comporte ", length(unique_y), " (", paste(head(unique_y, 4), collapse = ", "), if (length(unique_y) > 4) "..." else "", "). Veuillez recoder ou filtrer cette variable au pr\u00e9alable.")
        glm_state$calculated <- TRUE
        return()
      }

      # 2. Pr\u00e9paration du sous-ensemble et identification des modalit\u00e9s
      sub_df <- df[, c(var_y, vars_x), drop = FALSE]
      n_init <- nrow(sub_df)

      # D\u00e9termination explicite de la modalit\u00e9 \u00e9v\u00e9nement (1) et r\u00e9f\u00e9rence (0)
      is_num_01 <- is.numeric(sub_df[[var_y]]) && all(unique_y %in% c(0, 1))

      if (is_num_01) {
        ref_mod <- "0"
        evt_mod <- "1"
        code_prep <- ""
      } else {
        sub_df[[var_y]] <- as.factor(sub_df[[var_y]])
        ref_mod <- levels(sub_df[[var_y]])[1]
        evt_mod <- levels(sub_df[[var_y]])[2]
        code_prep <- paste0(
          "# Conversion de la variable reponse binaire en facteur (Reference: ", ref_mod, ", Evenement: ", evt_mod, ")\n",
          "data_sub <- ", ramses_code_symbol(ds_name), "\n",
          ramses_code_column("data_sub", var_y), " <- as.factor(", ramses_code_column("data_sub", var_y), ")\n"
        )
      }

      glm_state$var_y <- var_y
      glm_state$vars_x <- vars_x
      glm_state$alpha <- alpha_val
      glm_state$event_label <- evt_mod
      glm_state$ref_label <- ref_mod
      glm_state$n_initial <- n_init

      tryCatch({
        fml <- ramses_formula(response = var_y, terms = vars_x)
        fml_code <- ramses_formula_code(response = var_y, terms = vars_x)
        fml_null <- stats::as.formula(paste0(ramses_code_symbol(var_y), " ~ 1"))

        # Ajustement du mod\u00e8le complet et du mod\u00e8le nul sur les m\u00eames donn\u00e9es sans NA
        fit_glm <- stats::glm(fml, data = sub_df, family = stats::binomial(link = "logit"))
        
        # Pour que anova(mod_nul, mod_complet) soit parfaitement align\u00e9, utiliser les donn\u00e9es effectivement utilis\u00e9es par fit_glm
        used_indices <- as.numeric(rownames(stats::model.frame(fit_glm)))
        sub_df_used <- sub_df[rownames(sub_df) %in% rownames(stats::model.frame(fit_glm)), , drop = FALSE]
        fit_null <- stats::glm(fml_null, data = sub_df_used, family = stats::binomial(link = "logit"))

        n_used <- stats::nobs(fit_glm)
        n_excl <- n_init - n_used

        glm_state$model <- fit_glm
        glm_state$model_null <- fit_null
        glm_state$n_used <- n_used
        glm_state$n_excluded <- n_excl
        glm_state$df_model <- sub_df_used
        glm_state$error <- NULL
        glm_state$calculated <- TRUE

        data_sym <- if (nzchar(code_prep)) "data_sub" else ramses_code_symbol(ds_name)
        code_entry <- paste0(
          code_prep,
          "# Ajustement du modele de regression logistique binaire\n",
          "mod_glm <- glm(", fml_code, ", data = ", data_sym, ", family = binomial(link = 'logit'))\n",
          "summary(mod_glm)\n\n",
          "# Test global du rapport de vraisemblance (LRT)\n",
          "mod_null <- glm(", ramses_code_symbol(var_y), " ~ 1, data = model.frame(mod_glm), family = binomial())\n",
          "lrt_test <- anova(mod_null, mod_glm, test = 'Chisq')\n",
          "print(lrt_test)\n\n",
          "# Odds Ratios et Intervalles de confiance a 95 %\n",
          "coef_ci <- confint.default(mod_glm, level = 0.95)\n",
          "or_table <- cbind(OR = exp(coef(mod_glm)), exp(coef_ci))\n",
          "print(round(or_table, 4))\n\n",
          "# Pseudo-R2 de McFadden\n",
          "pseudo_r2 <- 1 - (mod_glm$deviance / mod_glm$null.deviance)\n",
          "cat('Pseudo-R2 de McFadden =', round(pseudo_r2, 4), '\\n')"
        )

        append_to_rmd(
          title = paste0("R\u00e9gression Logistique : ", var_y, " ~ ", paste(vars_x, collapse = " + ")),
          code = code_entry
        )
        shiny::showNotification("Mod\u00e8le de r\u00e9gression logistique ajust\u00e9 et consign\u00e9 avec succ\u00e8s !", type = "message")
      }, error = function(e) {
        glm_state$error <- paste0("Erreur lors de l'ajustement du mod\u00e8le logistique : ", e$message)
        glm_state$calculated <- TRUE
      })
    })

    output$glm_status_badge <- shiny::renderUI({
      if (!glm_state$calculated) {
        return(shiny::div(class = "alert alert-secondary py-2 px-3 small", "Cliquez sur 'Ajuster le mod\u00e8le logistique' pour lancer l'estimation."))
      }
      if (!is.null(glm_state$error)) {
        return(shiny::div(class = "alert alert-danger py-2 px-3 small", paste0("Erreur d'ajustement : ", glm_state$error)))
      }
      shiny::div(
        class = "alert alert-success py-2 px-3 small d-flex justify-content-between align-items-center flex-wrap gap-2",
        shiny::tags$span("Mod\u00e8le logistique ajust\u00e9 : ", paste(glm_state$var_y, "~", paste(glm_state$vars_x, collapse = " + "))),
        shiny::tags$span(
          class = "badge text-dark border",
          style = "background-color: #F3F4F6; border-color: #D1D5DB !important;",
          paste0("\u00c9v\u00e9nement : '", glm_state$event_label, "' (n = ", glm_state$n_used, ")")
        )
      )
    })

    output$glm_results_ui <- shiny::renderUI({
      shiny::req(glm_state$calculated)
      if (!is.null(glm_state$error)) {
        return(shiny::p(class = "text-danger small", glm_state$error))
      }
      # Appel de la fonction de restitution p\u00e9dagogique en 7 \u00e9tapes
      ramses_render_logistic_results(glm_state, glm_state$alpha)
    })

    output$glm_plot <- plotly::renderPlotly({
      shiny::req(glm_state$calculated, glm_state$model)
      mod <- glm_state$model
      dev_resids <- stats::residuals(mod, type = "deviance")
      pred_probs <- stats::predict(mod, type = "response")

      if (input$glm_plot_choice == "rvf") {
        plotly::plot_ly() %>%
          plotly::add_trace(
            x = pred_probs,
            y = dev_resids,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#1F2937", size = 6, opacity = 0.8),
            name = "R\u00e9sidus de d\u00e9viance"
          ) %>%
          plotly::add_lines(
            x = c(0, 1),
            y = c(0, 0),
            line = list(color = "#991B1B", dash = "dash"),
            name = "Ligne 0"
          ) %>%
          plotly::layout(
            font = list(family = "IBM Plex Sans"),
            title = list(text = "R\u00e9sidus de d\u00e9viance vs Probabilit\u00e9s pr\u00e9dites", font = list(size = 13)),
            xaxis = list(title = "Probabilit\u00e9s pr\u00e9dites P(Y = 1)", range = c(-0.02, 1.02)),
            yaxis = list(title = "R\u00e9sidus de d\u00e9viance")
          )
      } else {
        # Graphique des distances de Cook (observations influentes)
        cooks_d <- stats::cooks.distance(mod)
        obs_idx <- seq_along(cooks_d)
        threshold <- 4 / length(cooks_d)

        plotly::plot_ly() %>%
          plotly::add_trace(
            x = obs_idx,
            y = cooks_d,
            type = "bar",
            marker = list(color = "#374151"),
            name = "Distance de Cook"
          ) %>%
          plotly::add_lines(
            x = range(obs_idx),
            y = c(threshold, threshold),
            line = list(color = "#991B1B", dash = "dash"),
            name = "Seuil 4/n"
          ) %>%
          plotly::layout(
            font = list(family = "IBM Plex Sans"),
            title = list(text = "Levier et observations influentes (Distance de Cook)", font = list(size = 13)),
            xaxis = list(title = "Num\u00e9ro d'observation"),
            yaxis = list(title = "Distance de Cook")
          )
      }
    })

    output$glm_pedagogy_ui <- shiny::renderUI({
      ramses_pedagogy_reg(
        list(
          model = glm_state$model,
          var_y = glm_state$var_y,
          vars_x = glm_state$vars_x,
          model_type = "logistic",
          alpha = glm_state$alpha,
          calculated = glm_state$calculated
        ),
        data_holder$df
      )
    })

  })
}
