#' @title Interface utilisateur pour le module créateur de graphiques
#'
#' @description Module dédié à la création graphique interactive et intuitive
#'   dans le package Ramses. Inspiré de l'ergonomie de Tableau Software, il permet
#'   de sélectionner des variables réparties par type (numériques vs facteurs),
#'   de mapper les dimensions esthétiques (X, Y, Couleur, Taille, Facet), de choisir
#'   parmi 8 types de visualisations ggplot2 et de personnaliser les thèmes, palettes,
#'   titres et étiquettes. Le graphique est rendu de manière interactive via Plotly
#'   et le code ggplot2 reproductible est affiché en direct et injectable dans le Journal R Markdown.
#'
#' @param id Identifiant de namespace Shiny.
#' @return Interface utilisateur Shiny (\code{tagList}).
#' @export
mod_chart_builder_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$span(style = "font-weight: 600;", "Aesthetics & Paramètres")
      ),
      width = 360,
      open = "open",

      # 1. Résumé des variables disponibles
      shiny::div(
        class = "mb-3 p-2 bg-light rounded border",
        shiny::div(
          class = "d-flex justify-content-between align-items-center mb-1",
          shiny::tags$strong(class = "small text-uppercase text-muted", "Variables du dataset"),
          shiny::uiOutput(ns("var_count_badge"))
        ),
        shiny::uiOutput(ns("vars_summary_ui"))
      ),

      # 2. Type de Graphique (Geom)
      shiny::div(
        class = "mb-3",
        shiny::tags$label(
          class = "form-label fw-bold small text-dark d-flex align-items-center gap-1",
          "Type de graphique (Geom) :"
        ),
        shiny::selectInput(
          inputId = ns("chart_type"),
          label = NULL,
          choices = c(
            "Nuage de points (Scatter plot)" = "scatter",
            "Diagramme en barres (Barplot)" = "bar",
            "Boîte à moustaches (Boxplot)" = "boxplot",
            "Diagramme en violon (Violin plot)" = "violin",
            "Histogramme de distribution" = "histogram",
            "Courbe / Ligne (Line plot)" = "line",
            "Densité continue" = "density",
            "Carte thermique (Heatmap 2D)" = "heatmap"
          ),
          selected = "scatter",
          selectize = TRUE
        )
      ),

      # 3. Aesthetics Mapping (Axes & Dimensions)
      shiny::div(
        class = "mb-3 p-2 bg-white rounded border shadow-sm",
        shiny::tags$div(
          class = "small fw-bold text-dark mb-2 pb-1 border-bottom d-flex align-items-center gap-1",
          "Assignation des Axes & Dimensions"
        ),

        # Axe X
        shiny::div(
          class = "mb-2",
          shiny::tags$label(class = "form-label small mb-1 fw-semibold", "Axe X (Abscisse) :"),
          shiny::selectInput(
            inputId = ns("axis_x"),
            label = NULL,
            choices = NULL
          )
        ),

        # Axe Y (optionnel selon géométrie)
        shiny::div(
          class = "mb-2",
          shiny::tags$label(class = "form-label small mb-1 fw-semibold", "Axe Y (Ordonnée - optionnel en 1D) :"),
          shiny::selectInput(
            inputId = ns("axis_y"),
            label = NULL,
            choices = c("(Aucun - 1D / distribution)" = "")
          )
        ),

        # Statistique d'agrégation conditionnelle pour Barplot avec Axe Y quantitatif
        shiny::conditionalPanel(
          condition = "input.chart_type == 'bar' && input.axis_y != ''",
          ns = ns,
          shiny::div(
            class = "mb-2 p-2 bg-light rounded border",
            shiny::tags$label(class = "form-label small mb-1 fw-semibold text-dark", "Statistique d'agrégation (pour Axe Y) :"),
            shiny::selectInput(
              inputId = ns("bar_stat"),
              label = NULL,
              choices = c(
                "Moyenne" = "mean",
                "Somme" = "sum",
                "Médiane" = "median"
              ),
              selected = "mean"
            )
          )
        ),

        # Variable Couleur / Remplissage
        shiny::div(
          class = "mb-2",
          shiny::tags$label(class = "form-label small mb-1 fw-semibold", "Couleur / Remplissage (Color/Fill) :"),
          shiny::selectInput(
            inputId = ns("aesthetic_color"),
            label = NULL,
            choices = c("(Aucune)" = "")
          )
        ),

        # Variable Taille
        shiny::div(
          class = "mb-2",
          shiny::tags$label(class = "form-label small mb-1 fw-semibold", "Taille des points (Size) :"),
          shiny::selectInput(
            inputId = ns("aesthetic_size"),
            label = NULL,
            choices = c("(Aucune)" = "")
          )
        ),

        # Variable Facet (Découpage en sous-graphiques)
        shiny::div(
          class = "mb-1",
          shiny::tags$label(class = "form-label small mb-1 fw-semibold", "Découpage en facettes (Facet Wrap) :"),
          shiny::selectInput(
            inputId = ns("aesthetic_facet"),
            label = NULL,
            choices = c("(Aucun)" = "")
          )
        )
      ),

      # 4. Accordéon d'options avancées
      bslib::accordion(
        id = ns("accordion_advanced_options"),
        open = FALSE,
        class = "mb-3",

        # Panneau Titres & Légendes
        bslib::accordion_panel(
          title = "Titres & Légendes",
          shiny::textInput(
            inputId = ns("plot_title"),
            label = "Titre principal :",
            value = "",
            placeholder = "Ex: Relation entre Longueur et Largeur"
          ),
          shiny::textInput(
            inputId = ns("plot_subtitle"),
            label = "Sous-titre :",
            value = "",
            placeholder = "Ex: Données de mesures morphologiques"
          ),
          shiny::textInput(
            inputId = ns("plot_xlab"),
            label = "Étiquette de l'Axe X :",
            value = "",
            placeholder = "(Par défaut : nom de la variable)"
          ),
          shiny::textInput(
            inputId = ns("plot_ylab"),
            label = "Étiquette de l'Axe Y :",
            value = "",
            placeholder = "(Par défaut : nom de la variable ou effectif)"
          ),
          shiny::textInput(
            inputId = ns("plot_legend_title"),
            label = "Titre de la légende :",
            value = "",
            placeholder = "(Par défaut : nom de la variable couleur)"
          )
        ),

        # Panneau Étiquettes de données
        bslib::accordion_panel(
          title = "Étiquettes de données",
          shiny::checkboxInput(
            inputId = ns("show_data_labels"),
            label = "Afficher les valeurs numériques sur le graphique (geom_text)",
            value = FALSE
          ),
          shiny::sliderInput(
            inputId = ns("label_size"),
            label = "Taille de police des étiquettes :",
            min = 2, max = 8, value = 3.5, step = 0.5
          )
        ),

        # Panneau Thème & Styles
        bslib::accordion_panel(
          title = "Thèmes & Palettes",
          shiny::selectInput(
            inputId = ns("ggplot_theme"),
            label = "Thème ggplot2 :",
            choices = c(
              "Minimal (theme_minimal)" = "minimal",
              "Noir & Blanc (theme_bw)" = "bw",
              "Classique épuré (theme_classic)" = "classic",
              "Clair avec grille (theme_light)" = "light",
              "Sombre (theme_dark)" = "dark"
            ),
            selected = "minimal"
          ),
          shiny::selectInput(
            inputId = ns("color_palette"),
            label = "Palette de couleurs :",
            choices = c(
              "Viridis (Standard daltonien)" = "viridis",
              "ColorBrewer (Set1)" = "Set1",
              "ColorBrewer (Dark2)" = "Dark2",
              "ColorBrewer (Paired)" = "Paired",
              "Défaut ggplot2" = "default"
            ),
            selected = "viridis"
          ),
          shiny::sliderInput(
            inputId = ns("geom_alpha"),
            label = "Transparence (Alpha) :",
            min = 0.1, max = 1.0, value = 0.8, step = 0.05
          )
        )
      ),

      # 5. Bouton d'injection R Markdown
      shiny::actionButton(
        inputId = ns("btn_inject_rmd"),
        label = "Injecter dans le Journal R Markdown",
        class = "btn-success w-100 shadow-sm"
      )
    ),

    # Panneau Principal (Main Panel - 100% hauteur dédiée au graphique avec bouton modale Code R)
    shiny::div(
      class = "d-flex flex-column w-100 h-100",
      bslib::card(
        full_screen = TRUE,
        class = "shadow-sm flex-grow-1 h-100 border-0",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center py-2 bg-light border-bottom",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Visualisation Interactive"),
            shiny::tags$span(class = "badge bg-light text-dark border ms-1", "Plotly")
          ),
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::uiOutput(ns("plot_type_badge")),
            shiny::actionButton(
              inputId = ns("btn_show_r_code"),
              label = "Code R",
              class = "btn-outline-secondary btn-sm px-2 py-1 shadow-sm"
            )
          )
        ),
        bslib::card_body(
          padding = 0,
          shiny::div(
            style = "height: calc(100vh - 165px); min-height: 560px; width: 100%; display: flex; flex-direction: column;",
            plotly::plotlyOutput(ns("chart_output"), height = "100%", width = "100%")
          )
        )
      )
    )
  )
}

#' @title Logique serveur pour le module créateur de graphiques
#'
#' @description Gère l'interactivité du créateur graphique, la validation des variables,
#'   la construction dynamique du code ggplot2 et le rendu interactif Plotly.
#'
#' @param id Identifiant de namespace Shiny.
#' @param data_holder Réactif contenant le dataframe actif (\code{data_holder$df}) et son nom (\code{data_holder$name}).
#' @param append_to_rmd Fonction d'ajout au Journal R Markdown.
#' @return Un module serveur Shiny.
#' @export
mod_chart_builder_server <- function(id, data_holder, append_to_rmd) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper réactif : analyse des variables du dataset actif
    dataset_vars <- shiny::reactive({
      df <- data_holder$df
      if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
        return(list(numeric = character(0), categorical = character(0), all = character(0)))
      }
      col_types <- vapply(df, function(x) is.numeric(x) || is.integer(x), logical(1))
      num_cols <- names(df)[col_types]
      cat_cols <- names(df)[!col_types]
      list(
        numeric = num_cols,
        categorical = cat_cols,
        all = names(df)
      )
    })

    # Badge du nombre de variables
    output$var_count_badge <- shiny::renderUI({
      vars <- dataset_vars()
      shiny::tags$span(
        class = "badge bg-secondary",
        paste0(length(vars$all), " colonnes")
      )
    })

    # Résumé des variables groupées par type (style Tableau Data Pane)
    output$vars_summary_ui <- shiny::renderUI({
      vars <- dataset_vars()
      shiny::tagList(
        # Variables Numériques (Quantitatives)
        shiny::div(
          class = "mb-2",
          shiny::div(
            class = "d-flex align-items-center gap-1 text-secondary small fw-semibold mb-1",
            shiny::tags$span(class = "fw-bold", "[#]"),
            shiny::tags$span(paste0("Numériques (", length(vars$numeric), ")"))
          ),
          if (length(vars$numeric) > 0) {
            shiny::div(
              class = "d-flex flex-wrap gap-1",
              lapply(vars$numeric, function(v) {
                shiny::tags$span(
                  class = "badge bg-light text-dark border font-monospace text-truncate",
                  style = "max-width: 140px;",
                  title = v,
                  v
                )
              })
            )
          } else {
            shiny::tags$span(class = "text-muted small fst-italic", "Aucune variable quantitative")
          }
        ),

        # Variables Catégorielles (Qualitatives / Facteurs)
        shiny::div(
          shiny::div(
            class = "d-flex align-items-center gap-1 text-secondary small fw-semibold mb-1",
            shiny::tags$span(class = "fw-bold", "[Aa]"),
            shiny::tags$span(paste0("Catégorielles (", length(vars$categorical), ")"))
          ),
          if (length(vars$categorical) > 0) {
            shiny::div(
              class = "d-flex flex-wrap gap-1",
              lapply(vars$categorical, function(v) {
                shiny::tags$span(
                  class = "badge bg-light text-dark border font-monospace text-truncate",
                  style = "max-width: 140px;",
                  title = v,
                  v
                )
              })
            )
          } else {
            shiny::tags$span(class = "text-muted small fst-italic", "Aucune variable qualitative")
          }
        )
      )
    })

    # Mise à jour dynamique des choix des listes déroulantes dès que le dataset change
    shiny::observeEvent(data_holder$df, {
      vars <- dataset_vars()
      req(length(vars$all) > 0)

      # Choix pour Axe X
      current_x <- isolate(input$axis_x)
      sel_x <- if (!is.null(current_x) && current_x %in% vars$all) current_x else vars$all[1]
      shiny::updateSelectInput(
        session = session,
        inputId = "axis_x",
        choices = c(
          list("Numériques" = vars$numeric),
          list("Catégorielles" = vars$categorical)
        ),
        selected = sel_x
      )

      # Choix pour Axe Y
      current_y <- isolate(input$axis_y)
      sel_y <- if (!is.null(current_y) && (current_y == "" || current_y %in% vars$all)) {
        current_y
      } else if (length(vars$numeric) >= 2) {
        vars$numeric[2]
      } else {
        ""
      }

      shiny::updateSelectInput(
        session = session,
        inputId = "axis_y",
        choices = c(
          c("(Aucun - 1D / distribution)" = ""),
          list("Numériques" = vars$numeric),
          list("Catégorielles" = vars$categorical)
        ),
        selected = sel_y
      )

      # Choix Couleur
      current_col <- isolate(input$aesthetic_color)
      sel_col <- if (!is.null(current_col) && current_col %in% vars$all) current_col else ""
      shiny::updateSelectInput(
        session = session,
        inputId = "aesthetic_color",
        choices = c(
          c("(Aucune)" = ""),
          list("Catégorielles" = vars$categorical),
          list("Numériques" = vars$numeric)
        ),
        selected = sel_col
      )

      # Choix Taille
      current_sz <- isolate(input$aesthetic_size)
      sel_sz <- if (!is.null(current_sz) && current_sz %in% vars$numeric) current_sz else ""
      shiny::updateSelectInput(
        session = session,
        inputId = "aesthetic_size",
        choices = c(
          c("(Aucune)" = ""),
          list("Numériques" = vars$numeric)
        ),
        selected = sel_sz
      )

      # Choix Facet
      current_fct <- isolate(input$aesthetic_facet)
      sel_fct <- if (!is.null(current_fct) && current_fct %in% vars$categorical) current_fct else ""
      shiny::updateSelectInput(
        session = session,
        inputId = "aesthetic_facet",
        choices = c(
          c("(Aucun)" = ""),
          list("Catégorielles" = vars$categorical)
        ),
        selected = sel_fct
      )
    }, ignoreNULL = FALSE)

    # Ajustement automatique des sélections quand le type de graphique change
    shiny::observeEvent(input$chart_type, {
      vars <- dataset_vars()
      chart <- input$chart_type

      if (chart %in% c("histogram", "density")) {
        # Si histogramme ou densité, Axe Y est optionnel / implicite
        shiny::updateSelectInput(session, "axis_y", selected = "")
        if (length(vars$numeric) > 0 && !(input$axis_x %in% vars$numeric)) {
          shiny::updateSelectInput(session, "axis_x", selected = vars$numeric[1])
        }
      } else if (chart == "bar") {
        if (length(vars$categorical) > 0 && !(input$axis_x %in% vars$categorical)) {
          shiny::updateSelectInput(session, "axis_x", selected = vars$categorical[1])
        }
      } else if (chart %in% c("boxplot", "violin")) {
        if (length(vars$categorical) > 0 && !(input$axis_x %in% vars$categorical)) {
          shiny::updateSelectInput(session, "axis_x", selected = vars$categorical[1])
        }
        if (length(vars$numeric) > 0 && (is.null(input$axis_y) || input$axis_y == "" || !(input$axis_y %in% vars$numeric))) {
          if (chart == "violin" || length(vars$numeric) >= 1) {
            shiny::updateSelectInput(session, "axis_y", selected = vars$numeric[1])
          }
        }
      } else if (chart %in% c("scatter", "line")) {
        if (length(vars$numeric) >= 1 && !(input$axis_x %in% vars$numeric)) {
          shiny::updateSelectInput(session, "axis_x", selected = vars$numeric[1])
        }
        if (length(vars$numeric) >= 2 && (is.null(input$axis_y) || input$axis_y == "" || !(input$axis_y %in% vars$numeric))) {
          shiny::updateSelectInput(session, "axis_y", selected = vars$numeric[2])
        }
      } else if (chart == "heatmap") {
        if (length(vars$categorical) >= 2) {
          shiny::updateSelectInput(session, "axis_x", selected = vars$categorical[1])
          shiny::updateSelectInput(session, "axis_y", selected = vars$categorical[2])
        }
      }
    })

    # Badge affichant le nom du type de graphique
    output$plot_type_badge <- shiny::renderUI({
      type_labels <- c(
        scatter = "Nuage de points",
        bar = "Diagramme en barres",
        boxplot = "Boîte à moustaches",
        violin = "Diagramme en violon",
        histogram = "Histogramme",
        line = "Courbe / Ligne",
        density = "Densité",
        heatmap = "Carte thermique"
      )
      lbl <- type_labels[[input$chart_type]]
      if (is.null(lbl)) lbl <- "Graphique"
      shiny::tags$span(class = "badge bg-dark text-white", lbl)
    })

    # Générateur de code ggplot2 réactif
    generated_code <- shiny::reactive({
      df <- data_holder$df
      df_name <- data_holder$name
      if (is.null(df_name) || df_name == "") df_name <- "dataset"

      x_var <- input$axis_x
      y_var <- input$axis_y
      col_var <- input$aesthetic_color
      size_var <- input$aesthetic_size
      facet_var <- input$aesthetic_facet
      chart_type <- input$chart_type
      alpha_val <- input$geom_alpha
      theme_choice <- input$ggplot_theme
      palette_choice <- input$color_palette
      show_labels <- input$show_data_labels
      label_size <- input$label_size

      # Validation de base
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return("# Aucun jeu de données actif disponible.")
      }
      if (is.null(x_var) || x_var == "" || !(x_var %in% names(df))) {
        return("# En attente de la sélection des variables pour générer le code ggplot2...")
      }

      has_y <- !is.null(y_var) && y_var != "" && (y_var %in% names(df))
      x_is_num <- is.numeric(df[[x_var]]) || is.integer(df[[x_var]])
      y_is_num <- if (has_y) is.numeric(df[[y_var]]) || is.integer(df[[y_var]]) else FALSE

      # Validation spécifique selon le type de graphique
      needs_y <- chart_type %in% c("scatter", "line", "violin", "heatmap")
      if (needs_y && !has_y) {
        return(paste0("# Veuillez sélectionner une variable pour l'Axe Y afin de générer le code du graphique '", chart_type, "'."))
      }

      # Identification du cas Boxplot et Barplot
      is_boxplot_univariate <- FALSE
      is_boxplot_inverted <- FALSE
      is_bar_aggregated <- (chart_type == "bar" && has_y && y_is_num)

      if (chart_type == "boxplot") {
        if (!has_y) {
          # CAS 1 : Seule la variable X est renseignée et X est continue
          if (!x_is_num) {
            return("# Pour une boîte à moustaches univariée (sans Axe Y), la variable de l'Axe X doit être quantitative continue.")
          }
          is_boxplot_univariate <- TRUE
        } else {
          if (x_is_num && !y_is_num) {
            # CAS 3 : Variable X continue et variable Y catégorielle -> inversion
            is_boxplot_inverted <- TRUE
          } else if (!x_is_num && !y_is_num) {
            return("# Pour une boîte à moustaches, au moins l'une des deux variables doit être quantitative continue.")
          }
        }
      }

      bar_stat_choice <- if (!is.null(input$bar_stat) && input$bar_stat != "") input$bar_stat else "mean"
      bar_stat_label <- switch(bar_stat_choice, "mean" = "Moyenne", "sum" = "Somme", "median" = "Médiane", "Moyenne")

      # Construction du code de pré-agrégation si Barplot avec Y
      code_lines <- c(
        if (is_bar_aggregated) "library(dplyr)" else NULL,
        "library(ggplot2)",
        "library(plotly)",
        ""
      )
      code_lines <- code_lines[!vapply(code_lines, is.null, logical(1))]

      if (is_bar_aggregated) {
        agg_grp_vars <- x_var
        if (!is.null(col_var) && col_var != "" && col_var %in% names(df)) {
          agg_grp_vars <- c(agg_grp_vars, col_var)
        }
        code_lines <- c(
          code_lines,
          paste0("# Pré-agrégation des données pour le diagramme en barres (", bar_stat_label, ")"),
          paste0("df_agg <- ", df_name, " %>%"),
          paste0("  dplyr::filter(!is.na(", x_var, "), !is.na(", y_var, ")) %>%"),
          paste0("  dplyr::group_by(", paste(agg_grp_vars, collapse = ", "), ") %>%"),
          paste0("  dplyr::summarise(", y_var, "_", bar_stat_choice, " = ", bar_stat_choice, "(", y_var, ", na.rm = TRUE), .groups = 'drop')"),
          ""
        )
      }

      # Construction de l'aes()
      if (chart_type == "boxplot" && is_boxplot_univariate) {
        # CAS 1 : Mappe automatiquement la variable X sur l'axe Y : aes(x = "", y = VariableX)
        aes_parts <- c('x = ""', paste0("y = ", x_var))
      } else if (chart_type == "boxplot" && is_boxplot_inverted) {
        # CAS 3 : Variable X continue et variable Y catégorielle -> aes(x = VariableY, y = VariableX)
        aes_parts <- c(paste0("x = ", y_var), paste0("y = ", x_var), paste0("group = ", y_var))
      } else if (chart_type %in% c("boxplot", "violin") && has_y) {
        aes_parts <- c(paste0("x = ", x_var), paste0("y = ", y_var), paste0("group = ", x_var))
      } else if (is_bar_aggregated) {
        aes_parts <- c(paste0("x = ", x_var), paste0("y = ", y_var, "_", bar_stat_choice))
      } else {
        # CAS standard
        aes_parts <- c(paste0("x = ", x_var))
        if (has_y) {
          aes_parts <- c(aes_parts, paste0("y = ", y_var))
        }
      }

      if (!is.null(col_var) && col_var != "" && col_var %in% names(df)) {
        if (chart_type %in% c("bar", "histogram", "density", "boxplot", "violin")) {
          aes_parts <- c(aes_parts, paste0("fill = ", col_var))
        } else {
          aes_parts <- c(aes_parts, paste0("color = ", col_var))
        }
      }
      if (!is.null(size_var) && size_var != "" && size_var %in% names(df) && chart_type == "scatter") {
        aes_parts <- c(aes_parts, paste0("size = ", size_var))
      }

      aes_str <- paste(aes_parts, collapse = ", ")
      data_source_str <- if (is_bar_aggregated) "df_agg" else df_name
      code_lines <- c(
        code_lines,
        paste0("p <- ggplot(data = ", data_source_str, ", aes(", aes_str, ")) +")
      )

      # Couche géométrique (geom_...)
      has_col_aes <- !is.null(col_var) && col_var != "" && col_var %in% names(df)
      geom_line <- switch(
        chart_type,
        "scatter" = paste0("  geom_point(alpha = ", alpha_val, ") +"),
        "bar" = if (!has_y) {
          paste0("  geom_bar(alpha = ", alpha_val, ", color = 'white'", if (has_col_aes) ", position = 'dodge'" else "", ") +")
        } else {
          paste0("  geom_col(alpha = ", alpha_val, ", color = 'white'", if (has_col_aes) ", position = 'dodge'" else "", ") +")
        },
        "boxplot" = paste0("  geom_boxplot(alpha = ", alpha_val, ", outlier.size = 1.5) +"),
        "violin" = paste0("  geom_violin(alpha = ", alpha_val, ", trim = FALSE) +"),
        "histogram" = paste0("  geom_histogram(alpha = ", alpha_val, ", color = 'white', bins = 30) +"),
        "line" = paste0("  geom_line(alpha = ", alpha_val, ") +"),
        "density" = paste0("  geom_density(alpha = ", alpha_val, ") +"),
        "heatmap" = paste0("  geom_tile() +")
      )
      code_lines <- c(code_lines, geom_line)

      # Étiquettes de données (Data Labels)
      if (isTRUE(show_labels)) {
        if (is_bar_aggregated) {
          code_lines <- c(code_lines, paste0("  geom_text(aes(label = round(", y_var, "_", bar_stat_choice, ", 2)), vjust = -0.5, size = ", label_size, if (has_col_aes) ", position = position_dodge(width = 0.9)" else "", ") +"))
        } else {
          target_label_var <- if (chart_type == "boxplot" && (is_boxplot_univariate || is_boxplot_inverted)) x_var else y_var
          if (!is.null(target_label_var) && target_label_var != "" && target_label_var %in% names(df) && is.numeric(df[[target_label_var]])) {
            code_lines <- c(code_lines, paste0("  geom_text(aes(label = round(", target_label_var, ", 2)), vjust = -0.5, size = ", label_size, ") +"))
          }
        }
      }

      # Palette de couleurs
      if (!is.null(col_var) && col_var != "" && col_var %in% names(df)) {
        if (chart_type %in% c("bar", "histogram", "density", "boxplot", "violin")) {
          if (palette_choice == "viridis") {
            code_lines <- c(code_lines, "  scale_fill_viridis_d(option = 'D') +")
          } else if (palette_choice %in% c("Set1", "Dark2", "Paired")) {
            code_lines <- c(code_lines, paste0("  scale_fill_brewer(palette = '", palette_choice, "') +"))
          }
        } else {
          if (palette_choice == "viridis") {
            code_lines <- c(code_lines, "  scale_color_viridis_d(option = 'D') +")
          } else if (palette_choice %in% c("Set1", "Dark2", "Paired")) {
            code_lines <- c(code_lines, paste0("  scale_color_brewer(palette = '", palette_choice, "') +"))
          }
        }
      }

      # Titres & Labels
      title_str <- if (!is.null(input$plot_title) && input$plot_title != "") {
        input$plot_title
      } else {
        if (chart_type == "boxplot" && is_boxplot_univariate) {
          paste0("Boîte à moustaches : ", x_var)
        } else if (is_bar_aggregated) {
          paste0(bar_stat_label, " de ", y_var, " selon ", x_var)
        } else {
          paste0("Graphique : ", chart_type, " de ", x_var, if (has_y) paste0(" vs ", y_var) else "")
        }
      }
      subtitle_str <- if (!is.null(input$plot_subtitle) && input$plot_subtitle != "") input$plot_subtitle else NULL

      if (chart_type == "boxplot" && is_boxplot_univariate) {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else ""
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else x_var
      } else if (chart_type == "boxplot" && is_boxplot_inverted) {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else y_var
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else x_var
      } else if (is_bar_aggregated) {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else x_var
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else paste0(bar_stat_label, " de ", y_var)
      } else {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else x_var
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else (if (has_y) y_var else "Effectifs (N)")
      }

      legend_str <- if (!is.null(input$plot_legend_title) && input$plot_legend_title != "") input$plot_legend_title else col_var

      labs_parts <- c(paste0("title = \"", title_str, "\""))
      if (!is.null(subtitle_str)) labs_parts <- c(labs_parts, paste0("subtitle = \"", subtitle_str, "\""))
      labs_parts <- c(labs_parts, paste0("x = \"", xlab_str, "\""))
      labs_parts <- c(labs_parts, paste0("y = \"", ylab_str, "\""))
      if (!is.null(legend_str) && legend_str != "") {
        if (chart_type %in% c("bar", "histogram", "density", "boxplot", "violin")) {
          labs_parts <- c(labs_parts, paste0("fill = \"", legend_str, "\""))
        } else {
          labs_parts <- c(labs_parts, paste0("color = \"", legend_str, "\""))
        }
      }
      code_lines <- c(code_lines, paste0("  labs(", paste(labs_parts, collapse = ", "), ") +"))

      # Thème
      theme_func <- paste0("theme_", theme_choice, "()")
      code_lines <- c(code_lines, paste0("  ", theme_func))

      # Facet
      if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(df)) {
        code_lines[length(code_lines)] <- paste0(code_lines[length(code_lines)], " +")
        code_lines <- c(code_lines, paste0("  facet_wrap(~ ", facet_var, ")"))
      }

      # Conversion plotly
      code_lines <- c(
        code_lines,
        "",
        "# Rendu interactif avec Plotly",
        "interactive_plot <- plotly::ggplotly(p)",
        "interactive_plot"
      )

      paste(code_lines, collapse = "\n")
    })

    # Modale d'affichage du code R ggplot2 reproductible
    shiny::observeEvent(input$btn_show_r_code, {
      code_text <- generated_code()
      shiny::showModal(
        shiny::modalDialog(
          title = shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$span(style = "font-weight: 600; color: #111827;", "Code R Reproductible (ggplot2 & Plotly)")
          ),
          size = "l",
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Fermer"),
            shiny::actionButton(
              inputId = ns("btn_copy_ggplot_code"),
              label = "Copier le code",
              class = "btn-dark"
            )
          ),
          shiny::div(
            class = "mb-2 d-flex justify-content-between align-items-center small text-muted",
            shiny::tags$span("Ce code est mis à jour réactivement en direct selon tous vos réglages esthétiques."),
            shiny::tags$span(class = "badge bg-light text-dark border", "ggplot2 + plotly")
          ),
          shiny::tags$pre(
            class = "p-3 rounded font-monospace small mb-0",
            style = "max-height: 420px; overflow-y: auto; line-height: 1.55; font-size: 13px; background-color: #F3F4F6 !important; color: #111827 !important; border: 1px solid #E5E7EB !important;",
            code_text
          )
        )
      )
    })

    # Copie du code dans le presse-papiers
    shiny::observeEvent(input$btn_copy_ggplot_code, {
      shiny::showNotification("Code R ggplot2 copié dans le presse-papiers !", type = "message", duration = 3)
    })

    # Construction de l'objet ggplot2 réactif réel
    build_ggplot_object <- shiny::reactive({
      df <- data_holder$df
      shiny::validate(
        shiny::need(!is.null(df) && is.data.frame(df) && nrow(df) > 0, "Aucun jeu de données actif disponible."),
        shiny::need(!is.null(input$axis_x) && input$axis_x != "" && input$axis_x %in% names(df), "Veuillez sélectionner au moins une variable sur l'Axe X.")
      )

      x_var <- input$axis_x
      y_var <- input$axis_y
      col_var <- input$aesthetic_color
      size_var <- input$aesthetic_size
      facet_var <- input$aesthetic_facet
      chart_type <- input$chart_type
      alpha_val <- input$geom_alpha
      theme_choice <- input$ggplot_theme
      palette_choice <- input$color_palette
      show_labels <- input$show_data_labels
      label_size <- input$label_size

      has_y <- !is.null(y_var) && y_var != "" && (y_var %in% names(df))
      x_is_num <- is.numeric(df[[x_var]]) || is.integer(df[[x_var]])
      y_is_num <- if (has_y) is.numeric(df[[y_var]]) || is.integer(df[[y_var]]) else FALSE

      # Validation stricte des variables obligatoires selon le type de graphique
      if (chart_type %in% c("scatter", "line")) {
        shiny::validate(
          shiny::need(has_y, "Ce type de graphique (2D) requiert la sélection d'une variable pour l'Axe Y (Ordonnée).")
        )
      } else if (chart_type == "violin") {
        shiny::validate(
          shiny::need(has_y && y_is_num, "Le diagramme en violon requiert la sélection d'une variable quantitative pour l'Axe Y.")
        )
      } else if (chart_type == "heatmap") {
        shiny::validate(
          shiny::need(has_y, "La carte thermique requiert la sélection d'une variable pour l'Axe Y.")
        )
      } else if (chart_type == "boxplot") {
        if (!has_y) {
          # CAS 1 : Seule la variable X est renseignée
          shiny::validate(
            shiny::need(x_is_num, "Pour une boîte à moustaches univariée (sans Axe Y), la variable de l'Axe X doit être quantitative continue.")
          )
        } else {
          # CAS 2 & CAS 3 : Au moins une variable quantitative
          shiny::validate(
            shiny::need(x_is_num || y_is_num, "Pour une boîte à moustaches, au moins l'une des deux variables (X ou Y) doit être quantitative continue.")
          )
        }
      }

      # Détermination du mode Boxplot et Barplot
      is_boxplot_univariate <- (chart_type == "boxplot" && !has_y)
      is_boxplot_inverted <- (chart_type == "boxplot" && has_y && x_is_num && !y_is_num)
      is_bar_aggregated <- (chart_type == "bar" && has_y && y_is_num)

      bar_stat_choice <- if (!is.null(input$bar_stat) && input$bar_stat != "") input$bar_stat else "mean"
      bar_stat_label <- switch(bar_stat_choice, "mean" = "Moyenne", "sum" = "Somme", "median" = "Médiane", "Moyenne")

      # Préparation du jeu de données nettoyé
      clean_df <- df[!is.na(df[[x_var]]), ]
      if (has_y) {
        clean_df <- clean_df[!is.na(clean_df[[y_var]]), ]
      }
      if (!is.null(col_var) && col_var != "" && col_var %in% names(clean_df)) {
        clean_df <- clean_df[!is.na(clean_df[[col_var]]), ]
      }

      # Traitement spécifique pour le Barplot avec agrégation
      if (is_bar_aggregated) {
        agg_grp_vars <- x_var
        if (!is.null(col_var) && col_var != "" && col_var %in% names(clean_df)) {
          agg_grp_vars <- unique(c(agg_grp_vars, col_var))
        }

        stat_func <- match.fun(bar_stat_choice)
        plot_data <- clean_df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(agg_grp_vars))) %>%
          dplyr::summarise(
            Y_agg = stat_func(.data[[y_var]], na.rm = TRUE),
            .groups = "drop"
          )
        plot_data[[x_var]] <- droplevels(as.factor(plot_data[[x_var]]))

        aes_args <- list(x = rlang::sym(x_var), y = quote(Y_agg))
        if (!is.null(col_var) && col_var != "" && col_var %in% names(plot_data)) {
          aes_args$fill <- rlang::sym(col_var)
        }
      } else {
        plot_data <- clean_df

        # Préparation des aesthetics pour les autres types
        if (is_boxplot_univariate) {
          # CAS 1 : Boxplot 1D -> x = "", y = VariableX
          aes_args <- list(x = "", y = rlang::sym(x_var))
        } else if (is_boxplot_inverted) {
          # CAS 3 : X continue, Y catégorielle
          plot_data[[y_var]] <- droplevels(as.factor(plot_data[[y_var]]))
          aes_args <- list(x = rlang::sym(y_var), y = rlang::sym(x_var), group = rlang::sym(y_var))
        } else if (chart_type %in% c("boxplot", "violin") && has_y) {
          plot_data[[x_var]] <- droplevels(as.factor(plot_data[[x_var]]))
          aes_args <- list(x = rlang::sym(x_var), y = rlang::sym(y_var), group = rlang::sym(x_var))
        } else if (chart_type == "bar") {
          plot_data[[x_var]] <- droplevels(as.factor(plot_data[[x_var]]))
          aes_args <- list(x = rlang::sym(x_var))
        } else {
          # CAS standard
          aes_args <- list(x = rlang::sym(x_var))
          if (has_y) {
            aes_args$y <- rlang::sym(y_var)
          }
        }

        if (!is.null(col_var) && col_var != "" && col_var %in% names(plot_data)) {
          if (chart_type %in% c("bar", "histogram", "density", "boxplot", "violin")) {
            aes_args$fill <- rlang::sym(col_var)
          } else {
            aes_args$color <- rlang::sym(col_var)
          }
        }
        if (!is.null(size_var) && size_var != "" && size_var %in% names(plot_data) && chart_type == "scatter") {
          aes_args$size <- rlang::sym(size_var)
        }
      }

      p <- ggplot2::ggplot(plot_data, do.call(ggplot2::aes, aes_args))

      has_col_aes <- !is.null(col_var) && col_var != "" && col_var %in% names(plot_data)

      # Ajout de la géométrie
      p <- switch(
        chart_type,
        "scatter" = p + ggplot2::geom_point(alpha = alpha_val),
        "bar" = if (!has_y) {
          p + ggplot2::geom_bar(alpha = alpha_val, color = "white", position = if (has_col_aes) "dodge" else "stack")
        } else {
          p + ggplot2::geom_col(alpha = alpha_val, color = "white", position = if (has_col_aes) "dodge" else "stack")
        },
        "boxplot" = p + ggplot2::geom_boxplot(alpha = alpha_val, outlier.size = 1.5),
        "violin" = p + ggplot2::geom_violin(alpha = alpha_val, trim = FALSE),
        "histogram" = p + ggplot2::geom_histogram(alpha = alpha_val, color = "white", bins = 30),
        "line" = p + ggplot2::geom_line(alpha = alpha_val),
        "density" = p + ggplot2::geom_density(alpha = alpha_val),
        "heatmap" = p + ggplot2::geom_tile(),
        if (has_y) {
          p + ggplot2::geom_point(alpha = alpha_val)
        } else {
          p + ggplot2::geom_bar(alpha = alpha_val, color = "white")
        }
      )

      # Labels de données (Data Labels)
      if (isTRUE(show_labels)) {
        if (is_bar_aggregated) {
          p <- p + ggplot2::geom_text(
            ggplot2::aes(label = round(Y_agg, 2)),
            vjust = -0.5,
            size = label_size,
            position = if (has_col_aes) ggplot2::position_dodge(width = 0.9) else "identity"
          )
        } else {
          target_label_var <- if (chart_type == "boxplot" && (is_boxplot_univariate || is_boxplot_inverted)) x_var else y_var
          if (!is.null(target_label_var) && target_label_var != "" && target_label_var %in% names(plot_data) && is.numeric(plot_data[[target_label_var]])) {
            p <- p + ggplot2::geom_text(
              ggplot2::aes(label = round(!!rlang::sym(target_label_var), 2)),
              vjust = -0.5,
              size = label_size
            )
          }
        }
      }

      # Palettes
      if (!is.null(col_var) && col_var != "" && col_var %in% names(plot_data)) {
        is_num_col <- is.numeric(plot_data[[col_var]])
        if (chart_type %in% c("bar", "histogram", "density", "boxplot", "violin")) {
          if (palette_choice == "viridis") {
            p <- if (is_num_col) p + ggplot2::scale_fill_viridis_c() else p + ggplot2::scale_fill_viridis_d()
          } else if (palette_choice %in% c("Set1", "Dark2", "Paired") && !is_num_col) {
            p <- p + ggplot2::scale_fill_brewer(palette = palette_choice)
          }
        } else {
          if (palette_choice == "viridis") {
            p <- if (is_num_col) p + ggplot2::scale_color_viridis_c() else p + ggplot2::scale_color_viridis_d()
          } else if (palette_choice %in% c("Set1", "Dark2", "Paired") && !is_num_col) {
            p <- p + ggplot2::scale_color_brewer(palette = palette_choice)
          }
        }
      }

      # Titres & Légendes
      title_str <- if (!is.null(input$plot_title) && input$plot_title != "") {
        input$plot_title
      } else {
        if (chart_type == "boxplot" && is_boxplot_univariate) {
          paste0("Boîte à moustaches : ", x_var)
        } else if (is_bar_aggregated) {
          paste0(bar_stat_label, " de ", y_var, " selon ", x_var)
        } else {
          paste0(chart_type, " : ", x_var, if (has_y) paste0(" vs ", y_var) else "")
        }
      }

      subtitle_str <- if (!is.null(input$plot_subtitle) && input$plot_subtitle != "") input$plot_subtitle else NULL

      if (chart_type == "boxplot" && is_boxplot_univariate) {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else ""
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else x_var
      } else if (chart_type == "boxplot" && is_boxplot_inverted) {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else y_var
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else x_var
      } else if (is_bar_aggregated) {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else x_var
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else paste0(bar_stat_label, " de ", y_var)
      } else {
        xlab_str <- if (!is.null(input$plot_xlab) && input$plot_xlab != "") input$plot_xlab else x_var
        ylab_str <- if (!is.null(input$plot_ylab) && input$plot_ylab != "") input$plot_ylab else (if (has_y) y_var else "Effectifs (N)")
      }

      labs_args <- list(title = title_str, x = xlab_str, y = ylab_str)
      if (!is.null(subtitle_str)) labs_args$subtitle <- subtitle_str
      if (!is.null(input$plot_legend_title) && input$plot_legend_title != "") {
        if (chart_type %in% c("bar", "histogram", "density", "boxplot", "violin")) {
          labs_args$fill <- input$plot_legend_title
        } else {
          labs_args$color <- input$plot_legend_title
        }
      }
      p <- p + do.call(ggplot2::labs, labs_args)

      # Thème ggplot2
      p <- switch(
        theme_choice,
        "minimal" = p + ggplot2::theme_minimal(),
        "bw" = p + ggplot2::theme_bw(),
        "classic" = p + ggplot2::theme_classic(),
        "light" = p + ggplot2::theme_light(),
        "dark" = p + ggplot2::theme_dark(),
        p + ggplot2::theme_minimal()
      )

      # Facet
      if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(df)) {
        p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))
      }

      p
    })

    # Rendu Plotly interactif
    output$chart_output <- plotly::renderPlotly({
      p <- build_ggplot_object()

      tryCatch({
        plotly::ggplotly(p) %>%
          plotly::layout(
            autosize = TRUE,
            margin = list(l = 50, r = 30, b = 50, t = 60)
          ) %>%
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            modeBarButtonsToRemove = c("sendDataToCloud", "lasso2d")
          )
      }, error = function(e) {
        fallback_p <- ggplot2::ggplot() +
          ggplot2::annotate(
            "text", x = 0.5, y = 0.5,
            label = paste0("Impossible d'afficher le graphique interactif :\n", e$message, "\n\nVeuillez ajuster les dimensions sélectionnées."),
            color = "#4B5563", size = 4, hjust = 0.5
          ) +
          ggplot2::theme_void()

        plotly::ggplotly(fallback_p) %>%
          plotly::layout(autosize = TRUE)
      })
    })

    # Injection dans le Journal R Markdown
    shiny::observeEvent(input$btn_inject_rmd, {
      code_str <- generated_code()
      title_label <- if (!is.null(input$plot_title) && input$plot_title != "") {
        input$plot_title
      } else {
        paste0("Visualisation ", input$chart_type, " (", input$axis_x, ")")
      }

      append_to_rmd(
        title = paste0("Visualisation : ", title_label),
        code = code_str
      )

      shiny::showNotification(
        paste0("Graphique '", title_label, "' injecté avec succès dans le Journal R Markdown !"),
        type = "message",
        duration = 4
      )
    })
  })
}
