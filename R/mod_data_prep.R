# ==============================================================================
# Module Shiny : Preparation et Nettoyage des donnees
# Fichier : R/mod_data_prep.R
# ==============================================================================

#' Interface utilisateur du module de preparation des donnees
#'
#' @param id Identifiant de module Shiny
#' @return Interface utilisateur Shiny (tagList / layout)
#' @export
mod_data_prep_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$span(style = "font-weight: 700; color: #111827;", "Pr\u00e9paration des donn\u00e9es")
      ),
      width = 300,
      shiny::div(
        class = "text-muted small mb-2",
        "Op\u00e9rations de nettoyage, transformation et filtrage sur le jeu de donn\u00e9es actif."
      ),
      shiny::uiOutput(ns("sidebar_summary")),
      shiny::hr(class = "my-2"),

      # Actions de gestion du pipeline
      shiny::actionButton(
        inputId = ns("btn_undo_step"),
        label = "Annuler derni\u00e8re op\u00e9ration",
        class = "btn-outline-secondary btn-sm w-100 mb-2"
      ),
      shiny::actionButton(
        inputId = ns("btn_reset_dataset"),
        label = "R\u00e9initialiser aux donn\u00e9es brutes",
        class = "btn-outline-danger btn-sm w-100 mb-3"
      ),

      shiny::tags$h6(class = "fw-bold small text-dark mb-2", "Historique du pipeline :"),
      shiny::uiOutput(ns("pipeline_history_list"))
    ),

    # Contenu principal avec les 4 sous-sections
    bslib::navset_card_tab(
      id = ns("prep_main_tabs"),

      # ------------------------------------------------------------------------
      # 1. EXPLORER
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("magnifying-glass", fill = "#6B7280", height = "0.85em"), "Explorer")),
        value = "tab_explore",
        bslib::card_body(
          fillable = FALSE,
          class = "p-3 d-flex flex-column gap-4",
          shiny::div(
            class = "row row-cols-2 row-cols-md-3 row-cols-lg-5 g-3",
            shiny::div(
              class = "col",
              bslib::value_box(
                title = "Observations",
                value = shiny::textOutput(ns("box_rows")),
                showcase = fontawesome::fa("table-cells", fill = "#9CA3AF", height = "1.5em"),
                theme = "light"
              )
            ),
            shiny::div(
              class = "col",
              bslib::value_box(
                title = "Variables",
                value = shiny::textOutput(ns("box_cols")),
                showcase = fontawesome::fa("columns", fill = "#9CA3AF", height = "1.5em"),
                theme = "light"
              )
            ),
            shiny::div(
              class = "col",
              bslib::value_box(
                title = "Valeurs Manquantes",
                value = shiny::textOutput(ns("box_nas")),
                showcase = fontawesome::fa("circle-question", fill = "#9CA3AF", height = "1.5em"),
                theme = "light"
              )
            ),
            shiny::div(
              class = "col",
              bslib::value_box(
                title = "Lignes en Doublon",
                value = shiny::textOutput(ns("box_duplicates")),
                showcase = fontawesome::fa("copy", fill = "#9CA3AF", height = "1.5em"),
                theme = "light"
              )
            ),
            shiny::div(
              class = "col",
              bslib::value_box(
                title = "Valeurs Aberrantes",
                value = shiny::textOutput(ns("box_outliers")),
                showcase = fontawesome::fa("triangle-exclamation", fill = "#9CA3AF", height = "1.5em"),
                theme = "light"
              )
            )
          ),

          # 1. Diagnostic et types des variables
          shiny::div(
            class = "card border shadow-sm w-100",
            shiny::div(
              class = "card-header bg-white py-2 px-3 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
              fontawesome::fa("list-check", fill = "#1F2937"),
              "Diagnostic et types des variables"
            ),
            shiny::div(
              class = "card-body p-3",
              style = "overflow-x: auto; overflow-y: auto; max-height: 450px; width: 100%; position: relative;",
              DT::dataTableOutput(ns("table_diagnosis"))
            )
          ),

          # 2. Donnees apres traitement
          shiny::div(
            class = "card border shadow-sm w-100",
            shiny::div(
              class = "card-header bg-white py-2 px-3 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
              fontawesome::fa("table", fill = "#1F2937"),
              "Donn\u00e9es apr\u00e8s traitement"
            ),
            shiny::div(
              class = "card-body p-3",
              style = "overflow-x: auto; overflow-y: auto; max-height: 550px; width: 100%; position: relative;",
              DT::dataTableOutput(ns("table_preview"))
            )
          )
        )
      ),

      # ------------------------------------------------------------------------
      # 2. MODIFIER LES VARIABLES
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("pen-to-square", fill = "#6B7280", height = "0.85em"), "Modifier les variables")),
        value = "tab_modify",
        bslib::card_body(
          class = "p-3",
          shiny::div(
            class = "row g-3",
            shiny::div(
              class = "col-md-4",
              shiny::selectInput(
                inputId = ns("mod_action_type"),
                label = "Action sur les variables :",
                choices = c(
                  "Renommer une variable" = "rename",
                  "Supprimer des variables" = "drop",
                  "R\u00e9organiser l'ordre" = "reorder",
                  "Changer le type" = "cast",
                  "Recoder des modalit\u00e9s" = "recode",
                  "Remplacer des valeurs" = "replace_val",
                  "Cr\u00e9er une variable calcul\u00e9e" = "compute"
                ),
                selected = "rename"
              ),
              shiny::uiOutput(ns("mod_action_controls")),
              shiny::actionButton(
                inputId = ns("btn_apply_mod_action"),
                label = "Appliquer la modification",
                class = "btn-dark btn-sm w-100 mt-3 shadow-sm"
              )
            ),
            shiny::div(
              class = "col-md-8",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Aper\u00e7u du r\u00e9sultat apr\u00e8s modification :"),
              shiny::uiOutput(ns("mod_action_preview_info")),
              DT::dataTableOutput(ns("mod_action_preview_table"))
            )
          )
        )
      ),

      # ------------------------------------------------------------------------
      # 3. NETTOYER
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("broom", fill = "#6B7280", height = "0.85em"), "Nettoyer")),
        value = "tab_clean",
        bslib::card_body(
          class = "p-3",
          shiny::div(
            class = "row g-3",
            shiny::div(
              class = "col-md-4",
              shiny::selectInput(
                inputId = ns("clean_action_type"),
                label = "Type de nettoyage :",
                choices = c(
                  "Valeurs manquantes (NA)" = "impute_na",
                  "Supprimer les doublons" = "deduplicate",
                  "Nettoyer les espaces" = "trim_ws",
                  "Modifier la casse du texte" = "change_case"
                ),
                selected = "impute_na"
              ),
              shiny::uiOutput(ns("clean_action_controls")),
              shiny::actionButton(
                inputId = ns("btn_apply_clean_action"),
                label = "Appliquer le nettoyage",
                class = "btn-dark btn-sm w-100 mt-3 shadow-sm"
              )
            ),
            shiny::div(
              class = "col-md-8",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Aper\u00e7u des donn\u00e9es nettoy\u00e9es :"),
              shiny::uiOutput(ns("clean_action_preview_info")),
              DT::dataTableOutput(ns("clean_action_preview_table"))
            )
          )
        )
      ),

      # ------------------------------------------------------------------------
      # 4. FILTRER ET TRIER
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("filter", fill = "#6B7280", height = "0.85em"), "Filtrer & Trier")),
        value = "tab_filter_sort",
        bslib::card_body(
          class = "p-3",
          shiny::div(
            class = "row g-3",
            shiny::div(
              class = "col-md-4",
              shiny::selectInput(
                inputId = ns("filter_sort_mode"),
                label = "Op\u00e9ration :",
                choices = c(
                  "Filtrer les observations (lignes)" = "filter",
                  "Trier les observations" = "sort",
                  "S\u00e9lectionner les variables (colonnes)" = "select_vars"
                ),
                selected = "filter"
              ),
              shiny::uiOutput(ns("filter_sort_controls")),
              shiny::actionButton(
                inputId = ns("btn_apply_filter_sort"),
                label = "Appliquer le filtre / tri",
                class = "btn-dark btn-sm w-100 mt-3 shadow-sm"
              )
            ),
            shiny::div(
              class = "col-md-8",
              shiny::tags$h6(class = "fw-bold text-dark mb-2", "Aper\u00e7u des observations retenues / tri\u00e9es :"),
              shiny::uiOutput(ns("filter_sort_preview_info")),
              DT::dataTableOutput(ns("filter_sort_preview_table"))
            )
          )
        )
      )
    )
  )
}

#' Logique serveur du module de preparation des donnees
#'
#' @param id Identifiant de module Shiny
#' @param data_holder Objet reactiveValues contenant name et df
#' @param append_to_rmd Fonction de journalisation Rmd
#' @export
mod_data_prep_server <- function(id, data_holder, append_to_rmd) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Historique reactif du pipeline de preparation
    pipeline_state <- shiny::reactiveValues(
      history = list(),
      initial_df = NULL,
      initial_name = NULL
    )

    # Initialisation / synchronisation avec le dataset de depart
    shiny::observe({
      df <- data_holder$df
      if (!is.null(df) && is.null(pipeline_state$initial_df)) {
        pipeline_state$initial_df <- df
        pipeline_state$initial_name <- data_holder$name
      }
    })

    # Diagnostic reactif
    diag_data <- shiny::reactive({
      df <- data_holder$df
      shiny::req(df)
      ramses_prep_diagnose(df)
    })

    # Sommaire dans la barre laterale
    output$sidebar_summary <- shiny::renderUI({
      df <- data_holder$df
      shiny::req(df)
      d <- diag_data()
      ov <- d$overview
      shiny::tagList(
        shiny::tags$ul(
          class = "list-unstyled small mb-0",
          shiny::tags$li(shiny::tags$strong("Jeu : "), data_holder$name),
          shiny::tags$li(shiny::tags$strong("Dimensions : "), paste0(ov$n_rows, " obs. x ", ov$n_cols, " var.")),
          shiny::tags$li(shiny::tags$strong("Compl\u00e9tude : "), paste0(ov$pct_complete, "% cas complets")),
          shiny::tags$li(shiny::tags$strong("Doublons : "), paste0(ov$n_duplicates, " (", ov$pct_duplicates, "%)")),
          shiny::tags$li(shiny::tags$strong("Aberrantes : "), paste0(ov$total_outliers, " (", ov$pct_outliers, "%)"))
        )
      )
    })

    # Liste d'historique dans la barre laterale
    output$pipeline_history_list <- shiny::renderUI({
      steps <- pipeline_state$history
      if (length(steps) == 0) {
        return(shiny::div(class = "text-muted small fst-italic", "Aucune transformation appliqu\u00e9e pour le moment."))
      }

      shiny::tags$ol(
        class = "ps-3 small mb-0",
        lapply(seq_along(steps), function(i) {
          s <- steps[[i]]
          shiny::tags$li(
            class = "mb-1 text-break",
            shiny::tags$strong(paste0("#", i, " ")),
            s$label
          )
        })
      )
    })

    # Value boxes de l'onglet Explorer
    output$box_rows <- shiny::renderText({
      d <- diag_data()
      as.character(d$overview$n_rows)
    })

    output$box_cols <- shiny::renderText({
      d <- diag_data()
      as.character(d$overview$n_cols)
    })

    output$box_nas <- shiny::renderText({
      d <- diag_data()
      paste0(d$overview$total_nas, " (", d$overview$pct_nas, "%)")
    })

    output$box_duplicates <- shiny::renderText({
      d <- diag_data()
      paste0(d$overview$n_duplicates, " (", d$overview$pct_duplicates, "%)")
    })

    output$box_outliers <- shiny::renderText({
      d <- diag_data()
      paste0(d$overview$total_outliers, " (", d$overview$pct_outliers, "%)")
    })

    # Tableau de diagnostic complet
    output$table_diagnosis <- DT::renderDataTable({
      d <- diag_data()
      DT::datatable(
        d$columns,
        colnames = c("Variable", "Type", "Classe R", "Total N", "Manquants (NA)", "% NA", "Valeurs uniques", "Exemples"),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = "tip",
          language = list(search = "Filtrer :", paginate = list(previous = "Pr\u00e9c\u00e9dent", `next` = "Suivant"))
        ),
        class = "compact stripe hover border w-100",
        rownames = FALSE,
        fillContainer = FALSE
      )
    })

    # Apercu du dataset
    output$table_preview <- DT::renderDataTable({
      df <- data_holder$df
      shiny::req(df)
      DT::datatable(
        df,
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = "tip",
          language = list(search = "Filtrer :", paginate = list(previous = "Pr\u00e9c\u00e9dent", `next` = "Suivant"))
        ),
        class = "compact stripe hover border w-100",
        rownames = TRUE,
        fillContainer = FALSE
      )
    })

    # =========================================================================
    # SECTION 2 : MODIFIER LES VARIABLES - CONTROLES
    # =========================================================================
    output$mod_action_controls <- shiny::renderUI({
      act <- input$mod_action_type
      df <- data_holder$df
      shiny::req(df)
      col_names <- names(df)

      if (identical(act, "rename")) {
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("rename_var_select"),
            label = "Variable \u00e0 renommer :",
            choices = col_names,
            selected = col_names[1]
          ),
          shiny::textInput(
            inputId = ns("rename_new_name"),
            label = "Nouveau nom :",
            value = paste0(col_names[1], "_mod")
          )
        )
      } else if (identical(act, "drop")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("drop_vars_select"),
            label = "Variable(s) \u00e0 supprimer :",
            choices = col_names,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        )
      } else if (identical(act, "reorder")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("reorder_vars_select"),
            label = "Variables prioritaires (\u00e0 placer en t\u00eate) :",
            choices = col_names,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::p(class = "text-muted small", "Les autres variables conserveront leur ordre relatif.")
        )
      } else if (identical(act, "cast")) {
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("cast_var_select"),
            label = "Variable \u00e0 convertir :",
            choices = col_names,
            selected = col_names[1]
          ),
          shiny::selectInput(
            inputId = ns("cast_target_type"),
            label = "Type de destination :",
            choices = c(
              "Num\u00e9rique (numeric)" = "numeric",
              "Texte (character)" = "character",
              "Facteur / Cat\u00e9gorielle (factor)" = "factor",
              "Logique VRAI/FAUX (logical)" = "logical",
              "Date (Date)" = "Date"
            ),
            selected = "factor"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'Date'", ns("cast_target_type")),
            shiny::textInput(
              inputId = ns("cast_date_format"),
              label = "Format de date (ex: %%Y-%%m-%%d, %%d/%%m/%%Y) :",
              value = "%Y-%m-%d"
            )
          )
        )
      } else if (identical(act, "recode")) {
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("recode_var_select"),
            label = "Variable \u00e0 recoder :",
            choices = col_names,
            selected = col_names[1]
          ),
          shiny::uiOutput(ns("recode_mapping_ui")),
          shiny::selectInput(
            inputId = ns("recode_default_mode"),
            label = "Valeurs non modifi\u00e9es :",
            choices = c(
              "Conserver la valeur d'origine" = "keep",
              "Remplacer par NA" = "na"
            ),
            selected = "keep"
          )
        )
      } else if (identical(act, "replace_val")) {
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("replace_var_select"),
            label = "Variable cible :",
            choices = col_names,
            selected = col_names[1]
          ),
          shiny::checkboxInput(
            inputId = ns("replace_is_old_na"),
            label = "Remplacer les valeurs manquantes (NA)",
            value = FALSE
          ),
          shiny::conditionalPanel(
            condition = sprintf("!input['%s']", ns("replace_is_old_na")),
            shiny::textInput(
              inputId = ns("replace_old_val"),
              label = "Valeur exacte \u00e0 remplacer :",
              value = ""
            )
          ),
          shiny::checkboxInput(
            inputId = ns("replace_is_new_na"),
            label = "Mettre \u00e0 NA (valeur manquante)",
            value = FALSE
          ),
          shiny::conditionalPanel(
            condition = sprintf("!input['%s']", ns("replace_is_new_na")),
            shiny::textInput(
              inputId = ns("replace_new_val"),
              label = "Nouvelle valeur de remplacement :",
              value = ""
            )
          )
        )
      } else if (identical(act, "compute")) {
        shiny::tagList(
          shiny::textInput(
            inputId = ns("compute_new_var"),
            label = "Nom de la nouvelle variable :",
            value = "nouvelle_var"
          ),
          shiny::textInput(
            inputId = ns("compute_expr_str"),
            label = "Formule de calcul :",
            placeholder = "Ex: colA + colB / 100",
            value = ""
          ),
          shiny::div(
            class = "card bg-light p-2 small border text-muted",
            shiny::tags$strong("Variables disponibles :"),
            shiny::div(class = "font-monospace mt-1", paste(head(col_names, 12), collapse = ", ")),
            shiny::tags$strong(class = "mt-2", "Op\u00e9rateurs s\u00e9curis\u00e9s :"),
            shiny::div(class = "font-monospace", "+, -, *, /, ^, sqrt(), log(), exp(), abs(), round()")
          )
        )
      }
    })

    # UI dynamique pour les modalites a recoder
    output$recode_mapping_ui <- shiny::renderUI({
      df <- data_holder$df
      shiny::req(df, input$recode_var_select)
      v <- input$recode_var_select
      if (!(v %in% names(df))) return(NULL)

      raw_vals <- unique(df[[v]][!is.na(df[[v]])])
      top_vals <- head(raw_vals, 8)

      shiny::tagList(
        shiny::tags$label(class = "form-label mt-2 mb-1", "Recodage des modalit\u00e9s (anciennes \u2192 nouvelles) :"),
        shiny::div(
          class = "d-flex flex-column gap-2",
          lapply(seq_along(top_vals), function(i) {
            val_str <- as.character(top_vals[i])
            shiny::div(
              class = "row g-2 align-items-center mb-0",
              shiny::div(
                class = "col-5",
                shiny::tags$span(
                  class = "badge bg-light text-dark border d-flex align-items-center text-truncate w-100 px-2 font-monospace",
                  style = "height: 36px; font-size: 0.82rem;",
                  title = val_str,
                  val_str
                )
              ),
              shiny::div(
                class = "col-1 text-center text-muted px-0 d-flex align-items-center justify-content-center",
                style = "height: 36px;",
                "\u2192"
              ),
              shiny::div(
                class = "col-6",
                shiny::tags$div(
                  class = "form-group mb-0",
                  shiny::textInput(
                    inputId = ns(paste0("recode_val_", i)),
                    label = NULL,
                    value = val_str,
                    width = "100%"
                  )
                )
              )
            )
          })
        )
      )
    })

    # =========================================================================
    # SECTION 3 : NETTOYER - CONTROLES
    # =========================================================================
    output$clean_action_controls <- shiny::renderUI({
      act <- input$clean_action_type
      df <- data_holder$df
      shiny::req(df)
      col_names <- names(df)
      num_cols <- col_names[vapply(df, is.numeric, logical(1))]
      char_cols <- col_names[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]

      if (identical(act, "impute_na")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("clean_na_vars"),
            label = "Variables concern\u00e9es :",
            choices = col_names,
            selected = col_names,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("clean_na_method"),
            label = "Traitement des valeurs manquantes :",
            choices = c(
              "Supprimer les lignes avec NA" = "drop_rows",
              "Remplacer par une valeur fixe" = "fixed",
              "Imputer par la moyenne (num\u00e9rique)" = "mean",
              "Imputer par la m\u00e9diane (num\u00e9rique)" = "median",
              "Imputer par le mode (valeur la plus fr\u00e9quente)" = "mode"
            ),
            selected = "drop_rows"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'fixed'", ns("clean_na_method")),
            shiny::textInput(
              inputId = ns("clean_na_fixed_val"),
              label = "Valeur fixe :",
              value = "0"
            )
          )
        )
      } else if (identical(act, "deduplicate")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("clean_dedup_vars"),
            label = "Colonnes de d\u00e9tection (toutes par d\u00e9faut) :",
            choices = col_names,
            selected = col_names,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::radioButtons(
            inputId = ns("clean_dedup_keep"),
            label = "Occurrence \u00e0 conserver :",
            choices = c("Premi\u00e8re (First)" = "first", "Derni\u00e8re (Last)" = "last"),
            selected = "first",
            inline = TRUE
          )
        )
      } else if (identical(act, "trim_ws")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("clean_trim_vars"),
            label = "Variables texte \u00e0 nettoyer :",
            choices = if (length(char_cols) > 0) char_cols else col_names,
            selected = if (length(char_cols) > 0) char_cols else col_names[1],
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("clean_trim_mode"),
            label = "Mode d'espaces :",
            choices = c(
              "D\u00e9but et fin (trim)" = "both",
              "D\u00e9but, fin et espaces multiples internes (squish)" = "squish",
              "D\u00e9but seulement" = "left",
              "Fin seulement" = "right"
            ),
            selected = "both"
          )
        )
      } else if (identical(act, "change_case")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("clean_case_vars"),
            label = "Variables texte cible :",
            choices = if (length(char_cols) > 0) char_cols else col_names,
            selected = if (length(char_cols) > 0) char_cols else col_names[1],
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectInput(
            inputId = ns("clean_case_target"),
            label = "Casse cible :",
            choices = c(
              "minuscules (lower case)" = "lower",
              "MAJUSCULES (UPPER CASE)" = "upper",
              "Premi\u00e8re Lettre De Chaque Mot (Title Case)" = "title",
              "Premi\u00e8re lettre de la phrase (Sentence case)" = "sentence"
            ),
            selected = "lower"
          )
        )
      }
    })

    # =========================================================================
    # SECTION 4 : FILTRER & TRIER - CONTROLES
    # =========================================================================
    output$filter_sort_controls <- shiny::renderUI({
      mode <- input$filter_sort_mode
      df <- data_holder$df
      shiny::req(df)
      col_names <- names(df)

      if (identical(mode, "filter")) {
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("filter_var_1"),
            label = "Variable :",
            choices = col_names,
            selected = col_names[1]
          ),
          shiny::selectInput(
            inputId = ns("filter_op_1"),
            label = "Op\u00e9rateur :",
            choices = c(
              "\u00c9gal \u00e0 (==)" = "eq",
              "Diff\u00e9rent de (!=)" = "neq",
              "Sup\u00e9rieur \u00e0 (>)" = "gt",
              "Inf\u00e9rieur \u00e0 (<)" = "lt",
              "Sup\u00e9rieur ou \u00e9gal (>=)" = "gte",
              "Inf\u00e9rieur ou \u00e9gal (<=)" = "lte",
              "Contient le texte" = "contains",
              "Commence par" = "starts_with",
              "Se termine par" = "ends_with",
              "Est manquant (NA)" = "is_na",
              "N'est pas manquant (non-NA)" = "not_na",
              "Dans la liste (s\u00e9par\u00e9e par virgules)" = "in"
            ),
            selected = "eq"
          ),
          shiny::conditionalPanel(
            condition = sprintf("!['is_na', 'not_na'].includes(input['%s'])", ns("filter_op_1")),
            shiny::textInput(
              inputId = ns("filter_val_1"),
              label = "Valeur :",
              value = ""
            )
          ),
          shiny::hr(class = "my-2"),
          shiny::checkboxInput(
            inputId = ns("filter_enable_2"),
            label = "Ajouter une 2\u00e8me condition",
            value = FALSE
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s']", ns("filter_enable_2")),
            shiny::selectInput(
              inputId = ns("filter_combine_op"),
              label = "Combinaison logique :",
              choices = c("ET (AND)" = "AND", "OU (OR)" = "OR"),
              selected = "AND"
            ),
            shiny::selectInput(
              inputId = ns("filter_var_2"),
              label = "Variable 2 :",
              choices = col_names,
              selected = if (length(col_names) > 1) col_names[2] else col_names[1]
            ),
            shiny::selectInput(
              inputId = ns("filter_op_2"),
              label = "Op\u00e9rateur 2 :",
              choices = c(
                "\u00c9gal \u00e0 (==)" = "eq",
                "Diff\u00e9rent de (!=)" = "neq",
                "Sup\u00e9rieur \u00e0 (>)" = "gt",
                "Inf\u00e9rieur \u00e0 (<)" = "lt",
                "Sup\u00e9rieur ou \u00e9gal (>=)" = "gte",
                "Inf\u00e9rieur ou \u00e9gal (<=)" = "lte",
                "Contient le texte" = "contains",
                "Commence par" = "starts_with",
                "Se termine par" = "ends_with",
                "Est manquant (NA)" = "is_na",
                "N'est pas manquant (non-NA)" = "not_na"
              ),
              selected = "eq"
            ),
            shiny::conditionalPanel(
              condition = sprintf("!['is_na', 'not_na'].includes(input['%s'])", ns("filter_op_2")),
              shiny::textInput(
                inputId = ns("filter_val_2"),
                label = "Valeur 2 :",
                value = ""
              )
            )
          )
        )
      } else if (identical(mode, "sort")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("sort_vars_select"),
            label = "Variable(s) de tri :",
            choices = col_names,
            selected = col_names[1],
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::radioButtons(
            inputId = ns("sort_direction"),
            label = "Sens du tri :",
            choices = c("Croissant (A \u2192 Z, 0 \u2192 9)" = "asc", "D\u00e9croissant (Z \u2192 A, 9 \u2192 0)" = "desc"),
            selected = "asc",
            inline = TRUE
          )
        )
      } else if (identical(mode, "select_vars")) {
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("select_vars_cols"),
            label = "Variables \u00e0 conserver :",
            choices = col_names,
            selected = col_names,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::p(class = "text-muted small", "Toutes les variables non s\u00e9lectionn\u00e9es seront retir\u00e9es.")
        )
      }
    })

    # Helper interne pour appliquer et enregistrer une transformation
    execute_step <- function(step_obj) {
      tryCatch({
        df_curr <- data_holder$df
        step_res <- ramses_prep_apply_step(df_curr, step_obj)

        # Mise a jour de l'etat des donnees
        data_holder$df <- step_res$df

        # Mise a jour de l'historique du pipeline
        hist <- pipeline_state$history
        hist[[length(hist) + 1]] <- step_res
        pipeline_state$history <- hist

        # Journalisation dans le Rmd
        append_to_rmd(
          title = paste0("Pr\u00e9paration : ", step_res$label),
          code = paste0("# Op\u00e9ration de pr\u00e9paration des donn\u00e9es\n", step_res$code)
        )

        shiny::showNotification(
          paste0("Transformation r\u00e9ussie : ", step_res$label),
          type = "message",
          duration = 4
        )
      }, error = function(e) {
        shiny::showNotification(
          paste0("Erreur lors de la transformation : ", e$message),
          type = "error",
          duration = 8
        )
      })
    }

    # =========================================================================
    # APPLICATION DES ACTIONS DE MODIFICATION DES VARIABLES
    # =========================================================================
    shiny::observeEvent(input$btn_apply_mod_action, {
      act <- input$mod_action_type

      if (identical(act, "rename")) {
        old_n <- input$rename_var_select
        new_n <- trimws(input$rename_new_name)
        if (!nzchar(new_n)) {
          shiny::showNotification("Le nouveau nom ne peut pas \u00eatre vide.", type = "warning")
          return()
        }
        execute_step(list(type = "rename", params = list(old_name = old_n, new_name = new_n)))

      } else if (identical(act, "drop")) {
        vars_drop <- input$drop_vars_select
        if (length(vars_drop) == 0) {
          shiny::showNotification("Veuillez s\u00e9lectionner au moins une variable \u00e0 supprimer.", type = "warning")
          return()
        }
        execute_step(list(type = "drop", params = list(vars = vars_drop)))

      } else if (identical(act, "reorder")) {
        first_v <- input$reorder_vars_select
        if (length(first_v) == 0) {
          shiny::showNotification("Veuillez s\u00e9lectionner au moins une variable prioritaire.", type = "warning")
          return()
        }
        execute_step(list(type = "reorder", params = list(first_vars = first_v)))

      } else if (identical(act, "cast")) {
        v <- input$cast_var_select
        tgt <- input$cast_target_type
        fmt <- input$cast_date_format
        execute_step(list(type = "cast", params = list(var = v, target_type = tgt, date_format = fmt)))

      } else if (identical(act, "recode")) {
        v <- input$recode_var_select
        df <- data_holder$df
        raw_vals <- unique(df[[v]][!is.na(df[[v]])])
        top_vals <- head(raw_vals, 8)

        mapping <- list()
        for (i in seq_along(top_vals)) {
          inp_val <- input[[paste0("recode_val_", i)]]
          if (!is.null(inp_val)) {
            mapping[[as.character(top_vals[i])]] <- inp_val
          }
        }
        def_mode <- input$recode_default_mode
        execute_step(list(type = "recode", params = list(var = v, mapping = mapping, default = def_mode)))

      } else if (identical(act, "replace_val")) {
        v <- input$replace_var_select
        is_o_na <- isTRUE(input$replace_is_old_na)
        is_n_na <- isTRUE(input$replace_is_new_na)
        old_v <- if (is_o_na) NULL else input$replace_old_val
        new_v <- if (is_n_na) NULL else input$replace_new_val

        execute_step(list(
          type = "replace_value",
          params = list(
            var = v,
            old_val = old_v,
            new_val = new_v,
            is_old_na = is_o_na,
            is_new_na = is_n_na
          )
        ))

      } else if (identical(act, "compute")) {
        n_var <- trimws(input$compute_new_var)
        expr_s <- trimws(input$compute_expr_str)

        if (!nzchar(n_var)) {
          shiny::showNotification("Veuillez saisir un nom pour la nouvelle variable.", type = "warning")
          return()
        }
        if (!nzchar(expr_s)) {
          shiny::showNotification("Veuillez saisir une formule de calcul.", type = "warning")
          return()
        }

        execute_step(list(type = "compute", params = list(new_var = n_var, expr_str = expr_s)))
      }
    })

    # =========================================================================
    # APPLICATION DES ACTIONS DE NETTOYAGE
    # =========================================================================
    shiny::observeEvent(input$btn_apply_clean_action, {
      act <- input$clean_action_type

      if (identical(act, "impute_na")) {
        vars_sel <- input$clean_na_vars
        meth <- input$clean_na_method
        fix_v <- input$clean_na_fixed_val
        execute_step(list(type = "impute_na", params = list(vars = vars_sel, method = meth, fixed_value = fix_v)))

      } else if (identical(act, "deduplicate")) {
        vars_sel <- input$clean_dedup_vars
        keep_v <- input$clean_dedup_keep
        execute_step(list(type = "deduplicate", params = list(vars = vars_sel, keep = keep_v)))

      } else if (identical(act, "trim_ws")) {
        vars_sel <- input$clean_trim_vars
        mode_v <- input$clean_trim_mode
        execute_step(list(type = "trim_ws", params = list(vars = vars_sel, mode = mode_v)))

      } else if (identical(act, "change_case")) {
        vars_sel <- input$clean_case_vars
        tgt_case <- input$clean_case_target
        execute_step(list(type = "change_case", params = list(vars = vars_sel, target_case = tgt_case)))
      }
    })

    # =========================================================================
    # APPLICATION DU FILTRAGE ET TRI
    # =========================================================================
    shiny::observeEvent(input$btn_apply_filter_sort, {
      mode <- input$filter_sort_mode

      if (identical(mode, "filter")) {
        v1 <- input$filter_var_1
        op1 <- input$filter_op_1
        val1 <- input$filter_val_1

        conds <- list(list(var = v1, op = op1, val = val1))

        if (isTRUE(input$filter_enable_2)) {
          v2 <- input$filter_var_2
          op2 <- input$filter_op_2
          val2 <- input$filter_val_2
          conds[[2]] <- list(var = v2, op = op2, val = val2)
        }

        comb <- if (isTRUE(input$filter_enable_2)) input$filter_combine_op else "AND"
        execute_step(list(type = "filter", params = list(conditions = conds, combine_op = comb)))

      } else if (identical(mode, "sort")) {
        vars_s <- input$sort_vars_select
        dir_s <- input$sort_direction
        execute_step(list(type = "sort", params = list(vars = vars_s, orders = dir_s)))

      } else if (identical(mode, "select_vars")) {
        vars_s <- input$select_vars_cols
        execute_step(list(type = "select_vars", params = list(vars = vars_s)))
      }
    })

    # =========================================================================
    # ANNULATION ET REINITIALISATION
    # =========================================================================
    shiny::observeEvent(input$btn_undo_step, {
      hist <- pipeline_state$history
      if (length(hist) == 0) {
        shiny::showNotification("Aucune \u00e9tape \u00e0 annuler.", type = "warning")
        return()
      }

      # Retirer la derniere etape
      hist <- hist[-length(hist)]
      pipeline_state$history <- hist

      # Re-executer le pipeline depuis les donnees initiales
      init_df <- pipeline_state$initial_df
      if (!is.null(init_df)) {
        pipe_res <- ramses_prep_apply_pipeline(init_df, hist)
        data_holder$df <- pipe_res$df
        shiny::showNotification("Derni\u00e8re op\u00e9ration annul\u00e9e avec succ\u00e8s.", type = "message")
      }
    })

    shiny::observeEvent(input$btn_reset_dataset, {
      init_df <- pipeline_state$initial_df
      if (!is.null(init_df)) {
        data_holder$df <- init_df
        pipeline_state$history <- list()
        shiny::showNotification("Jeu de donn\u00e9es r\u00e9initialis\u00e9 aux valeurs initiales.", type = "default")
      }
    })

    # Tables d'apercu pour chaque onglet
    render_prep_table <- function() {
      DT::renderDataTable({
        df <- data_holder$df
        shiny::req(df)
        DT::datatable(
          df,
          options = list(
            pageLength = 8,
            scrollX = TRUE,
            dom = "tip",
            language = list(search = "Filtrer :", paginate = list(previous = "Pr\u00e9c\u00e9dent", `next` = "Suivant"))
          ),
          class = "compact stripe hover border",
          rownames = TRUE
        )
      })
    }

    output$mod_action_preview_table <- render_prep_table()
    output$clean_action_preview_table <- render_prep_table()
    output$filter_sort_preview_table <- render_prep_table()

    output$mod_action_preview_info <- shiny::renderUI({
      df <- data_holder$df
      shiny::req(df)
      shiny::div(class = "text-muted small mb-2", paste0("Dimensions actuelles : ", nrow(df), " lignes \u00d7 ", ncol(df), " colonnes"))
    })

    output$clean_action_preview_info <- shiny::renderUI({
      df <- data_holder$df
      shiny::req(df)
      shiny::div(class = "text-muted small mb-2", paste0("Dimensions actuelles : ", nrow(df), " lignes \u00d7 ", ncol(df), " colonnes"))
    })

    output$filter_sort_preview_info <- shiny::renderUI({
      df <- data_holder$df
      shiny::req(df)
      shiny::div(class = "text-muted small mb-2", paste0("Dimensions actuelles : ", nrow(df), " lignes \u00d7 ", ncol(df), " colonnes"))
    })

  })
}
