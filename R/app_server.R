#' Logique serveur de l'application Ramses
#'
#' @param input Entr\u00e9es Shiny
#' @param output Sorties Shiny
#' @param session Session Shiny
#'
#' @return Rien (effets de bord Shiny)
#' @noRd
app_server <- function(input, output, session) {

  # 0. Initialisation des ressources statiques (favicon, cours-statistiques, etc.)
  res_dir <- system.file("app/www", package = "Ramses")
  if (dir.exists(res_dir)) {
    tryCatch({
      shiny::addResourcePath("ramses_res", res_dir)
      shiny::addResourcePath("www", res_dir)
    }, error = function(e) NULL)
  }

  cours_dir <- system.file("www/cours-statistiques", package = "Ramses")
  if (!nzchar(cours_dir) || !dir.exists(cours_dir)) {
    cours_dir <- file.path(getwd(), "inst", "www", "cours-statistiques")
  }
  if (!dir.exists(cours_dir)) {
    cours_dir <- file.path(getwd(), "Ramses", "inst", "www", "cours-statistiques")
  }
  if (dir.exists(cours_dir)) {
    tryCatch({
      shiny::addResourcePath("cours_stats", cours_dir)
    }, error = function(e) NULL)
  }

  # 1. R\u00e9actif principal contenant le jeu de donn\u00e9es (iris par d\u00e9faut)
  data_holder <- shiny::reactiveValues(
    name = "iris",
    df = datasets::iris,
    source_file_name = NULL,
    source_file_datapath = NULL
  )

  # 2. R\u00e9actif pour le journal R Markdown avec en-t\u00eate propre
  initial_rmd_header <- paste(
    "---",
    "title: \"Analyse statistique Ramses\"",
    "author: \"Utilisateur Ramses\"",
    paste0("date: \"", Sys.Date(), "\""),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    theme: zephyr",
    "---",
    "",
    "<style>",
    "@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600;700&display=swap');",
    "body, html, * { font-family: 'IBM Plex Sans', sans-serif !important; }",
    "code, pre, pre *, code *, .monospace { font-family: monospace !important; }",
    "</style>",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "library(Ramses)",
    "library(dplyr)",
    "```",
    "",
    "## 1. Initialisation des donn\u00e9es",
    "",
    "```{r load-data}",
    "# Chargement du jeu de donn\u00e9es initial",
    "data(iris)",
    "dataset <- iris",
    "head(dataset)",
    "```",
    "",
    sep = "\n"
  )

  rmd_log <- shiny::reactiveVal(initial_rmd_header)

  # =========================================================================
  # GESTION DE L'\u00c9TAT DU RAPPORT (DIRTY STATE) & CYCLE DE VIE
  # =========================================================================
  report_state <- shiny::reactiveValues(
    dirty = FALSE,
    last_saved = NULL,
    saved_format = NULL
  )

  mark_report_dirty <- function() {
    report_state$dirty <- TRUE
  }

  mark_report_saved <- function(format_name = "Rapport") {
    report_state$dirty <- FALSE
    report_state$last_saved <- Sys.time()
    report_state$saved_format <- format_name
  }

  pending_lifecycle_action <- shiny::reactiveVal(NULL)

  # Helper interne pour journaliser les op\u00e9rations en R Markdown
  append_to_rmd <- function(title, code) {
    new_entry <- paste0(
      "\n## ", title, "\n\n",
      "```{r}\n",
      code, "\n",
      "```\n"
    )
    rmd_log(paste0(rmd_log(), new_entry))
    mark_report_dirty()
  }

  # Mise \u00e0 jour dynamique des s\u00e9lecteurs de variables selon le dataset actif
  shiny::observe({
    df <- data_holder$df
    if (is.data.frame(df)) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)

      shiny::updateSelectInput(
        session = session,
        inputId = "desc_var",
        choices = num_cols,
        selected = if (length(num_cols) > 0) num_cols[1] else NULL
      )

      factor_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      shiny::updateSelectInput(
        session = session,
        inputId = "desc_group",
        choices = c("Aucune" = "", factor_cols),
        selected = if (length(factor_cols) > 0) factor_cols[1] else ""
      )
    }
  })

  # Badge du dataset dans le card header
  output$dataset_badge <- shiny::renderUI({
    df <- data_holder$df
    shiny::tags$span(
      class = "badge text-dark border",
      style = "background-color: #F3F4F6; border-color: #E5E7EB !important; font-size: 0.78rem; font-weight: 600; padding: 4px 8px;",
      paste0(data_holder$name, " (", nrow(df), " obs. x ", ncol(df), " var.)")
    )
  })

  # Sommaire lat\u00e9ral dans le volet de l'onglet Donn\u00e9es
  output$data_summary_sidebar <- shiny::renderUI({
    df <- data_holder$df
    src_info <- if (!is.null(data_holder$source_file_name)) {
      data_holder$source_file_name
    } else if (identical(data_holder$name, "iris")) {
      "Exemple (datasets::iris)"
    } else {
      "Session R / m\u00e9moire (.GlobalEnv)"
    }
    shiny::tagList(
      shiny::tags$ul(
        class = "list-unstyled small mb-0",
        shiny::tags$li(shiny::tags$strong("Nom : "), data_holder$name),
        shiny::tags$li(shiny::tags$strong("Source : "), src_info),
        shiny::tags$li(shiny::tags$strong("Lignes : "), nrow(df)),
        shiny::tags$li(shiny::tags$strong("Colonnes : "), ncol(df)),
        shiny::tags$li(
          shiny::tags$strong("Variables num\u00e9riques : "),
          sum(sapply(df, is.numeric))
        ),
        shiny::tags$li(
          shiny::tags$strong("Variables qualitatives : "),
          sum(sapply(df, function(x) is.factor(x) || is.character(x)))
        )
      )
    )
  })

  # Affichage interactif du dataset avec DT::dataTableOutput
  output$dataset_table <- DT::renderDataTable({
    shiny::req(data_holder$df)
    df <- data_holder$df

    # 1. D\u00e9tection des types de variable & Formatage des en-t\u00eates HTML sans balise d'ic\u00f4ne
    col_names_raw <- names(df)
    enriched_colnames <- vapply(seq_along(df), function(i) {
      col <- df[[i]]
      is_quanti <- is.numeric(col) || is.integer(col)
      if (is_quanti) {
        paste0("<span style='color:#6B7280; font-weight:bold;'>[#]</span> ", htmltools::htmlEscape(col_names_raw[i]))
      } else {
        paste0("<span style='color:#6B7280; font-weight:bold;'>[Aa]</span> ", htmltools::htmlEscape(col_names_raw[i]))
      }
    }, character(1))

    DT::datatable(
      df,
      colnames = enriched_colnames,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ entr\u00e9es",
          info = "Affichage de _START_ \u00e0 _END_ sur _TOTAL_ entr\u00e9es",
          paginate = list(
            previous = "Pr\u00e9c\u00e9dent",
            `next` = "Suivant"
          )
        )
      ),
      class = "compact stripe hover border",
      rownames = TRUE
    )
  })

  # Affichage du journal R Markdown dans un bloc de code stylis\u00e9
  output$rmd_log_output <- shiny::renderText({
    rmd_log()
  })

  # Badge de statistiques du journal (nombre de lignes et blocs R)
  output$rmd_stats_badge <- shiny::renderText({
    text <- rmd_log()
    lines_count <- length(strsplit(text, "\n")[[1]])
    chunks_count <- length(gregexpr("```\\{r", text)[[1]])
    if (chunks_count == 1 && gregexpr("```\\{r", text)[[1]][1] == -1) chunks_count <- 0
    paste0(lines_count, " lignes \u2022 ", chunks_count, " blocs R")
  })

  # Copier le script Rmd dans le presse-papier
  shiny::observeEvent(input$btn_copy_rmd, {
    shiny::showNotification(
      "Script R Markdown copi\u00e9 dans le presse-papier !",
      type = "message",
      duration = 3
    )
  })

  # Dialogue modal pour ajouter une note textuelle au journal
  shiny::observeEvent(input$btn_add_rmd_note, {
    shiny::showModal(
      shiny::modalDialog(
        title = shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::tags$span(style = "font-weight: 600;", "Ajouter une note textuelle au Journal")
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(
            inputId = "btn_confirm_add_note",
            label = "Ins\u00e9rer dans le journal",
            class = "btn-dark"
          )
        ),
        shiny::p(
          class = "text-muted small mb-3",
          "Ins\u00e9rez des commentaires d'analyse, hypoth\u00e8ses ou conclusions en syntaxe Markdown. Ces notes s'ins\u00e8reront entre les blocs de code R reproductibles."
        ),
        shiny::textInput(
          inputId = "rmd_note_title",
          label = "Titre de la section / remarque :",
          placeholder = "Ex : Interpr\u00e9tation clinique et conclusions"
        ),
        shiny::textAreaInput(
          inputId = "rmd_note_content",
          label = "Commentaires (Markdown accept\u00e9) :",
          rows = 5,
          placeholder = "Ex : Les r\u00e9sultats montrent une corr\u00e9lation statistiquement significative (p < 0.05). Une analyse compl\u00e9mentaire sera requise..."
        )
      )
    )
  })

  # Validation de l'ajout de la note textuelle
  shiny::observeEvent(input$btn_confirm_add_note, {
    note_title <- trimws(input$rmd_note_title)
    note_content <- trimws(input$rmd_note_content)

    if (!nzchar(note_title)) {
      note_title <- "Remarques & Commentaires"
    }

    if (!nzchar(note_content)) {
      shiny::showNotification("Veuillez saisir un texte pour votre note.", type = "warning")
      return()
    }

    new_entry <- paste0(
      "\n## ", note_title, "\n\n",
      note_content, "\n\n"
    )

    rmd_log(paste0(rmd_log(), new_entry))
    mark_report_dirty()
    shiny::removeModal()
    shiny::showNotification("Note textuelle ins\u00e9r\u00e9e dans le journal R Markdown !", type = "message")
  })

  # Dialogue modal de confirmation d'effacement du journal
  shiny::observeEvent(input$btn_clear_rmd, {
    shiny::showModal(
      shiny::modalDialog(
        title = shiny::div(
          class = "d-flex align-items-center gap-2 text-danger",
          shiny::tags$span(style = "font-weight: 600;", "R\u00e9initialiser le Journal R Markdown ?")
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(
            inputId = "btn_confirm_clear_rmd",
            label = "Oui, effacer et r\u00e9initialiser",
            class = "btn-danger"
          )
        ),
        shiny::p("Attention : Cette action r\u00e9initialisera l'int\u00e9gralit\u00e9 du script R Markdown au mod\u00e8le de d\u00e9part avec le jeu de donn\u00e9es initial."),
        shiny::p(class = "text-muted small mb-0", "Toutes les \u00e9tapes d'analyse descriptives, graphiques ou tests non export\u00e9s seront effac\u00e9s.")
      )
    )
  })

  # Confirmation de l'effacement
  shiny::observeEvent(input$btn_confirm_clear_rmd, {
    rmd_log(initial_rmd_header)
    # R\u00e9initialisation logique de l'\u00e9tat du rapport selon le jeu de donn\u00e9es
    if (ramses_is_initial_report(initial_rmd_header, initial_rmd_header, data_holder$name)) {
      report_state$dirty <- FALSE
    } else {
      report_state$dirty <- TRUE
    }
    shiny::removeModal()
    shiny::showNotification("Le journal R Markdown a \u00e9t\u00e9 r\u00e9initialis\u00e9.", type = "message")
  })

  # R\u00e9initialiser avec le jeu de donn\u00e9es iris
  shiny::observeEvent(input$sidebar_btn_reset_iris, {
    data_holder$name <- "iris"
    data_holder$df <- datasets::iris
    data_holder$source_file_name <- NULL
    data_holder$source_file_datapath <- NULL
    if (ramses_is_initial_report(rmd_log(), initial_rmd_header, "iris")) {
      report_state$dirty <- FALSE
    }
    shiny::showNotification("Jeu de donn\u00e9es 'iris' recharg\u00e9.", type = "default")
  })

  # \u00c9tat r\u00e9actif de la liste des jeux de donn\u00e9es du workspace
  workspace_datasets_list <- shiny::reactiveVal(NULL)

  refresh_workspace_list <- function() {
    ds <- ramses_list_workspace_datasets(.GlobalEnv)
    workspace_datasets_list(ds)
  }

  # Ouvrir la fen\u00eatre modale d'importation
  open_import_modal <- function() {
    refresh_workspace_list()
    shiny::showModal(modal_import_data())
  }

  shiny::observeEvent(input$btn_import, { open_import_modal() })
  shiny::observeEvent(input$sidebar_btn_import, { open_import_modal() })
  shiny::observeEvent(input$menu_btn_import, { open_import_modal() })

  # Actualisation manuelle de la liste des objets R
  shiny::observeEvent(input$btn_refresh_workspace_datasets, {
    refresh_workspace_list()
    shiny::showNotification("Liste des jeux de donn\u00e9es R actualis\u00e9e.", type = "default", duration = 2)
  })

  # Bascule vers l'onglet fichier externe
  shiny::observeEvent(input$btn_switch_to_file_tab, {
    bslib::nav_select(id = "import_source_tabs", selected = "tab_import_file")
  })

  # Affichage dynamique des jeux de donn\u00e9es du workspace
  output$workspace_datasets_ui <- shiny::renderUI({
    ds <- workspace_datasets_list()
    if (is.null(ds)) {
      ds <- ramses_list_workspace_datasets(.GlobalEnv)
      workspace_datasets_list(ds)
    }

    if (nrow(ds) == 0) {
      return(
        shiny::div(
          class = "alert alert-secondary py-3 px-3 small mb-0",
          shiny::tags$div(
            class = "d-flex align-items-center gap-2 mb-2",
            shiny::tags$strong("Aucun jeu de donn\u00e9es compatible n'a \u00e9t\u00e9 d\u00e9tect\u00e9 dans votre session R.")
          ),
          shiny::tags$p(
            class = "mb-2 text-muted",
            "Pour utiliser directement un objet R, cr\u00e9ez ou chargez un ",
            shiny::tags$code("data.frame"), ", ", shiny::tags$code("tibble"), " ou ", shiny::tags$code("data.table"),
            " dans votre console R ou RStudio (ex : ", shiny::tags$code("donnees <- read.csv('fichier.csv')"), "), puis cliquez sur ",
            shiny::tags$strong("Actualiser"), "."
          ),
          shiny::tags$div(
            class = "mt-3",
            shiny::actionButton(
              inputId = "btn_switch_to_file_tab",
              label = "Importer plut\u00f4t un fichier externe",
              class = "btn-outline-dark btn-sm"
            )
          )
        )
      )
    }

    valid_names <- ds$name[ds$valid]
    first_valid <- if (length(valid_names) > 0) valid_names[1] else ds$name[1]

    shiny::div(
      class = "d-flex flex-column gap-2",
      shiny::tags$div(
        class = "text-muted small mb-1",
        paste0(nrow(ds), " jeu(x) de donn\u00e9es d\u00e9tect\u00e9(s) dans votre environnement :")
      ),
      shiny::radioButtons(
        inputId = "selected_workspace_dataset",
        label = NULL,
        choiceNames = lapply(seq_len(nrow(ds)), function(i) {
          row <- ds[i, ]
          is_v <- row$valid
          shiny::div(
            class = paste0("p-2 rounded border mb-2 ", if (is_v) "bg-white" else "bg-light opacity-75"),
            style = "cursor: pointer;",
            shiny::div(
              class = "d-flex justify-content-between align-items-center",
              shiny::tags$span(
                class = "fw-bold text-dark",
                style = "font-size: 0.95rem;",
                row$name
              ),
              if (is_v) {
                shiny::tags$span(
                  class = "badge bg-secondary text-white",
                  style = "font-size: 0.72rem;",
                  row$class
                )
              } else {
                shiny::tags$span(
                  class = "badge bg-danger text-white",
                  style = "font-size: 0.72rem;",
                  "Vide (non utilisable)"
                )
              }
            ),
            shiny::div(
              class = "text-muted small mt-1",
              if (is_v) {
                paste0(
                  row$nrow, " observation(s) \u00d7 ", row$ncol, " variable(s)",
                  if (nzchar(row$size)) paste0(" \u2022 ", row$size) else ""
                )
              } else {
                "Ce tableau est vide (0 observation ou 0 variable) et ne peut pas \u00eatre utilis\u00e9."
              }
            )
          )
        }),
        choiceValues = ds$name,
        selected = first_valid,
        width = "100%"
      )
    )
  })

  # Chargement du jeu de donn\u00e9es s\u00e9lectionn\u00e9 depuis le workspace R
  shiny::observeEvent(input$btn_load_workspace_dataset, {
    sel_name <- input$selected_workspace_dataset
    if (is.null(sel_name) || !nzchar(sel_name)) {
      shiny::showNotification(
        "Veuillez s\u00e9lectionner un jeu de donn\u00e9es dans la liste.",
        type = "warning"
      )
      return()
    }

    tryCatch({
      loaded_df <- ramses_get_workspace_dataset(sel_name, envir = .GlobalEnv)

      # Mise \u00e0 jour centrale des donn\u00e9es
      data_holder$name <- sel_name
      data_holder$df <- loaded_df
      data_holder$source_file_name <- NULL
      data_holder$source_file_datapath <- NULL

      # Journalisation dans le R Markdown avec le nom exact de l'objet
      code_entry <- paste0(
        "# Jeu de donn\u00e9es provenant de l'environnement R\n",
        sprintf("dataset <- as.data.frame(%s)\n\n", ramses_code_symbol(sel_name)),
        "# V\u00e9rification de la structure et aper\u00e7u\n",
        sprintf("dim(%s)\n", ramses_code_symbol(sel_name)),
        sprintf("head(%s)", ramses_code_symbol(sel_name))
      )

      append_to_rmd(
        title = paste0("Chargement depuis l'environnement R (", sel_name, ")"),
        code = code_entry
      )

      # Fermeture de la modale
      shiny::removeModal()

      # Notification de succ\u00e8s
      shiny::showNotification(
        paste0("Jeu de donn\u00e9es '", sel_name, "' charg\u00e9 avec succ\u00e8s depuis la session R !"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      shiny::showNotification(
        paste0("Erreur lors du chargement : ", e$message),
        type = "error",
        duration = 8
      )
    })
  })

  # D\u00e9tection du format effectif (si auto-d\u00e9tection choisie)
  detected_format <- shiny::reactive({
    fmt <- input$import_format
    if (is.null(fmt) || fmt == "auto") {
      if (!is.null(input$import_file)) {
        ext <- tolower(tools::file_ext(input$import_file$name))
        switch(
          ext,
          "csv" = "csv",
          "txt" = "csv",
          "tsv" = "csv",
          "xlsx" = "excel",
          "xls" = "excel",
          "sav" = "spss",
          "dta" = "stata",
          "rds" = "rds",
          "csv"
        )
      } else {
        "csv"
      }
    } else {
      fmt
    }
  })

  # Rendu dynamique des options de lecture selon le format
  output$import_dynamic_options <- shiny::renderUI({
    fmt <- detected_format()

    if (fmt == "csv") {
      shiny::tagList(
        shiny::div(
          class = "row g-2",
          shiny::div(
            class = "col-md-4",
            shiny::selectInput(
              inputId = "import_sep",
              label = "S\u00e9parateur de champs :",
              choices = c(
                "Virgule (,)" = ",",
                "Point-virgule (;)" = ";",
                "Tabulation (\\t)" = "\t",
                "Espace ( )" = " "
              ),
              selected = ","
            )
          ),
          shiny::div(
            class = "col-md-4",
            shiny::selectInput(
              inputId = "import_dec",
              label = "S\u00e9parateur d\u00e9cimal :",
              choices = c(
                "Point (.)" = ".",
                "Virgule (,)" = ","
              ),
              selected = "."
            )
          ),
          shiny::div(
            class = "col-md-4",
            shiny::selectInput(
              inputId = "import_header",
              label = "En-t\u00eate (Header) :",
              choices = c(
                "Oui (1\u00e8re ligne = noms)" = "TRUE",
                "Non (pas d'en-t\u00eate)" = "FALSE"
              ),
              selected = "TRUE"
            )
          )
        ),
        shiny::div(
          class = "mt-2",
          shiny::checkboxInput(
            inputId = "import_stringsAsFactors",
            label = "Convertir les cha\u00eenes de caract\u00e8res en facteurs (stringsAsFactors)",
            value = TRUE
          )
        )
      )
    } else if (fmt == "excel") {
      # R\u00e9cup\u00e9ration dynamique des feuilles si un fichier Excel a \u00e9t\u00e9 t\u00e9l\u00e9vers\u00e9
      sheet_choices <- "1"
      if (!is.null(input$import_file)) {
        tryCatch({
          sheets <- readxl::excel_sheets(input$import_file$datapath)
          if (length(sheets) > 0) sheet_choices <- sheets
        }, error = function(e) {})
      }

      shiny::tagList(
        shiny::div(
          class = "row g-2",
          shiny::div(
            class = "col-md-6",
            if (length(sheet_choices) > 1) {
              shiny::selectInput(
                inputId = "import_excel_sheet",
                label = "Feuille \u00e0 importer :",
                choices = sheet_choices,
                selected = sheet_choices[1]
              )
            } else {
              shiny::textInput(
                inputId = "import_excel_sheet",
                label = "Feuille (nom ou num\u00e9ro 1-index\u00e9) :",
                value = "1"
              )
            }
          ),
          shiny::div(
            class = "col-md-6",
            shiny::selectInput(
              inputId = "import_excel_col_names",
              label = "Noms de colonnes (en-t\u00eate) :",
              choices = c(
                "Oui (premi\u00e8re ligne)" = "TRUE",
                "Non (g\u00e9n\u00e9rer ..1, ..2)" = "FALSE"
              ),
              selected = "TRUE"
            )
          )
        ),
        shiny::p(
          class = "text-muted small mb-0 mt-2",
          "Utilise la fonction performante `readxl::read_excel()`."
        )
      )
    } else if (fmt == "spss") {
      shiny::tagList(
        shiny::checkboxInput(
          inputId = "import_spss_factors",
          label = "Convertir les variables labellis\u00e9es SPSS en facteurs (haven::as_factor)",
          value = TRUE
        ),
        shiny::p(
          class = "text-muted small mb-0",
          "Utilise `haven::read_sav()` pour une compatibilit\u00e9 native avec IBM SPSS Statistics."
        )
      )
    } else if (fmt == "stata") {
      shiny::tagList(
        shiny::checkboxInput(
          inputId = "import_stata_factors",
          label = "Convertir les variables labellis\u00e9es Stata en facteurs (haven::as_factor)",
          value = TRUE
        ),
        shiny::p(
          class = "text-muted small mb-0",
          "Utilise `haven::read_dta()` pour la lecture des fichiers Stata (.dta)."
        )
      )
    } else if (fmt == "rds") {
      shiny::tagList(
        shiny::p(
          class = "text-muted small mb-0",
          "Objet s\u00e9rialis\u00e9 R (.rds) : sera lu directement via la fonction standard `readRDS()`."
        )
      )
    }
  })

  # Affichage de l'aper\u00e7u / statut du fichier s\u00e9lectionn\u00e9
  output$import_preview_info <- shiny::renderUI({
    if (is.null(input$import_file)) {
      shiny::div(
        class = "alert alert-secondary py-2 px-3 small mb-0",
        "Veuillez choisir un fichier pour afficher les d\u00e9tails et d\u00e9bloquer l'importation."
      )
    } else {
      file <- input$import_file
      size_kb <- round(file$size / 1024, 1)
      ext <- tools::file_ext(file$name)
      shiny::div(
        class = "alert alert-success py-2 px-3 small mb-0 d-flex justify-content-between align-items-center",
        shiny::span(
          shiny::strong(file$name),
          sprintf(" (extension : .%s, taille : %s Ko)", ext, size_kb)
        ),
        shiny::tags$span(class = "badge bg-success", "Pr\u00eat \u00e0 \u00eatre charg\u00e9")
      )
    }
  })

  # Validation et chargement des donn\u00e9es
  shiny::observeEvent(input$btn_validate_import, {
    if (is.null(input$import_file)) {
      shiny::showNotification(
        "Veuillez d'abord s\u00e9lectionner un fichier avant de valider.",
        type = "warning"
      )
      return()
    }

    file <- input$import_file
    fmt <- detected_format()

    tryCatch({
      loaded_df <- NULL
      r_snippet <- ""
      target_name <- if (nzchar(trimws(input$import_dataset_name))) {
        trimws(input$import_dataset_name)
      } else {
        tools::file_path_sans_ext(file$name)
      }

      # 1. Lecture selon le format s\u00e9lectionn\u00e9
      if (fmt == "csv") {
        sep_val <- if (!is.null(input$import_sep)) input$import_sep else ","
        dec_val <- if (!is.null(input$import_dec)) input$import_dec else "."
        header_val <- if (!is.null(input$import_header)) as.logical(input$import_header) else TRUE
        saf_val <- if (!is.null(input$import_stringsAsFactors)) as.logical(input$import_stringsAsFactors) else TRUE

        # S\u00e9lection propre de la fonction (read.csv2 pour le format europ\u00e9en classique)
        if (sep_val == ";" && dec_val == ",") {
          loaded_df <- utils::read.csv2(
            file$datapath,
            header = header_val,
            stringsAsFactors = saf_val
          )
          r_snippet <- sprintf(
            '%s <- read.csv2(%s, header = %s, stringsAsFactors = %s)',
            ramses_code_symbol(target_name), ramses_code_string(file$name), header_val, saf_val
          )
        } else {
          loaded_df <- utils::read.table(
            file$datapath,
            header = header_val,
            sep = sep_val,
            dec = dec_val,
            stringsAsFactors = saf_val
          )
          r_snippet <- sprintf(
            '%s <- read.table(%s, header = %s, sep = %s, dec = %s, stringsAsFactors = %s)',
            ramses_code_symbol(target_name), ramses_code_string(file$name), header_val, ramses_code_string(sep_val), ramses_code_string(dec_val), saf_val
          )
        }

      } else if (fmt == "excel") {
        sheet_input <- if (!is.null(input$import_excel_sheet)) input$import_excel_sheet else "1"
        sheet_val <- if (grepl("^[0-9]+$", sheet_input)) as.numeric(sheet_input) else sheet_input
        col_names_val <- if (!is.null(input$import_excel_col_names)) as.logical(input$import_excel_col_names) else TRUE

        loaded_df <- readxl::read_excel(
          file$datapath,
          sheet = sheet_val,
          col_names = col_names_val
        )

        sheet_code <- if (is.numeric(sheet_val)) sheet_val else ramses_code_string(sheet_val)
        r_snippet <- sprintf(
          'library(readxl)\n%s <- read_excel(%s, sheet = %s, col_names = %s)',
          ramses_code_symbol(target_name), ramses_code_string(file$name), sheet_code, col_names_val
        )

      } else if (fmt == "spss") {
        loaded_df <- haven::read_sav(file$datapath)
        as_fact <- if (!is.null(input$import_spss_factors)) isTRUE(input$import_spss_factors) else TRUE
        if (as_fact) {
          loaded_df <- haven::as_factor(loaded_df)
        }

        r_snippet <- sprintf(
          'library(haven)\n%s <- read_sav(%s)%s',
          ramses_code_symbol(target_name),
          ramses_code_string(file$name),
          if (as_fact) paste0('\n', ramses_code_symbol(target_name), ' <- as_factor(', ramses_code_symbol(target_name), ')') else ''
        )

      } else if (fmt == "stata") {
        loaded_df <- haven::read_dta(file$datapath)
        as_fact <- if (!is.null(input$import_stata_factors)) isTRUE(input$import_stata_factors) else TRUE
        if (as_fact) {
          loaded_df <- haven::as_factor(loaded_df)
        }

        r_snippet <- sprintf(
          'library(haven)\n%s <- read_dta(%s)%s',
          ramses_code_symbol(target_name),
          ramses_code_string(file$name),
          if (as_fact) paste0('\n', ramses_code_symbol(target_name), ' <- as_factor(', ramses_code_symbol(target_name), ')') else ''
        )

      } else if (fmt == "rds") {
        loaded_df <- readRDS(file$datapath)
        r_snippet <- sprintf('%s <- readRDS(%s)', ramses_code_symbol(target_name), ramses_code_string(file$name))

      } else {
        stop("Format de donn\u00e9es inconnu ou non support\u00e9.")
      }

      # 2. Conversion en data.frame standard
      final_df <- as.data.frame(loaded_df)

      # 3. Mise \u00e0 jour de la variable r\u00e9active
      data_holder$name <- target_name
      data_holder$df <- final_df
      data_holder$source_file_name <- file$name
      data_holder$source_file_datapath <- file$datapath

      # 4. Inscription du code d'importation dans le journal R Markdown
      code_entry <- paste0(
        "# Importation du jeu de donn\u00e9es depuis le fichier source\n",
        r_snippet, "\n\n",
        "# V\u00e9rification de la structure et aper\u00e7u\n",
        sprintf("dim(%s)\n", ramses_code_symbol(target_name)),
        sprintf("head(%s)", ramses_code_symbol(target_name))
      )

      append_to_rmd(
        title = paste0("Importation des donn\u00e9es (", file$name, ")"),
        code = code_entry
      )

      # 5. Fermeture de la modale
      shiny::removeModal()

      # 6. Notification utilisateur de succ\u00e8s
      shiny::showNotification(
        "Donn\u00e9es charg\u00e9es avec succ\u00e8s !",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      shiny::showNotification(
        paste0("Erreur lors du chargement : ", e$message),
        type = "error",
        duration = 8
      )
    })
  })

  # Helper interne pour extraire le code R ex\u00e9cutable pur depuis le texte R Markdown
  extract_r_script_from_rmd <- function(rmd_text) {
    temp_rmd <- tempfile(fileext = ".Rmd")
    temp_r <- tempfile(fileext = ".R")
    writeLines(rmd_text, temp_rmd)
    on.exit(unlink(c(temp_rmd, temp_r)), add = TRUE)

    # Tentative d'extraction avec knitr::purl
    res <- tryCatch({
      knitr::purl(input = temp_rmd, output = temp_r, documentation = 1L, quiet = TRUE)
      paste(readLines(temp_r, warn = FALSE), collapse = "\n")
    }, error = function(e) {
      NULL
    })

    # Fallback par parsing ligne par ligne si purl rencontre une contrainte d'environnement
    if (is.null(res) || !nzchar(trimws(res))) {
      lines <- strsplit(rmd_text, "\n")[[1]]
      in_chunk <- FALSE
      r_lines <- character(0)
      for (l in lines) {
        if (grepl("^```\\{r", l)) {
          in_chunk <- TRUE
        } else if (grepl("^```\\s*$", l) && in_chunk) {
          in_chunk <- FALSE
          r_lines <- c(r_lines, "")
        } else if (in_chunk) {
          r_lines <- c(r_lines, l)
        } else if (grepl("^##\\s+", l)) {
          r_lines <- c(r_lines, paste0("\n# ", l))
        }
      }
      res <- paste(r_lines, collapse = "\n")
    }

    res
  }

  # Fen\u00eatre modale de configuration et d'exportation du rapport
  modal_export_report <- function() {
    shiny::modalDialog(
      title = shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$span(style = "font-weight: 600;", "Exporter l'analyse & le rapport final")
      ),
      size = "l",
      easyClose = TRUE,
      footer = shiny::modalButton("Fermer"),

      shiny::div(
        class = "mb-4",
        shiny::p(
          class = "text-muted small mb-3",
          "S\u00e9lectionnez le format d'exportation adapt\u00e9 \u00e0 vos besoins de partage ou de publication scientifique :"
        ),

        # 3 Cartes de formats disponibles
        shiny::div(
          class = "row g-3 mb-4",

          # Option 1 : Script R simple (.R)
          shiny::div(
            class = "col-md-4",
            shiny::div(
              class = "card h-100 border shadow-sm p-3 text-center d-flex flex-column justify-content-between",
              shiny::div(
                shiny::div(
                  class = "mb-2",
                  shiny::tags$span(class = "badge bg-light text-dark border px-3 py-1 font-monospace", ".R")
                ),
                shiny::tags$h6(class = "fw-bold mb-1", "Script R (.R)"),
                shiny::tags$p(class = "small text-muted mb-3", "Code R ex\u00e9cutable pur, pr\u00eat \u00e0 ex\u00e9cuter dans RStudio ou en batch sans syntaxe Markdown.")
              ),
              shiny::downloadButton(
                outputId = "download_r_script",
                label = "T\u00e9l\u00e9charger (.R)",
                class = "btn-outline-primary btn-sm w-100"
              )
            )
          ),

          # Option 2 : R Markdown (.Rmd)
          shiny::div(
            class = "col-md-4",
            shiny::div(
              class = "card h-100 border shadow-sm p-3 text-center d-flex flex-column justify-content-between",
              shiny::div(
                shiny::div(
                  class = "mb-2",
                  shiny::tags$span(class = "badge bg-light text-dark border px-3 py-1 font-monospace", ".Rmd")
                ),
                shiny::tags$h6(class = "fw-bold mb-1", "Document Rmd (.Rmd)"),
                shiny::tags$p(class = "small text-muted mb-3", "Fichier source complet avec en-t\u00eate YAML, textes, chunks knitr et commentaires.")
              ),
              shiny::downloadButton(
                outputId = "download_rmd_file",
                label = "T\u00e9l\u00e9charger (.Rmd)",
                class = "btn-outline-success btn-sm w-100"
              )
            )
          ),

          # Option 3 : Rapport HTML compil\u00e9 (.html)
          shiny::div(
            class = "col-md-4",
            shiny::div(
              class = "card h-100 border shadow-sm p-3 text-center d-flex flex-column justify-content-between",
              shiny::div(
                shiny::div(
                  class = "mb-2",
                  shiny::tags$span(class = "badge bg-dark text-white px-3 py-1 font-monospace", ".html")
                ),
                shiny::tags$h6(class = "fw-bold text-dark mb-1", "Rapport HTML (.html)"),
                shiny::tags$p(class = "small text-muted mb-3", "Document Web interactif compil\u00e9 avec knitr et pandoc, pr\u00eat pour publication.")
              ),
              shiny::downloadButton(
                outputId = "download_html_report",
                label = "G\u00e9n\u00e9rer HTML (.html)",
                class = "btn-dark btn-sm w-100 shadow-sm"
              )
            )
          )
        ),

        # Section de personnalisation du rapport
        shiny::div(
          class = "card border bg-light p-3 mb-3",
          shiny::tags$h6(
            class = "fw-bold text-dark mb-3 d-flex align-items-center gap-2",
            "Configuration des m\u00e9tadonn\u00e9es & Th\u00e8me HTML"
          ),
          shiny::div(
            class = "row g-3",
            shiny::div(
              class = "col-md-6",
              shiny::textInput(
                inputId = "export_report_title",
                label = "Titre du document :",
                value = paste0("Rapport d'analyse statistique - ", data_holder$name)
              )
            ),
            shiny::div(
              class = "col-md-6",
              shiny::textInput(
                inputId = "export_report_author",
                label = "Auteur :",
                value = "Utilisateur Ramses"
              )
            ),
            shiny::div(
              class = "col-md-6",
              shiny::selectInput(
                inputId = "export_report_theme",
                label = "Th\u00e8me visuel HTML (knitr / rmarkdown) :",
                choices = c(
                  "Par d\u00e9faut (default)" = "default",
                  "Cerulean (Style bleu \u00e9pur\u00e9)" = "cerulean",
                  "Journal (Style presse minimaliste)" = "journal",
                  "Flatly (Design moderne & plat)" = "flatly",
                  "Readable (Haute lisibilit\u00e9 typographique)" = "readable"
                ),
                selected = "flatly"
              )
            ),
            shiny::div(
              class = "col-md-6 d-flex align-items-center pt-3",
              shiny::checkboxInput(
                inputId = "export_report_echo",
                label = "Inclure les blocs de code R source dans le rapport (echo = TRUE)",
                value = TRUE
              )
            )
          )
        ),

        # Aper\u00e7u du contenu actuel du journal
        shiny::div(
          class = "card border p-2 bg-white",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-1",
            shiny::tags$span(class = "small fw-semibold text-muted", "Aper\u00e7u du journal R Markdown actuel :"),
            shiny::tags$span(class = "badge bg-secondary-subtle text-secondary font-monospace", shiny::textOutput("rmd_stats_badge", inline = TRUE))
          ),
          shiny::tags$pre(
            style = "max-height: 160px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-size: 0.8rem; margin: 0;",
            rmd_log()
          )
        )
      )
    )
  }

  # D\u00e9clencheurs pour l'ouverture de la fen\u00eatre d'exportation
  open_export_modal <- function() {
    shiny::showModal(modal_export_report())
  }
  shiny::observeEvent(input$btn_export_report, { open_export_modal() })
  shiny::observeEvent(input$btn_export_from_journal, { open_export_modal() })

  # Helpers centralis\u00e9s d'exportation (r\u00e9utilis\u00e9s par l'export standard et le cycle de vie)
  export_generate_filename <- function(default_slug, ext) {
    slug <- gsub("[^A-Za-z0-9_]+", "_", if (!is.null(input$export_report_title)) input$export_report_title else default_slug)
    slug <- gsub("^_+|_+$", "", slug)
    if (!nzchar(slug)) slug <- default_slug
    paste0(slug, "_", format(Sys.Date(), "%Y%m%d_%H%M%S"), ".", ext)
  }

  export_write_r_script <- function(file) {
    r_code <- extract_r_script_from_rmd(rmd_log())
    rep_title <- if (!is.null(input$export_report_title) && nzchar(input$export_report_title)) input$export_report_title else "Script R Ramses"
    rep_author <- if (!is.null(input$export_report_author) && nzchar(input$export_report_author)) input$export_report_author else "Utilisateur Ramses"

    header_comment <- paste0(
      "################################################################\n",
      "# ", rep_title, "\n",
      "# Auteur : ", rep_author, "\n",
      "# Date   : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
      "# Jeu de donn\u00e9es : ", data_holder$name, "\n",
      "# G\u00e9n\u00e9r\u00e9 automatiquement par le package Ramses\n",
      "################################################################\n\n"
    )

    writeLines(paste0(header_comment, r_code), con = file)
    mark_report_saved("R")
    shiny::showNotification("Script R (.R) g\u00e9n\u00e9r\u00e9 et t\u00e9l\u00e9charg\u00e9 avec succ\u00e8s.", type = "message")
  }

  export_write_rmd_file <- function(file) {
    rep_title <- if (!is.null(input$export_report_title) && nzchar(input$export_report_title)) input$export_report_title else paste0("Analyse statistique - ", data_holder$name)
    rep_author <- if (!is.null(input$export_report_author) && nzchar(input$export_report_author)) input$export_report_author else "Utilisateur Ramses"

    raw_rmd <- rmd_log()
    custom_rmd <- sub('title: "[^"]*"', paste0('title: "', gsub('"', '\\\\"', rep_title), '"'), raw_rmd)
    custom_rmd <- sub('author: "[^"]*"', paste0('author: "', gsub('"', '\\\\"', rep_author), '"'), custom_rmd)

    if (!is.null(data_holder$source_file_name)) {
      ext <- tools::file_ext(data_holder$source_file_name)
      base_name <- tools::file_path_sans_ext(data_holder$source_file_name)
      clean_base <- gsub("[^A-Za-z0-9_.-]", "_", base_name)
      clean_base <- gsub("_+", "_", clean_base)
      if (clean_base == "" || clean_base == "_") clean_base <- "dataset"
      safe_name <- paste0(clean_base, ".", ext)
      rel_path <- paste0("data/", safe_name)
      custom_rmd <- gsub(ramses_code_string(data_holder$source_file_name), ramses_code_string(rel_path), custom_rmd, fixed = TRUE)
    }

    writeLines(custom_rmd, con = file)
    mark_report_saved("Rmd")
    shiny::showNotification("Document R Markdown (.Rmd) t\u00e9l\u00e9charg\u00e9 avec succ\u00e8s.", type = "message")
  }

  export_write_html_report <- function(file) {
    shiny::withProgress(
      message = "G\u00e9n\u00e9ration du rapport HTML...",
      detail = "Pr\u00e9paration du document R Markdown...",
      value = 0.2,
      {
        # 1. Cr\u00e9ation d'un environnement de rendu temporaire s\u00e9curis\u00e9
        temp_dir <- tempfile("ramses_render_")
        dir.create(temp_dir)
        on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

        rep_title <- if (!is.null(input$export_report_title) && nzchar(input$export_report_title)) input$export_report_title else paste0("Rapport d'analyse statistique - ", data_holder$name)
        rep_author <- if (!is.null(input$export_report_author) && nzchar(input$export_report_author)) input$export_report_author else "Utilisateur Ramses"
        rep_theme <- if (!is.null(input$export_report_theme) && nzchar(input$export_report_theme)) input$export_report_theme else "flatly"
        rep_echo <- if (!is.null(input$export_report_echo)) isTRUE(input$export_report_echo) else TRUE

        # 2. Adaptation des options YAML et knitr
        raw_rmd <- rmd_log()
        custom_rmd <- sub('title: "[^"]*"', paste0('title: "', gsub('"', '\\\\"', rep_title), '"'), raw_rmd)
        custom_rmd <- sub('author: "[^"]*"', paste0('author: "', gsub('"', '\\\\"', rep_author), '"'), custom_rmd)
        custom_rmd <- sub('theme: [a-z0-9_-]+', paste0('theme: ', rep_theme), custom_rmd)

        if (!rep_echo) {
          custom_rmd <- sub('knitr::opts_chunk\\$set\\(echo = TRUE', 'knitr::opts_chunk$set(echo = FALSE', custom_rmd)
        }

        temp_rmd_path <- file.path(temp_dir, "rapport_analyse.Rmd")

        shiny::incProgress(0.4, detail = "Compilation knitr & pandoc...")

        # 3. Ex\u00e9cution s\u00e9curis\u00e9e via rmarkdown::render
        tryCatch({
          # Copier le fichier de donn\u00e9es s'il a \u00e9t\u00e9 import\u00e9 et adapter le Rmd
          if (!is.null(data_holder$source_file_name) && !is.null(data_holder$source_file_datapath)) {
            rel_path <- ramses_prepare_report_data_file(
              source_datapath = data_holder$source_file_datapath,
              original_name = data_holder$source_file_name,
              report_dir = temp_dir
            )
            # Remplacement s\u00e9curis\u00e9 dans le code g\u00e9n\u00e9r\u00e9
            custom_rmd <- gsub(ramses_code_string(data_holder$source_file_name), ramses_code_string(rel_path), custom_rmd, fixed = TRUE)
          } else if (!is.null(data_holder$source_file_name)) {
            stop(paste0("Le fichier source de donn\u00e9es (", data_holder$source_file_name, ") n'est plus accessible ou n'existe pas."))
          }

          writeLines(custom_rmd, con = temp_rmd_path)

          out_html <- rmarkdown::render(
            input = temp_rmd_path,
            output_format = rmarkdown::html_document(
              theme = rep_theme,
              toc = TRUE,
              toc_float = TRUE,
              toc_depth = 3,
              number_sections = FALSE
            ),
            output_dir = temp_dir,
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )

          shiny::incProgress(0.3, detail = "Finalisation du fichier HTML...")
          file.copy(out_html, file)
          mark_report_saved("HTML")

          shiny::showNotification(
            "Rapport HTML compil\u00e9 avec succ\u00e8s !",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          shiny::showNotification(
            paste0("Erreur lors de la compilation du rapport HTML : ", e$message),
            type = "error",
            duration = 10
          )

          # Document HTML de secours avec explications d\u00e9taill\u00e9es
          fallback_html <- paste0(
            "<!DOCTYPE html>\n<html>\n<head>\n",
            "<meta charset='utf-8'>\n<title>Erreur de compilation</title>\n",
            "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css'>\n",
            "</head>\n<body class='p-4 bg-light'>\n",
            "<div class='container bg-white p-4 rounded shadow-sm'>\n",
            "<h3 class='text-danger'>Rapport d'erreur de compilation R Markdown</h3>\n",
            "<p class='text-muted'>Une erreur est survenue lors de l'ex\u00e9cution de <code>rmarkdown::render()</code> :</p>\n",
            "<div class='alert alert-danger font-monospace small'>", htmltools::htmlEscape(e$message), "</div>\n",
            "<h5>Contenu du script source soumis :</h5>\n",
            "<pre class='p-3 bg-light text-dark border rounded small font-monospace' style='max-height: 400px; overflow-y: auto; background-color: #F3F4F6 !important; color: #111827 !important; border: 1px solid #E5E7EB !important;'>",
            htmltools::htmlEscape(custom_rmd),
            "</pre>\n",
            "</div>\n</body>\n</html>"
          )
          writeLines(fallback_html, con = file)
        })
      }
    )
  }

  # Handler de t\u00e9l\u00e9chargement du Script R (.R)
  output$download_r_script <- shiny::downloadHandler(
    filename = function() { export_generate_filename("script_ramses", "R") },
    content = export_write_r_script
  )
  output$lifecycle_save_r <- shiny::downloadHandler(
    filename = function() { export_generate_filename("script_ramses", "R") },
    content = export_write_r_script
  )

  # Handler de t\u00e9l\u00e9chargement du fichier R Markdown (.Rmd)
  output$download_rmd_file <- shiny::downloadHandler(
    filename = function() { export_generate_filename("analyse_ramses", "Rmd") },
    content = export_write_rmd_file
  )
  output$lifecycle_save_rmd <- shiny::downloadHandler(
    filename = function() { export_generate_filename("analyse_ramses", "Rmd") },
    content = export_write_rmd_file
  )

  # Handler de g\u00e9n\u00e9ration et t\u00e9l\u00e9chargement du Rapport HTML (.html)
  output$download_html_report <- shiny::downloadHandler(
    filename = function() { export_generate_filename("rapport_ramses", "html") },
    content = export_write_html_report
  )
  output$lifecycle_save_html <- shiny::downloadHandler(
    filename = function() { export_generate_filename("rapport_ramses", "html") },
    content = export_write_html_report
  )

  # =========================================================================
  # GESTION DU CYCLE DE VIE (NOUVELLE INSTANCE, RED\u00c9MARRER, FERMER)
  # =========================================================================

  # Dialogue modal de confirmation d'arr\u00eat simple (rapport d\u00e9j\u00e0 sauvegard\u00e9 ou vierge)
  modal_confirm_close_app <- function() {
    shiny::modalDialog(
      title = shiny::div(
        class = "d-flex align-items-center gap-2",
        fontawesome::fa("power-off", fill = "#991B1B"),
        shiny::tags$span(style = "font-weight: 600; color: #111827;", "Fermer l'application Ramses")
      ),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Annuler"),
        shiny::actionButton(
          inputId = "btn_execute_stop_clean",
          label = "Fermer Ramses",
          class = "btn-danger"
        )
      ),
      shiny::p("Souhaitez-vous vraiment fermer l'application Ramses ?"),
      shiny::div(
        class = "alert alert-warning py-2 px-3 small mb-0",
        shiny::strong("Important : "),
        "Fermer Ramses arr\u00eatera le serveur de l'application, y compris les autres instances \u00e9ventuellement ouvertes."
      )
    )
  }

  # Dialogue modal de sauvegarde avant action du cycle de vie (red\u00e9marrer ou fermer)
  modal_unsaved_changes <- function(action = c("restart", "stop")) {
    action <- match.arg(action)
    action_label <- if (action == "restart") "red\u00e9marrer" else "fermer"
    proceed_label <- if (action == "restart") "Red\u00e9marrer sans sauvegarder" else "Fermer sans sauvegarder"
    confirm_after_save_label <- if (action == "restart") "Finaliser le red\u00e9marrage" else "Finaliser la fermeture"

    shiny::modalDialog(
      title = shiny::div(
        class = "d-flex align-items-center gap-2 text-warning-emphasis",
        fontawesome::fa("triangle-exclamation", fill = "#D97706"),
        shiny::tags$span(style = "font-weight: 600; color: #111827;", "Modifications non sauvegard\u00e9es")
      ),
      size = "m",
      easyClose = FALSE,
      footer = shiny::tagList(
        shiny::modalButton("Annuler"),
        shiny::actionButton(
          inputId = "btn_lifecycle_discard_proceed",
          label = proceed_label,
          class = "btn-outline-danger btn-sm"
        )
      ),

      shiny::p(
        class = "mb-3",
        shiny::strong("Votre rapport contient des modifications non sauvegard\u00e9es. Que souhaitez-vous faire ?")
      ),

      if (action == "stop") {
        shiny::div(
          class = "alert alert-warning py-2 px-3 small mb-3",
          shiny::strong("Information importante : "),
          "Fermer Ramses arr\u00eatera le serveur de l'application, y compris les autres instances \u00e9ventuellement ouvertes."
        )
      } else {
        shiny::div(
          class = "alert alert-secondary py-2 px-3 small mb-3",
          "Le red\u00e9marrage va recharger compl\u00e8tement la session active et r\u00e9initialiser les analyses et le jeu de donn\u00e9es."
        )
      },

      shiny::div(
        class = "card border shadow-sm p-3 mb-2 bg-light",
        shiny::h6(
          class = "fw-bold mb-2 text-dark",
          shiny::HTML(paste(fontawesome::fa("floppy-disk", fill = "#374151", height = "0.9em"), " Sauvegarder puis ", action_label))
        ),
        shiny::p(
          class = "small text-muted mb-2",
          "T\u00e9l\u00e9chargez votre rapport dans le format de votre choix :"
        ),
        shiny::div(
          class = "d-flex flex-wrap gap-2 mb-3",
          shiny::downloadButton(
            outputId = "lifecycle_save_r",
            label = "Script R (.R)",
            class = "btn-outline-primary btn-sm"
          ),
          shiny::downloadButton(
            outputId = "lifecycle_save_rmd",
            label = "Document Rmd (.Rmd)",
            class = "btn-outline-success btn-sm"
          ),
          shiny::downloadButton(
            outputId = "lifecycle_save_html",
            label = "Rapport HTML (.html)",
            class = "btn-dark btn-sm"
          )
        ),
        shiny::div(
          class = "pt-2 border-top d-flex justify-content-between align-items-center flex-wrap gap-2",
          shiny::span(
            class = "small text-muted",
            "Apr\u00e8s avoir lanc\u00e9 le t\u00e9l\u00e9chargement :"
          ),
          shiny::actionButton(
            inputId = "btn_lifecycle_proceed_after_save",
            label = confirm_after_save_label,
            class = "btn-success btn-sm fw-semibold"
          )
        )
      )
    )
  }

  # Ex\u00e9cution propre de l'arr\u00eat de l'application
  execute_app_stop <- function() {
    shiny::removeModal()
    session$sendCustomMessage("ramses_stop_app", list())
    shiny::stopApp()
  }

  # Nouvelle instance ind\u00e9pendante
  shiny::observeEvent(input$menu_session_new, {
    session$sendCustomMessage("ramses_new_instance", list())
    shiny::showNotification(
      "Nouvelle instance ind\u00e9pendante ouverte dans un nouvel onglet.",
      type = "message",
      duration = 3
    )
  })

  # Red\u00e9marrer la session courante
  shiny::observeEvent(input$menu_session_restart, {
    if (ramses_should_prompt_save(report_state$dirty)) {
      pending_lifecycle_action("restart")
      shiny::showModal(modal_unsaved_changes("restart"))
    } else {
      session$reload()
    }
  })

  # Fermer l'application compl\u00e8te
  shiny::observeEvent(input$menu_session_stop, {
    if (ramses_should_prompt_save(report_state$dirty)) {
      pending_lifecycle_action("stop")
      shiny::showModal(modal_unsaved_changes("stop"))
    } else {
      shiny::showModal(modal_confirm_close_app())
    }
  })

  # Ex\u00e9cution de l'action de cycle de vie sans sauvegarder
  shiny::observeEvent(input$btn_lifecycle_discard_proceed, {
    act <- pending_lifecycle_action()
    shiny::removeModal()
    if (identical(act, "restart")) {
      session$reload()
    } else if (identical(act, "stop")) {
      execute_app_stop()
    }
  })

  # Ex\u00e9cution de l'action de cycle de vie apr\u00e8s sauvegarde
  shiny::observeEvent(input$btn_lifecycle_proceed_after_save, {
    act <- pending_lifecycle_action()
    shiny::removeModal()
    if (identical(act, "restart")) {
      session$reload()
    } else if (identical(act, "stop")) {
      execute_app_stop()
    }
  })

  # Ex\u00e9cution de la fermeture confirm\u00e9e depuis la modale simple
  shiny::observeEvent(input$btn_execute_stop_clean, {
    execute_app_stop()
  })

  # Initialisation du module de Preparation et Nettoyage des donnees
  mod_data_prep_server(
    id = "data_prep_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Initialisation du module complet de Statistiques Descriptives
  mod_descriptives_server(
    id = "descriptives_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Initialisation du module Cr\u00e9ateur Graphique (Tableau-Style Chart Builder)
  mod_chart_builder_server(
    id = "chart_builder_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Initialisation du module complet de Tests Statistiques
  mod_tests_server(
    id = "tests_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Initialisation du module d\u00e9di\u00e9 Mod\u00e8les de R\u00e9gression (Lin\u00e9aire & Logistique)
  mod_regression_server(
    id = "regression_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Navigation et gestion des pages "A propos de Ramses" et "Formation (Cours)"
  previous_tab <- shiny::reactiveVal("data_preview")

  # Suivi de l'onglet actif (sauvegarder l'onglet precedent avant d'ouvrir A propos ou Formation)
  shiny::observeEvent(input$main_nav, {
    if (!is.null(input$main_nav) && nzchar(input$main_nav) && 
        input$main_nav != "about_page" && input$main_nav != "cours_stats_page") {
      previous_tab(input$main_nav)
    }
  }, ignoreInit = FALSE)

  # Clic sur "A propos de Ramses" (depuis le menu ? ou bouton historique)
  shiny::observeEvent(input$menu_open_about, {
    bslib::nav_select("main_nav", "about_page")
  })

  shiny::observeEvent(input$btn_about_ramses, {
    bslib::nav_select("main_nav", "about_page")
  })

  # Clic sur "Formation" depuis le menu ?
  # Ouvrir dans un nouvel onglet via JS avec fallback vers l'onglet integre cours_stats_page
  shiny::observeEvent(input$menu_open_formation, {
    shiny::showNotification(
      "Ouverture de la formation...",
      type = "message",
      duration = 2
    )
    shiny::getDefaultReactiveDomain()$sendCustomMessage(
      "open_formation_window",
      list(url = "cours_stats/index.html")
    )
    # Basculer egalement vers la page integree de formation
    bslib::nav_select("main_nav", "cours_stats_page")
  })

  # Clic sur "Retour a Ramses" sur la page "A propos" : revenir a l'onglet precedent
  shiny::observeEvent(input$btn_about_back, {
    target <- previous_tab()
    if (is.null(target) || !nzchar(target) || target == "about_page" || target == "cours_stats_page") {
      target <- "data_preview"
    }
    bslib::nav_select("main_nav", target)
  })

  # Clic sur "Retour a Ramses" sur la page "Formation" : revenir a l'onglet precedent
  shiny::observeEvent(input$btn_cours_back, {
    target <- previous_tab()
    if (is.null(target) || !nzchar(target) || target == "about_page" || target == "cours_stats_page") {
      target <- "data_preview"
    }
    bslib::nav_select("main_nav", target)
  })
}
