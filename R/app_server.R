#' Logique serveur de l'application Ramses
#'
#' @param input Entrées Shiny
#' @param output Sorties Shiny
#' @param session Session Shiny
#'
#' @return Rien (effets de bord Shiny)
#' @noRd
app_server <- function(input, output, session) {

  # 0. Initialisation des ressources statiques (favicon, etc.)
  res_dir <- system.file("app/www", package = "Ramses")
  if (dir.exists(res_dir)) {
    tryCatch({
      shiny::addResourcePath("ramses_res", res_dir)
      shiny::addResourcePath("www", res_dir)
    }, error = function(e) NULL)
  }

  # 1. Réactif principal contenant le jeu de données (iris par défaut)
  data_holder <- shiny::reactiveValues(
    name = "iris",
    df = datasets::iris
  )

  # 2. Réactif pour le journal R Markdown avec en-tête propre
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
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "library(Ramses)",
    "library(dplyr)",
    "```",
    "",
    "## 1. Initialisation des données",
    "",
    "```{r load-data}",
    "# Chargement du jeu de données initial",
    "data(iris)",
    "dataset <- iris",
    "head(dataset)",
    "```",
    "",
    sep = "\n"
  )

  rmd_log <- shiny::reactiveVal(initial_rmd_header)

  # Helper interne pour journaliser les opérations en R Markdown
  append_to_rmd <- function(title, code) {
    new_entry <- paste0(
      "\n## ", title, "\n\n",
      "```{r}\n",
      code, "\n",
      "```\n"
    )
    rmd_log(paste0(rmd_log(), new_entry))
  }

  # Mise à jour dynamique des sélecteurs de variables selon le dataset actif
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

  # Sommaire latéral dans le volet de l'onglet Données
  output$data_summary_sidebar <- shiny::renderUI({
    df <- data_holder$df
    shiny::tagList(
      shiny::tags$ul(
        class = "list-unstyled small mb-0",
        shiny::tags$li(shiny::tags$strong("Nom : "), data_holder$name),
        shiny::tags$li(shiny::tags$strong("Lignes : "), nrow(df)),
        shiny::tags$li(shiny::tags$strong("Colonnes : "), ncol(df)),
        shiny::tags$li(
          shiny::tags$strong("Variables numériques : "),
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

    # 1. Détection des types de variable & Formatage des en-têtes HTML sans balise d'icône
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
          lengthMenu = "Afficher _MENU_ entrées",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
          paginate = list(
            previous = "Précédent",
            `next` = "Suivant"
          )
        )
      ),
      class = "compact stripe hover border",
      rownames = TRUE
    )
  })

  # Affichage du journal R Markdown dans un bloc de code stylisé
  output$rmd_log_output <- shiny::renderText({
    rmd_log()
  })

  # Badge de statistiques du journal (nombre de lignes et blocs R)
  output$rmd_stats_badge <- shiny::renderText({
    text <- rmd_log()
    lines_count <- length(strsplit(text, "\n")[[1]])
    chunks_count <- length(gregexpr("```\\{r", text)[[1]])
    if (chunks_count == 1 && gregexpr("```\\{r", text)[[1]][1] == -1) chunks_count <- 0
    paste0(lines_count, " lignes • ", chunks_count, " blocs R")
  })

  # Copier le script Rmd dans le presse-papier
  shiny::observeEvent(input$btn_copy_rmd, {
    shiny::showNotification(
      "Script R Markdown copié dans le presse-papier !",
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
            label = "Insérer dans le journal",
            class = "btn-dark"
          )
        ),
        shiny::p(
          class = "text-muted small mb-3",
          "Insérez des commentaires d'analyse, hypothèses ou conclusions en syntaxe Markdown. Ces notes s'insèreront entre les blocs de code R reproductibles."
        ),
        shiny::textInput(
          inputId = "rmd_note_title",
          label = "Titre de la section / remarque :",
          placeholder = "Ex : Interprétation clinique et conclusions"
        ),
        shiny::textAreaInput(
          inputId = "rmd_note_content",
          label = "Commentaires (Markdown accepté) :",
          rows = 5,
          placeholder = "Ex : Les résultats montrent une corrélation statistiquement significative (p < 0.05). Une analyse complémentaire sera requise..."
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
    shiny::removeModal()
    shiny::showNotification("Note textuelle insérée dans le journal R Markdown !", type = "message")
  })

  # Dialogue modal de confirmation d'effacement du journal
  shiny::observeEvent(input$btn_clear_rmd, {
    shiny::showModal(
      shiny::modalDialog(
        title = shiny::div(
          class = "d-flex align-items-center gap-2 text-danger",
          shiny::tags$span(style = "font-weight: 600;", "Réinitialiser le Journal R Markdown ?")
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(
            inputId = "btn_confirm_clear_rmd",
            label = "Oui, effacer et réinitialiser",
            class = "btn-danger"
          )
        ),
        shiny::p("Attention : Cette action réinitialisera l'intégralité du script R Markdown au modèle de départ avec le jeu de données initial."),
        shiny::p(class = "text-muted small mb-0", "Toutes les étapes d'analyse descriptives, graphiques ou tests non exportés seront effacés.")
      )
    )
  })

  # Confirmation de l'effacement
  shiny::observeEvent(input$btn_confirm_clear_rmd, {
    rmd_log(initial_rmd_header)
    shiny::removeModal()
    shiny::showNotification("Le journal R Markdown a été réinitialisé.", type = "message")
  })

  # Réinitialiser avec le jeu de données iris
  shiny::observeEvent(input$sidebar_btn_reset_iris, {
    data_holder$name <- "iris"
    data_holder$df <- datasets::iris
    shiny::showNotification("Jeu de données 'iris' rechargé.", type = "default")
  })

  # Ouvrir la fenêtre modale d'importation
  open_import_modal <- function() {
    shiny::showModal(modal_import_data())
  }

  shiny::observeEvent(input$btn_import, { open_import_modal() })
  shiny::observeEvent(input$sidebar_btn_import, { open_import_modal() })
  shiny::observeEvent(input$menu_btn_import, { open_import_modal() })

  # Détection du format effectif (si auto-détection choisie)
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
              label = "Séparateur de champs :",
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
              label = "Séparateur décimal :",
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
              label = "En-tête (Header) :",
              choices = c(
                "Oui (1ère ligne = noms)" = "TRUE",
                "Non (pas d'en-tête)" = "FALSE"
              ),
              selected = "TRUE"
            )
          )
        ),
        shiny::div(
          class = "mt-2",
          shiny::checkboxInput(
            inputId = "import_stringsAsFactors",
            label = "Convertir les chaînes de caractères en facteurs (stringsAsFactors)",
            value = TRUE
          )
        )
      )
    } else if (fmt == "excel") {
      # Récupération dynamique des feuilles si un fichier Excel a été téléversé
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
                label = "Feuille à importer :",
                choices = sheet_choices,
                selected = sheet_choices[1]
              )
            } else {
              shiny::textInput(
                inputId = "import_excel_sheet",
                label = "Feuille (nom ou numéro 1-indexé) :",
                value = "1"
              )
            }
          ),
          shiny::div(
            class = "col-md-6",
            shiny::selectInput(
              inputId = "import_excel_col_names",
              label = "Noms de colonnes (en-tête) :",
              choices = c(
                "Oui (première ligne)" = "TRUE",
                "Non (générer ..1, ..2)" = "FALSE"
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
          label = "Convertir les variables labellisées SPSS en facteurs (haven::as_factor)",
          value = TRUE
        ),
        shiny::p(
          class = "text-muted small mb-0",
          "Utilise `haven::read_sav()` pour une compatibilité native avec IBM SPSS Statistics."
        )
      )
    } else if (fmt == "stata") {
      shiny::tagList(
        shiny::checkboxInput(
          inputId = "import_stata_factors",
          label = "Convertir les variables labellisées Stata en facteurs (haven::as_factor)",
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
          "Objet sérialisé R (.rds) : sera lu directement via la fonction standard `readRDS()`."
        )
      )
    }
  })

  # Affichage de l'aperçu / statut du fichier sélectionné
  output$import_preview_info <- shiny::renderUI({
    if (is.null(input$import_file)) {
      shiny::div(
        class = "alert alert-secondary py-2 px-3 small mb-0",
        "Veuillez choisir un fichier pour afficher les détails et débloquer l'importation."
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
        shiny::tags$span(class = "badge bg-success", "Prêt à être chargé")
      )
    }
  })

  # Validation et chargement des données
  shiny::observeEvent(input$btn_validate_import, {
    if (is.null(input$import_file)) {
      shiny::showNotification(
        "Veuillez d'abord sélectionner un fichier avant de valider.",
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

      # 1. Lecture selon le format sélectionné
      if (fmt == "csv") {
        sep_val <- if (!is.null(input$import_sep)) input$import_sep else ","
        dec_val <- if (!is.null(input$import_dec)) input$import_dec else "."
        header_val <- if (!is.null(input$import_header)) as.logical(input$import_header) else TRUE
        saf_val <- if (!is.null(input$import_stringsAsFactors)) as.logical(input$import_stringsAsFactors) else TRUE

        # Sélection propre de la fonction (read.csv2 pour le format européen classique)
        if (sep_val == ";" && dec_val == ",") {
          loaded_df <- utils::read.csv2(
            file$datapath,
            header = header_val,
            stringsAsFactors = saf_val
          )
          r_snippet <- sprintf(
            '%s <- read.csv2("%s", header = %s, stringsAsFactors = %s)',
            target_name, file$name, header_val, saf_val
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
            '%s <- read.table("%s", header = %s, sep = "%s", dec = "%s", stringsAsFactors = %s)',
            target_name, file$name, header_val, sep_val, dec_val, saf_val
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

        sheet_code <- if (is.numeric(sheet_val)) sheet_val else paste0('"', sheet_val, '"')
        r_snippet <- sprintf(
          'library(readxl)\n%s <- read_excel("%s", sheet = %s, col_names = %s)',
          target_name, file$name, sheet_code, col_names_val
        )

      } else if (fmt == "spss") {
        loaded_df <- haven::read_sav(file$datapath)
        as_fact <- if (!is.null(input$import_spss_factors)) isTRUE(input$import_spss_factors) else TRUE
        if (as_fact) {
          loaded_df <- haven::as_factor(loaded_df)
        }

        r_snippet <- sprintf(
          'library(haven)\n%s <- read_sav("%s")%s',
          target_name,
          file$name,
          if (as_fact) paste0('\n', target_name, ' <- as_factor(', target_name, ')') else ''
        )

      } else if (fmt == "stata") {
        loaded_df <- haven::read_dta(file$datapath)
        as_fact <- if (!is.null(input$import_stata_factors)) isTRUE(input$import_stata_factors) else TRUE
        if (as_fact) {
          loaded_df <- haven::as_factor(loaded_df)
        }

        r_snippet <- sprintf(
          'library(haven)\n%s <- read_dta("%s")%s',
          target_name,
          file$name,
          if (as_fact) paste0('\n', target_name, ' <- as_factor(', target_name, ')') else ''
        )

      } else if (fmt == "rds") {
        loaded_df <- readRDS(file$datapath)
        r_snippet <- sprintf('%s <- readRDS("%s")', target_name, file$name)

      } else {
        stop("Format de données inconnu ou non supporté.")
      }

      # 2. Conversion en data.frame standard
      final_df <- as.data.frame(loaded_df)

      # 3. Mise à jour de la variable réactive
      data_holder$name <- target_name
      data_holder$df <- final_df

      # 4. Inscription du code d'importation dans le journal R Markdown
      code_entry <- paste0(
        "# Importation du jeu de données depuis le fichier source\n",
        r_snippet, "\n\n",
        "# Vérification de la structure et aperçu\n",
        sprintf("dim(%s)\n", target_name),
        sprintf("head(%s)", target_name)
      )

      append_to_rmd(
        title = paste0("Importation des données (", file$name, ")"),
        code = code_entry
      )

      # 5. Fermeture de la modale
      shiny::removeModal()

      # 6. Notification utilisateur de succès
      shiny::showNotification(
        "Données chargées avec succès !",
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

  # Helper interne pour extraire le code R exécutable pur depuis le texte R Markdown
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

  # Fenêtre modale de configuration et d'exportation du rapport
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
          "Sélectionnez le format d'exportation adapté à vos besoins de partage ou de publication scientifique :"
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
                shiny::tags$p(class = "small text-muted mb-3", "Code R exécutable pur, prêt à exécuter dans RStudio ou en batch sans syntaxe Markdown.")
              ),
              shiny::downloadButton(
                outputId = "download_r_script",
                label = "Télécharger (.R)",
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
                shiny::tags$p(class = "small text-muted mb-3", "Fichier source complet avec en-tête YAML, textes, chunks knitr et commentaires.")
              ),
              shiny::downloadButton(
                outputId = "download_rmd_file",
                label = "Télécharger (.Rmd)",
                class = "btn-outline-success btn-sm w-100"
              )
            )
          ),

          # Option 3 : Rapport HTML compilé (.html)
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
                shiny::tags$p(class = "small text-muted mb-3", "Document Web interactif compilé avec knitr et pandoc, prêt pour publication.")
              ),
              shiny::downloadButton(
                outputId = "download_html_report",
                label = "Générer HTML (.html)",
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
            "Configuration des métadonnées & Thème HTML"
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
                label = "Thème visuel HTML (knitr / rmarkdown) :",
                choices = c(
                  "Par défaut (default)" = "default",
                  "Cerulean (Style bleu épuré)" = "cerulean",
                  "Journal (Style presse minimaliste)" = "journal",
                  "Flatly (Design moderne & plat)" = "flatly",
                  "Readable (Haute lisibilité typographique)" = "readable"
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

        # Aperçu du contenu actuel du journal
        shiny::div(
          class = "card border p-2 bg-white",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-1",
            shiny::tags$span(class = "small fw-semibold text-muted", "Aperçu du journal R Markdown actuel :"),
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

  # Déclencheurs pour l'ouverture de la fenêtre d'exportation
  open_export_modal <- function() {
    shiny::showModal(modal_export_report())
  }
  shiny::observeEvent(input$btn_export_report, { open_export_modal() })
  shiny::observeEvent(input$btn_export_from_journal, { open_export_modal() })

  # Handler de téléchargement du Script R (.R)
  output$download_r_script <- shiny::downloadHandler(
    filename = function() {
      slug <- gsub("[^A-Za-z0-9_]+", "_", if (!is.null(input$export_report_title)) input$export_report_title else "script_ramses")
      slug <- gsub("^_+|_+$", "", slug)
      if (!nzchar(slug)) slug <- "script_ramses"
      paste0(slug, "_", format(Sys.Date(), "%Y%m%d_%H%M%S"), ".R")
    },
    content = function(file) {
      r_code <- extract_r_script_from_rmd(rmd_log())
      rep_title <- if (!is.null(input$export_report_title) && nzchar(input$export_report_title)) input$export_report_title else "Script R Ramses"
      rep_author <- if (!is.null(input$export_report_author) && nzchar(input$export_report_author)) input$export_report_author else "Utilisateur Ramses"

      header_comment <- paste0(
        "################################################################\n",
        "# ", rep_title, "\n",
        "# Auteur : ", rep_author, "\n",
        "# Date   : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        "# Jeu de données : ", data_holder$name, "\n",
        "# Généré automatiquement par le package Ramses\n",
        "################################################################\n\n"
      )

      writeLines(paste0(header_comment, r_code), con = file)
      shiny::showNotification("Script R (.R) généré et téléchargé avec succès.", type = "message")
    }
  )

  # Handler de téléchargement du fichier R Markdown (.Rmd)
  output$download_rmd_file <- shiny::downloadHandler(
    filename = function() {
      slug <- gsub("[^A-Za-z0-9_]+", "_", if (!is.null(input$export_report_title)) input$export_report_title else "analyse_ramses")
      slug <- gsub("^_+|_+$", "", slug)
      if (!nzchar(slug)) slug <- "analyse_ramses"
      paste0(slug, "_", format(Sys.Date(), "%Y%m%d_%H%M%S"), ".Rmd")
    },
    content = function(file) {
      rep_title <- if (!is.null(input$export_report_title) && nzchar(input$export_report_title)) input$export_report_title else paste0("Analyse statistique - ", data_holder$name)
      rep_author <- if (!is.null(input$export_report_author) && nzchar(input$export_report_author)) input$export_report_author else "Utilisateur Ramses"

      raw_rmd <- rmd_log()
      custom_rmd <- sub('title: "[^"]*"', paste0('title: "', gsub('"', '\\\\"', rep_title), '"'), raw_rmd)
      custom_rmd <- sub('author: "[^"]*"', paste0('author: "', gsub('"', '\\\\"', rep_author), '"'), custom_rmd)

      writeLines(custom_rmd, con = file)
      shiny::showNotification("Document R Markdown (.Rmd) téléchargé avec succès.", type = "message")
    }
  )

  # Handler de génération et téléchargement du Rapport HTML (.html)
  output$download_html_report <- shiny::downloadHandler(
    filename = function() {
      slug <- gsub("[^A-Za-z0-9_]+", "_", if (!is.null(input$export_report_title)) input$export_report_title else "rapport_ramses")
      slug <- gsub("^_+|_+$", "", slug)
      if (!nzchar(slug)) slug <- "rapport_ramses"
      paste0(slug, "_", format(Sys.Date(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      shiny::withProgress(
        message = "Génération du rapport HTML...",
        detail = "Préparation du document R Markdown...",
        value = 0.2,
        {
          # 1. Création d'un environnement de rendu temporaire sécurisé
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
          writeLines(custom_rmd, con = temp_rmd_path)

          shiny::incProgress(0.4, detail = "Compilation knitr & pandoc...")

          # 3. Exécution sécurisée via rmarkdown::render
          tryCatch({
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

            shiny::showNotification(
              "Rapport HTML compilé avec succès !",
              type = "message",
              duration = 5
            )
          }, error = function(e) {
            shiny::showNotification(
              paste0("Erreur lors de la compilation du rapport HTML : ", e$message),
              type = "error",
              duration = 10
            )

            # Document HTML de secours avec explications détaillées
            fallback_html <- paste0(
              "<!DOCTYPE html>\n<html>\n<head>\n",
              "<meta charset='utf-8'>\n<title>Erreur de compilation</title>\n",
              "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css'>\n",
              "</head>\n<body class='p-4 bg-light'>\n",
              "<div class='container bg-white p-4 rounded shadow-sm'>\n",
              "<h3 class='text-danger'>Rapport d'erreur de compilation R Markdown</h3>\n",
              "<p class='text-muted'>Une erreur est survenue lors de l'exécution de <code>rmarkdown::render()</code> :</p>\n",
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
  )

  # Initialisation du module complet de Statistiques Descriptives
  mod_descriptives_server(
    id = "descriptives_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Initialisation du module Créateur Graphique (Tableau-Style Chart Builder)
  mod_chart_builder_server(
    id = "chart_builder_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )

  # Initialisation du module complet de Tests Statistiques & Modélisation
  mod_tests_server(
    id = "tests_module",
    data_holder = data_holder,
    append_to_rmd = append_to_rmd
  )
}
