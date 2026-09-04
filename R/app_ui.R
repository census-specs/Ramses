#' Boîte de dialogue modale d'importation de données
#'
#' @return Une structure modale Shiny.
#' @noRd
modal_import_data <- function() {
  shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-center gap-2",
      shiny::tags$span(style = "font-weight: 600; color: #111827;", "Importer un fichier de données")
    ),
    size = "l",
    easyClose = FALSE,
    footer = shiny::tagList(
      shiny::modalButton("Annuler"),
      shiny::actionButton(
        inputId = "btn_validate_import",
        label = "Valider et Charger",
        class = "btn-dark"
      )
    ),

    # Sélecteur de fichier
    shiny::div(
      class = "mb-3",
      shiny::fileInput(
        inputId = "import_file",
        label = shiny::strong("Sélectionner un fichier sur votre poste :"),
        accept = c(
          ".csv", ".txt", ".tsv",
          ".xlsx", ".xls",
          ".sav", ".dta",
          ".rds", ".RData"
        ),
        buttonLabel = "Parcourir...",
        placeholder = "Formats acceptés : .csv, .txt, .xlsx, .xls, .sav, .dta, .rds"
      )
    ),

    # Ligne de configuration générale (Format & Nom assigné)
    shiny::div(
      class = "row g-3 mb-3",
      shiny::div(
        class = "col-md-6",
        shiny::selectInput(
          inputId = "import_format",
          label = shiny::strong("Format du fichier :"),
          choices = c(
            "Auto-détection (selon l'extension)" = "auto",
            "Fichier texte délimité (CSV, TXT)" = "csv",
            "Feuille de calcul Excel (.xlsx, .xls)" = "excel",
            "Fichier SPSS (.sav)" = "spss",
            "Fichier Stata (.dta)" = "stata",
            "Objet R sérialisé (.rds)" = "rds"
          ),
          selected = "auto"
        )
      ),
      shiny::div(
        class = "col-md-6",
        shiny::textInput(
          inputId = "import_dataset_name",
          label = shiny::strong("Nom de l'objet R créé :"),
          value = "dataset"
        )
      )
    ),

    # Options dynamiques selon le format sélectionné
    shiny::div(
      class = "card bg-light border p-3 mb-2",
      shiny::uiOutput("import_dynamic_options")
    ),

    # Retour d'information / aperçu rapide
    shiny::uiOutput("import_preview_info")
  )
}

#' Interface utilisateur principale de Ramses
#'
#' @return Une structure UI Shiny basée sur \code{bslib::page_navbar}.
#' @noRd
app_ui <- function() {
  # Déclaration sécurisée du dossier de ressources statiques pour le favicon
  res_dir <- system.file("app/www", package = "Ramses")
  if (dir.exists(res_dir)) {
    shiny::addResourcePath("ramses_res", res_dir)
  }

  bslib::page_navbar(
    id = "main_nav",
    title = shiny::tags$span(
      "RAMSES (v0.1.0)",
      style = "margin-right: 40px; font-weight: bold; color: #1F2937; font-size: 1.1rem;"
    ),
    theme = bslib::bs_theme(
      version = 5,
      bg = "#FFFFFF",
      fg = "#1F2937",
      primary = "#1F2937",
      secondary = "#6B7280",
      success = "#374151",
      info = "#4B5563",
      warning = "#4B5563",
      danger = "#991B1B"
    ),
    header = shiny::tags$head(
      # Favicon de l'application Ramses (support multi-navigateurs et mode Chromium standalone)
      shiny::tags$link(rel = "shortcut icon", href = "ramses_res/favicon.svg"),
      shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "ramses_res/favicon.svg"),
      shiny::tags$link(rel = "shortcut icon", href = "favicon.ico"),
      shiny::tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),

      # FontAwesome 6.4.2 CDN pour rendu parfait des icônes
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
      ),
      # Typographie globale Aptos Narrow & Fira Code
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Aptos+Narrow:wght@400;500;600;700&family=Fira+Code:wght@400;500;600&family=Segoe+UI:wght@400;500;600;700&display=swap"
      ),
      shiny::tags$style(shiny::HTML("
        /* === Ramses Global Typography (Aptos Narrow) & Monochrome Scientific Palette === */
        :root {
          --bs-font-sans-serif: 'Aptos Narrow', 'Aptos', 'Segoe UI', -apple-system, BlinkMacSystemFont, Roboto, sans-serif;
          --bs-body-font-family: var(--bs-font-sans-serif);
          --bs-body-font-size: 0.84rem;
          --bs-body-line-height: 1.45;
          --ramses-bg: #FFFFFF;
          --ramses-surface: #F9FAFB;
          --ramses-border: #E5E7EB;
          --ramses-text: #1F2937;
          --ramses-text-dark: #111827;
          --ramses-text-muted: #6B7280;
        }

        body, html, * {
          font-family: 'Aptos Narrow', 'Aptos', 'Segoe UI', -apple-system, BlinkMacSystemFont, Roboto, sans-serif !important;
        }

        body {
          font-size: 13.5px !important;
          background-color: #FFFFFF !important;
          color: #1F2937 !important;
          letter-spacing: -0.01em !important;
        }

        /* === Navigation Bar Monochrome Ultra-Sobre & Alignement à Gauche === */
        .navbar {
          background: #FFFFFF !important;
          background-color: #FFFFFF !important;
          border-bottom: 1px solid #E5E7EB !important;
          padding: 0.25rem 0.85rem !important;
          min-height: 44px !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.03) !important;
        }

        .navbar-brand {
          font-size: 1.05rem !important;
          font-weight: 700 !important;
          letter-spacing: -0.02em !important;
          color: #111827 !important;
          padding: 0.15rem 0.5rem !important;
          margin-right: 2.5rem !important;
        }

        .navbar-nav {
          display: flex !important;
          flex-direction: row !important;
          justify-content: flex-start !important;
          align-items: center !important;
          text-align: left !important;
        }

        .nav-link, .dropdown-item {
          display: flex !important;
          align-items: center !important;
          justify-content: flex-start !important;
          text-align: left !important;
          gap: 8px !important;
        }

        .dropdown-menu {
          text-align: left !important;
          font-size: 0.82rem !important;
          background-color: #FFFFFF !important;
          border: 1px solid #E5E7EB !important;
          border-radius: 6px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.07), 0 2px 4px -2px rgba(0, 0, 0, 0.04) !important;
          padding: 0.35rem 0 !important;
          min-width: 240px !important;
        }

        /* Forcer l'icône SVG et le texte à s'aligner naturellement de gauche à droite */
        .nav-link svg, .dropdown-item svg {
          flex-shrink: 0 !important;
          margin-right: 6px !important;
          vertical-align: -0.125em !important;
        }

        .navbar .nav-link {
          font-size: 0.83rem !important;
          font-weight: 500 !important;
          color: #4B5563 !important;
          padding: 0.35rem 0.65rem !important;
          border-radius: 4px !important;
          transition: all 0.15s ease-in-out !important;
        }

        .navbar .nav-link:hover, .navbar .nav-link:focus {
          color: #111827 !important;
          background-color: #F3F4F6 !important;
        }

        .navbar .nav-link.active, .navbar .dropdown.show > .nav-link {
          color: #111827 !important;
          background-color: #E5E7EB !important;
          font-weight: 600 !important;
        }

        .dropdown-item {
          padding: 0.4rem 0.9rem !important;
          color: #374151 !important;
          font-weight: 500 !important;
          font-size: 0.82rem !important;
        }

        .dropdown-item:hover, .dropdown-item:focus {
          background-color: #F3F4F6 !important;
          color: #111827 !important;
        }

        .dropdown-item.active {
          background-color: #E5E7EB !important;
          color: #111827 !important;
          font-weight: 600 !important;
        }

        /* === Compact Form Elements & Controls === */
        .form-control, .form-select, .selectize-input {
          font-size: 0.82rem !important;
          padding: 0.25rem 0.55rem !important;
          min-height: 28px !important;
          border-radius: 4px !important;
          border-color: #D1D5DB !important;
          color: #1F2937 !important;
          background-color: #FFFFFF !important;
        }

        .form-control:focus, .form-select:focus, .selectize-input.focus {
          border-color: #6B7280 !important;
          box-shadow: 0 0 0 2px rgba(107, 114, 128, 0.15) !important;
        }

        .form-label, label {
          font-size: 0.79rem !important;
          font-weight: 600 !important;
          margin-bottom: 0.2rem !important;
          color: #374151 !important;
        }

        .form-check-label {
          font-size: 0.8rem !important;
          font-weight: 400 !important;
          color: #374151 !important;
        }

        .form-check-input {
          margin-top: 0.18rem !important;
          border-color: #D1D5DB !important;
        }

        .form-check-input:checked {
          background-color: #1F2937 !important;
          border-color: #1F2937 !important;
        }

        /* === Boutons Monochrome Scientifiques === */
        .btn {
          font-size: 0.82rem !important;
          padding: 0.25rem 0.65rem !important;
          border-radius: 4px !important;
          font-weight: 500 !important;
          transition: all 0.15s ease !important;
        }

        .btn-sm {
          font-size: 0.78rem !important;
          padding: 0.18rem 0.5rem !important;
        }

        .btn-primary, .btn-dark {
          background-color: #1F2937 !important;
          border-color: #1F2937 !important;
          color: #FFFFFF !important;
        }

        .btn-primary:hover, .btn-primary:focus, .btn-dark:hover, .btn-dark:focus {
          background-color: #111827 !important;
          border-color: #111827 !important;
          color: #FFFFFF !important;
        }

        .btn-secondary, .btn-light {
          background-color: #F3F4F6 !important;
          border-color: #D1D5DB !important;
          color: #1F2937 !important;
        }

        .btn-secondary:hover, .btn-secondary:focus, .btn-light:hover, .btn-light:focus {
          background-color: #E5E7EB !important;
          border-color: #9CA3AF !important;
          color: #111827 !important;
        }

        .btn-outline-primary, .btn-outline-secondary, .btn-outline-dark {
          background-color: #FFFFFF !important;
          border-color: #D1D5DB !important;
          color: #374151 !important;
        }

        .btn-outline-primary:hover, .btn-outline-primary:focus,
        .btn-outline-secondary:hover, .btn-outline-secondary:focus,
        .btn-outline-dark:hover, .btn-outline-dark:focus {
          background-color: #F3F4F6 !important;
          border-color: #9CA3AF !important;
          color: #111827 !important;
        }

        .btn-success {
          background-color: #374151 !important;
          border-color: #374151 !important;
          color: #FFFFFF !important;
        }

        .btn-success:hover, .btn-success:focus {
          background-color: #1F2937 !important;
          border-color: #1F2937 !important;
          color: #FFFFFF !important;
        }

        /* === Cartes, En-têtes et Panneaux Monochrome === */
        .card {
          border-radius: 6px !important;
          border: 1px solid #E5E7EB !important;
          border-top: 1px solid #E5E7EB !important;
          box-shadow: none !important;
          margin-bottom: 0.75rem !important;
          background-color: #FFFFFF !important;
        }

        .card-header {
          padding: 0.4rem 0.75rem !important;
          font-size: 0.84rem !important;
          font-weight: 600 !important;
          background-color: #F9FAFB !important;
          border-bottom: 1px solid #E5E7EB !important;
          color: #1F2937 !important;
        }

        .card-body {
          padding: 0.75rem !important;
          background-color: #FFFFFF !important;
        }

        .sidebar {
          padding: 0.75rem !important;
          background-color: #F9FAFB !important;
          border-right: 1px solid #E5E7EB !important;
        }

        .nav-tabs {
          border-bottom: 1px solid #E5E7EB !important;
        }

        .nav-tabs .nav-link {
          font-size: 0.82rem !important;
          padding: 0.35rem 0.75rem !important;
          font-weight: 600 !important;
          color: #6B7280 !important;
          border: 1px solid transparent !important;
        }

        .nav-tabs .nav-link:hover {
          color: #111827 !important;
          border-color: #E5E7EB #E5E7EB transparent !important;
        }

        .nav-tabs .nav-link.active {
          color: #111827 !important;
          background-color: #FFFFFF !important;
          border-color: #E5E7EB #E5E7EB #FFFFFF !important;
        }

        /* === Neutralisation des bleus et couleurs sombres résiduelles === */
        .border-primary, .border-info, .border-3 {
          border-color: #E5E7EB !important;
          border-width: 1px !important;
        }

        .text-primary, .text-primary-emphasis, .text-info {
          color: #4B5563 !important;
        }

        .bg-primary, .bg-primary-subtle {
          background-color: #F3F4F6 !important;
          color: #111827 !important;
        }

        /* === Blocs de Code R Scientifiques Neutres === */
        pre, code, kbd, samp, #rmd_console_pre {
          background-color: #F3F4F6 !important;
          color: #111827 !important;
          border: 1px solid #E5E7EB !important;
          border-radius: 4px !important;
          font-family: 'Fira Code', 'Courier New', 'SFMono-Regular', Consolas, monospace !important;
        }

        /* === Tables & DataTables Styling === */
        table.dataTable {
          font-size: 0.8rem !important;
          border: 1px solid #E5E7EB !important;
        }

        table.dataTable thead th {
          background-color: #F9FAFB !important;
          color: #1F2937 !important;
          font-weight: 600 !important;
          padding: 6px 10px !important;
          border-bottom: 1px solid #E5E7EB !important;
        }

        table.dataTable tbody td {
          padding: 4px 10px !important;
          border-bottom: 1px solid #F3F4F6 !important;
          color: #1F2937 !important;
        }

        table.dataTable.stripe tbody tr.odd {
          background-color: #FAFAFA !important;
        }

        table.dataTable.hover tbody tr:hover {
          background-color: #F3F4F6 !important;
        }

        /* === Modal Dialogs === */
        .modal-content {
          border: 1px solid #E5E7EB !important;
          border-radius: 6px !important;
          background-color: #FFFFFF !important;
        }

        .modal-header {
          background-color: #F9FAFB !important;
          border-bottom: 1px solid #E5E7EB !important;
          padding: 0.6rem 1rem !important;
        }

        .modal-title {
          font-size: 0.98rem !important;
          font-weight: 600 !important;
          color: #111827 !important;
        }

        .modal-body {
          padding: 1rem !important;
          background-color: #FFFFFF !important;
        }

        .modal-footer {
          background-color: #F9FAFB !important;
          border-top: 1px solid #E5E7EB !important;
          padding: 0.5rem 1rem !important;
        }
      "))
    ),

    # =========================================================================
    # 1. MENU FICHIER & DONNÉES (Style SPSS)
    # =========================================================================
    bslib::nav_menu(
      title = shiny::HTML(paste(fontawesome::fa("database", fill = "#4B5563", height = "0.9em"), "Fichier & Données")),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("table", fill = "#6B7280", height = "0.85em"), "Aperçu des données")),
        value = "data_preview",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Gestion des données",
            width = 280,
            shiny::div(
              class = "text-muted small mb-2",
              "Jeu de données actif en mémoire pour la session."
            ),
            shiny::uiOutput("data_summary_sidebar"),
            shiny::hr(class = "my-2"),
            shiny::actionButton(
              inputId = "sidebar_btn_import",
              label = "Importer un fichier...",
              class = "btn-dark btn-sm w-100 mb-2 shadow-sm"
            ),
            shiny::actionButton(
              inputId = "sidebar_btn_reset_iris",
              label = "Réinitialiser avec 'iris'",
              class = "btn-outline-secondary btn-sm w-100"
            )
          ),
          bslib::card(
            full_screen = TRUE,
            class = "shadow-sm border-0 h-100",
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center py-2 bg-light flex-wrap gap-2",
              shiny::div(
                class = "d-flex align-items-center gap-3",
                shiny::tags$strong("Aperçu interactif du jeu de données"),
                shiny::tags$span(
                  class = "text-muted ms-1 ps-2 border-start d-none d-sm-inline",
                  shiny::HTML("<small><span style='color:#6B7280; font-weight:bold;'>[#]</span> Quantitative &nbsp;|&nbsp; <span style='color:#6B7280; font-weight:bold;'>[Aa]</span> Qualitative</small>")
                )
              ),
              shiny::uiOutput("dataset_badge")
            ),
            bslib::card_body(
              padding = 0,
              DT::dataTableOutput("dataset_table")
            )
          )
        )
      ),
      bslib::nav_item(
        shiny::actionLink(
          inputId = "menu_btn_import",
          label = shiny::HTML(paste(fontawesome::fa("file-import", fill = "#6B7280", height = "0.85em"), "Importer un fichier...")),
          class = "dropdown-item"
        )
      )
    ),

    # =========================================================================
    # 2. MENU STATISTIQUES DESCRIPTIVES (Style SPSS)
    # =========================================================================
    bslib::nav_menu(
      title = shiny::HTML(paste(fontawesome::fa("chart-pie", fill = "#4B5563", height = "0.9em"), "Statistiques Descriptives")),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("chart-line", fill = "#6B7280", height = "0.85em"), "Variables Quantitatives")),
        value = "desc_quanti",
        mod_descriptives_quanti_ui("descriptives_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("list-check", fill = "#6B7280", height = "0.85em"), "Variables Qualitatives")),
        value = "desc_quali",
        mod_descriptives_quali_ui("descriptives_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("table-cells-large", fill = "#6B7280", height = "0.85em"), "Tableaux Croisés")),
        value = "desc_cross",
        mod_descriptives_cross_ui("descriptives_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("border-all", fill = "#6B7280", height = "0.85em"), "Matrice de Corrélation")),
        value = "desc_cor",
        mod_descriptives_cor_ui("descriptives_module")
      )
    ),

    # =========================================================================
    # 3. MENU TESTS STATISTIQUES & MODÉLISATION (Style SPSS)
    # =========================================================================
    bslib::nav_menu(
      title = shiny::HTML(paste(fontawesome::fa("flask", fill = "#4B5563", height = "0.9em"), "Tests Statistiques")),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("wave-square", fill = "#6B7280", height = "0.85em"), "Normalité & Variance")),
        value = "test_norm",
        mod_tests_norm_ui("tests_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("equals", fill = "#6B7280", height = "0.85em"), "Comparaison de 2 Groupes")),
        value = "test_two",
        mod_tests_two_ui("tests_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("layer-group", fill = "#6B7280", height = "0.85em"), "Comparaison de 3+ Groupes")),
        value = "test_multi",
        mod_tests_multi_ui("tests_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("percent", fill = "#6B7280", height = "0.85em"), "Tests de Contingence")),
        value = "test_cont",
        mod_tests_cont_ui("tests_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("sliders", fill = "#6B7280", height = "0.85em"), "Modèles de Régression")),
        value = "test_reg",
        mod_tests_reg_ui("tests_module")
      )
    ),

    # =========================================================================
    # 4. ONGLET VISUALISATION (Chart Builder)
    # =========================================================================
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("chart-column", fill = "#4B5563", height = "0.9em"), "Visualisation")),
      value = "chart_builder",
      mod_chart_builder_ui("chart_builder_module")
    ),

    # =========================================================================
    # 5. ONGLET JOURNAL R MARKDOWN
    # =========================================================================
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("code", fill = "#4B5563", height = "0.9em"), "Journal Rmd")),
      value = "rmd_journal",
      bslib::card(
        full_screen = TRUE,
        class = "shadow-sm flex-grow-1 border-0 h-100",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center py-2 flex-wrap gap-2 bg-light",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::tags$strong("Journal d'analyse & Script Reproductible"),
            shiny::tags$span(class = "badge bg-light text-secondary border ms-1", shiny::textOutput("rmd_stats_badge", inline = TRUE))
          ),
          shiny::div(
            class = "btn-toolbar gap-2",
            shiny::actionButton(
              inputId = "btn_add_rmd_note",
              label = "Ajouter une note",
              class = "btn-outline-secondary btn-sm"
            ),
            shiny::actionButton(
              inputId = "btn_copy_rmd",
              label = "Copier le script",
              class = "btn-outline-secondary btn-sm"
            ),
            shiny::actionButton(
              inputId = "btn_clear_rmd",
              label = "Effacer le journal",
              class = "btn-outline-danger btn-sm"
            ),
            shiny::actionButton(
              inputId = "btn_export_from_journal",
              label = "Exporter le rapport...",
              class = "btn-dark btn-sm shadow-sm"
            )
          )
        ),
        bslib::card_body(
          padding = 0,
          shiny::div(
            style = "height: calc(100vh - 165px); min-height: 520px; display: flex; flex-direction: column;",
            shiny::div(
              class = "p-2 bg-light border-bottom d-flex justify-content-between align-items-center small text-muted",
              shiny::span(
                "Console R Markdown en temps réel - synchronisée avec les analyses, graphiques et tests statistiques"
              ),
              shiny::span(class = "font-monospace small", "Format : R Markdown (YAML + blocs knitr)")
            ),
            shiny::tags$pre(
              id = "rmd_console_pre",
              style = "flex-grow: 1; margin: 0; overflow-y: auto; background-color: #F3F4F6; color: #111827; font-family: 'Fira Code', 'Courier New', 'SFMono-Regular', Consolas, monospace; font-size: 13px; line-height: 1.55; padding: 16px; border: 1px solid #E5E7EB; border-radius: 4px;",
              shiny::textOutput("rmd_log_output")
            )
          )
        )
      )
    ),

    bslib::nav_spacer(),

    # =========================================================================
    # BOUTONS D'ACTION DU HEADER (TEXTE PUR)
    # =========================================================================
    bslib::nav_item(
      shiny::actionButton(
        inputId = "btn_import",
        label = "Importer un fichier",
        class = "btn-outline-secondary btn-sm my-auto me-2 px-3 py-1 shadow-sm"
      )
    ),
    bslib::nav_item(
      shiny::actionButton(
        inputId = "btn_export_report",
        label = "Exporter le rapport",
        class = "btn-dark btn-sm my-auto px-3 py-1 shadow-sm"
      )
    )
  )
}
