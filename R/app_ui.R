#' Bo\u00eete de dialogue modale d'importation de donn\u00e9es
#'
#' @return Une structure modale Shiny.
#' @noRd
modal_import_data <- function() {
  shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-center gap-2",
      shiny::tags$span(style = "font-weight: 600; color: #111827;", "Charger un jeu de donn\u00e9es dans Ramses")
    ),
    size = "l",
    easyClose = FALSE,
    footer = shiny::tagList(
      shiny::modalButton("Fermer")
    ),

    bslib::navset_card_tab(
      id = "import_source_tabs",
      
      # ------------------------------------------------------------------------
      # ONGLET 1 : OBJETS R DU WORKSPACE (.GlobalEnv)
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("database", fill = "#4B5563", height = "0.9em"), "Objets R en m\u00e9moire")),
        value = "tab_workspace_r",
        bslib::card_body(
          class = "p-3",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-3",
            shiny::tags$div(
              shiny::tags$h6(class = "fw-bold text-dark mb-1", "Jeux de donn\u00e9es disponibles dans R"),
              shiny::tags$p(class = "text-muted small mb-0", "S\u00e9lectionnez un tableau (data.frame, tibble ou data.table) d\u00e9j\u00e0 pr\u00e9sent dans votre session R/RStudio.")
            ),
            shiny::actionButton(
              inputId = "btn_refresh_workspace_datasets",
              label = "Actualiser",
              icon = shiny::icon("rotate"),
              class = "btn-outline-secondary btn-sm"
            )
          ),
          
          # Liste dynamique des objets ou message d'information
          shiny::uiOutput("workspace_datasets_ui"),
          
          shiny::hr(class = "my-3"),
          shiny::div(
            class = "d-flex justify-content-end",
            shiny::actionButton(
              inputId = "btn_load_workspace_dataset",
              label = "Charger le jeu de donn\u00e9es s\u00e9lectionn\u00e9",
              class = "btn-dark shadow-sm"
            )
          )
        )
      ),

      # ------------------------------------------------------------------------
      # ONGLET 2 : IMPORTATION DE FICHIER EXTERNE
      # ------------------------------------------------------------------------
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("file-arrow-up", fill = "#4B5563", height = "0.9em"), "Fichier externe")),
        value = "tab_import_file",
        bslib::card_body(
          class = "p-3",
          # S\u00e9lecteur de fichier
          shiny::div(
            class = "mb-3",
            shiny::fileInput(
              inputId = "import_file",
              label = shiny::strong("S\u00e9lectionner un fichier sur votre poste :"),
              accept = c(
                ".csv", ".txt", ".tsv",
                ".xlsx", ".xls",
                ".sav", ".dta",
                ".rds", ".RData"
              ),
              buttonLabel = "Parcourir...",
              placeholder = "Formats accept\u00e9s : .csv, .txt, .xlsx, .xls, .sav, .dta, .rds"
            )
          ),

          # Ligne de configuration g\u00e9n\u00e9rale (Format & Nom assign\u00e9)
          shiny::div(
            class = "row g-3 mb-3",
            shiny::div(
              class = "col-md-6",
              shiny::selectInput(
                inputId = "import_format",
                label = shiny::strong("Format du fichier :"),
                choices = c(
                  "Auto-d\u00e9tection (selon l'extension)" = "auto",
                  "Fichier texte d\u00e9limit\u00e9 (CSV, TXT)" = "csv",
                  "Feuille de calcul Excel (.xlsx, .xls)" = "excel",
                  "Fichier SPSS (.sav)" = "spss",
                  "Fichier Stata (.dta)" = "stata",
                  "Objet R s\u00e9rialis\u00e9 (.rds)" = "rds"
                ),
                selected = "auto"
              )
            ),
            shiny::div(
              class = "col-md-6",
              shiny::textInput(
                inputId = "import_dataset_name",
                label = shiny::strong("Nom de l'objet R cr\u00e9\u00e9 :"),
                value = "dataset"
              )
            )
          ),

          # Options dynamiques selon le format s\u00e9lectionn\u00e9
          shiny::div(
            class = "card bg-light border p-3 mb-2",
            shiny::uiOutput("import_dynamic_options")
          ),

          # Retour d'information / aper\u00e7u rapide
          shiny::uiOutput("import_preview_info"),

          shiny::hr(class = "my-3"),
          shiny::div(
            class = "d-flex justify-content-end",
            shiny::actionButton(
              inputId = "btn_validate_import",
              label = "Valider et Charger le fichier",
              class = "btn-dark shadow-sm"
            )
          )
        )
      )
    )
  )
}

#' Interface utilisateur principale de Ramses
#'
#' @return Une structure UI Shiny bas\u00e9e sur \code{bslib::page_navbar}.
#' @noRd
app_ui <- function() {
  # Declaration securisee des dossiers de ressources statiques (favicon, cours-statistiques)
  res_dir <- system.file("app/www", package = "Ramses")
  if (dir.exists(res_dir)) {
    shiny::addResourcePath("ramses_res", res_dir)
  }

  cours_dir <- system.file("www/cours-statistiques", package = "Ramses")
  if (!nzchar(cours_dir) || !dir.exists(cours_dir)) {
    cours_dir <- file.path(getwd(), "inst", "www", "cours-statistiques")
  }
  if (!dir.exists(cours_dir)) {
    cours_dir <- file.path(getwd(), "Ramses", "inst", "www", "cours-statistiques")
  }
  if (dir.exists(cours_dir)) {
    shiny::addResourcePath("cours_stats", cours_dir)
  }

  bslib::page_navbar(
    id = "main_nav",
    title = "Ramses 1.0",
    window_title = "Ramses 1.0",
    theme = bslib::bs_theme(
      version = 5,
      bg = "#FFFFFF",
      fg = "#1F2937",
      primary = "#1F2937",
      secondary = "#6B7280",
      success = "#374151",
      info = "#4B5563",
      warning = "#4B5563",
      danger = "#991B1B",
      base_font = bslib::font_google("IBM Plex Sans")
    ),
    header = shiny::tagList(
      # Favicon et titre de l'application Ramses (support multi-navigateurs et mode Chromium standalone)
      shiny::tags$head(
        shiny::tags$title("Ramses 1.0"),
        shiny::tags$link(rel = "shortcut icon", href = "ramses_res/favicon.svg"),
        shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "ramses_res/favicon.svg"),
        shiny::tags$link(rel = "shortcut icon", href = "favicon.ico"),
        shiny::tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
  
        # FontAwesome 6.4.2 CDN pour rendu parfait des ic\u00f4nes
        shiny::tags$link(
          rel = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
        )
      ),

      # Script de gestion du cycle de vie (fermeture propre, red\u00e9marrage, nouvelle instance)
      shiny::tags$script(shiny::HTML("
        $(document).on('shiny:connected', function() {
          if (window.Shiny && Shiny.addCustomMessageHandler) {
            Shiny.addCustomMessageHandler('ramses_new_instance', function(msg) {
              window.open(window.location.href, '_blank');
            });
            Shiny.addCustomMessageHandler('ramses_reload_session', function(msg) {
              window.location.reload();
            });
            Shiny.addCustomMessageHandler('ramses_stop_app', function(msg) {
              if (window.ramsesAPI && typeof window.ramsesAPI.quitApp === 'function') {
                window.ramsesAPI.quitApp();
              } else {
                try {
                  window.close();
                } catch(e) {}
                setTimeout(function() {
                  document.body.innerHTML = '<div style=\"display:flex;flex-direction:column;align-items:center;justify-content:center;height:100vh;font-family:\'IBM Plex Sans\', sans-serif;background-color:#F9FAFB;color:#1F2937;text-align:center;padding:24px;\"><div style=\"background:#FFFFFF;border:1px solid #E5E7EB;border-radius:8px;padding:36px 44px;max-width:540px;box-shadow:0 4px 6px -1px rgba(0,0,0,0.05);\"><div style=\"font-size:42px;color:#374151;margin-bottom:18px;\">&#10004;</div><h3 style=\"font-size:20px;font-weight:600;margin-bottom:12px;color:#111827;\">Ramses est arr&ecirc;t&eacute;</h3><p style=\"color:#4B5563;font-size:14px;line-height:1.55;margin-bottom:24px;\">Le serveur de l\\'application a &eacute;t&eacute; ferm&eacute; proprement.<br>Vous pouvez fermer cet onglet ou cette fen&ecirc;tre en toute s&eacute;curit&eacute;.</p><button onclick=\"window.close();\" class=\"btn btn-outline-secondary btn-sm\" style=\"font-size:13px;padding:6px 18px;border-radius:4px;cursor:pointer;\">Fermer la page</button></div></div>';
                }, 150);
              }
            });
            Shiny.addCustomMessageHandler('open_formation_window', function(msg) {
              var targetUrl = (msg && msg.url) ? msg.url : 'cours_stats/index.html';
              if (window.ramsesAPI && typeof window.ramsesAPI.openExternal === 'function') {
                var fullUrl = window.location.origin + '/' + targetUrl.replace(/^\\//, '');
                window.ramsesAPI.openExternal(fullUrl);
              } else {
                try {
                  window.open(targetUrl, '_blank');
                } catch(e) {
                  console.log('window.open fallback', e);
                }
              }
            });
          }
        });
      ")),
      shiny::tags$style(shiny::HTML(paste0(
        "
        /* === Ramses Global Typography & Monochrome Scientific Palette === */
        :root {
          --bs-font-sans-serif: 'IBM Plex Sans', sans-serif;
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

        body, html {
          font-family: 'IBM Plex Sans', sans-serif !important;
        }

        body {
          font-size: 13.5px !important;
          background-color: #FFFFFF !important;
          color: #1F2937 !important;
          letter-spacing: -0.01em !important;
        }

        /* === Navigation Bar Monochrome Ultra-Sobre & Alignement a Gauche === */
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

        /* Forcer l'icone SVG et le texte a s'aligner naturellement de gauche a droite */
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

        /* === Masquer les onglets speciaux de la barre de navigation principale === */
        .nav-link[data-value='about_page'],
        a[data-value='about_page'],
        .nav-item:has(a[data-value='about_page']),
        .nav-link[data-value='cours_stats_page'],
        a[data-value='cours_stats_page'],
        .nav-item:has(a[data-value='cours_stats_page']) {
          display: none !important;
        }

        /* === Bouton compact ? avec menu deroulant === */
        .ramses-help-menu .nav-link {
          font-weight: 700 !important;
          color: #4B5563 !important;
          border: 1px solid #D1D5DB !important;
          border-radius: 9999px !important;
          width: 28px !important;
          height: 28px !important;
          padding: 0 !important;
          display: inline-flex !important;
          align-items: center !important;
          justify-content: center !important;
          font-size: 0.88rem !important;
          background-color: #FFFFFF !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05) !important;
          transition: all 0.15s ease-in-out !important;
        }
        .ramses-help-menu .nav-link:hover,
        .ramses-help-menu .nav-link:focus,
        .ramses-help-menu.show > .nav-link {
          color: #111827 !important;
          background-color: #F3F4F6 !important;
          border-color: #9CA3AF !important;
        }
        .ramses-help-menu .nav-link::after {
          display: none !important;
        }
        ",
        "
        /* === Compact Form Elements & Controls === */
        .form-control, .form-select, .selectize-input {
          font-size: 0.78rem !important;
          padding: 0.2rem 0.5rem !important;
          min-height: 26px !important;
          border-radius: 4px !important;
          border-color: #D1D5DB !important;
          color: #1F2937 !important;
          background-color: #FFFFFF !important;
          line-height: 1.35 !important;
        }

        .selectize-input .item,
        .selectize-input input {
          font-size: 0.78rem !important;
          line-height: 1.35 !important;
        }

        .form-control:focus, .form-select:focus, .selectize-input.focus {
          border-color: #6B7280 !important;
          box-shadow: 0 0 0 2px rgba(107, 114, 128, 0.15) !important;
        }

        /* === Options a l'interieur des menus deroulants === */
        .selectize-dropdown,
        .selectize-dropdown-content {
          font-size: 0.78rem !important;
          border-color: #E5E7EB !important;
          border-radius: 4px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.08), 0 2px 4px -1px rgba(0, 0, 0, 0.04) !important;
          background-color: #FFFFFF !important;
        }

        .selectize-dropdown .option,
        select option,
        .form-select option {
          font-size: 0.78rem !important;
          padding: 0.25rem 0.55rem !important;
          line-height: 1.35 !important;
          color: #1F2937 !important;
          white-space: nowrap !important;
          text-overflow: ellipsis !important;
          overflow: hidden !important;
        }

        .selectize-dropdown .option.active,
        .selectize-dropdown .option:hover {
          background-color: #F3F4F6 !important;
          color: #111827 !important;
        }

        .selectize-dropdown .option.selected {
          background-color: #E5E7EB !important;
          color: #111827 !important;
          font-weight: 600 !important;
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
          text-align: left !important;
          display: inline-flex !important;
          align-items: center !important;
          justify-content: flex-start !important;
          gap: 6px !important;
          transition: all 0.15s ease !important;
        }

        .btn-sm, .btn.btn-sm {
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
        ",
        "
        /* === Cartes, En-tetes et Panneaux Monochrome === */
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

        /* === Neutralisation des bleus et couleurs sombres residuelles === */
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
        ",
        "
        /* === Blocs de Code R Scientifiques Neutres === */
        pre, code, kbd, samp, #rmd_console_pre {
          background-color: #F3F4F6 !important;
          color: #111827 !important;
          border: 1px solid #E5E7EB !important;
          border-radius: 4px !important;
          font-family: monospace !important;
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
        "
      )))
    ),
      
    # =========================================================================
    # 1. MENU FICHIER & DONN\u00c9ES (Style SPSS)
    # =========================================================================
    bslib::nav_menu(
      title = shiny::HTML(paste(fontawesome::fa("database", fill = "#4B5563", height = "0.9em"), "Fichier & Donn\u00e9es")),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("table", fill = "#6B7280", height = "0.85em"), "Aper\u00e7u des donn\u00e9es")),
        value = "data_preview",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Gestion des donn\u00e9es",
            width = 280,
            shiny::div(
              class = "text-muted small mb-2",
              "Jeu de donn\u00e9es actif en m\u00e9moire pour la session."
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
              label = "R\u00e9initialiser avec 'iris'",
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
                shiny::tags$strong("Aper\u00e7u interactif du jeu de donn\u00e9es"),
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
    # 2. MENU PREPARATION DES DONNEES
    # =========================================================================
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("wand-magic-sparkles", fill = "#4B5563", height = "0.9em"), "Pr\u00e9parer les donn\u00e9es")),
      value = "data_prep_tab",
      mod_data_prep_ui("data_prep_module")
    ),

    # =========================================================================
    # 3. MENU STATISTIQUES DESCRIPTIVES (Style SPSS)
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
        title = shiny::HTML(paste(fontawesome::fa("table-cells-large", fill = "#6B7280", height = "0.85em"), "Tableaux Crois\u00e9s")),
        value = "desc_cross",
        mod_descriptives_cross_ui("descriptives_module")
      ),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("border-all", fill = "#6B7280", height = "0.85em"), "Matrice de Corr\u00e9lation")),
        value = "desc_cor",
        mod_descriptives_cor_ui("descriptives_module")
      )
    ),

    # =========================================================================
    # 3. MENU TESTS STATISTIQUES & MOD\u00c9LISATION (Style SPSS)
    # =========================================================================
    bslib::nav_menu(
      title = shiny::HTML(paste(fontawesome::fa("flask", fill = "#4B5563", height = "0.9em"), "Tests Statistiques")),
      bslib::nav_panel(
        title = shiny::HTML(paste(fontawesome::fa("wave-square", fill = "#6B7280", height = "0.85em"), "Normalit\u00e9 & Variance")),
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
        title = shiny::HTML(paste(fontawesome::fa("chart-pie", fill = "#6B7280", height = "0.85em"), "Tests de Proportions")),
        value = "test_prop",
        mod_tests_prop_ui("tests_module")
      )
    ),

    # =========================================================================
    # 4. MENU MOD\u00c8LES DE R\u00c9GRESSION (Menu ind\u00e9pendant)
    # =========================================================================
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("chart-line", fill = "#4B5563", height = "0.9em"), "Mod\u00e8les de r\u00e9gression")),
      value = "regression_models",
      mod_regression_ui("regression_module")
    ),

    # =========================================================================
    # 5. ONGLET VISUALISATION (Chart Builder)
    # =========================================================================
    bslib::nav_panel(
      title = shiny::HTML(paste(fontawesome::fa("chart-column", fill = "#4B5563", height = "0.9em"), "Visualisation")),
      value = "chart_builder",
      mod_chart_builder_ui("chart_builder_module")
    ),

    # =========================================================================
    # 6. ONGLET JOURNAL R MARKDOWN
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
                "Console R Markdown en temps r\u00e9el - synchronis\u00e9e avec les analyses, graphiques et tests statistiques"
              ),
              shiny::span(class = "font-monospace small", "Format : R Markdown (YAML + blocs knitr)")
            ),
            shiny::tags$pre(
              id = "rmd_console_pre",
              style = "flex-grow: 1; margin: 0; overflow-y: auto; background-color: #F3F4F6; color: #111827; font-family: monospace; font-size: 13px; line-height: 1.55; padding: 16px; border: 1px solid #E5E7EB; border-radius: 4px;",
              shiny::textOutput("rmd_log_output")
            )
          )
        )
      )
    ),

    # =========================================================================
    # 6. PAGE "A PROPOS DE RAMSES" (Vue complete integree)
    # =========================================================================
    bslib::nav_panel(
      title = NULL,
      value = "about_page",
      page_about_ramses_ui()
    ),

    # =========================================================================
    # 7. PAGE "COURS DE METHODES STATISTIQUES" (Vue complete integree)
    # =========================================================================
    bslib::nav_panel(
      title = NULL,
      value = "cours_stats_page",
      page_cours_statistiques_ui()
    ),

    bslib::nav_spacer(),

    # =========================================================================
    # MENU SESSION (Gestion du cycle de vie)
    # =========================================================================
    bslib::nav_menu(
      title = shiny::HTML(paste(fontawesome::fa("power-off", fill = "#4B5563", height = "0.9em"), "Session")),
      align = "right",
      bslib::nav_item(
        shiny::actionLink(
          inputId = "menu_session_new",
          label = shiny::HTML(paste(fontawesome::fa("window-restore", fill = "#6B7280", height = "0.85em"), "Nouvelle instance")),
          class = "dropdown-item",
          onclick = "window.open(window.location.href, '_blank'); return false;"
        )
      ),
      bslib::nav_item(
        shiny::actionLink(
          inputId = "menu_session_restart",
          label = shiny::HTML(paste(fontawesome::fa("arrows-rotate", fill = "#6B7280", height = "0.85em"), "Red\u00e9marrer")),
          class = "dropdown-item"
        )
      ),
      bslib::nav_item(shiny::tags$hr(class = "dropdown-divider")),
      bslib::nav_item(
        shiny::actionLink(
          inputId = "menu_session_stop",
          label = shiny::HTML(paste(fontawesome::fa("power-off", fill = "#991B1B", height = "0.85em"), shiny::tags$span(style = "color: #991B1B; font-weight: 500;", "Fermer l'application"))),
          class = "dropdown-item"
        )
      )
    ),

    # =========================================================================
    # MENU D'AIDE ET FORMATION (?)
    # =========================================================================
    bslib::nav_menu(
      title = "?",
      align = "right",
      bslib::nav_item(
        shiny::actionLink(
          inputId = "menu_open_formation",
          label = shiny::HTML(paste(fontawesome::fa("graduation-cap", fill = "#1F2937", height = "0.9em"), " <strong>Formation</strong>")),
          class = "dropdown-item"
        )
      ),
      bslib::nav_item(shiny::tags$hr(class = "dropdown-divider")),
      bslib::nav_item(
        shiny::actionLink(
          inputId = "menu_open_about",
          label = shiny::HTML(paste(fontawesome::fa("circle-info", fill = "#6B7280", height = "0.85em"), " \u00c0 propos de Ramses")),
          class = "dropdown-item"
        )
      )
    )
  )
}

#' Interface de la page entiere "A propos de Ramses"
#'
#' @return Une structure UI Shiny occupant tout l'espace disponible.
#' @noRd
page_about_ramses_ui <- function() {
  ver_str <- ramses_get_version()

  shiny::div(
    class = "container-fluid py-4 px-3 px-md-5",
    style = "max-width: 1200px; margin: 0 auto; background-color: #FFFFFF;",

    # Barre de retour superieure
    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-4 pb-3 border-bottom",
      shiny::actionButton(
        inputId = "btn_about_back",
        label = shiny::HTML(paste(fontawesome::fa("arrow-left", height = "0.9em"), " \u2190 Retour \u00e0 Ramses")),
        class = "btn btn-outline-secondary btn-sm px-3 py-2 fw-semibold shadow-sm"
      ),
      shiny::tags$span(
        class = "badge bg-light text-dark border font-monospace px-3 py-2 fs-6",
        paste0("Ramses v", ver_str)
      )
    ),

    # En-tete principal / Banner
    shiny::div(
      class = "card border-0 p-4 mb-4 rounded-3 shadow-sm",
      style = "background: linear-gradient(135deg, #F9FAFB 0%, #F3F4F6 100%); border-left: 5px solid #1F2937 !important;",
      shiny::div(
        class = "d-flex align-items-center gap-3 mb-2",
        shiny::tags$span(
          style = "font-size: 2.2rem; color: #1F2937;",
          fontawesome::fa("circle-info")
        ),
        shiny::div(
          shiny::h2(
            class = "mb-1 fw-bold text-dark tracking-tight",
            paste0("Ramses ", ver_str)
          ),
          shiny::h5(
            class = "text-secondary fw-normal mb-0",
            "Ramses \u2014 Analyses statistiques guid\u00e9es pour tous"
          )
        )
      ),
      shiny::p(
        class = "text-muted fs-6 mt-3 mb-0",
        style = "max-width: 950px; line-height: 1.6;",
        "Ramses est une interface graphique moderne et interactive destin\u00e9e \u00e0 faciliter la pr\u00e9paration, ",
        "l'exploration, la visualisation et l'analyse statistique des donn\u00e9es avec R. ",
        "Con\u00e7ue comme une alternative contemporaine et intuitive \u00e0 Rcmdr, Ramses propose une exp\u00e9rience ",
        "fluide tout en garantissant une reproductibilit\u00e9 analytique totale gr\u00e2ce \u00e0 la g\u00e9n\u00e9ration automatique ",
        "de code R et un journal R Markdown int\u00e9gr\u00e9."
      )
    ),

    # Grille principale a 2 colonnes
    shiny::div(
      class = "row g-4",

      # Colonne 1 : Fonctionnalites de la version actuelle
      shiny::div(
        class = "col-lg-7 col-xl-8",
        shiny::div(
          class = "card h-100 border shadow-sm",
          shiny::div(
            class = "card-header bg-white py-3 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
            fontawesome::fa("list-check", fill = "#1F2937"),
            paste0("Fonctionnalit\u00e9s disponibles (Ramses ", ver_str, ")")
          ),
          shiny::div(
            class = "card-body p-4",
            shiny::div(
              class = "row g-3",

              # Prep
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("wand-magic-sparkles", fill = "#374151"), " Pr\u00e9paration des donn\u00e9es"))),
                  shiny::p(class = "small text-muted mb-0", "Importation multi-formats (CSV, Excel, SPSS, Stata, RDS), nettoyage, filtres, cr\u00e9ations et calculs de variables, gestion des valeurs manquantes et recodages.")
                )
              ),

              # Descriptives
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("chart-line", fill = "#374151"), " Statistiques descriptives"))),
                  shiny::p(class = "small text-muted mb-0", "Moyenne, m\u00e9diane, \u00e9cart-type, quartiles, IQR, minimum et maximum avec synth\u00e8se statistique automatique.")
                )
              ),

              # Quanti/Quali
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("layer-group", fill = "#374151"), " Variables quantitatives & qualitatives"))),
                  shiny::p(class = "small text-muted mb-0", "Analyses univari\u00e9es cibl\u00e9es par type de variable avec distributions et repr\u00e9sentations visuelles d\u00e9di\u00e9es.")
                )
              ),

              # Frequences & Croises
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("table-cells-large", fill = "#374151"), " Tableaux de fr\u00e9quences & crois\u00e9s"))),
                  shiny::p(class = "small text-muted mb-0", "Effectifs, pourcentages bruts et cumul\u00e9s, tableaux de contingence bivari\u00e9s avec pourcentages en ligne, en colonne et totaux.")
                )
              ),

              # Visualisations
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("chart-column", fill = "#374151"), " Visualisations interactives"))),
                  shiny::p(class = "small text-muted mb-0", "Cr\u00e9ateur de graphiques bas\u00e9 sur ggplot2 et Plotly : histogrammes, bo\u00eetes \u00e0 moustaches, nuages de points, diagrammes en barres et secteurs.")
                )
              ),

              # Tests
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("flask", fill = "#374151"), " Tests statistiques"))),
                  shiny::p(class = "small text-muted mb-0", "Tests de normalit\u00e9 (Shapiro-Wilk, KS), comparaison de 2 groupes (Student, Welch, Mann-Whitney, t appari\u00e9), 3+ groupes (ANOVA, Kruskal-Wallis) et contingence (Chi-deux, Fisher, McNemar).")
                )
              ),

              # Correl & Reg
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("sliders", fill = "#374151"), " Corr\u00e9lations & R\u00e9gressions"))),
                  shiny::p(class = "small text-muted mb-0", "Matrices de corr\u00e9lation de Pearson et Spearman avec p-values, mod\u00e8les de r\u00e9gression lin\u00e9aire (simple/multiple) et logistique binaire.")
                )
              ),

              # Tailles d'effet & Puissance
              shiny::div(
                class = "col-md-6",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("scale-balanced", fill = "#374151"), " Tailles d'effet & Puissance"))),
                  shiny::p(class = "small text-muted mb-0", "Calculs automatiques du d de Cohen, r de rank-biserial, \u03b7\u00b2, V de Cram\u00e9r, Odds Ratio et de la puissance statistique (1-\u03b2) lorsqu'applicables.")
                )
              ),

              # Code R & Rmd
              shiny::div(
                class = "col-md-12",
                shiny::div(
                  class = "p-3 rounded border bg-light h-100",
                  shiny::h6(class = "fw-bold text-dark mb-1", shiny::HTML(paste(fontawesome::fa("code", fill = "#374151"), " Code R reproductible & Journal R Markdown"))),
                  shiny::p(class = "small text-muted mb-0", "G\u00e9n\u00e9ration dynamique du code R exact pour chaque op\u00e9ration et console R Markdown int\u00e9gr\u00e9e permettant l'annotation, la copie de scripts et l'exportation de rapports.")
                )
              )
            )
          )
        )
      ),

      # Colonne 2 : Auteur, Projet, Infos techniques, Licence, Credits
      shiny::div(
        class = "col-lg-5 col-xl-4",
        shiny::div(
          class = "d-flex flex-column gap-3",

          # Carte Auteur
          shiny::div(
            class = "card border shadow-sm",
            shiny::div(
              class = "card-header bg-white py-2 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
              fontawesome::fa("user", fill = "#1F2937"),
              "Auteur"
            ),
            shiny::div(
              class = "card-body p-3",
              shiny::h6(class = "fw-bold text-dark mb-1", "Pierre Valdeze MBOM MBOM"),
              shiny::p(class = "small text-muted mb-0", "Cr\u00e9ateur et d\u00e9veloppeur de Ramses.")
            )
          ),

          # Carte Projet
          shiny::div(
            class = "card border shadow-sm",
            shiny::div(
              class = "card-header bg-white py-2 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
              fontawesome::fa("folder-open", fill = "#1F2937"),
              "Projet"
            ),
            shiny::div(
              class = "card-body p-3",
              shiny::tags$ul(
                class = "list-unstyled mb-0 small",
                shiny::tags$li(
                  class = "mb-2 d-flex align-items-center",
                  shiny::tags$span(class = "me-2", shiny::HTML(fontawesome::fa("github", fill = "#1F2937"))),
                  shiny::tags$a(
                    href = "https://github.com/census-specs/Ramses",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    class = "fw-semibold text-decoration-none text-primary",
                    "Code source sur GitHub"
                  )
                ),
                shiny::tags$li(
                  class = "text-muted d-flex align-items-center",
                  shiny::tags$span(class = "me-2", shiny::HTML(fontawesome::fa("globe", fill = "#9CA3AF"))),
                  "Site web \u2014 bient\u00f4t disponible"
                )
              )
            )
          ),

          # Carte Informations techniques
          shiny::div(
            class = "card border shadow-sm",
            shiny::div(
              class = "card-header bg-white py-2 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
              fontawesome::fa("gears", fill = "#1F2937"),
              "Informations techniques"
            ),
            shiny::div(
              class = "card-body p-3 small text-muted",
              shiny::tags$ul(
                class = "list-unstyled mb-0 d-flex flex-column gap-1",
                shiny::tags$li(shiny::tags$strong("Langage : "), "R (>= 4.1.0)"),
                shiny::tags$li(shiny::tags$strong("Interface : "), "Shiny / bslib (Bootstrap 5)"),
                shiny::tags$li(shiny::tags$strong("Version de Ramses : "), paste0("Ramses ", ver_str)),
                shiny::tags$li(
                  class = "mt-2 pt-2 border-top",
                  shiny::tags$strong("Biblioth\u00e8ques R utilis\u00e9es :"),
                  shiny::div(
                    class = "d-flex flex-wrap gap-1 mt-2",
                    shiny::tags$span(class = "badge bg-light text-dark border", "shiny"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "bslib"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "DT"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "plotly"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "rmarkdown"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "knitr"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "haven"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "readxl"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "dplyr"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "ggplot2"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "htmltools"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "fontawesome"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "rlang"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "stats"),
                    shiny::tags$span(class = "badge bg-light text-dark border", "utils")
                  )
                )
              )
            )
          ),

          # Carte Licence & Credits
          shiny::div(
            class = "card border shadow-sm",
            shiny::div(
              class = "card-header bg-white py-2 border-bottom d-flex align-items-center gap-2 fw-bold text-dark",
              fontawesome::fa("scale-balanced", fill = "#1F2937"),
              "Licence & Cr\u00e9dits"
            ),
            shiny::div(
              class = "card-body p-3 small text-muted",
              shiny::div(class = "fw-bold text-dark mb-1", "MIT License"),
              shiny::p(class = "mb-2", "Ramses est un logiciel libre distribu\u00e9 sous licence MIT."),
              shiny::hr(class = "my-2"),
              shiny::div(class = "fw-bold text-dark mb-1", "Cr\u00e9dits"),
              shiny::p(class = "mb-0", "Ramses s'appuie avec gratitude sur l'\u00e9cosyst\u00e8me R open source, R Core Team, Posit/RStudio et l'ensemble des mainteneurs de packages.")
            )
          )
        )
      )
    )
  )
}

#' Interface de la page enti\u00e8re "Cours de m\u00e9thodes statistiques"
#'
#' @return Une structure UI Shiny avec iframe plein \u00e9cran et barre d'actions
#' @noRd
page_cours_statistiques_ui <- function() {
  shiny::div(
    class = "container-fluid p-0 d-flex flex-column",
    style = "height: calc(100vh - 46px); min-height: 560px; background-color: #FFFFFF;",

    # Barre d'actions sup\u00e9rieure
    shiny::div(
      class = "d-flex justify-content-between align-items-center px-3 py-2 border-bottom bg-light",
      style = "min-height: 42px;",
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::actionButton(
          inputId = "btn_cours_back",
          label = shiny::HTML(paste(fontawesome::fa("arrow-left", height = "0.9em"), " \u2190 Retour \u00e0 Ramses")),
          class = "btn btn-outline-secondary btn-sm px-3 py-1 fw-semibold shadow-sm"
        ),
        shiny::tags$span(
          class = "fw-bold text-dark ms-2 small d-none d-sm-inline",
          shiny::HTML("<strong>\U0001F4DA M\u00e9thodes statistiques</strong> \u2014 Cours & Manuel interactif de r\u00e9f\u00e9rence")
        )
      ),
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$a(
          href = "cours_stats/index.html",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "btn btn-outline-dark btn-sm px-2 py-1",
          shiny::HTML(paste(fontawesome::fa("arrow-up-right-from-square", height = "0.8em"), " Ouvrir dans un nouvel onglet"))
        )
      )
    ),

    # Iframe autonome contenant l'int\u00e9gralit\u00e9 du module HTML
    shiny::tags$iframe(
      src = "cours_stats/index.html",
      id = "iframe_cours_statistiques",
      style = "flex-grow: 1; width: 100%; height: 100%; border: none; background: #F9FAFB;",
      title = "Cours de m\u00e9thodes statistiques"
    )
  )
}

