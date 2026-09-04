#' @title Lancer l'interface utilisateur graphique Ramses
#'
#' @description Lance l'application graphique interactive Shiny fournie par le
#'   package Ramses. Conçue comme une alternative moderne, fluide et intuitive à Rcmdr,
#'   cette interface permet d'importer des fichiers (CSV, Excel, SPSS, Stata, RDS),
#'   d'explorer les données de manière interactive, de calculer des statistiques
#'   descriptives univariées et bivariées, de construire des graphiques de type Tableau
#'   (basés sur ggplot2 et Plotly), et d'exécuter une gamme exhaustive de tests statistiques
#'   paramétriques et non-paramétriques, tout en consignant automatiquement chaque commande
#'   dans un journal R Markdown reproductible.
#'
#' @param standalone Valeur logique indiquant si l'application doit être ouverte dans
#'   sa propre fenêtre de bureau dédiée (mode standalone sans barre d'adresse ni onglets
#'   via le mode \code{--app=} de Chromium / Microsoft Edge). Par défaut \code{TRUE}.
#'   Si aucun navigateur compatible n'est trouvé, bascule automatiquement et de façon
#'   transparente sur le navigateur système standard.
#' @param port Entier optionnel spécifiant le port TCP sur lequel écouter (ex: \code{3838} ou \code{3000}).
#'   Par défaut \code{NULL} : un port local libre est automatiquement attribué via
#'   \code{httpuv::randomPort()} afin d'éviter tout conflit entre instances.
#' @param launch.browser Valeur logique ou fonction indiquant s'il faut ouvrir
#'   automatiquement l'application au démarrage. Par défaut \code{TRUE} en session interactive.
#' @param host Adresse IP sur laquelle écouter. Par défaut \code{"127.0.0.1"} (localhost).
#'   Utiliser \code{"0.0.0.0"} pour autoriser les connexions réseau externes ou conteneurisées.
#' @param ... Arguments supplémentaires transmis à \code{\link[shiny]{shinyApp}}.
#'
#' @return Un objet d'application Shiny exécutable (\code{shiny.appobj}).
#' @export
#'
#' @import shiny
#' @import bslib
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom rmarkdown render html_document
#' @importFrom dplyr %>% select mutate filter group_by summarize arrange desc across everything
#' @importFrom ggplot2 ggplot aes geom_point geom_bar geom_histogram geom_boxplot geom_violin geom_density geom_line geom_smooth geom_tile facet_wrap theme_minimal labs
#'
#' @examples
#' \dontrun{
#'   library(Ramses)
#'   # Lancement automatique en fenêtre dédiée (mode standalone)
#'   run_app()
#'
#'   # Lancement dans le navigateur standard sur un port fixe
#'   run_app(standalone = FALSE, port = 3838)
#'
#'   # Démarrage en arrière-plan sans lancer de fenêtre
#'   run_app(launch.browser = FALSE)
#' }

#' Recherche interne d'un exécutable Chromium / Edge pour le mode standalone (--app=)
#'
#' @return Chaîne de caractères contenant le chemin absolu de l'exécutable ou NULL si non trouvé.
#' @noRd
find_chromium_browser <- function() {
  sys_os <- Sys.info()["sysname"]

  if (sys_os == "Windows") {
    prog_files <- Sys.getenv("PROGRAMFILES", "C:\\Program Files")
    prog_files_x86 <- Sys.getenv("PROGRAMFILES(X86)", "C:\\Program Files (x86)")
    local_appdata <- Sys.getenv("LOCALAPPDATA", "")

    candidates <- c(
      file.path(prog_files_x86, "Microsoft", "Edge", "Application", "msedge.exe"),
      file.path(prog_files, "Microsoft", "Edge", "Application", "msedge.exe"),
      file.path(prog_files, "Google", "Chrome", "Application", "chrome.exe"),
      file.path(prog_files_x86, "Google", "Chrome", "Application", "chrome.exe"),
      if (nzchar(local_appdata)) file.path(local_appdata, "Microsoft", "Edge", "Application", "msedge.exe"),
      if (nzchar(local_appdata)) file.path(local_appdata, "Google", "Chrome", "Application", "chrome.exe")
    )

    for (path in candidates) {
      if (!is.null(path) && file.exists(path)) {
        return(normalizePath(path, winslash = "/", mustWork = FALSE))
      }
    }
  } else if (sys_os == "Darwin") {
    candidates <- c(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
      "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
      file.path(Sys.getenv("HOME"), "Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
      file.path(Sys.getenv("HOME"), "Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
    )

    for (path in candidates) {
      if (file.exists(path)) {
        return(path)
      }
    }
  } else if (sys_os == "Linux") {
    binaries <- c(
      "google-chrome",
      "google-chrome-stable",
      "chromium",
      "chromium-browser",
      "microsoft-edge",
      "microsoft-edge-stable",
      "brave-browser"
    )

    for (bin in binaries) {
      loc <- Sys.which(bin)
      if (nzchar(loc) && file.exists(loc)) {
        return(unname(loc))
      }
      loc_sys <- tryCatch(
        system2("which", args = bin, stdout = TRUE, stderr = FALSE),
        error = function(e) character(0)
      )
      if (length(loc_sys) > 0 && nzchar(loc_sys[1]) && file.exists(loc_sys[1])) {
        return(loc_sys[1])
      }
    }
  }

  return(NULL)
}

run_app <- function(standalone = TRUE,
                    port = NULL,
                    launch.browser = TRUE,
                    host = "127.0.0.1",
                    ...) {

  # Attribution automatique d'un port local libre aléatoire si non spécifié
  if (is.null(port)) {
    port <- tryCatch({
      if (requireNamespace("httpuv", quietly = TRUE)) {
        httpuv::randomPort()
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }

  # Définition du lanceur de navigateur selon le mode standalone
  browser_handler <- launch.browser

  if (isTRUE(launch.browser)) {
    if (isTRUE(standalone)) {
      browser_bin <- find_chromium_browser()
      if (!is.null(browser_bin)) {
        browser_handler <- function(url) {
          system2(
            command = browser_bin,
            args = c(paste0("--app=", url), "--window-size=1280,850"),
            wait = FALSE
          )
        }
      } else {
        # Fallback sécurisé : navigateur par défaut du système
        browser_handler <- utils::browseURL
      }
    } else {
      browser_handler <- utils::browseURL
    }
  }

  app_options <- list(
    port = port,
    launch.browser = browser_handler,
    host = host
  )

  app <- shiny::shinyApp(
    ui = app_ui(),
    server = app_server,
    options = app_options,
    ...
  )

  return(app)
}

