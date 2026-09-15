#' @title Lancer l'interface utilisateur graphique Ramses 1.0
#'
#' @description Lance l'application graphique interactive Shiny fournie par le
#'   package Ramses (version 1.0). Con\u00e7ue comme une alternative moderne, fluide et intuitive \u00e0 Rcmdr,
#'   cette interface permet d'importer des fichiers (CSV, Excel, SPSS, Stata, RDS),
#'   d'explorer les donn\u00e9es de mani\u00e8re interactive, de calculer des statistiques
#'   descriptives univari\u00e9es et bivari\u00e9es, de construire des graphiques de type Tableau
#'   (bas\u00e9s sur ggplot2 et Plotly), et d'ex\u00e9cuter une gamme exhaustive de tests statistiques
#'   param\u00e9triques et non-param\u00e9triques, tout en consignant automatiquement chaque commande
#'   dans un journal R Markdown reproductible.
#'
#' @param standalone Valeur logique indiquant si l'application doit \u00eatre ouverte dans
#'   sa propre fen\u00eatre de bureau d\u00e9di\u00e9e (mode standalone sans barre d'adresse ni onglets
#'   via le mode \code{--app=} de Chromium / Microsoft Edge / Google Chrome / Brave). Par d\u00e9faut \code{TRUE}.
#'   Si aucun navigateur compatible n'est trouv\u00e9, bascule automatiquement et de fa\u00e7on
#'   transparente sur le navigateur syst\u00e8me standard.
#' @param port Entier optionnel sp\u00e9cifiant le port TCP sur lequel \u00e9couter (ex: \code{3838} ou \code{3000}).
#'   Par d\u00e9faut \code{NULL} : un port local libre est automatiquement attribu\u00e9 via
#'   \code{httpuv::randomPort()} afin d'\u00e9viter tout conflit entre instances.
#' @param launch.browser Valeur logique ou fonction indiquant s'il faut ouvrir
#'   automatiquement l'application au d\u00e9marrage. Par d\u00e9faut \code{TRUE} en session interactive.
#'   D\u00e9finir \u00e0 \code{FALSE} pour d\u00e9marrer le serveur en arri\u00e8re-plan sans ouvrir de fen\u00eatre.
#' @param host Adresse IP sur laquelle \u00e9couter. Par d\u00e9faut \code{"127.0.0.1"} (localhost).
#'   Utiliser \code{"0.0.0.0"} pour autoriser les connexions r\u00e9seau externes ou conteneuris\u00e9es.
#' @param ... Arguments suppl\u00e9mentaires transmis \u00e0 \code{\link[shiny]{shinyApp}}.
#'
#' @return Un objet d'application Shiny ex\u00e9cutable (\code{shiny.appobj}).
#' @export
#'
#' @import shiny
#' @import bslib
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom rmarkdown render html_document
#' @importFrom dplyr %>% select mutate filter group_by summarize arrange desc across everything
#' @importFrom ggplot2 ggplot aes geom_point geom_bar geom_histogram geom_boxplot geom_violin geom_density geom_line geom_smooth geom_tile facet_wrap theme_minimal labs
#'
#' @examples
#' \dontrun{
#'   library(Ramses)
#'   # Lancement automatique en fen\u00eatre d\u00e9di\u00e9e (mode standalone)
#'   run_app()
#'
#'   # Lancement dans le navigateur standard sur un port fixe
#'   run_app(standalone = FALSE, port = 3838)
#'
#'   # D\u00e9marrage en arri\u00e8re-plan sans lancer de fen\u00eatre
#'   run_app(launch.browser = FALSE)
#' }

#' Recherche interne d'un ex\u00e9cutable Chromium / Edge pour le mode standalone (--app=)
#'
#' @return Cha\u00eene de caract\u00e8res contenant le chemin absolu de l'ex\u00e9cutable ou NULL si non trouv\u00e9.
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

  # Attribution automatique d'un port local libre al\u00e9atoire si non sp\u00e9cifi\u00e9
  if (is.null(port)) {
    port <- tryCatch({
      if (requireNamespace("httpuv", quietly = TRUE)) {
        httpuv::randomPort()
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }

  # D\u00e9finition du lanceur de navigateur selon le mode standalone
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
        # Fallback s\u00e9curis\u00e9 : navigateur par d\u00e9faut du syst\u00e8me
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

