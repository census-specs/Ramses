#' @title Lancer l'interface utilisateur graphique Ramses
#'
#' @description Lance l'application graphique interactive Shiny fournie par le
#'   package Ramses. Concue comme une alternative moderne a Rcmdr,
#'   cette interface permet d'importer des fichiers (CSV, Excel, SPSS, Stata, RDS),
#'   d'explorer les donnees de maniere interactive, de calculer des statistiques
#'   descriptives univariees et bivariees, de construire des graphiques bases sur
#'   ggplot2 et Plotly, et d'executer une gamme de tests statistiques, tout en
#'   consignant automatiquement les commandes dans un journal R Markdown.
#'
#' @param standalone Valeur logique indiquant si l'application doit etre ouverte dans
#'   sa propre fenetre de bureau dediee (mode standalone sans barre d'adresse ni onglets
#'   via le mode \code{--app=} de Chromium / Microsoft Edge). Par defaut \code{TRUE}.
#'   Si aucun navigateur compatible n'est trouve, bascule automatiquement sur le navigateur
#'   systeme standard.
#' @param port Entier optionnel specifiant le port TCP sur lequel ecouter (ex: \code{3838} ou \code{3000}).
#'   Par defaut \code{NULL} : un port local libre est automatiquement attribue via
#'   \code{httpuv::randomPort()} lorsque httpuv est disponible.
#' @param launch.browser Valeur logique ou fonction indiquant s'il faut ouvrir
#'   automatiquement l'application au demarrage. Par defaut \code{TRUE}.
#' @param host Adresse IP sur laquelle ecouter. Par defaut \code{"127.0.0.1"} (localhost).
#'   Utiliser \code{"0.0.0.0"} pour autoriser les connexions reseau externes ou conteneurisees.
#' @param ... Arguments supplementaires transmis a \code{\link[shiny]{shinyApp}}.
#'
#' @return Un objet d'application Shiny executable (\code{shiny.appobj}).
#' @export
#'
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
#'   run_app()
#'   run_app(standalone = FALSE, port = 3838)
#'   run_app(launch.browser = FALSE)
#' }

#' Recherche interne d'un executable Chromium / Edge pour le mode standalone (--app=)
#'
#' @return Chaine de caracteres contenant le chemin absolu de l'executable ou NULL si non trouve.
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

  if (is.null(port)) {
    port <- tryCatch({
      if (requireNamespace("httpuv", quietly = TRUE)) {
        httpuv::randomPort()
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }

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
