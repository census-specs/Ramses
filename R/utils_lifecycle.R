#' Utilitaires de gestion du cycle de vie et de l'\u00e9tat du rapport pour Ramses
#'
#' Fonctions pures d\u00e9di\u00e9es au suivi de l'\u00e9tat du journal d'analyse
#' (rapport inchang\u00e9, modifi\u00e9, sauvegard\u00e9) et \u00e0 la logique d\u00e9cisionnelle
#' du cycle de vie de l'application (Nouvelle instance, Red\u00e9marrer, Fermer).
#'
#' @name utils_lifecycle
#' @keywords internal
NULL

#' Cr\u00e9er une structure d'\u00e9tat du rapport
#'
#' @param dirty Logique. \code{TRUE} si le rapport comporte des modifications non sauvegard\u00e9es.
#' @param last_saved Heure de derni\u00e8re sauvegarde ou \code{NULL}.
#' @param saved_format Nom du format de sauvegarde r\u00e9cent ou \code{NULL}.
#' @return Une liste repr\u00e9sentant l'\u00e9tat du rapport.
#' @noRd
ramses_create_report_state <- function(dirty = FALSE, last_saved = NULL, saved_format = NULL) {
  list(
    dirty = isTRUE(dirty),
    last_saved = last_saved,
    saved_format = saved_format
  )
}

#' Marquer le rapport comme modifi\u00e9 (dirty)
#'
#' @param state Structure d'\u00e9tat du rapport (liste ou reactiveValues).
#' @return L'\u00e9tat mis \u00e0 jour avec \code{dirty = TRUE}.
#' @noRd
ramses_mark_report_dirty <- function(state) {
  state$dirty <- TRUE
  state
}

#' Marquer le rapport comme sauvegard\u00e9 (propre)
#'
#' @param state Structure d'\u00e9tat du rapport (liste ou reactiveValues).
#' @param format_name Cha\u00eene identifiant le format sauvegard\u00e9 (ex: "R", "Rmd", "HTML").
#' @param timestamp Heure de sauvegarde (par d\u00e9faut \code{Sys.time()}).
#' @return L'\u00e9tat mis \u00e0 jour avec \code{dirty = FALSE}.
#' @noRd
ramses_mark_report_saved <- function(state, format_name = "Rapport", timestamp = Sys.time()) {
  state$dirty <- FALSE
  state$last_saved <- timestamp
  state$saved_format <- format_name
  state
}

#' V\u00e9rifier si une demande de sauvegarde doit \u00eatre pr\u00e9sent\u00e9e
#'
#' @param dirty Logique. \code{TRUE} si le rapport est modifi\u00e9.
#' @return \code{TRUE} si une confirmation de sauvegarde est requise, \code{FALSE} sinon.
#' @noRd
ramses_should_prompt_save <- function(dirty) {
  isTRUE(dirty)
}

#' D\u00e9terminer si le rapport correspond strictement \u00e0 l'\u00e9tat initial vierge
#'
#' @param rmd_text Texte actuel du journal Rmd.
#' @param initial_header Texte initial du journal au d\u00e9marrage.
#' @param dataset_name Nom du jeu de donn\u00e9es actif en m\u00e9moire.
#' @return \code{TRUE} si le rapport est strictement vierge d'origine, \code{FALSE} sinon.
#' @noRd
ramses_is_initial_report <- function(rmd_text, initial_header, dataset_name = "iris") {
  identical(trimws(rmd_text), trimws(initial_header)) && identical(as.character(dataset_name), "iris")
}
