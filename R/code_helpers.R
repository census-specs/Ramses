# Helpers for safe construction of R symbols, formulas, and reproducible code.

#' Convert a user-supplied name to valid R source code.
#'
#' Non-syntactic names (spaces, accents, punctuation, etc.) are represented
#' with backticks where necessary. The returned value is source code, not data.
#' @noRd
ramses_code_symbol <- function(name) {
  stopifnot(length(name) == 1L, is.character(name), !is.na(name), nzchar(name))
  rlang::expr_text(rlang::sym(name), width = Inf)
}

#' Convert a character value to a safe R string literal.
#'
#' The returned value is source code and escapes quotes and control characters.
#' @noRd
ramses_code_string <- function(value) {
  stopifnot(length(value) == 1L, is.character(value), !is.na(value))
  rlang::expr_text(rlang::expr(!!value), width = Inf)
}

#' Build a safe reference to a dataframe column for generated R code.
#' @noRd
ramses_code_column <- function(data_name, column_name) {
  paste0(
    ramses_code_symbol(data_name),
    "[[",
    ramses_code_string(column_name),
    "]]"
  )
}

#' Build a formula from column names without parsing user input as operators.
#' @noRd
ramses_formula <- function(response, predictors) {
  stopifnot(
    length(response) == 1L,
    is.character(response),
    !is.na(response),
    nzchar(response),
    is.character(predictors),
    length(predictors) >= 1L,
    all(!is.na(predictors)),
    all(nzchar(predictors))
  )

  rhs <- rlang::syms(predictors)
  rhs_expr <- if (length(rhs) == 1L) {
    rhs[[1L]]
  } else {
    rlang::call2("+", !!!rhs)
  }

  rlang::new_formula(
    lhs = rlang::sym(response),
    rhs = rhs_expr,
    env = rlang::caller_env()
  )
}

#' Build reproducible source code for a formula.
#' @noRd
ramses_formula_code <- function(response, predictors) {
  ramses_code <- ramses_formula(response, predictors)
  rlang::expr_text(ramses_code, width = Inf)
}
