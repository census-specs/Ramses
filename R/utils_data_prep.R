# ==============================================================================
# Helpers et fonctions de transformation securisees pour la preparation des donnees
# Fichier : R/utils_data_prep.R
# ==============================================================================

#' Diagnostic detaille de la qualite et de la structure d'un jeu de donnees
#'
#' @param df Data.frame a inspecter.
#' @return Une liste contenant les diagnostics par colonne et les metadonnees globales.
#' @export
ramses_prep_diagnose <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)

  if (n_cols == 0) {
    return(list(
      overview = list(
        n_rows = n_rows,
        n_cols = 0,
        n_duplicates = 0,
        pct_duplicates = 0,
        total_nas = 0,
        pct_nas = 0,
        complete_cases = 0,
        pct_complete = 0,
        total_outliers = 0,
        pct_outliers = 0
      ),
      columns = data.frame(
        column = character(0),
        type = character(0),
        class = character(0),
        n_total = integer(0),
        n_missing = integer(0),
        pct_missing = numeric(0),
        n_unique = integer(0),
        sample_values = character(0),
        stringsAsFactors = FALSE
      )
    ))
  }

  # Calcul des doublons globaux
  n_duplicates <- if (n_rows > 0) sum(duplicated(df)) else 0
  pct_duplicates <- if (n_rows > 0) round((n_duplicates / n_rows) * 100, 1) else 0

  # Calcul des cas complets
  comp_cases <- if (n_rows > 0) stats::complete.cases(df) else logical(0)
  n_complete <- sum(comp_cases)
  pct_complete <- if (n_rows > 0) round((n_complete / n_rows) * 100, 1) else 0

  # Total des NA et valeurs aberrantes (regle IQR standard boxplot)
  total_cells <- n_rows * n_cols
  total_nas <- sum(is.na(df))
  pct_nas <- if (total_cells > 0) round((total_nas / total_cells) * 100, 1) else 0

  total_outliers <- 0

  col_names <- names(df)
  col_rows <- vector("list", n_cols)

  for (i in seq_len(n_cols)) {
    col_name <- col_names[i]
    col_val <- df[[i]]

    c_type <- if (is.numeric(col_val)) {
      if (is.integer(col_val)) "Entier" else "Num\u00e9rique"
    } else if (is.factor(col_val)) {
      "Facteur"
    } else if (is.character(col_val)) {
      "Texte"
    } else if (is.logical(col_val)) {
      "Logique"
    } else if (inherits(col_val, "Date") || inherits(col_val, "POSIXt")) {
      "Date / Heure"
    } else {
      class(col_val)[1]
    }

    n_na <- sum(is.na(col_val))
    pct_na <- if (n_rows > 0) round((n_na / n_rows) * 100, 1) else 0

    non_na_vals <- col_val[!is.na(col_val)]
    n_uniq <- length(unique(non_na_vals))

    # Detection des valeurs aberrantes sur les variables numeriques (regle de Tukey / boxplot)
    n_outliers <- 0
    if (is.numeric(col_val)) {
      num_vals <- non_na_vals
      if (length(num_vals) >= 4) {
        fn <- stats::fivenum(num_vals)
        iqr_v <- fn[4] - fn[2]
        if (iqr_v > 0) {
          n_outliers <- sum(num_vals < (fn[2] - 1.5 * iqr_v) | num_vals > (fn[4] + 1.5 * iqr_v))
          total_outliers <- total_outliers + n_outliers
        }
      }
    }

    # Apercu de quelques valeurs
    sample_txt <- if (length(non_na_vals) == 0) {
      "(que des NA)"
    } else {
      samples <- utils::head(unique(as.character(non_na_vals)), 3)
      paste(samples, collapse = ", ")
    }

    col_rows[[i]] <- data.frame(
      column = col_name,
      type = c_type,
      class = class(col_val)[1],
      n_total = n_rows,
      n_missing = n_na,
      pct_missing = pct_na,
      n_unique = n_uniq,
      sample_values = sample_txt,
      stringsAsFactors = FALSE
    )
  }

  pct_outliers <- if (total_cells > 0) round((total_outliers / total_cells) * 100, 1) else 0

  cols_df <- do.call(rbind, col_rows)

  list(
    overview = list(
      n_rows = n_rows,
      n_cols = n_cols,
      n_duplicates = n_duplicates,
      pct_duplicates = pct_duplicates,
      total_nas = total_nas,
      pct_nas = pct_nas,
      complete_cases = n_complete,
      pct_complete = pct_complete,
      total_outliers = total_outliers,
      pct_outliers = pct_outliers
    ),
    columns = cols_df
  )
}

#' Renommer une variable de maniere securisee
#'
#' @param df Data.frame source.
#' @param old_name Nom actuel de la colonne.
#' @param new_name Nouveau nom souhaite pour la colonne.
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_rename <- function(df, old_name, new_name) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (!is.character(old_name) || length(old_name) != 1 || !nzchar(old_name)) {
    stop("Le param\u00e8tre 'old_name' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }
  if (!is.character(new_name) || length(new_name) != 1 || !nzchar(new_name)) {
    stop("Le param\u00e8tre 'new_name' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }
  if (!(old_name %in% names(df))) {
    stop(paste0("La variable '", old_name, "' n'existe pas dans le jeu de donn\u00e9es."))
  }

  match_idx <- which(names(df) == old_name)[1]
  out_df <- df
  names(out_df)[match_idx] <- new_name

  code_r <- sprintf(
    'names(dataset)[names(dataset) == %s] <- %s',
    ramses_code_string(old_name),
    ramses_code_string(new_name)
  )

  list(
    df = out_df,
    code = code_r,
    label = paste0("Renommer '", old_name, "' en '", new_name, "'")
  )
}

#' Supprimer une ou plusieurs variables
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes a supprimer.
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_drop <- function(df, vars) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(vars) || length(vars) == 0) {
    return(list(df = df, code = "# Aucune variable a supprimer", label = "Aucune suppression"))
  }

  vars_to_drop <- intersect(vars, names(df))
  if (length(vars_to_drop) == 0) {
    return(list(df = df, code = "# Variables inexistantes ignorees", label = "Aucune suppression"))
  }

  out_df <- df[, !(names(df) %in% vars_to_drop), drop = FALSE]

  code_r <- if (length(vars_to_drop) == 1) {
    sprintf('dataset[[%s]] <- NULL', ramses_code_string(vars_to_drop[1]))
  } else {
    sprintf(
      'dataset <- dataset[, !(names(dataset) %%in%% %s), drop = FALSE]',
      ramses_code_string(vars_to_drop)
    )
  }

  list(
    df = out_df,
    code = code_r,
    label = paste0("Supprimer : ", paste(vars_to_drop, collapse = ", "))
  )
}

#' Reorganiser l'ordre des variables
#'
#' @param df Data.frame source.
#' @param first_vars Vecteur de noms de colonnes a positionner en tete.
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_reorder <- function(df, first_vars) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(first_vars) || length(first_vars) == 0) {
    return(list(df = df, code = "# Ordre inchange", label = "Reorganisation des variables"))
  }

  valid_first <- intersect(first_vars, names(df))
  remaining <- setdiff(names(df), valid_first)
  new_order <- c(valid_first, remaining)

  out_df <- df[, new_order, drop = FALSE]

  code_r <- sprintf(
    'dataset <- dataset[, %s, drop = FALSE]',
    ramses_code_string(new_order)
  )

  list(
    df = out_df,
    code = code_r,
    label = paste0("R\u00e9organiser l'ordre des colonnes")
  )
}

#' Selectionner un sous-ensemble de variables
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes a conserver.
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_select_vars <- function(df, vars) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(vars) || length(vars) == 0) {
    stop("Veuillez s\u00e9lectionner au moins une variable \u00e0 conserver.")
  }

  valid_vars <- intersect(vars, names(df))
  if (length(valid_vars) == 0) {
    stop("Aucune des variables sp\u00e9cifi\u00e9es n'existe dans le jeu de donn\u00e9es.")
  }

  out_df <- df[, valid_vars, drop = FALSE]

  code_r <- sprintf(
    'dataset <- dataset[, %s, drop = FALSE]',
    ramses_code_string(valid_vars)
  )

  list(
    df = out_df,
    code = code_r,
    label = paste0("S\u00e9lectionner ", length(valid_vars), " variable(s)")
  )
}

#' Changer le type d'une variable
#'
#' @param df Data.frame source.
#' @param var Nom de la colonne a convertir.
#' @param target_type Type cible : "numeric", "character", "factor", "logical", "Date".
#' @param date_format Format de date si target_type == "Date" (ex: "%Y-%m-%d", "%d/%m/%Y").
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_cast <- function(df, var, target_type, date_format = "%Y-%m-%d") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (!is.character(var) || length(var) != 1 || !(var %in% names(df))) {
    stop(paste0("La variable '", var, "' n'existe pas dans le jeu de donn\u00e9es."))
  }

  allowed_types <- c("numeric", "character", "factor", "logical", "Date")
  if (!(target_type %in% allowed_types)) {
    stop(paste0("Type cible non support\u00e9 : '", target_type, "'. Types autoris\u00e9s : ", paste(allowed_types, collapse = ", ")))
  }

  out_df <- df
  raw_col <- out_df[[var]]

  converted <- tryCatch({
    if (target_type == "numeric") {
      if (is.factor(raw_col)) {
        as.numeric(as.character(raw_col))
      } else {
        as.numeric(raw_col)
      }
    } else if (target_type == "character") {
      as.character(raw_col)
    } else if (target_type == "factor") {
      as.factor(raw_col)
    } else if (target_type == "logical") {
      as.logical(raw_col)
    } else if (target_type == "Date") {
      fmt <- if (!is.null(date_format) && nzchar(date_format)) date_format else "%Y-%m-%d"
      as.Date(as.character(raw_col), format = fmt)
    }
  }, error = function(e) {
    stop(paste0("Erreur lors de la conversion de '", var, "' vers '", target_type, "' : ", e$message))
  })

  out_df[[var]] <- converted

  code_r <- if (target_type == "numeric") {
    if (is.factor(raw_col)) {
      sprintf('dataset[[%s]] <- as.numeric(as.character(dataset[[%s]]))', ramses_code_string(var), ramses_code_string(var))
    } else {
      sprintf('dataset[[%s]] <- as.numeric(dataset[[%s]])', ramses_code_string(var), ramses_code_string(var))
    }
  } else if (target_type == "character") {
    sprintf('dataset[[%s]] <- as.character(dataset[[%s]])', ramses_code_string(var), ramses_code_string(var))
  } else if (target_type == "factor") {
    sprintf('dataset[[%s]] <- as.factor(dataset[[%s]])', ramses_code_string(var), ramses_code_string(var))
  } else if (target_type == "logical") {
    sprintf('dataset[[%s]] <- as.logical(dataset[[%s]])', ramses_code_string(var), ramses_code_string(var))
  } else if (target_type == "Date") {
    fmt <- if (!is.null(date_format) && nzchar(date_format)) date_format else "%Y-%m-%d"
    sprintf('dataset[[%s]] <- as.Date(as.character(dataset[[%s]]), format = %s)', ramses_code_string(var), ramses_code_string(var), ramses_code_string(fmt))
  }

  list(
    df = out_df,
    code = code_r,
    label = paste0("Convertir '", var, "' en ", target_type)
  )
}

#' Recoder les valeurs d'une variable
#'
#' @param df Data.frame source.
#' @param var Nom de la colonne a recoder.
#' @param mapping Liste ou vecteur nomme associant anciennes valeurs -> nouvelles valeurs.
#' @param default Mode pour les valeurs non specifiees : "keep" (conserver l'ancienne valeur), "na" (mettre a NA), ou valeur fixe.
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_recode <- function(df, var, mapping, default = "keep") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (!is.character(var) || length(var) != 1 || !(var %in% names(df))) {
    stop(paste0("La variable '", var, "' n'existe pas dans le jeu de donn\u00e9es."))
  }
  if (is.null(mapping) || length(mapping) == 0) {
    return(list(df = df, code = "# Aucun recodage specifie", label = "Recodage inchange"))
  }

  out_df <- df
  raw_col <- out_df[[var]]
  char_col <- as.character(raw_col)

  # Construction du vecteur de correspondance
  old_vals <- names(mapping)
  new_vals <- unname(unlist(mapping))

  new_col <- if (identical(default, "keep")) {
    char_col
  } else if (identical(default, "na")) {
    rep(NA_character_, length(char_col))
  } else {
    rep(as.character(default), length(char_col))
  }

  for (k in seq_along(old_vals)) {
    old_v <- old_vals[k]
    new_v <- new_vals[k]

    if (is.na(old_v) || old_v == "__RAMSES_NA__" || old_v == "NA") {
      mask <- is.na(raw_col)
    } else {
      mask <- !is.na(char_col) & (char_col == old_v)
    }

    if (is.na(new_v) || new_v == "__RAMSES_NA__" || new_v == "NA") {
      new_col[mask] <- NA_character_
    } else {
      new_col[mask] <- new_v
    }
  }

  # Si la colonne d'origine etait un facteur, recreer un facteur propre
  if (is.factor(raw_col)) {
    out_df[[var]] <- factor(new_col)
  } else if (is.numeric(raw_col) && all(is.na(new_col) | grepl("^-?[0-9]+(\\.[0-9]+)?$", new_col))) {
    out_df[[var]] <- as.numeric(new_col)
  } else {
    out_df[[var]] <- new_col
  }

  # Generation du code R
  code_lines <- c(
    sprintf('rec_map <- c(%s)', paste(sprintf('%s = %s', vapply(old_vals, ramses_code_string, character(1)), vapply(new_vals, ramses_code_string, character(1))), collapse = ", ")),
    if (identical(default, "keep")) {
      sprintf('dataset[[%s]] <- ifelse(as.character(dataset[[%s]]) %%in%% names(rec_map), rec_map[as.character(dataset[[%s]])], as.character(dataset[[%s]]))', ramses_code_string(var), ramses_code_string(var), ramses_code_string(var), ramses_code_string(var))
    } else {
      sprintf('dataset[[%s]] <- rec_map[as.character(dataset[[%s]])]', ramses_code_string(var), ramses_code_string(var))
    }
  )

  list(
    df = out_df,
    code = paste(code_lines, collapse = "\n"),
    label = paste0("Recoder les modalit\u00e9s de '", var, "'")
  )
}

#' Remplacer des valeurs specifiques dans une variable
#'
#' @param df Data.frame source.
#' @param var Nom de la colonne cible.
#' @param old_val Valeur a rechercher (ou NULL si is_na = TRUE).
#' @param new_val Nouvelle valeur de remplacement (ou NULL si is_new_na = TRUE).
#' @param is_old_na Booleen indiquant si on remplace les NA.
#' @param is_new_na Booleen indiquant si la nouvelle valeur est NA.
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_replace_value <- function(df, var, old_val = NULL, new_val = NULL, is_old_na = FALSE, is_new_na = FALSE) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (!is.character(var) || length(var) != 1 || !(var %in% names(df))) {
    stop(paste0("La variable '", var, "' n'existe pas dans le jeu de donn\u00e9es."))
  }

  out_df <- df
  col_val <- out_df[[var]]

  if (isTRUE(is_old_na)) {
    mask <- is.na(col_val)
    if (isTRUE(is_new_na)) {
      # NA remplace par NA -> rien a faire
    } else {
      if (is.numeric(col_val)) {
        col_val[mask] <- as.numeric(new_val)
      } else if (is.factor(col_val)) {
        levels(col_val) <- c(levels(col_val), as.character(new_val))
        col_val[mask] <- as.character(new_val)
      } else {
        col_val[mask] <- as.character(new_val)
      }
    }
    code_r <- if (isTRUE(is_new_na)) {
      "# Remplacement NA par NA (inchange)"
    } else {
      sprintf('dataset[[%s]][is.na(dataset[[%s]])] <- %s', ramses_code_string(var), ramses_code_string(var), if (is.numeric(col_val)) new_val else ramses_code_string(new_val))
    }
  } else {
    char_col <- as.character(col_val)
    target_str <- as.character(old_val)
    mask <- !is.na(char_col) & (char_col == target_str)

    if (isTRUE(is_new_na)) {
      col_val[mask] <- NA
      code_r <- sprintf('dataset[[%s]][dataset[[%s]] == %s] <- NA', ramses_code_string(var), ramses_code_string(var), if (is.numeric(col_val)) old_val else ramses_code_string(old_val))
    } else {
      if (is.numeric(col_val)) {
        col_val[mask] <- as.numeric(new_val)
        code_r <- sprintf('dataset[[%s]][dataset[[%s]] == %s] <- %s', ramses_code_string(var), ramses_code_string(var), old_val, new_val)
      } else if (is.factor(col_val)) {
        levels(col_val) <- unique(c(levels(col_val), as.character(new_val)))
        col_val[mask] <- as.character(new_val)
        code_r <- sprintf('dataset[[%s]][dataset[[%s]] == %s] <- %s', ramses_code_string(var), ramses_code_string(var), ramses_code_string(old_val), ramses_code_string(new_val))
      } else {
        col_val[mask] <- as.character(new_val)
        code_r <- sprintf('dataset[[%s]][dataset[[%s]] == %s] <- %s', ramses_code_string(var), ramses_code_string(var), ramses_code_string(old_val), ramses_code_string(new_val))
      }
    }
  }

  out_df[[var]] <- col_val

  list(
    df = out_df,
    code = code_r,
    label = paste0("Remplacer '", if (is_old_na) "NA" else old_val, "' par '", if (is_new_na) "NA" else new_val, "' dans '", var, "'")
  )
}

#' Validation recurrente securisee d'un arbre syntaxique R (AST)
#'
#' @param node Noeud d'expression R.
#' @param allowed_cols Noms des colonnes du jeu de donnees.
#' @return Booleen TRUE si securise, sinon leve une erreur explicite.
#' @noRd
ramses_validate_ast_node <- function(node, allowed_cols) {
  if (is.null(node)) return(TRUE)

  # Constante litterale (nombre, chaine, booleen)
  if (is.numeric(node) || is.character(node) || is.logical(node)) {
    return(TRUE)
  }

  # Symbole / variable
  if (is.symbol(node) || is.name(node)) {
    sym_name <- as.character(node)
    # Constantes math autorisees
    if (sym_name %in% c("pi", "TRUE", "FALSE", "NULL", "NA", "Inf", "NaN")) {
      return(TRUE)
    }
    # Doit appartenir aux colonnes du dataframe
    if (sym_name %in% allowed_cols) {
      return(TRUE)
    }
    stop(paste0("Symbole non autoris\u00e9 ou colonne introuvable dans l'expression : '", sym_name, "'"))
  }

  # Appel de fonction / operateur
  if (is.call(node)) {
    fn_name <- as.character(node[[1]])

    # Liste blanche stricte des fonctions et operateurs autorises
    allowed_fns <- c(
      "+", "-", "*", "/", "^", "(",
      "log", "log10", "log2", "exp", "sqrt", "abs",
      "round", "floor", "ceiling", "trunc", "sign",
      "sin", "cos", "tan", "asin", "acos", "atan",
      "pmin", "pmax"
    )

    if (!(fn_name %in% allowed_fns)) {
      stop(paste0("Op\u00e9rateur ou fonction non autoris\u00e9(e) pour des raisons de s\u00e9curit\u00e9 : '", fn_name, "()'"))
    }

    # Validation recursive de tous les arguments
    if (length(node) > 1) {
      for (i in 2:length(node)) {
        ramses_validate_ast_node(node[[i]], allowed_cols)
      }
    }
    return(TRUE)
  }

  stop("Type de structure syntaxique non autoris\u00e9 dans l'expression calcul\u00e9e.")
}

#' Creer une variable calculee de maniere securisee
#'
#' @param df Data.frame source.
#' @param new_var Nom de la nouvelle variable a creer.
#' @param expr_str Expression mathematique arithmetique (ex: "revenu_agricole + revenu_autre").
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_compute <- function(df, new_var, expr_str) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (!is.character(new_var) || length(new_var) != 1 || !nzchar(new_var)) {
    stop("Le nom de la nouvelle variable 'new_var' doit \u00eatre une cha\u00eene de caract\u00e8res non vide.")
  }
  if (!is.character(expr_str) || length(expr_str) != 1 || !nzchar(trimws(expr_str))) {
    stop("L'expression de calcul 'expr_str' doit \u00eatre une formule non vide.")
  }

  trimmed_expr <- trimws(expr_str)

  # 1. Parsing securise sans execution
  parsed_expr <- tryCatch({
    parse(text = trimmed_expr, keep.source = FALSE)
  }, error = function(e) {
    stop(paste0("Erreur de syntaxe dans l'expression : ", e$message))
  })

  if (length(parsed_expr) != 1) {
    stop("L'expression doit \u00eatre une formule unique sans instructions multiples.")
  }

  # 2. Validation de securite AST sur liste blanche
  ramses_validate_ast_node(parsed_expr[[1]], names(df))

  # 3. Evaluation securisee dans un environnement isole contenant uniquement les colonnes du df
  eval_env <- list2env(as.list(df), parent = baseenv())

  # Ajout des fonctions mathematiques de base autorisees
  eval_env$`+` <- base::`+`
  eval_env$`-` <- base::`-`
  eval_env$`*` <- base::`*`
  eval_env$`\`/\`` <- base::`/`
  eval_env$`/` <- base::`/`
  eval_env$`^` <- base::`^`
  eval_env$`(` <- base::`(`
  eval_env$log <- base::log
  eval_env$log10 <- base::log10
  eval_env$log2 <- base::log2
  eval_env$exp <- base::exp
  eval_env$sqrt <- base::sqrt
  eval_env$abs <- base::abs
  eval_env$round <- base::round
  eval_env$floor <- base::floor
  eval_env$ceiling <- base::ceiling
  eval_env$trunc <- base::trunc
  eval_env$sign <- base::sign
  eval_env$sin <- base::sin
  eval_env$cos <- base::cos
  eval_env$tan <- base::tan
  eval_env$asin <- base::asin
  eval_env$acos <- base::acos
  eval_env$atan <- base::atan
  eval_env$pmin <- base::pmin
  eval_env$pmax <- base::pmax
  eval_env$pi <- base::pi

  calc_res <- tryCatch({
    eval(parsed_expr[[1]], envir = eval_env)
  }, error = function(e) {
    stop(paste0("Erreur lors de l'\u00e9valuation de l'expression : ", e$message))
  })

  # Verification de la longueur du resultat
  if (nrow(df) > 0 && length(calc_res) == 1) {
    calc_res <- rep(calc_res, nrow(df))
  } else if (length(calc_res) != nrow(df)) {
    stop(paste0("La taille du r\u00e9sultat (", length(calc_res), ") ne correspond pas au nombre de lignes du jeu de donn\u00e9es (", nrow(df), ")."))
  }

  out_df <- df
  out_df[[new_var]] <- calc_res

  code_r <- sprintf(
    'dataset[[%s]] <- with(dataset, %s)',
    ramses_code_string(new_var),
    trimmed_expr
  )

  list(
    df = out_df,
    code = code_r,
    label = paste0("Cr\u00e9er la variable calcul\u00e9e '", new_var, "' = ", trimmed_expr)
  )
}

#' Imputation ou traitement des valeurs manquantes (NA)
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes cible (si NULL, toutes les colonnes).
#' @param method Methode de traitement : "drop_rows", "fixed", "mean", "median", "mode".
#' @param fixed_value Valeur fixe utilisee si method == "fixed".
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_impute_na <- function(df, vars = NULL, method = "drop_rows", fixed_value = NULL) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }

  target_vars <- if (is.null(vars) || length(vars) == 0) {
    names(df)
  } else {
    intersect(vars, names(df))
  }

  if (length(target_vars) == 0) {
    return(list(df = df, code = "# Aucune variable a traiter", label = "Gestion des valeurs manquantes"))
  }

  allowed_methods <- c("drop_rows", "fixed", "mean", "median", "mode")
  if (!(method %in% allowed_methods)) {
    stop(paste0("M\u00e9thode de traitement NA non reconnue : '", method, "'"))
  }

  out_df <- df

  if (method == "drop_rows") {
    # Supprimer les lignes contenant des NA sur les colonnes selectionnees
    na_matrix <- is.na(out_df[, target_vars, drop = FALSE])
    has_na_row <- apply(na_matrix, 1, any)
    out_df <- out_df[!has_na_row, , drop = FALSE]

    code_r <- if (length(target_vars) == ncol(df)) {
      'dataset <- dataset[stats::complete.cases(dataset), , drop = FALSE]'
    } else {
      sprintf(
        'dataset <- dataset[stats::complete.cases(dataset[, %s, drop = FALSE]), , drop = FALSE]',
        ramses_code_string(target_vars)
      )
    }

    label_txt <- paste0("Supprimer les lignes avec NA (", sum(has_na_row), " lignes supprim\u00e9es)")

  } else if (method == "fixed") {
    if (is.null(fixed_value)) {
      stop("Veuillez sp\u00e9cifier une valeur fixe pour l'imputation.")
    }

    for (v in target_vars) {
      col_val <- out_df[[v]]
      mask <- is.na(col_val)
      if (any(mask)) {
        if (is.numeric(col_val)) {
          col_val[mask] <- as.numeric(fixed_value)
        } else if (is.factor(col_val)) {
          levels(col_val) <- unique(c(levels(col_val), as.character(fixed_value)))
          col_val[mask] <- as.character(fixed_value)
        } else {
          col_val[mask] <- as.character(fixed_value)
        }
        out_df[[v]] <- col_val
      }
    }

    code_r <- paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]][is.na(dataset[[%s]])] <- %s', ramses_code_string(v), ramses_code_string(v), if (is.numeric(out_df[[v]])) fixed_value else ramses_code_string(fixed_value))
      }, character(1)),
      collapse = "\n"
    )

    label_txt <- paste0("Remplacer les NA par '", fixed_value, "' sur ", length(target_vars), " variable(s)")

  } else if (method == "mean") {
    # Verification des types numeriques
    non_num <- target_vars[!vapply(out_df[target_vars], is.numeric, logical(1))]
    if (length(non_num) > 0) {
      stop(paste0("L'imputation par la moyenne requiert des variables num\u00e9riques. Variables incompatibles : ", paste(non_num, collapse = ", ")))
    }

    for (v in target_vars) {
      col_val <- out_df[[v]]
      mask <- is.na(col_val)
      if (any(mask)) {
        non_na <- col_val[!mask]
        if (length(non_na) == 0) {
          stop(paste0("La variable '", v, "' est enti\u00e8rement manquante. Impossible de calculer la moyenne."))
        }
        m_val <- mean(non_na)
        col_val[mask] <- m_val
        out_df[[v]] <- col_val
      }
    }

    code_r <- paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]][is.na(dataset[[%s]])] <- mean(dataset[[%s]], na.rm = TRUE)', ramses_code_string(v), ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )

    label_txt <- paste0("Imputer les NA par la moyenne sur : ", paste(target_vars, collapse = ", "))

  } else if (method == "median") {
    # Verification des types numeriques
    non_num <- target_vars[!vapply(out_df[target_vars], is.numeric, logical(1))]
    if (length(non_num) > 0) {
      stop(paste0("L'imputation par la m\u00e9diane requiert des variables num\u00e9riques. Variables incompatibles : ", paste(non_num, collapse = ", ")))
    }

    for (v in target_vars) {
      col_val <- out_df[[v]]
      mask <- is.na(col_val)
      if (any(mask)) {
        non_na <- col_val[!mask]
        if (length(non_na) == 0) {
          stop(paste0("La variable '", v, "' est enti\u00e8rement manquante. Impossible de calculer la m\u00e9diane."))
        }
        med_val <- stats::median(non_na)
        col_val[mask] <- med_val
        out_df[[v]] <- col_val
      }
    }

    code_r <- paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]][is.na(dataset[[%s]])] <- stats::median(dataset[[%s]], na.rm = TRUE)', ramses_code_string(v), ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )

    label_txt <- paste0("Imputer les NA par la m\u00e9diane sur : ", paste(target_vars, collapse = ", "))

  } else if (method == "mode") {
    # Mode statistique (plus frequente valeur non-NA)
    for (v in target_vars) {
      col_val <- out_df[[v]]
      mask <- is.na(col_val)
      if (any(mask)) {
        non_na <- col_val[!mask]
        if (length(non_na) == 0) {
          stop(paste0("La variable '", v, "' est enti\u00e8rement manquante. Impossible de calculer le mode."))
        }
        tab <- table(non_na)
        mode_val_char <- names(tab)[which.max(tab)]

        if (is.numeric(col_val)) {
          col_val[mask] <- as.numeric(mode_val_char)
        } else if (is.factor(col_val)) {
          col_val[mask] <- as.character(mode_val_char)
        } else {
          col_val[mask] <- as.character(mode_val_char)
        }
        out_df[[v]] <- col_val
      }
    }

    code_r <- paste(
      vapply(target_vars, function(v) {
        sprintf(
          'mode_val <- names(sort(table(dataset[[%s]]), decreasing = TRUE))[1]\ndataset[[%s]][is.na(dataset[[%s]])] <- %s',
          ramses_code_string(v),
          ramses_code_string(v),
          ramses_code_string(v),
          if (is.numeric(out_df[[v]])) 'as.numeric(mode_val)' else 'mode_val'
        )
      }, character(1)),
      collapse = "\n"
    )

    label_txt <- paste0("Imputer les NA par le mode sur : ", paste(target_vars, collapse = ", "))
  }

  list(
    df = out_df,
    code = code_r,
    label = label_txt
  )
}

#' Supprimer les doublons dans un jeu de donnees
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes cible (si NULL, l'integralite de la ligne).
#' @param keep Conserver la premiere ("first") ou derniere occurrence ("last").
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_deduplicate <- function(df, vars = NULL, keep = "first") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }

  if (nrow(df) <= 1) {
    return(list(df = df, code = "# Aucun doublon a supprimer", label = "D\u00e9doublonnage inchange"))
  }

  target_vars <- if (is.null(vars) || length(vars) == 0) names(df) else intersect(vars, names(df))
  from_last <- identical(keep, "last")

  sub_df <- df[, target_vars, drop = FALSE]
  dup_mask <- duplicated(sub_df, fromLast = from_last)
  n_dups <- sum(dup_mask)

  out_df <- df[!dup_mask, , drop = FALSE]

  code_r <- if (length(target_vars) == ncol(df)) {
    if (from_last) {
      'dataset <- dataset[!duplicated(dataset, fromLast = TRUE), , drop = FALSE]'
    } else {
      'dataset <- dataset[!duplicated(dataset), , drop = FALSE]'
    }
  } else {
    if (from_last) {
      sprintf('dataset <- dataset[!duplicated(dataset[, %s, drop = FALSE], fromLast = TRUE), , drop = FALSE]', ramses_code_string(target_vars))
    } else {
      sprintf('dataset <- dataset[!duplicated(dataset[, %s, drop = FALSE]), , drop = FALSE]', ramses_code_string(target_vars))
    }
  }

  list(
    df = out_df,
    code = code_r,
    label = paste0("Supprimer les doublons (", n_dups, " lignes supprim\u00e9es)")
  )
}

#' Nettoyer les espaces superflus dans les variables textuelles
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes texte/facteur a nettoyer.
#' @param mode Mode de nettoyage : "both" (debut/fin), "left", "right", "squish" (debut/fin + espaces internes multiples).
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_trim_ws <- function(df, vars, mode = "both") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(vars) || length(vars) == 0) {
    return(list(df = df, code = "# Aucun nettoyage d'espace", label = "Nettoyage des espaces"))
  }

  target_vars <- intersect(vars, names(df))
  if (length(target_vars) == 0) {
    return(list(df = df, code = "# Variables inexistantes", label = "Nettoyage des espaces"))
  }

  out_df <- df

  for (v in target_vars) {
    col_val <- out_df[[v]]
    was_factor <- is.factor(col_val)
    char_val <- as.character(col_val)

    cleaned <- if (mode == "both") {
      trimws(char_val, which = "both")
    } else if (mode == "left") {
      trimws(char_val, which = "left")
    } else if (mode == "right") {
      trimws(char_val, which = "right")
    } else if (mode == "squish") {
      trimmed <- trimws(char_val, which = "both")
      gsub("\\s+", " ", trimmed)
    } else {
      trimws(char_val)
    }

    out_df[[v]] <- if (was_factor) factor(cleaned) else cleaned
  }

  code_r <- if (mode == "squish") {
    paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]] <- gsub("\\\\s+", " ", trimws(dataset[[%s]]))', ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )
  } else {
    paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]] <- trimws(dataset[[%s]], which = %s)', ramses_code_string(v), ramses_code_string(v), ramses_code_string(mode))
      }, character(1)),
      collapse = "\n"
    )
  }

  list(
    df = out_df,
    code = code_r,
    label = paste0("Nettoyer les espaces (", mode, ") sur : ", paste(target_vars, collapse = ", "))
  )
}

#' Modifier la casse du texte
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes texte/facteur.
#' @param target_case Casse cible : "lower", "upper", "title", "sentence".
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_change_case <- function(df, vars, target_case = "lower") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(vars) || length(vars) == 0) {
    return(list(df = df, code = "# Aucune modification de casse", label = "Modification de la casse"))
  }

  target_vars <- intersect(vars, names(df))
  if (length(target_vars) == 0) {
    return(list(df = df, code = "# Variables inexistantes", label = "Modification de la casse"))
  }

  out_df <- df

  for (v in target_vars) {
    col_val <- out_df[[v]]
    was_factor <- is.factor(col_val)
    char_val <- as.character(col_val)

    converted <- if (target_case == "lower") {
      tolower(char_val)
    } else if (target_case == "upper") {
      toupper(char_val)
    } else if (target_case == "title") {
      # Premiere lettre de chaque mot en majuscule
      tools::toTitleCase(tolower(char_val))
    } else if (target_case == "sentence") {
      # Premiere lettre en majuscule, reste en minuscule
      vapply(char_val, function(s) {
        if (is.na(s) || !nzchar(s)) return(s)
        paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
      }, character(1), USE.NAMES = FALSE)
    } else {
      char_val
    }

    out_df[[v]] <- if (was_factor) factor(converted) else converted
  }

  code_r <- if (target_case == "lower") {
    paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]] <- tolower(dataset[[%s]])', ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )
  } else if (target_case == "upper") {
    paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]] <- toupper(dataset[[%s]])', ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )
  } else if (target_case == "title") {
    paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]] <- tools::toTitleCase(tolower(dataset[[%s]]))', ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )
  } else {
    paste(
      vapply(target_vars, function(v) {
        sprintf('dataset[[%s]] <- paste0(toupper(substr(dataset[[%s]], 1, 1)), tolower(substr(dataset[[%s]], 2, nchar(dataset[[%s]]))))', ramses_code_string(v), ramses_code_string(v), ramses_code_string(v), ramses_code_string(v))
      }, character(1)),
      collapse = "\n"
    )
  }

  list(
    df = out_df,
    code = code_r,
    label = paste0("Modifier la casse en '", target_case, "' sur : ", paste(target_vars, collapse = ", "))
  )
}

#' Filtrer les observations selon des criteres securises
#'
#' @param df Data.frame source.
#' @param conditions Liste de conditions. Chaque condition est une liste : list(var = "col", op = "eq", val = "x").
#' @param combine_op Operateur de combinaison logique : "AND" ou "OR".
#' @return Une liste contenant le data.frame filtre et le code R genere.
#' @export
ramses_prep_filter <- function(df, conditions, combine_op = "AND") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(conditions) || length(conditions) == 0) {
    return(list(df = df, code = "# Aucun filtre applique", label = "Filtrage inchange"))
  }

  n_rows <- nrow(df)
  if (n_rows == 0) {
    return(list(df = df, code = "# Data.frame vide", label = "Filtrage"))
  }

  allowed_ops <- c("eq", "neq", "gt", "lt", "gte", "lte", "contains", "starts_with", "ends_with", "is_na", "not_na", "in")

  condition_masks <- vector("list", length(conditions))
  condition_code_snippets <- character(length(conditions))

  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]
    var_name <- cond$var
    op <- cond$op
    val <- cond$val

    if (!is.character(var_name) || length(var_name) != 1 || !(var_name %in% names(df))) {
      stop(paste0("Condition ", i, " : la variable '", var_name, "' n'existe pas dans le jeu de donn\u00e9es."))
    }
    if (!(op %in% allowed_ops)) {
      stop(paste0("Condition ", i, " : op\u00e9rateur de filtre non reconnu '", op, "'"))
    }

    col_data <- df[[var_name]]
    is_num <- is.numeric(col_data)

    mask <- switch(
      op,
      "eq" = {
        if (is_num) {
          num_val <- as.numeric(val)
          !is.na(col_data) & (col_data == num_val)
        } else {
          !is.na(col_data) & (as.character(col_data) == as.character(val))
        }
      },
      "neq" = {
        if (is_num) {
          num_val <- as.numeric(val)
          !is.na(col_data) & (col_data != num_val)
        } else {
          !is.na(col_data) & (as.character(col_data) != as.character(val))
        }
      },
      "gt" = {
        num_val <- as.numeric(val)
        !is.na(col_data) & (col_data > num_val)
      },
      "lt" = {
        num_val <- as.numeric(val)
        !is.na(col_data) & (col_data < num_val)
      },
      "gte" = {
        num_val <- as.numeric(val)
        !is.na(col_data) & (col_data >= num_val)
      },
      "lte" = {
        num_val <- as.numeric(val)
        !is.na(col_data) & (col_data <= num_val)
      },
      "contains" = {
        !is.na(col_data) & grepl(as.character(val), as.character(col_data), fixed = TRUE)
      },
      "starts_with" = {
        !is.na(col_data) & grepl(paste0("^", gsub("([.\\\\+*?\\[\\^\\$\\(-\\]])", "\\\\\\1", as.character(val))), as.character(col_data))
      },
      "ends_with" = {
        !is.na(col_data) & grepl(paste0(gsub("([.\\\\+*?\\[\\^\\$\\(-\\]])", "\\\\\\1", as.character(val)), "$"), as.character(col_data))
      },
      "is_na" = {
        is.na(col_data)
      },
      "not_na" = {
        !is.na(col_data)
      },
      "in" = {
        vals_vec <- if (is.character(val)) unlist(strsplit(val, ",\\s*")) else as.character(val)
        if (is_num) {
          !is.na(col_data) & (col_data %in% as.numeric(vals_vec))
        } else {
          !is.na(col_data) & (as.character(col_data) %in% vals_vec)
        }
      }
    )

    condition_masks[[i]] <- mask

    # Code R correspondant
    col_code <- sprintf('dataset[[%s]]', ramses_code_string(var_name))
    snippet <- switch(
      op,
      "eq" = sprintf('(!is.na(%s) & %s == %s)', col_code, col_code, if (is_num) val else ramses_code_string(val)),
      "neq" = sprintf('(!is.na(%s) & %s != %s)', col_code, col_code, if (is_num) val else ramses_code_string(val)),
      "gt" = sprintf('(!is.na(%s) & %s > %s)', col_code, col_code, val),
      "lt" = sprintf('(!is.na(%s) & %s < %s)', col_code, col_code, val),
      "gte" = sprintf('(!is.na(%s) & %s >= %s)', col_code, col_code, val),
      "lte" = sprintf('(!is.na(%s) & %s <= %s)', col_code, col_code, val),
      "contains" = sprintf('(!is.na(%s) & grepl(%s, %s, fixed = TRUE))', col_code, ramses_code_string(val), col_code),
      "starts_with" = sprintf('(!is.na(%s) & grepl(%s, %s))', col_code, ramses_code_string(paste0("^", as.character(val))), col_code),
      "ends_with" = sprintf('(!is.na(%s) & grepl(%s, %s))', col_code, ramses_code_string(paste0(as.character(val), "$")), col_code),
      "is_na" = sprintf('is.na(%s)', col_code),
      "not_na" = sprintf('!is.na(%s)', col_code),
      "in" = {
        vals_vec <- if (is.character(val)) unlist(strsplit(val, ",\\s*")) else as.character(val)
        sprintf('(!is.na(%s) & %s %%in%% %s)', col_code, col_code, ramses_code_string(if (is_num) as.numeric(vals_vec) else vals_vec))
      }
    )
    condition_code_snippets[i] <- snippet
  }

  # Combinaison logique des masques
  final_mask <- if (combine_op == "OR") {
    Reduce(`|`, condition_masks)
  } else {
    Reduce(`&`, condition_masks)
  }

  # Remplacement d'eventuels NA dans le masque par FALSE
  final_mask[is.na(final_mask)] <- FALSE

  out_df <- df[final_mask, , drop = FALSE]

  combiner_str <- if (combine_op == "OR") "\n  | " else "\n  & "
  code_r <- sprintf(
    'filter_mask <- %s\ndataset <- dataset[filter_mask, , drop = FALSE]',
    paste(condition_code_snippets, collapse = combiner_str)
  )

  list(
    df = out_df,
    code = code_r,
    label = paste0("Filtrer (", length(conditions), " condition(s), ", nrow(out_df), "/", n_rows, " lignes retenues)")
  )
}

#' Trier les observations d'un jeu de donnees
#'
#' @param df Data.frame source.
#' @param vars Vecteur de noms de colonnes de tri.
#' @param orders Vecteur ou valeur unique de directions : "asc" (croissant) ou "desc" (decroissant).
#' @return Une liste contenant le data.frame modifie et le code R genere.
#' @export
ramses_prep_sort <- function(df, vars, orders = "asc") {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(vars) || length(vars) == 0) {
    return(list(df = df, code = "# Aucun tri applique", label = "Tri inchange"))
  }

  valid_vars <- intersect(vars, names(df))
  if (length(valid_vars) == 0) {
    return(list(df = df, code = "# Variables de tri inexistantes", label = "Tri inchange"))
  }

  if (length(orders) == 1 && length(valid_vars) > 1) {
    orders <- rep(orders, length(valid_vars))
  }

  # Construction de la liste des cles pour do.call(order, ...)
  order_args <- vector("list", length(valid_vars))
  for (i in seq_along(valid_vars)) {
    v <- valid_vars[i]
    col_v <- df[[v]]
    is_desc <- (orders[i] == "desc")

    if (is_desc) {
      if (is.numeric(col_v)) {
        order_args[[i]] <- -col_v
      } else {
        # xtfrm gere le classement decroissant des chaines et facteurs
        order_args[[i]] <- -stats::xtfrm(col_v)
      }
    } else {
      order_args[[i]] <- col_v
    }
  }

  sort_order <- do.call(order, order_args)
  out_df <- df[sort_order, , drop = FALSE]

  # Generation du code R avec stats::xtfrm pour compatibilite universelle
  code_order_items <- vapply(seq_along(valid_vars), function(i) {
    v <- valid_vars[i]
    is_desc <- (orders[i] == "desc")
    col_ref <- sprintf('dataset[[%s]]', ramses_code_string(v))
    if (is_desc) {
      if (is.numeric(df[[v]])) sprintf('-%s', col_ref) else sprintf('-xtfrm(%s)', col_ref)
    } else {
      col_ref
    }
  }, character(1))

  code_r <- sprintf(
    'dataset <- dataset[order(%s), , drop = FALSE]',
    paste(code_order_items, collapse = ", ")
  )

  list(
    df = out_df,
    code = code_r,
    label = paste0("Trier par : ", paste(sprintf("%s (%s)", valid_vars, orders), collapse = ", "))
  )
}

#' Appliquer une etape unique de transformation dans le moteur de pipeline
#'
#' @param df Data.frame source.
#' @param step Structure de l'etape : list(type = "...", params = list(...)).
#' @return Une liste contenant le nouveau data.frame, le code R genere et le statut.
#' @export
ramses_prep_apply_step <- function(df, step) {
  if (is.null(step) || !is.list(step) || is.null(step$type)) {
    stop("Structure d'\u00e9tape de transformation invalide.")
  }

  op_type <- step$type
  p <- step$params

  res <- switch(
    op_type,
    "rename" = ramses_prep_rename(df, old_name = p$old_name, new_name = p$new_name),
    "drop" = ramses_prep_drop(df, vars = p$vars),
    "reorder" = ramses_prep_reorder(df, first_vars = p$first_vars),
    "select_vars" = ramses_prep_select_vars(df, vars = p$vars),
    "cast" = ramses_prep_cast(df, var = p$var, target_type = p$target_type, date_format = p$date_format),
    "recode" = ramses_prep_recode(df, var = p$var, mapping = p$mapping, default = if (!is.null(p$default)) p$default else "keep"),
    "replace_value" = ramses_prep_replace_value(df, var = p$var, old_val = p$old_val, new_val = p$new_val, is_old_na = isTRUE(p$is_old_na), is_new_na = isTRUE(p$is_new_na)),
    "compute" = ramses_prep_compute(df, new_var = p$new_var, expr_str = p$expr_str),
    "impute_na" = ramses_prep_impute_na(df, vars = p$vars, method = p$method, fixed_value = p$fixed_value),
    "deduplicate" = ramses_prep_deduplicate(df, vars = p$vars, keep = if (!is.null(p$keep)) p$keep else "first"),
    "trim_ws" = ramses_prep_trim_ws(df, vars = p$vars, mode = if (!is.null(p$mode)) p$mode else "both"),
    "change_case" = ramses_prep_change_case(df, vars = p$vars, target_case = if (!is.null(p$target_case)) p$target_case else "lower"),
    "filter" = ramses_prep_filter(df, conditions = p$conditions, combine_op = if (!is.null(p$combine_op)) p$combine_op else "AND"),
    "sort" = ramses_prep_sort(df, vars = p$vars, orders = if (!is.null(p$orders)) p$orders else "asc"),
    stop(paste0("Type d'op\u00e9ration inconnu : '", op_type, "'"))
  )

  res$step_id <- if (!is.null(step$id)) step$id else paste0("step_", format(Sys.time(), "%Y%m%d%H%M%S"))
  res$type <- op_type
  res
}

#' Appliquer une suite ordonnee d'etapes de transformation (Pipeline)
#'
#' @param df Data.frame initial.
#' @param pipeline Liste d'etapes de transformation.
#' @return Une liste contenant le data.frame final et le code R cumule.
#' @export
ramses_prep_apply_pipeline <- function(df, pipeline) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Le param\u00e8tre 'df' doit \u00eatre un data.frame valide.")
  }
  if (is.null(pipeline) || length(pipeline) == 0) {
    return(list(
      df = df,
      code = "# Aucune transformation dans le pipeline",
      history = list(),
      n_steps = 0
    ))
  }

  current_df <- df
  history_entries <- list()
  code_snippets <- character(0)

  for (i in seq_along(pipeline)) {
    step <- pipeline[[i]]
    step_res <- ramses_prep_apply_step(current_df, step)
    current_df <- step_res$df
    history_entries[[i]] <- step_res
    code_snippets <- c(code_snippets, paste0("# \u00c9tape ", i, " : ", step_res$label, "\n", step_res$code))
  }

  list(
    df = current_df,
    code = paste(code_snippets, collapse = "\n\n"),
    history = history_entries,
    n_steps = length(pipeline)
  )
}
