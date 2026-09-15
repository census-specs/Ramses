# ==============================================================================
# Tests cibles pour la reproductibilite du code de regression logistique
# Fichier : tests/testthat/test-logistic-regression-reproducibility.R
# ==============================================================================

generate_test_logistic_code <- function(df, ds_name, var_y, vars_x) {
  fml_code <- ramses_formula_code(response = var_y, terms = vars_x)
  has_filtering <- FALSE
  target_mods <- NULL
  if (is.factor(df[[var_y]]) || is.character(df[[var_y]])) {
    levs <- unique(as.character(df[[var_y]]))
    levs <- levs[!is.na(levs)]
    if (length(levs) > 2) {
      has_filtering <- TRUE
      target_mods <- levs[1:2]
    }
  }

  if (has_filtering) {
    mods_code <- ramses_code_string(target_mods)
    paste0(
      "# Sous-ensemble filtre sur les 2 modalites retenues pour la regression logistique\n",
      "data_sub <- ", ramses_code_symbol(ds_name), "[", ramses_code_column(ds_name, var_y), " %in% ", mods_code, ", , drop = FALSE]\n",
      ramses_code_column("data_sub", var_y), " <- as.factor(", ramses_code_column("data_sub", var_y), ")\n",
      "# Regression logistique binaire (glm binomial)\n",
      "mod_glm <- glm(", fml_code, ", data = data_sub, family = binomial(link = 'logit'))\n",
      "summary(mod_glm)"
    )
  } else if (is.character(df[[var_y]])) {
    paste0(
      "# Conversion de la variable cible binaire en facteur\n",
      "data_sub <- ", ramses_code_symbol(ds_name), "\n",
      ramses_code_column("data_sub", var_y), " <- as.factor(", ramses_code_column("data_sub", var_y), ")\n",
      "# Regression logistique binaire (glm binomial)\n",
      "mod_glm <- glm(", fml_code, ", data = data_sub, family = binomial(link = 'logit'))\n",
      "summary(mod_glm)"
    )
  } else {
    paste0(
      "# Regression logistique binaire (glm binomial)\n",
      "mod_glm <- glm(", fml_code, ", data = ", ramses_code_symbol(ds_name), ", family = binomial(link = 'logit'))\n",
      "summary(mod_glm)"
    )
  }
}

test_that("1. Cible binaire avec exactement 2 modalites (pas de filtrage superflu)", {
  df_bin <- data.frame(
    Y = factor(c("Non", "Oui", "Non", "Oui", "Oui")),
    X = c(1.2, 2.4, 1.8, 3.1, 2.9),
    stringsAsFactors = FALSE
  )

  code <- generate_test_logistic_code(df_bin, "df_bin", "Y", "X")

  # Le code doit etre syntaxiquement valide et parsable
  expect_silent(parsed <- parse(text = code))

  # Pas de sous-ensemble filtre necessaire
  expect_false(grepl("%in%", code))
  expect_true(grepl("data = df_bin", code))
  expect_true(grepl("glm\\(Y ~ X", code))
})

test_that("2. Cible avec 3 modalites : filtrage reproductible et syntaxe valide", {
  df_3 <- data.frame(
    Y = factor(c("A", "B", "C", "B", "A", "C")),
    X = c(10, 20, 30, 15, 25, 35),
    stringsAsFactors = FALSE
  )

  code <- generate_test_logistic_code(df_3, "df_3", "Y", "X")

  # Verifie validite syntaxique
  expect_silent(parsed <- parse(text = code))

  # Verifie presence explicite du filtrage
  expect_true(grepl("%in%", code))
  expect_true(grepl("data_sub <- df_3\\[df_3\\[\\[\"Y\"\\]\\] %in% c\\(\"A\", \"B\"\\)", code))
  expect_true(grepl("data_sub\\[\\[\"Y\"\\]\\] <- as.factor\\(data_sub\\[\\[\"Y\"\\]\\]\\)", code))
  expect_true(grepl("data = data_sub", code))

  # Verifie execution et exactitude de la restriction
  env <- new.env()
  env$df_3 <- df_3
  eval(parsed, envir = env)
  expect_equal(nrow(env$data_sub), 4)
  expect_true(all(env$data_sub$Y %in% c("A", "B")))
  expect_false("C" %in% env$data_sub$Y)
  expect_s3_class(env$mod_glm, "glm")
})

test_that("3. Cible avec plus de 3 modalites (>3) : filtrage exact des 2 premieres", {
  df_5 <- data.frame(
    Y = c("Mod1", "Mod2", "Mod3", "Mod4", "Mod5", "Mod1", "Mod2"),
    X = 1:7,
    stringsAsFactors = FALSE
  )

  code <- generate_test_logistic_code(df_5, "dataset", "Y", "X")

  expect_silent(parsed <- parse(text = code))
  expect_true(grepl("c\\(\"Mod1\", \"Mod2\"\\)", code))
  expect_false(grepl("Mod3", code))
  expect_false(grepl("Mod4", code))
  expect_false(grepl("Mod5", code))

  env <- new.env()
  env$dataset <- df_5
  eval(parsed, envir = env)
  expect_equal(nrow(env$data_sub), 4)
  expect_true(all(env$data_sub$Y %in% c("Mod1", "Mod2")))
  expect_s3_class(env$mod_glm, "glm")
})

test_that("4. Modalites contenant des espaces", {
  df_spaces <- data.frame(
    Y = c("Groupe Alpha", "Groupe Beta", "Groupe Gamma", "Groupe Alpha"),
    X = c(1.1, 2.2, 3.3, 2.9),
    stringsAsFactors = FALSE
  )

  code <- generate_test_logistic_code(df_spaces, "df_spaces", "Y", "X")

  expect_silent(parsed <- parse(text = code))
  expect_true(grepl("c\\(\"Groupe Alpha\", \"Groupe Beta\"\\)", code))

  env <- new.env()
  env$df_spaces <- df_spaces
  eval(parsed, envir = env)
  expect_equal(nrow(env$data_sub), 3)
  expect_s3_class(env$mod_glm, "glm")
})

test_that("5. Modalites contenant des accents", {
  df_accents <- data.frame(
    Y = c("Succ\u00e8s", "\u00c9chec", "Ind\u00e9termin\u00e9", "Succ\u00e8s"),
    X = c(12, 18, 25, 24),
    stringsAsFactors = FALSE
  )

  code <- generate_test_logistic_code(df_accents, "df_accents", "Y", "X")

  expect_silent(parsed <- parse(text = code))

  env <- new.env()
  env$df_accents <- df_accents
  eval(parsed, envir = env)
  expect_equal(nrow(env$data_sub), 3)
  expect_s3_class(env$mod_glm, "glm")
})

test_that("6. Modalites contenant des apostrophes ou guillemets", {
  df_quotes <- data.frame(
    Y = c("L'option A", 'Traitement "B"', "Autre", "L'option A"),
    X = c(5, 7, 9, 8),
    stringsAsFactors = FALSE
  )

  code <- generate_test_logistic_code(df_quotes, "df_quotes", "Y", "X")

  expect_silent(parsed <- parse(text = code))

  env <- new.env()
  env$df_quotes <- df_quotes
  eval(parsed, envir = env)
  expect_equal(nrow(env$data_sub), 3)
  expect_s3_class(env$mod_glm, "glm")
})

test_that("7. Noms de variables et dataset complexes avec caracteres speciaux", {
  df_complex <- data.frame(
    id = 1:5,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df_complex[["Statut / R\u00e9sultat (+)"]] <- c("Positif", "N\u00e9gatif", "Neutre", "Positif", "N\u00e9gatif")
  df_complex[["Chiffre d'affaires"]] <- c(100, 200, 150, 300, 250)
  df_complex[['Zone "Nord"']] <- c(1, 2, 1, 3, 2)

  code <- generate_test_logistic_code(
    df_complex,
    "mes donn\u00e9es",
    "Statut / R\u00e9sultat (+)",
    c("Chiffre d'affaires", 'Zone "Nord"')
  )

  # Le code doit etre syntaxiquement correct malgre les backticks, guillemets, slashs, accents
  expect_silent(parsed <- parse(text = code))

  # Verifie que les symboles non-syntaxiques sont proteges par backticks dans la formule
  expect_true(grepl("`Statut / R\u00e9sultat \\(\\+\\)`", code))
  expect_true(grepl("`Chiffre d'affaires`", code))
  expect_true(grepl("`Zone \"Nord\"`", code))

  # Verifie que le nom du dataset est protege
  expect_true(grepl("`mes donn\u00e9es`", code))

  # Verifie que les modalites retenues sont saines
  expect_true(grepl("c\\(\"Positif\", \"N\u00e9gatif\"\\)", code))

  env <- new.env()
  env[["mes donn\u00e9es"]] <- df_complex
  eval(parsed, envir = env)
  expect_equal(nrow(env$data_sub), 4)
  expect_s3_class(env$mod_glm, "glm")
})
