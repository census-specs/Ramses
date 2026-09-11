test_that("ramses_code_string correctly escapes simple and complex literal strings", {
  # Cas simples
  expect_equal(eval(parse(text = ramses_code_string("A"))), "A")
  expect_equal(eval(parse(text = ramses_code_string("Groupe 1"))), "Groupe 1")

  # Apostrophe
  val_apos <- "L'entreprise A"
  code_apos <- ramses_code_string(val_apos)
  expect_true(is.character(code_apos))
  expect_equal(eval(parse(text = code_apos)), val_apos)

  # Guillemets doubles
  val_quote <- 'Traitement "A"'
  code_quote <- ramses_code_string(val_quote)
  expect_equal(eval(parse(text = code_quote)), val_quote)

  # Slash
  val_slash <- "A/B"
  code_slash <- ramses_code_string(val_slash)
  expect_equal(eval(parse(text = code_slash)), val_slash)

  # Parentheses
  val_paren <- "Zone (Nord)"
  code_paren <- ramses_code_string(val_paren)
  expect_equal(eval(parse(text = code_paren)), val_paren)

  # Operateurs
  val_op <- "Prix > 100"
  code_op <- ramses_code_string(val_op)
  expect_equal(eval(parse(text = code_op)), val_op)

  # Antislash
  val_bs <- "valeur\\avec\\backslash"
  code_bs <- ramses_code_string(val_bs)
  expect_equal(eval(parse(text = code_bs)), val_bs)

  # Retour a la ligne
  val_nl <- "valeur\navec\nretour"
  code_nl <- ramses_code_string(val_nl)
  expect_equal(eval(parse(text = code_nl)), val_nl)

  # Cas combine complexe
  val_comb <- "L'entreprise \"Nord-Est\" / Secteur #1 (prix > 100€) \\ test\nsuite"
  code_comb <- ramses_code_string(val_comb)
  expect_equal(eval(parse(text = code_comb)), val_comb)
})

test_that("ramses_code_string handles character vectors safely", {
  vec <- c(
    "A",
    "L'entreprise A",
    'Traitement "A"',
    "A/B",
    "Zone (Nord)",
    "Prix > 100",
    "valeur\\avec\\backslash",
    "valeur\navec\nretour"
  )
  vec_code <- ramses_code_string(vec)
  parsed_vec <- eval(parse(text = vec_code))
  expect_identical(parsed_vec, vec)
})

test_that("Filters using subset() and %in% with complex modalities work accurately", {
  df_filter_test <- data.frame(
    id = 1:5,
    groupe = c("A", "L'entreprise A", 'Traitement "A"', "A/B", "Zone (Nord)"),
    valeur = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Test filtering 2 specific modalities
  selected_mods <- c("L'entreprise A", 'Traitement "A"')
  filter_code <- paste0(
    "subset(df_filter_test, ",
    ramses_code_column("df_filter_test", "groupe"),
    " %in% ",
    ramses_code_string(selected_mods),
    ")"
  )

  # Verify generated code is syntactically valid
  expect_silent(parsed_expr <- parse(text = filter_code))

  # Evaluate filter
  res_filtered <- eval(parsed_expr)
  expect_equal(nrow(res_filtered), 2)
  expect_identical(res_filtered$groupe, selected_mods)
  expect_equal(res_filtered$valeur, c(20, 30))
})

test_that("Import snippet code generation produces syntactically valid R code", {
  file_name <- 'rapport annuel "2024" (v1.0) & stats\\final.csv'
  sheet_name <- "Feuille 'Nord' (1)"
  ds_name <- "mes données"

  # CSV snippet
  csv_snippet <- sprintf(
    '%s <- read.csv2(%s, header = TRUE, stringsAsFactors = FALSE)',
    ramses_code_symbol(ds_name),
    ramses_code_string(file_name)
  )
  expect_silent(parse(text = csv_snippet))

  # Excel snippet
  excel_snippet <- sprintf(
    'library(readxl)\n%s <- read_excel(%s, sheet = %s, col_names = TRUE)',
    ramses_code_symbol(ds_name),
    ramses_code_string(file_name),
    ramses_code_string(sheet_name)
  )
  expect_silent(parse(text = excel_snippet))
})
