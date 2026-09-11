test_that("ramses_prepare_report_data_file handles various filenames correctly", {
  # Créer un environnement temp
  temp_dir <- tempfile("test_report_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Créer un faux fichier source
  fake_source <- tempfile("fake_source_", fileext = ".tmp")
  writeLines("test", fake_source)
  on.exit(unlink(fake_source), add = TRUE)
  
  # Test XLSX
  rel1 <- ramses_prepare_report_data_file(fake_source, "donnees tests.xlsx", temp_dir)
  expect_equal(rel1, "data/donnees_tests.xlsx")
  expect_true(file.exists(file.path(temp_dir, "data", "donnees_tests.xlsx")))
  
  # Test accents CSV
  rel2 <- ramses_prepare_report_data_file(fake_source, "données_agricoles.csv", temp_dir)
  expect_true(grepl("^data/donn_es_agricoles\\.csv$", rel2) || grepl("^data/données_agricoles\\.csv$", rel2) || grepl("data/.*\\.csv", rel2))
  
  # Test caractères spéciaux et espaces multiples
  rel3 <- ramses_prepare_report_data_file(fake_source, "enquête producteurs 2026.xlsx", temp_dir)
  expect_true(grepl("\\.xlsx$", rel3))
  expect_true(grepl("^data/", rel3))
  
  # Test parenthèses
  rel4 <- ramses_prepare_report_data_file(fake_source, "rapport (final).xlsx", temp_dir)
  expect_equal(rel4, "data/rapport__final_.xlsx")
  
  # Test guillemets
  rel5 <- ramses_prepare_report_data_file(fake_source, "données \"finales\".csv", temp_dir)
  expect_true(grepl("\\.csv$", rel5))
})

test_that("ramses_prepare_report_data_file prevents silent overwrite", {
  temp_dir <- tempfile("test_report_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  fake_source <- tempfile("fake_source_", fileext = ".tmp")
  writeLines("test", fake_source)
  on.exit(unlink(fake_source), add = TRUE)
  
  rel1 <- ramses_prepare_report_data_file(fake_source, "dataset.csv", temp_dir)
  expect_equal(rel1, "data/dataset.csv")
  
  # Deuxième appel avec le même nom, devrait générer un nom différent
  rel2 <- ramses_prepare_report_data_file(fake_source, "dataset.csv", temp_dir)
  expect_equal(rel2, "data/dataset_1.csv")
})

test_that("ramses_prepare_report_data_file handles all formats correctly", {
  temp_dir <- tempfile("test_report_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  fake_source <- tempfile("fake_source_", fileext = ".tmp")
  writeLines("test", fake_source)
  on.exit(unlink(fake_source), add = TRUE)
  
  exts <- c("xlsx", "csv", "txt", "tsv", "rds", "sav", "dta", "xls")
  for (ext in exts) {
    fname <- paste0("file.", ext)
    rel <- ramses_prepare_report_data_file(fake_source, fname, temp_dir)
    expect_equal(rel, paste0("data/file.", ext))
    expect_true(file.exists(file.path(temp_dir, "data", paste0("file.", ext))))
  }
})

test_that("Error thrown if source doesn't exist", {
  expect_error(
    ramses_prepare_report_data_file("doesnt_exist.csv", "original.csv", tempdir()),
    "n'est plus accessible ou n'existe pas"
  )
})
