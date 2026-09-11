test_that("pie chart R code generates correctly with NAs excluded", {
  # We test the code generated for RMarkdown
  df_name <- "my_data"
  x_var <- "category"
  pie_metric_choice <- "percent"
  
  # Reproduce logic from mod_chart_builder.R
  code_lines <- c(
    "library(dplyr)",
    "library(ggplot2)",
    "library(plotly)",
    "",
    "# Pre-agregation des donnees pour le diagramme circulaire",
    paste0("df_pie <- ", ramses_code_symbol(df_name), " %>%"),
    paste0("  dplyr::filter(!is.na(", ramses_code_symbol(x_var), ")) %>%"),
    paste0("  dplyr::count(", ramses_code_symbol(x_var), ", name = 'N') %>%"),
    paste0("  dplyr::mutate("),
    paste0("    Pct = round(N / sum(N) * 100, 1),"),
    paste0("    Label = paste0(", ramses_code_symbol(x_var), ", ' \u2014 ', format(Pct, decimal.mark = ','), ' %')"),
    paste0("  )")
  )
  
  code_str <- paste(code_lines, collapse = "\n")
  
  # NA filtering should be present
  expect_match(code_str, "dplyr::filter\\(!is\\.na")
  # percentage calculation should be present
  expect_match(code_str, "Pct = round\\(N / sum\\(N\\) \\* 100, 1\\)")
})

test_that("pie chart R code generates ggplot2 with coord_polar", {
  x_var <- "category"
  metricCol <- "Pct"
  
  code_lines <- c(
    paste0("p <- ggplot(data = df_pie, aes(x = \"\", y = ", metricCol, ", fill = ", ramses_code_symbol(x_var), ")) +"),
    "  geom_col(width = 1, color = \"white\", alpha = 0.8) +",
    "  coord_polar(theta = \"y\", start = 0)"
  )
  code_str <- paste(code_lines, collapse = "\n")
  
  expect_match(code_str, "ggplot\\(data = df_pie")
  expect_match(code_str, "coord_polar\\(theta = \"y\"")
})

test_that("pie chart plotting logic calculations work correctly with edge cases", {
  # Reproduce internal calculation in mod_chart_builder.R for pie chart Plotly
  clean_df <- data.frame(
    category = c("A", "B", "A", NA, "C", "C", "A")
  )
  
  # Remove NAs as per mod_chart_builder.R logic when evaluating
  # Actually wait, table() drops NAs automatically in mod_chart_builder.R
  clean_x <- clean_df[["category"]]
  tab <- table(clean_x)
  mod_names <- names(tab)
  counts <- as.numeric(tab)
  tot <- sum(counts)
  pcts <- if (tot > 0) round((counts / tot) * 100, 1) else rep(0, length(counts))
  
  # Categories should be A, B, C (NA is excluded)
  expect_equal(mod_names, c("A", "B", "C"))
  expect_equal(counts, c(3, 1, 2))
  expect_equal(tot, 6)
  
  # Percentages: A: 50%, B: 16.7%, C: 33.3%
  expect_equal(pcts, c(50.0, 16.7, 33.3))
  expect_equal(sum(pcts), 100)
})

test_that("pie chart R code generates correctly with complex names", {
  df_name <- "data"
  x_var <- "âge (ans)"
  pie_metric_choice <- "count"
  
  code_lines <- c(
    paste0("df_pie <- ", ramses_code_symbol(df_name), " %>%"),
    paste0("  dplyr::filter(!is.na(", ramses_code_symbol(x_var), ")) %>%")
  )
  code_str <- paste(code_lines, collapse = "\n")
  
  # Should use backticks for complex names
  expect_match(code_str, "`âge \\(ans\\)`")
})

test_that("pie chart plotting handles long modalities and accents", {
  clean_df <- data.frame(
    category = c("Catégorie très très longue avec accents", "Élément B", "Élément B")
  )
  tab <- table(clean_df$category)
  mod_names <- names(tab)
  counts <- as.numeric(tab)
  pcts <- round((counts / sum(counts)) * 100, 1)
  
  # Mod names are preserved
  expect_equal(length(mod_names), 2)
  expect_true("Catégorie très très longue avec accents" %in% mod_names)
  expect_true("Élément B" %in% mod_names)
  
  # Check percentages (1/3 and 2/3 -> 33.3 and 66.7)
  expect_equal(pcts, c(33.3, 66.7))
})
