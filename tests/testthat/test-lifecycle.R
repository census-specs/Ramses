test_that("ramses_create_report_state initializes clean state", {
  st <- ramses_create_report_state()
  expect_false(st$dirty)
  expect_null(st$last_saved)
  expect_null(st$saved_format)
})

test_that("ramses_mark_report_dirty and ramses_mark_report_saved update state properly", {
  st <- ramses_create_report_state()
  st <- ramses_mark_report_dirty(st)
  expect_true(st$dirty)
  expect_true(ramses_should_prompt_save(st$dirty))

  now <- Sys.time()
  st <- ramses_mark_report_saved(st, format_name = "Rmd", timestamp = now)
  expect_false(st$dirty)
  expect_false(ramses_should_prompt_save(st$dirty))
  expect_equal(st$saved_format, "Rmd")
  expect_equal(st$last_saved, now)

  # Check with format HTML
  st <- ramses_mark_report_dirty(st)
  expect_true(st$dirty)
  st <- ramses_mark_report_saved(st, format_name = "HTML")
  expect_false(st$dirty)
  expect_equal(st$saved_format, "HTML")

  # Check with format R
  st <- ramses_mark_report_dirty(st)
  expect_true(st$dirty)
  st <- ramses_mark_report_saved(st, format_name = "R")
  expect_false(st$dirty)
  expect_equal(st$saved_format, "R")
})

test_that("ramses_is_initial_report correctly identifies pristine vs modified states", {
  header <- paste(
    "---",
    "title: \"Journal d'analyse Ramses\"",
    "output: html_document",
    "---",
    sep = "\n"
  )

  # Pristine state
  expect_true(ramses_is_initial_report(header, header, "iris"))

  # Modified content
  modified_content <- paste0(header, "\n## Descriptives\nsummary(dataset)\n")
  expect_false(ramses_is_initial_report(modified_content, header, "iris"))

  # Changed dataset name even if content matches template
  expect_false(ramses_is_initial_report(header, header, "mtcars"))
  expect_false(ramses_is_initial_report(header, header, "custom_data.csv"))
})

test_that("app_ui contains session menu and lifecycle components", {
  ui_func <- app_ui()
  ui_str <- as.character(ui_func)

  # Check session menu presence
  expect_true(grepl("menu_session_new", ui_str))
  expect_true(grepl("menu_session_restart", ui_str))
  expect_true(grepl("menu_session_stop", ui_str))

  # Check custom message handler script for graceful stop and new instance
  expect_true(grepl("ramses_stop_app", ui_str))
  expect_true(grepl("ramses_new_instance", ui_str))
})
