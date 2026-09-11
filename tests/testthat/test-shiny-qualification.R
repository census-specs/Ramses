test_that("Shiny req() and isolate() calls are explicitly qualified", {
  r_files <- list.files(
    path = system.file("R", package = "Ramses"),
    pattern = "\\.[Rr]$",
    full.names = TRUE
  )

  # If running in dev / testing mode where system.file might not find package root directly
  if (length(r_files) == 0) {
    r_files <- list.files("../../R", pattern = "\\.[Rr]$", full.names = TRUE)
  }
  if (length(r_files) == 0) {
    r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
  }

  expect_gt(length(r_files), 0)

  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    # Match bare req(...) or isolate(...) not preceded by shiny:: or other namespace
    unqualified_req <- grep("(^|[^a-zA-Z0-9_:])req\\(", lines, value = TRUE)
    unqualified_req <- grep("shiny::req\\(", unqualified_req, value = TRUE, invert = TRUE)

    unqualified_isolate <- grep("(^|[^a-zA-Z0-9_:])isolate\\(", lines, value = TRUE)
    unqualified_isolate <- grep("shiny::isolate\\(", unqualified_isolate, value = TRUE, invert = TRUE)

    expect_equal(
      length(unqualified_req), 0,
      info = paste("Unqualified req() found in", basename(f), ":", paste(unqualified_req, collapse = "; "))
    )
    expect_equal(
      length(unqualified_isolate), 0,
      info = paste("Unqualified isolate() found in", basename(f), ":", paste(unqualified_isolate, collapse = "; "))
    )
  }
})
