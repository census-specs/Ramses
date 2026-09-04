test_that("package metadata is available", {
  expect_s3_class(packageVersion("Ramses"), "package_version")
  expect_equal(as.character(packageVersion("Ramses")), "0.1.0")
})

test_that("public launcher returns a Shiny app object", {
  app <- run_app(standalone = FALSE, launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("public module UI constructors return Shiny objects", {
  expect_s3_class(mod_descriptives_ui("test"), "shiny.tag")
  expect_s3_class(mod_chart_builder_ui("test"), "shiny.tag")
  expect_s3_class(mod_tests_ui("test"), "shiny.tag")
})
