source(testthat::test_path("fixtures", "data.R"))

test_that("theme_flood() returns a ggplot2 theme object", {
  th <- theme_flood()
  expect_s3_class(th, "theme")
})

test_that("theme_flood() accepts grid parameter variants", {
  expect_s3_class(theme_flood(grid = "xy"),   "theme")
  expect_s3_class(theme_flood(grid = "x"),    "theme")
  expect_s3_class(theme_flood(grid = "y"),    "theme")
  expect_s3_class(theme_flood(grid = "none"), "theme")
})

test_that("flode_theme() returns a FlodeTheme S7 object", {
  ft <- flode_theme()
  expect_s3_class(ft, "FlodeTheme")
  expect_equal(ft@palette_name, "flood")
  expect_equal(ft@base_size, 11)
})

test_that("flode_theme() rejects invalid palette", {
  expect_error(flode_theme(palette = "rainbow"), regexp = "palette_name")
})

test_that("print.FlodeTheme() outputs without error", {
  expect_output(print(flode_theme()), regexp = "<FlodeTheme>")
})

test_that("theme_flood() integrates with ggplot without error", {
  library(ggplot2)
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point() +
    theme_flood()
  expect_s3_class(p, "ggplot")
})
