source(testthat::test_path("fixtures", "data.R"))

# plot_condition_factors --------------------------------------------------------

test_that("plot_condition_factors() returns a ggplot object", {
  dt <- make_condition_dt()
  p  <- plot_condition_factors(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_condition_factors() uses coord_polar", {
  dt <- make_condition_dt()
  p  <- plot_condition_factors(dt)
  expect_s3_class(p$coordinates, "CoordPolar")
})

test_that("plot_condition_factors() works with a single model", {
  dt <- make_condition_dt()[model_id == "FMP_EDEN_v3"]
  p  <- plot_condition_factors(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_condition_factors() stops when required columns missing", {
  dt <- make_condition_dt()
  dt[, score := NULL]
  expect_error(plot_condition_factors(dt), regexp = "score")
})

# plot_condition_register -------------------------------------------------------

test_that("plot_condition_register() returns a ggplot object", {
  dt <- make_register_dt()
  p  <- plot_condition_register(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_condition_register() uses shape aesthetic for tier", {
  dt         <- make_register_dt()
  p          <- plot_condition_register(dt)
  scale_aes  <- vapply(p$scales$scales, function(s) s$aesthetics[1L], character(1L))
  expect_true("shape" %in% scale_aes)
})

test_that("plot_condition_register() handles all-suspended register", {
  dt <- make_register_dt()
  dt[, status := "Suspended"]
  expect_s3_class(plot_condition_register(dt), "ggplot")
})

# Internal helpers --------------------------------------------------------------

test_that(".score_to_rating() maps score boundaries correctly", {
  scores  <- c(0L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)
  ratings <- as.character(reach.viz:::.score_to_rating(scores))
  expect_equal(ratings[1:2], c("Critical", "Critical"))
  expect_equal(ratings[3:4], c("Poor",     "Poor"))
  expect_equal(ratings[7:8], c("Satisfactory", "Satisfactory"))
  expect_equal(ratings[9:10], c("Good", "Good"))
})

test_that(".qc_label() returns correct labels for all 6 flags", {
  labels <- reach.viz:::.qc_label(1:6)
  expect_equal(labels[1L], "Good")
  expect_equal(labels[5L], "No data")
  expect_equal(labels[6L], "Below detection")
})

test_that(".data_type_label() returns units for known codes", {
  expect_match(reach.viz:::.data_type_label("Q"), "m")
  expect_match(reach.viz:::.data_type_label("H"), "Stage")
  expect_match(reach.viz:::.data_type_label("P"), "Rainfall")
})
