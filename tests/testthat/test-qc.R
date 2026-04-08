source(testthat::test_path("fixtures", "data.R"))

# plot_qc_heatmap ---------------------------------------------------------------

test_that("plot_qc_heatmap() returns a ggplot object", {
  dt <- make_hydro_dt()
  p  <- plot_qc_heatmap(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_qc_heatmap() accepts all three period options", {
  dt <- make_hydro_dt()
  expect_s3_class(plot_qc_heatmap(dt, period = "day"),   "ggplot")
  expect_s3_class(plot_qc_heatmap(dt, period = "week"),  "ggplot")
  expect_s3_class(plot_qc_heatmap(dt, period = "month"), "ggplot")
})

test_that("plot_qc_heatmap() rejects invalid period", {
  dt <- make_hydro_dt()
  expect_error(plot_qc_heatmap(dt, period = "year"), regexp = "period")
})

test_that("plot_qc_heatmap() stops on missing columns", {
  dt <- make_hydro_dt()
  dt[, site_id := NULL]
  expect_error(plot_qc_heatmap(dt), regexp = "site_id")
})

# plot_data_availability --------------------------------------------------------

test_that("plot_data_availability() returns a ggplot object", {
  dt <- make_hydro_dt()
  p  <- plot_data_availability(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_data_availability() uses a fill scale", {
  dt <- make_hydro_dt()
  p  <- plot_data_availability(dt)
  # Check that a Scale is present on fill
  scale_names <- vapply(p$scales$scales, function(s) s$aesthetics[1L], character(1L))
  expect_true("fill" %in% scale_names)
})
