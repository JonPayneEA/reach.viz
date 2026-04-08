source(testthat::test_path("fixtures", "data.R"))

# plot_obs_sim_scatter ----------------------------------------------------------

test_that("plot_obs_sim_scatter() returns a ggplot object", {
  dt <- make_obs_sim_dt()
  p  <- plot_obs_sim_scatter(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_obs_sim_scatter() stops when obs column missing", {
  dt <- make_obs_sim_dt()
  dt[, observed := NULL]
  expect_error(plot_obs_sim_scatter(dt), regexp = "observed")
})

test_that("plot_obs_sim_scatter() accepts colour_by argument", {
  dt <- make_obs_sim_dt()
  p  <- plot_obs_sim_scatter(dt, colour_by = "period")
  scale_aes <- vapply(p$scales$scales, function(s) s$aesthetics[1L], character(1L))
  expect_true("colour" %in% scale_aes)
})

test_that("plot_obs_sim_scatter() facets by site_col", {
  dt <- make_obs_sim_dt()
  p  <- plot_obs_sim_scatter(dt, site_col = "site_id")
  expect_s3_class(p$facet, "FacetWrap")
})

# plot_residuals ----------------------------------------------------------------

test_that("plot_residuals() returns a ggplot object", {
  dt <- make_residual_dt()
  p  <- plot_residuals(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_residuals() stops when residual column missing", {
  dt <- make_residual_dt()
  dt[, residual := NULL]
  expect_error(plot_residuals(dt), regexp = "residual")
})

# plot_performance_metrics ------------------------------------------------------

test_that("plot_performance_metrics() returns a ggplot object", {
  dt <- make_performance_dt()
  p  <- plot_performance_metrics(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_performance_metrics() adds vlines for thresholds", {
  dt <- make_performance_dt()
  p  <- plot_performance_metrics(dt, thresholds = c(NSE = 0.6, KGE = 0.6))
  layer_geoms <- vapply(p$layers, function(l) class(l$geom)[1L], character(1L))
  expect_true(any(layer_geoms == "GeomVline"))
})

test_that("plot_performance_metrics() facets by metric", {
  dt <- make_performance_dt()
  p  <- plot_performance_metrics(dt)
  expect_s3_class(p$facet, "FacetWrap")
})
