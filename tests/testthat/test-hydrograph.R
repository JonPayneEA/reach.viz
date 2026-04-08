source(testthat::test_path("fixtures", "data.R"))

# plot_hydrograph ---------------------------------------------------------------

test_that("plot_hydrograph() returns a ggplot object", {
  dt <- make_hydro_dt()
  p  <- plot_hydrograph(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_hydrograph() facets when site_col is supplied", {
  dt <- make_hydro_dt()
  p  <- plot_hydrograph(dt, site_col = "site_id")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_hydrograph() stops on non-data.table input", {
  expect_error(plot_hydrograph(as.data.frame(make_hydro_dt())),
               regexp = "data.table")
})

test_that("plot_hydrograph() stops when required column is missing", {
  dt <- make_hydro_dt()
  dt[, qc_flag := NULL]
  expect_error(plot_hydrograph(dt), regexp = "qc_flag")
})

test_that("plot_hydrograph() shows raw layer when show_raw = TRUE", {
  dt <- make_hydro_dt()
  p  <- plot_hydrograph(dt, show_raw = TRUE)
  layer_geoms <- vapply(p$layers, function(l) class(l$geom)[1L], character(1L))
  expect_true(sum(layer_geoms == "GeomLine") >= 2L)
})

# plot_obs_sim_ts ---------------------------------------------------------------

test_that("plot_obs_sim_ts() returns a ggplot object", {
  dt <- make_obs_sim_dt()
  p  <- plot_obs_sim_ts(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_obs_sim_ts() stops when observed column is missing", {
  dt <- make_obs_sim_dt()
  dt[, observed := NULL]
  expect_error(plot_obs_sim_ts(dt), regexp = "observed")
})

# plot_flow_duration ------------------------------------------------------------

test_that("plot_flow_duration() returns a ggplot object", {
  dt <- make_hydro_dt()[site_id == "SITE_A"]
  p  <- plot_flow_duration(dt)
  expect_s3_class(p, "ggplot")
})

test_that("plot_flow_duration() excludes flags 4 and 5 silently", {
  dt <- make_hydro_dt()[site_id == "SITE_A"]
  # All flag 4/5 rows — should produce empty plot but not error
  dt_bad <- dt[qc_flag %in% c(4L, 5L)]
  if (nrow(dt_bad) > 0L) {
    dt_all4 <- dt[, qc_flag := 4L]
    expect_s3_class(plot_flow_duration(dt_all4), "ggplot")
  }
})

test_that("plot_flow_duration() accepts group_col", {
  dt <- make_hydro_dt()
  p  <- plot_flow_duration(dt, group_col = "site_id")
  expect_s3_class(p, "ggplot")
})
