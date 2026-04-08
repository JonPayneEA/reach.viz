source(testthat::test_path("fixtures", "data.R"))

# plot_ensemble_ribbon ----------------------------------------------------------

test_that("plot_ensemble_ribbon() returns a ggplot object", {
  ens <- make_ensemble_dt()
  p   <- plot_ensemble_ribbon(ens)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ensemble_ribbon() rejects wrong probs length", {
  ens <- make_ensemble_dt()
  expect_error(
    plot_ensemble_ribbon(ens, probs = c(0.1, 0.5, 0.9)),
    regexp = "length-5"
  )
})

test_that("plot_ensemble_ribbon() rejects probs without 0.5 in middle", {
  ens <- make_ensemble_dt()
  expect_error(
    plot_ensemble_ribbon(ens, probs = c(0.1, 0.25, 0.6, 0.75, 0.9)),
    regexp = "0.5"
  )
})

test_that("plot_ensemble_ribbon() accepts obs_dt overlay", {
  ens    <- make_ensemble_dt()
  obs_dt <- data.table::data.table(
    timestamp = unique(ens$timestamp),
    value     = stats::runif(length(unique(ens$timestamp)), 20, 60)
  )
  p <- plot_ensemble_ribbon(ens, obs_dt = obs_dt)
  expect_s3_class(p, "ggplot")
})

# plot_ensemble_spaghetti -------------------------------------------------------

test_that("plot_ensemble_spaghetti() returns a ggplot object", {
  ens <- make_ensemble_dt()
  p   <- plot_ensemble_spaghetti(ens)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ensemble_spaghetti() median highlight adds a layer", {
  ens  <- make_ensemble_dt()
  p_on  <- plot_ensemble_spaghetti(ens, highlight_median = TRUE)
  p_off <- plot_ensemble_spaghetti(ens, highlight_median = FALSE)
  expect_gt(length(p_on$layers), length(p_off$layers))
})

# plot_exceedance_probability ---------------------------------------------------

test_that("plot_exceedance_probability() returns a ggplot object", {
  ens <- make_ensemble_dt()
  p   <- plot_exceedance_probability(ens)
  expect_s3_class(p, "ggplot")
})

test_that("plot_exceedance_probability() accepts threshold annotations", {
  ens <- make_ensemble_dt()
  p   <- plot_exceedance_probability(ens, thresholds = c(Alert = 50, Warning = 100))
  expect_s3_class(p, "ggplot")
  # Two vline layers should be added
  layer_geoms <- vapply(p$layers, function(l) class(l$geom)[1L], character(1L))
  expect_true(any(layer_geoms == "GeomVline"))
})
