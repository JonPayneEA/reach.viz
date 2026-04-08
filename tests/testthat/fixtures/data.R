# ============================================================
# Tool:         fixtures/data.R
# Description:  Minimal embedded test data factories for reach.viz tests.
#               No external file dependencies.
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# ============================================================

#' Make a minimal hydrometric data.table (Silver format)
#'
#' 30-day hourly series, two sites, all six QC flag types represented.
#' @export
make_hydro_dt <- function() {
  n       <- 720L  # 30 days * 24 hours
  times   <- seq.POSIXt(
    as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    by    = "hour",
    length.out = n
  )
  sites   <- c("SITE_A", "SITE_B")
  set.seed(42L)

  dt <- data.table::CJ(timestamp = times, site_id = sites)
  dt[, qc_flag    := sample(c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 6L), .N, replace = TRUE)]
  dt[, qc_value   := ifelse(qc_flag %in% c(4L, 5L), NA_real_,
                            abs(stats::rnorm(.N, mean = 50, sd = 20)))]
  dt[, bronze_value := qc_value + stats::rnorm(.N, sd = 2)]
  dt[, data_type   := "Q"]
  dt
}

#' Make a minimal observed vs. simulated data.table
#' @export
make_obs_sim_dt <- function() {
  n      <- 200L
  t_seq  <- seq.POSIXt(as.POSIXct("2024-10-01", tz = "UTC"),
                        by = "hour", length.out = n)
  set.seed(7L)
  obs    <- cumsum(abs(stats::rnorm(n, sd = 3))) + 20
  data.table::data.table(
    timestamp = t_seq,
    observed  = obs,
    simulated = obs + stats::rnorm(n, sd = 4),
    site_id   = "EDEN_SHEEPMOUNT",
    period    = rep(c("calibration", "validation"), each = n %/% 2L)
  )
}

#' Make a minimal residual data.table
#' @export
make_residual_dt <- function() {
  dt <- make_obs_sim_dt()
  dt[, residual := simulated - observed]
  dt
}

#' Make a minimal long-format ensemble data.table (50 members, 72 h)
#' @export
make_ensemble_dt <- function() {
  n_members <- 50L
  n_times   <- 72L
  t_seq     <- seq.POSIXt(as.POSIXct("2025-03-15 00:00:00", tz = "UTC"),
                            by = "hour", length.out = n_times)
  set.seed(99L)
  dt <- data.table::CJ(
    timestamp       = t_seq,
    ensemble_member = paste0("M", formatC(seq_len(n_members), width = 2L,
                                          flag = "0"))
  )
  # Simple synthetic hydrograph: rises then falls
  base_flow <- stats::approx(
    x    = c(1L, 20L, 40L, n_times),
    y    = c(30, 120, 80, 25),
    xout = seq_len(n_times)
  )$y
  dt[, value := base_flow[match(timestamp, t_seq)] +
       stats::rnorm(.N, sd = 15)]
  dt[value < 0, value := 0]
  dt
}

#' Make a minimal performance metrics data.table (long format)
#' @export
make_performance_dt <- function() {
  sites   <- c("EDEN", "KENT", "LUNE")
  metrics <- c("NSE", "KGE", "FAR")
  periods <- c("calibration", "validation")
  set.seed(21L)
  dt <- data.table::CJ(site_id = sites, metric = metrics, period = periods)
  dt[metric == "FAR",  value := stats::runif(.N, 0, 0.4)]
  dt[metric != "FAR",  value := stats::runif(.N, 0.3, 0.95)]
  dt
}

#' Make a minimal condition factor data.table (long format)
#' @export
make_condition_dt <- function() {
  models  <- c("FMP_EDEN_v3", "PDM_KENT_v2", "FMP_LUNE_v4")
  factors <- c("data_currency", "structural_integrity", "calibration_currency",
               "software_currency", "documentation")
  set.seed(55L)
  dt <- data.table::CJ(model_id = models, factor = factors)
  dt[, score := sample(0L:2L, .N, replace = TRUE)]
  dt
}

#' Make a minimal model register data.table (one row per model)
#' @export
make_register_dt <- function() {
  set.seed(13L)
  data.table::data.table(
    model_id        = c("FMP_EDEN_v3", "PDM_KENT_v2", "FMP_LUNE_v4",
                        "BB_RIBBLE_v1", "FMP_MERSEY_v2",
                        "PDM_WYRE_v1", "FMP_CALDER_v3"),
    tier            = c("Tier 3", "Tier 3", "Tier 3",
                        "Tier 2", "Tier 3",
                        "Tier 2", "Tier 3"),
    condition_score = c(9L, 7L, 5L, 8L, 3L, 6L, 1L),
    condition_rating = c("Good", "Satisfactory", "Requires Attention",
                         "Satisfactory", "Poor", "Requires Attention", "Critical"),
    status          = c("Active", "Active", "Active",
                        "Active", "Active",
                        "Suspended", "Active"),
    last_assessed   = as.Date(c("2025-10-01", "2025-09-15", "2025-11-01",
                                 "2025-08-20", "2025-12-01",
                                 "2025-07-01", "2025-12-10"))
  )
}

#' Make a minimal connections data.table (edge list)
#' @export
make_connections_dt <- function() {
  data.table::data.table(
    upstream_model   = c("PDM_KENT_v2", "FMP_EDEN_v3",
                         "BB_RIBBLE_v1", "PDM_WYRE_v1",
                         "FMP_LUNE_v4"),
    downstream_model = c("FMP_EDEN_v3", "FMP_MERSEY_v2",
                         "FMP_MERSEY_v2", "FMP_CALDER_v3",
                         "FMP_MERSEY_v2"),
    connection_type  = c("Upstream inflow", "Downstream boundary",
                         "Intermediate routing", "Upstream inflow",
                         "Ensemble feed"),
    status           = c("Active", "Active", "Active", "Suspended", "Active")
  )
}
