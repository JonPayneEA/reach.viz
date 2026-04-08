# ============================================================
# Tool:         plot-performance.R
# Description:  Model calibration/validation performance plots: observed vs.
#               simulated scatter, residual time series, metric bar chart
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       data.table with observed/simulated pairs or long metric data
# Outputs:      ggplot2 objects
# Dependencies: ggplot2, data.table, scales
# ============================================================

#' Observed vs. simulated scatter plot
#'
#' Draws a scatter plot of observed against simulated values with a 1:1
#' reference line. Points can be coloured by an additional variable (e.g.,
#' calibration vs. validation period) and faceted by site.
#'
#' @param dt         A data.table with observed and simulated columns.
#' @param obs        Name of the observed value column. Default `"observed"`.
#' @param sim        Name of the simulated value column. Default `"simulated"`.
#' @param site_col   Optional column for faceting by site.
#' @param colour_by  Optional column name for point colour (e.g. `"period"`).
#' @param log_axes   Log-transform both axes. Default `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' dt <- make_obs_sim_dt()
#' plot_obs_sim_scatter(dt, colour_by = "period")
#'
#' @export
plot_obs_sim_scatter <- function(dt,
                                 obs       = "observed",
                                 sim       = "simulated",
                                 site_col  = NULL,
                                 colour_by = NULL,
                                 log_axes  = FALSE) {
  .check_dt(dt, c(obs, sim))

  aes_args <- ggplot2::aes(x = .data[[obs]], y = .data[[sim]])
  if (!is.null(colour_by)) {
    aes_args <- ggplot2::aes(x = .data[[obs]], y = .data[[sim]],
                              colour = .data[[colour_by]])
  }

  # Shared axis limits (square plot)
  range_all <- range(c(dt[[obs]], dt[[sim]]), na.rm = TRUE)

  p <- ggplot2::ggplot(dt, aes_args) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          colour = "#383838", linewidth = 0.5, linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.55, size = 1.8) +
    ggplot2::coord_equal(xlim = range_all, ylim = range_all) +
    ggplot2::labs(
      x      = paste0("Observed (", obs, ")"),
      y      = paste0("Simulated (", sim, ")"),
      colour = colour_by
    ) +
    theme_flood()

  if (!is.null(colour_by)) {
    p <- p + scale_colour_flood(name = colour_by)
  }

  if (log_axes) {
    p <- p +
      ggplot2::scale_x_log10(labels = scales::label_comma()) +
      ggplot2::scale_y_log10(labels = scales::label_comma())
  }

  if (!is.null(site_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[site_col]]))
  }

  p
}

#' Plot residual time series
#'
#' Draws residuals (simulated minus observed) against time with a zero
#' reference line. Positive residuals indicate over-prediction; negative
#' indicate under-prediction. Optionally faceted by site.
#'
#' @param dt           A data.table with a timestamp column and a residual
#'   column.
#' @param x            Name of the timestamp column. Default `"timestamp"`.
#' @param residual_col Name of the residual column (simulated - observed).
#'   Default `"residual"`.
#' @param site_col     Optional column for faceting by site.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' dt <- make_residual_dt()
#' plot_residuals(dt)
#'
#' @export
plot_residuals <- function(dt,
                           x            = "timestamp",
                           residual_col = "residual",
                           site_col     = NULL) {
  .check_dt(dt, c(x, residual_col))

  p <- ggplot2::ggplot(dt, ggplot2::aes(
    x    = .data[[x]],
    y    = .data[[residual_col]],
    fill = .data[[residual_col]] >= 0
  )) +
    ggplot2::geom_col(width = 1, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, colour = "#383838", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#009FAF", "FALSE" = "#D4351C")
    ) +
    ggplot2::labs(
      x = NULL,
      y = paste0("Residual (", residual_col, ")\nsimulated \u2212 observed")
    ) +
    theme_flood()

  if (!is.null(site_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[site_col]]),
                                  ncol = 1L, scales = "free_y")
  }

  p
}

#' Plot model performance metrics as a lollipop chart
#'
#' Draws a lollipop (point-and-segment) chart of one or more performance metrics
#' across sites. Optional threshold lines mark the pass/fail boundary for each
#' metric. Faceted by metric.
#'
#' @param dt           A data.table in long format with columns for metric name,
#'   value, and site identifier.
#' @param metric_col   Name of the metric column. Default `"metric"`.
#' @param value_col    Name of the numeric value column. Default `"value"`.
#' @param site_col     Name of the site identifier column. Default `"site_id"`.
#' @param thresholds   Optional named numeric vector of pass/fail thresholds,
#'   one per metric name (e.g. `c(NSE = 0.6, KGE = 0.6)`).
#'
#' @return A `ggplot` object (faceted by metric).
#'
#' @examples
#' library(data.table)
#' dt <- make_performance_dt()
#' plot_performance_metrics(dt, thresholds = c(NSE = 0.6, KGE = 0.6))
#'
#' @export
plot_performance_metrics <- function(dt,
                                     metric_col = "metric",
                                     value_col  = "value",
                                     site_col   = "site_id",
                                     thresholds = NULL) {
  .check_dt(dt, c(metric_col, value_col, site_col))

  dt_plot <- data.table::copy(dt)
  dt_plot[, site_ordered := stats::reorder(get(site_col), get(value_col))]

  p <- ggplot2::ggplot(dt_plot, ggplot2::aes(
    x     = .data[[value_col]],
    y     = site_ordered,
    colour = .data[[value_col]]
  )) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data[[value_col]],
                   y = site_ordered, yend = site_ordered),
      colour = "#D8D8D8", linewidth = 0.5
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_gradient2(
      low      = "#D4351C",
      mid      = "#F9A11B",
      high     = "#2D7D46",
      midpoint = 0.5,
      guide    = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_wrap(ggplot2::vars(.data[[metric_col]]),
                         scales = "free_x") +
    theme_flood()

  if (!is.null(thresholds)) {
    thr_dt <- data.table::data.table(
      metric = names(thresholds),
      thresh = unname(thresholds)
    )
    data.table::setnames(thr_dt, "metric", metric_col)
    p <- p + ggplot2::geom_vline(
      data        = thr_dt,
      ggplot2::aes(xintercept = thresh),
      colour      = "#383838",
      linetype    = "dashed",
      linewidth   = 0.5,
      inherit.aes = FALSE
    )
  }

  p
}
