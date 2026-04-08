# ============================================================
# Tool:         plot-hydrograph.R
# Description:  Time series plots for hydrometric data with QC flag encoding,
#               observed vs. simulated overlay, and flow duration curves
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       data.table in Silver/Gold long format (see schema in each fn)
# Outputs:      ggplot2 objects
# Dependencies: ggplot2, data.table
# ============================================================

#' Plot a hydrometric time series with QC flag colour encoding
#'
#' Draws a line plot of hydrometric data where line colour encodes the QC flag
#' value. Flags 4 and 5 (Rejected, No data) are drawn as gaps. An optional
#' background layer shows the raw Bronze value for comparison.
#'
#' @param dt A data.table in Silver or Gold format containing at minimum the
#'   columns `timestamp` (POSIXct), `qc_value` (numeric), and `qc_flag`
#'   (integer, 1–6).
#' @param x        Name of the timestamp column. Default `"timestamp"`.
#' @param y        Name of the analysis-ready value column. Default `"qc_value"`.
#' @param flag_col Name of the QC flag column. Default `"qc_flag"`.
#' @param site_col Optional column name for site identifier. When supplied the
#'   plot is faceted by site using `ggplot2::facet_wrap()`.
#' @param show_raw Logical. When `TRUE` and `bronze_value` column is present,
#'   a faint grey background line shows the raw Bronze value. Default `FALSE`.
#' @param title    Plot title string. Default `NULL`.
#' @param y_label  Y-axis label. When `NULL` (default) the label is derived
#'   from the `data_type` column if present, otherwise `y` is used.
#'
#' @return A `ggplot` object. Add further layers with `+`.
#'
#' @examples
#' library(data.table)
#' dt <- make_hydro_dt()
#' plot_hydrograph(dt)
#' plot_hydrograph(dt, site_col = "site_id", title = "Hourly flow by site")
#'
#' @export
plot_hydrograph <- function(dt,
                            x        = "timestamp",
                            y        = "qc_value",
                            flag_col = "qc_flag",
                            site_col = NULL,
                            show_raw = FALSE,
                            title    = NULL,
                            y_label  = NULL) {
  .check_dt(dt, c(x, y, flag_col))

  dt_plot <- data.table::copy(dt)
  # Treat flags 4 (Rejected) and 5 (No data) as gaps
  dt_plot[get(flag_col) %in% c(4L, 5L), (y) := NA_real_]
  dt_plot[, flag_label := factor(.qc_label(get(flag_col)),
                                  levels = c("Good", "Estimated", "Suspect",
                                             "Rejected", "No data", "Below detection"))]

  y_lab <- if (!is.null(y_label)) {
    y_label
  } else if ("data_type" %in% names(dt_plot)) {
    .data_type_label(dt_plot[["data_type"]][1L])
  } else {
    y
  }

  p <- ggplot2::ggplot(dt_plot, ggplot2::aes(
    x      = .data[[x]],
    y      = .data[[y]],
    colour = flag_label,
    group  = if (!is.null(site_col)) .data[[site_col]] else 1L
  ))

  if (show_raw && "bronze_value" %in% names(dt_plot)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = bronze_value),
      colour    = "#D8D8D8",
      linewidth = 0.4,
      na.rm     = TRUE
    )
  }

  p <- p +
    ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
    scale_colour_qc() +
    ggplot2::labs(
      x     = NULL,
      y     = y_lab,
      title = title
    ) +
    theme_flood()

  if (!is.null(site_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[site_col]]),
                                  ncol = 1L, scales = "free_y")
  }

  p
}

#' Plot observed vs. simulated time series
#'
#' Draws a paired line chart with observed values in dark navy and simulated
#' values in teal. Optionally faceted by site.
#'
#' @param dt A data.table with columns for timestamp, observed value, and
#'   simulated value. Wide format: one row per timestamp (per site).
#' @param x       Name of the timestamp column. Default `"timestamp"`.
#' @param obs     Name of the observed value column. Default `"observed"`.
#' @param sim     Name of the simulated value column. Default `"simulated"`.
#' @param site_col Optional column name for faceting by site.
#' @param title   Plot title. Default `NULL`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' dt <- make_obs_sim_dt()
#' plot_obs_sim_ts(dt, title = "Hindcast: Eden at Sheepmount")
#'
#' @export
plot_obs_sim_ts <- function(dt,
                            x        = "timestamp",
                            obs      = "observed",
                            sim      = "simulated",
                            site_col = NULL,
                            title    = NULL) {
  .check_dt(dt, c(x, obs, sim))

  # Melt to long for a clean shared aesthetic
  id_cols  <- c(x, site_col)
  dt_long  <- data.table::melt(
    dt,
    id.vars       = id_cols,
    measure.vars  = c(obs, sim),
    variable.name = "series",
    value.name    = "value"
  )
  dt_long[, series := factor(series,
                              levels = c(obs, sim),
                              labels = c("Observed", "Simulated"))]

  line_colours <- c("Observed" = "#1B3A6B", "Simulated" = "#009FAF")
  line_sizes   <- c("Observed" = 0.5,       "Simulated" = 0.7)

  p <- ggplot2::ggplot(
    dt_long,
    ggplot2::aes(x = .data[[x]], y = value,
                 colour = series, linewidth = series)
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = line_colours, name = NULL) +
    ggplot2::scale_linewidth_manual(values = line_sizes, name = NULL) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    theme_flood()

  if (!is.null(site_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[site_col]]),
                                  ncol = 1L, scales = "free_y")
  }

  p
}

#' Plot a flow duration curve
#'
#' Computes exceedance probabilities from the supplied time series and draws a
#' flow duration curve. QC flags 4 and 5 are excluded before ranking. An
#' optional grouping variable (e.g. site, season) produces overlaid curves.
#'
#' @param dt        A data.table with at minimum `value_col` and `flag_col`.
#' @param value_col Name of the flow/value column. Default `"qc_value"`.
#' @param flag_col  Name of the QC flag column (flags 4–5 excluded).
#'   Default `"qc_flag"`.
#' @param group_col Optional grouping column name (e.g. `"site_id"`).
#' @param log_y     Log-transform the Y axis. Default `TRUE`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' dt <- make_hydro_dt()
#' plot_flow_duration(dt)
#' plot_flow_duration(dt, group_col = "site_id", log_y = TRUE)
#'
#' @export
plot_flow_duration <- function(dt,
                               value_col = "qc_value",
                               flag_col  = "qc_flag",
                               group_col = NULL,
                               log_y     = TRUE) {
  .check_dt(dt, c(value_col, flag_col))

  dt_clean <- dt[!get(flag_col) %in% c(4L, 5L) & !is.na(get(value_col))]

  by_cols <- group_col

  dt_fdc <- dt_clean[, {
    vals   <- sort(get(value_col), decreasing = TRUE)
    n      <- length(vals)
    list(
      value            = vals,
      exceedance_prob  = seq_len(n) / (n + 1L)
    )
  }, by = by_cols]

  p <- ggplot2::ggplot(dt_fdc, ggplot2::aes(
    x      = exceedance_prob,
    y      = value,
    colour = if (!is.null(group_col)) .data[[group_col]] else NULL,
    group  = if (!is.null(group_col)) .data[[group_col]] else 1L
  )) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      name   = "Exceedance probability"
    ) +
    ggplot2::labs(y = "Flow (m\u00b3/s)", colour = group_col) +
    theme_flood()

  if (log_y) {
    p <- p + ggplot2::scale_y_log10(
      labels = scales::label_comma()
    )
  }

  if (!is.null(group_col)) {
    p <- p + scale_colour_flood(name = group_col)
  }

  p
}
