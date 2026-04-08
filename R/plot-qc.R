# ============================================================
# Tool:         plot-qc.R
# Description:  QC flag overview heatmap and data availability chart
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       data.table in Silver long format with qc_flag column
# Outputs:      ggplot2 objects
# Dependencies: ggplot2, data.table, scales
# ============================================================

#' Plot a QC flag distribution heatmap
#'
#' Aggregates QC flags by time period and site, then renders a tile heatmap
#' showing flag proportions. Useful for a quick visual audit of a dataset's
#' quality across time and space.
#'
#' @param dt       A data.table with columns `timestamp`, `qc_flag`, and
#'   `site_id`.
#' @param time_col Name of the timestamp column. Default `"timestamp"`.
#' @param flag_col Name of the QC flag column. Default `"qc_flag"`.
#' @param site_col Name of the site identifier column. Default `"site_id"`.
#' @param period   Aggregation period: `"day"`, `"week"`, or `"month"`.
#'   Default `"month"`.
#' @param flag_shown Integer vector of flag values to display. Defaults to
#'   flags 1–6.
#'
#' @return A `ggplot` object (faceted by flag value, one tile per period × site).
#'
#' @details
#' Each tile shows the proportion of observations in that period × site that
#' carry the given flag. Tiles are filled from white (0%) to the flag's
#' canonical colour (100%).
#'
#' @examples
#' library(data.table)
#' dt <- make_hydro_dt()
#' plot_qc_heatmap(dt)
#' plot_qc_heatmap(dt, period = "week")
#'
#' @export
plot_qc_heatmap <- function(dt,
                            time_col   = "timestamp",
                            flag_col   = "qc_flag",
                            site_col   = "site_id",
                            period     = "month",
                            flag_shown = 1:6) {
  .check_dt(dt, c(time_col, flag_col, site_col))
  period <- match.arg(period, c("day", "week", "month"))

  trunc_fn <- switch(period,
    "day"   = function(x) as.Date(x),
    "week"  = function(x) as.Date(cut(x, "week")),
    "month" = function(x) as.Date(cut(x, "month"))
  )

  dt_agg <- data.table::copy(dt)
  dt_agg[, time_period := trunc_fn(get(time_col))]

  dt_counts <- dt_agg[get(flag_col) %in% flag_shown,
    .(n_flag = .N), by = .(time_period, site = get(site_col), flag = get(flag_col))]
  dt_total  <- dt_agg[, .(total = .N), by = .(time_period, site = get(site_col))]
  dt_prop   <- dt_counts[dt_total, on = c("time_period", "site")]
  dt_prop[, prop_flag  := n_flag / total]
  dt_prop[, flag_label := factor(
    paste0(flag, " ", .qc_label(flag)),
    levels = paste0(1:6, " ", .qc_label(1:6))
  )]

  flag_colours <- stats::setNames(
    c("#2D7D46", "#F9A11B", "#E06B00", "#D4351C", "#767676", "#5694CA"),
    paste0(1:6, " ", .qc_label(1:6))
  )

  ggplot2::ggplot(dt_prop[flag %in% flag_shown],
                  ggplot2::aes(x = time_period, y = site, fill = prop_flag)) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 0.2) +
    ggplot2::facet_wrap(ggplot2::vars(flag_label), ncol = 2L) +
    ggplot2::scale_fill_gradient(
      low    = "#FFFFFF",
      high   = "#1B3A6B",
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name   = "Proportion"
    ) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_flood(grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      panel.border = ggplot2::element_blank()
    )
}

#' Plot data availability across sites and time
#'
#' Draws a stacked bar chart showing the proportion of each QC flag type per
#' site, over the entire period covered by the dataset. Provides a quick
#' single-page summary of overall data quality by site.
#'
#' @param dt       A data.table with `timestamp`, `qc_flag`, and `site_id`.
#' @param time_col Name of the timestamp column. Default `"timestamp"`.
#' @param flag_col Name of the QC flag column. Default `"qc_flag"`.
#' @param site_col Name of the site identifier column. Default `"site_id"`.
#'
#' @return A `ggplot` object (horizontal stacked bar, one bar per site).
#'
#' @examples
#' library(data.table)
#' dt <- make_hydro_dt()
#' plot_data_availability(dt)
#'
#' @export
plot_data_availability <- function(dt,
                                   time_col = "timestamp",
                                   flag_col = "qc_flag",
                                   site_col = "site_id") {
  .check_dt(dt, c(time_col, flag_col, site_col))

  dt_counts <- dt[, .(n_flag = .N),
                   by = .(site = get(site_col), flag = get(flag_col))]
  dt_total  <- dt[, .(total = .N), by = .(site = get(site_col))]
  dt_prop   <- dt_counts[dt_total, on = "site"]
  dt_prop[, prop_flag  := n_flag / total]
  dt_prop[, flag_label := factor(
    paste0(flag, " ", .qc_label(flag)),
    levels = rev(paste0(1:6, " ", .qc_label(1:6)))
  )]

  ggplot2::ggplot(dt_prop, ggplot2::aes(
    x    = prop_flag,
    y    = stats::reorder(site, prop_flag),
    fill = flag_label
  )) +
    ggplot2::geom_col(width = 0.7) +
    scale_fill_qc(name = "QC flag") +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(x = "Proportion of record", y = NULL) +
    theme_flood(grid = "x")
}
