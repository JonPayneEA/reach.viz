# ============================================================
# Tool:         plot-ensemble.R
# Description:  Ensemble forecast visualisation: ribbon, spaghetti,
#               probability of exceedance
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       data.table in long ensemble format (timestamp, value,
#               ensemble_member, optional site_id)
# Outputs:      ggplot2 objects
# Dependencies: ggplot2, data.table
# ============================================================

#' Plot ensemble forecast as percentile ribbons
#'
#' Computes quantiles across ensemble members at each timestep and renders
#' them as nested ribbons. The median is drawn as a solid line. An optional
#' observed time series can be overlaid.
#'
#' @param dt         A data.table in long ensemble format with columns for
#'   timestamp, value, and ensemble member.
#' @param x          Name of the timestamp column. Default `"timestamp"`.
#' @param value_col  Name of the value column. Default `"value"`.
#' @param member_col Name of the ensemble member identifier column.
#'   Default `"ensemble_member"`.
#' @param probs      Numeric vector of probability levels. Two pairs of
#'   symmetric quantiles plus the median are expected: the outer pair forms the
#'   widest ribbon, the inner pair the narrower band, and the central value the
#'   median line. Default `c(0.1, 0.25, 0.5, 0.75, 0.9)`.
#' @param obs_dt     Optional data.table of observed values. Must have the same
#'   `x` column and a column named `"value"`. Plotted as a dark navy line.
#' @param site_col   Optional column for faceting by site.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' ens <- make_ensemble_dt()
#' plot_ensemble_ribbon(ens)
#'
#' @export
plot_ensemble_ribbon <- function(dt,
                                 x          = "timestamp",
                                 value_col  = "value",
                                 member_col = "ensemble_member",
                                 probs      = c(0.1, 0.25, 0.5, 0.75, 0.9),
                                 obs_dt     = NULL,
                                 site_col   = NULL) {
  .check_dt(dt, c(x, value_col, member_col))
  if (length(probs) != 5L || probs[3L] != 0.5) {
    stop("plot_ensemble_ribbon(): `probs` must be a length-5 vector ",
         "with the central value 0.5 (the median). ",
         "Default: c(0.1, 0.25, 0.5, 0.75, 0.9).", call. = FALSE)
  }

  q_dt <- .ensemble_quantiles(dt, x, value_col, member_col,
                               group_col = site_col, probs = probs)

  # Column names produced by .ensemble_quantiles
  lo_outer <- paste0("q_", formatC(probs[1L] * 100, format = "d", flag = "0", width = 2))
  lo_inner <- paste0("q_", formatC(probs[2L] * 100, format = "d", flag = "0", width = 2))
  median_q <- paste0("q_", formatC(probs[3L] * 100, format = "d", flag = "0", width = 2))
  hi_inner <- paste0("q_", formatC(probs[4L] * 100, format = "d", flag = "0", width = 2))
  hi_outer <- paste0("q_", formatC(probs[5L] * 100, format = "d", flag = "0", width = 2))

  p <- ggplot2::ggplot(q_dt, ggplot2::aes(x = .data[[x]])) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[[lo_outer]], ymax = .data[[hi_outer]]),
      fill = "#009FAF", alpha = 0.15
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[[lo_inner]], ymax = .data[[hi_inner]]),
      fill = "#009FAF", alpha = 0.25
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[median_q]]),
      colour = "#009FAF", linewidth = 0.8
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Flow (m\u00b3/s)",
      caption = paste0(
        scales::percent(probs[1L]), "\u2013", scales::percent(probs[5L]),
        " and ",
        scales::percent(probs[2L]), "\u2013", scales::percent(probs[4L]),
        " intervals; median line"
      )
    ) +
    theme_flood()

  if (!is.null(obs_dt)) {
    .check_dt(obs_dt, c(x, "value"))
    p <- p + ggplot2::geom_line(
      data      = obs_dt,
      ggplot2::aes(x = .data[[x]], y = value),
      colour    = "#1B3A6B",
      linewidth = 0.6,
      inherit.aes = FALSE
    )
  }

  if (!is.null(site_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[site_col]]),
                                  ncol = 1L, scales = "free_y")
  }

  p
}

#' Plot ensemble forecast as a spaghetti plot
#'
#' Draws all individual ensemble member traces as thin lines with adjustable
#' opacity. Optionally highlights the ensemble median as a heavier line and
#' overlays an observed series.
#'
#' @param dt              A data.table in long ensemble format.
#' @param x               Name of the timestamp column. Default `"timestamp"`.
#' @param value_col       Name of the value column. Default `"value"`.
#' @param member_col      Name of the ensemble member column.
#'   Default `"ensemble_member"`.
#' @param highlight_median Logical. Draw the ensemble median as a solid navy
#'   line. Default `TRUE`.
#' @param alpha           Opacity of individual member lines. Default `0.25`.
#' @param obs_dt          Optional observed data.table (same schema as for
#'   [plot_ensemble_ribbon()]).
#' @param site_col        Optional column for faceting by site.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' ens <- make_ensemble_dt()
#' plot_ensemble_spaghetti(ens)
#'
#' @export
plot_ensemble_spaghetti <- function(dt,
                                    x                = "timestamp",
                                    value_col        = "value",
                                    member_col       = "ensemble_member",
                                    highlight_median = TRUE,
                                    alpha            = 0.25,
                                    obs_dt           = NULL,
                                    site_col         = NULL) {
  .check_dt(dt, c(x, value_col, member_col))

  p <- ggplot2::ggplot(dt, ggplot2::aes(
    x     = .data[[x]],
    y     = .data[[value_col]],
    group = .data[[member_col]]
  )) +
    ggplot2::geom_line(colour = "#009FAF", alpha = alpha, linewidth = 0.35) +
    ggplot2::labs(x = NULL, y = "Flow (m\u00b3/s)") +
    theme_flood()

  if (highlight_median) {
    by_cols   <- c(x, site_col)
    med_dt    <- dt[, .(value = stats::median(get(value_col), na.rm = TRUE)),
                     by = by_cols]
    p <- p + ggplot2::geom_line(
      data        = med_dt,
      ggplot2::aes(x = .data[[x]], y = value, group = 1L),
      colour      = "#1B3A6B",
      linewidth   = 0.9,
      inherit.aes = FALSE
    )
  }

  if (!is.null(obs_dt)) {
    .check_dt(obs_dt, c(x, "value"))
    p <- p + ggplot2::geom_line(
      data        = obs_dt,
      ggplot2::aes(x = .data[[x]], y = value, group = 1L),
      colour      = "#D4351C",
      linewidth   = 0.7,
      inherit.aes = FALSE
    )
  }

  if (!is.null(site_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[site_col]]),
                                  ncol = 1L, scales = "free_y")
  }

  p
}

#' Plot probability of exceedance across an ensemble
#'
#' Computes the empirical probability of exceeding a set of thresholds across
#' ensemble members and draws the exceedance curve. Optional threshold
#' annotation lines show alert/warning/severe levels.
#'
#' @param dt         A data.table in long ensemble format.
#' @param value_col  Name of the value column. Default `"value"`.
#' @param member_col Name of the ensemble member column.
#'   Default `"ensemble_member"`.
#' @param thresholds Optional named numeric vector of threshold values to
#'   annotate (e.g. `c(Alert = 50, Warning = 100, Severe = 150)`).
#' @param x_log      Log-transform the X axis (flow values). Default `FALSE`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' ens <- make_ensemble_dt()
#' plot_exceedance_probability(ens, thresholds = c(Alert = 30, Warning = 60))
#'
#' @export
plot_exceedance_probability <- function(dt,
                                        value_col  = "value",
                                        member_col = "ensemble_member",
                                        thresholds = NULL,
                                        x_log      = FALSE) {
  .check_dt(dt, c(value_col, member_col))

  n_members <- dt[, data.table::uniqueN(get(member_col))]

  # Empirical exceedance: for each unique value, what fraction of members exceed it?
  vals <- sort(dt[[value_col]], decreasing = FALSE)
  exc_probs <- (rev(seq_along(vals))) / n_members
  dt_exc <- data.table::data.table(value = vals, exceedance_prob = exc_probs)

  p <- ggplot2::ggplot(dt_exc, ggplot2::aes(x = value, y = exceedance_prob)) +
    ggplot2::geom_line(colour = "#1B3A6B", linewidth = 0.8) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      name   = "Probability of exceedance"
    ) +
    ggplot2::labs(x = paste0(value_col, " value")) +
    theme_flood()

  if (x_log) p <- p + ggplot2::scale_x_log10()

  if (!is.null(thresholds)) {
    thr_dt <- data.table::data.table(
      label = names(thresholds),
      value = unname(thresholds)
    )
    thr_colours <- c(Alert = "#F9A11B", Warning = "#E06B00", Severe = "#D4351C")
    thr_dt[, colour := thr_colours[label]]
    thr_dt[is.na(colour), colour := "#767676"]

    p <- p +
      ggplot2::geom_vline(
        data        = thr_dt,
        ggplot2::aes(xintercept = value),
        colour      = thr_dt[["colour"]],
        linetype    = "dashed",
        linewidth   = 0.6,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data        = thr_dt,
        ggplot2::aes(x = value, y = 0.95, label = label),
        colour      = thr_dt[["colour"]],
        hjust       = -0.1,
        size        = 3.2,
        inherit.aes = FALSE
      )
  }

  p
}
