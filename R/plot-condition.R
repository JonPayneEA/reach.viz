# ============================================================
# Tool:         plot-condition.R
# Description:  Model condition assessment visualisation: factor radar chart
#               and condition register overview
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       data.table with condition factor scores or register entries
# Outputs:      ggplot2 objects
# Dependencies: ggplot2, data.table
# ============================================================

# Standard factor order and display labels
.CONDITION_FACTORS <- c(
  "data_currency"        = "Data currency",
  "structural_integrity" = "Structural integrity",
  "calibration_currency" = "Calibration currency",
  "software_currency"    = "Software currency",
  "documentation"        = "Documentation"
)

#' Plot model condition factor scores as a radar chart
#'
#' Renders a polar-coordinate radar chart showing the five condition assessment
#' factors for one or more models. Each factor is scored 0–2 (Poor, Adequate,
#' Good). When multiple models are supplied, they are faceted.
#'
#' @param dt          A data.table in long format with one row per factor per
#'   model. Required columns: `model_id` (or the column named by `model_col`),
#'   `factor` (character, one of the five standard factor codes or labels), and
#'   `score` (integer 0–2).
#' @param model_col   Name of the model identifier column. Default `"model_id"`.
#' @param factor_col  Name of the factor column. Default `"factor"`.
#' @param score_col   Name of the score column. Default `"score"`.
#'
#' @return A `ggplot` object using `coord_polar()`.
#'
#' @details
#' Factor codes are automatically matched to display labels. If factor values
#' in `dt` do not match the standard codes, they are used as-is.
#'
#' The maximum polygon (score = 2 on all factors) is shown as a light grey
#' reference. The actual model polygon is filled with the colour corresponding
#' to the model's overall condition rating.
#'
#' @examples
#' library(data.table)
#' dt <- make_condition_dt()
#' plot_condition_factors(dt)
#' plot_condition_factors(dt[model_id == "FMP_EDEN_v3"])
#'
#' @export
plot_condition_factors <- function(dt,
                                   model_col  = "model_id",
                                   factor_col = "factor",
                                   score_col  = "score") {
  .check_dt(dt, c(model_col, factor_col, score_col))

  dt_plot <- data.table::copy(dt)

  # Map codes to display labels where possible
  dt_plot[, factor_label := {
    lbl <- .CONDITION_FACTORS[get(factor_col)]
    ifelse(is.na(lbl), get(factor_col), lbl)
  }]

  # Compute overall score per model for fill colour
  dt_scores <- dt_plot[, .(total_score = sum(get(score_col), na.rm = TRUE)),
                        by = .(model = get(model_col))]
  dt_scores[, rating := as.character(.score_to_rating(total_score))]
  dt_plot   <- dt_scores[dt_plot, on = setNames("model", model_col)]

  # Close the polygon by duplicating first factor row
  factor_levels <- unique(dt_plot[["factor_label"]])
  dt_plot[, factor_label := factor(factor_label, levels = factor_levels)]

  dt_closed <- rbind(
    dt_plot,
    dt_plot[, .SD[1L], by = model_col]
  )

  # Reference (max = 2) polygon
  n_factors   <- length(factor_levels)
  dt_ref      <- data.table::data.table(
    factor_label = factor(c(factor_levels, factor_levels[1L]),
                          levels = factor_levels),
    score        = 2L
  )

  ggplot2::ggplot(dt_closed, ggplot2::aes(
    x     = factor_label,
    y     = get(score_col),
    group = .data[[model_col]]
  )) +
    ggplot2::geom_polygon(
      data        = dt_ref,
      ggplot2::aes(x = factor_label, y = score, group = 1L),
      fill        = "#F4F4F4",
      colour      = "#D8D8D8",
      linewidth   = 0.4,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = rating),
      alpha     = 0.55,
      colour    = NA
    ) +
    ggplot2::geom_path(
      ggplot2::aes(colour = rating),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = rating),
      size = 2.5
    ) +
    scale_fill_condition(name = "Rating") +
    scale_colour_condition(name = "Rating") +
    ggplot2::scale_y_continuous(
      limits = c(0, 2),
      breaks = 0:2,
      labels = c("Poor", "Adequate", "Good")
    ) +
    ggplot2::coord_polar(start = -pi / n_factors) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::facet_wrap(ggplot2::vars(.data[[model_col]])) +
    theme_flood(grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
}

#' Plot the model condition register overview
#'
#' Draws a horizontal lollipop chart of all models in the register, sorted by
#' condition score (worst at the top). Point colour encodes the condition rating;
#' point shape encodes tier; suspended or deprecated models are shown in grey.
#'
#' @param dt           A data.table with one row per model. Required columns:
#'   `model_id`, `condition_score` (integer 0–10), `condition_rating`
#'   (character), `tier` (character), and `status` (character).
#' @param model_col    Name of the model identifier column. Default `"model_id"`.
#' @param score_col    Name of the condition score column.
#'   Default `"condition_score"`.
#' @param rating_col   Name of the condition rating column.
#'   Default `"condition_rating"`.
#' @param tier_col     Name of the tier column. Default `"tier"`.
#' @param status_col   Name of the status column. Default `"status"`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(data.table)
#' dt <- make_register_dt()
#' plot_condition_register(dt)
#'
#' @export
plot_condition_register <- function(dt,
                                    model_col  = "model_id",
                                    score_col  = "condition_score",
                                    rating_col = "condition_rating",
                                    tier_col   = "tier",
                                    status_col = "status") {
  .check_dt(dt, c(model_col, score_col, rating_col, tier_col, status_col))

  dt_plot <- data.table::copy(dt)

  # Greyed-out aesthetics for non-active models
  dt_plot[, display_rating := data.table::fifelse(
    get(status_col) == "Active",
    get(rating_col),
    "Suspended/Deprecated"
  )]
  dt_plot[, display_alpha := data.table::fifelse(
    get(status_col) == "Active", 1.0, 0.4
  )]
  dt_plot[, model_ordered := stats::reorder(get(model_col), -get(score_col))]

  tier_shapes <- c("Tier 3" = 16L, "Tier 2" = 17L, "Tier 1" = 15L)

  rating_colours_ext <- c(
    .CONDITION_COLOURS,
    "Suspended/Deprecated" = "#D8D8D8"
  )

  p <- ggplot2::ggplot(dt_plot, ggplot2::aes(
    x     = .data[[score_col]],
    y     = model_ordered
  )) +
    # Threshold band annotations
    ggplot2::annotate("rect", xmin = 0,  xmax = 2,  ymin = -Inf, ymax = Inf,
                      fill = "#D4351C", alpha = 0.06) +
    ggplot2::annotate("rect", xmin = 3,  xmax = 4,  ymin = -Inf, ymax = Inf,
                      fill = "#E06B00", alpha = 0.06) +
    ggplot2::annotate("rect", xmin = 5,  xmax = 6,  ymin = -Inf, ymax = Inf,
                      fill = "#F9A11B", alpha = 0.06) +
    ggplot2::annotate("rect", xmin = 7,  xmax = 8,  ymin = -Inf, ymax = Inf,
                      fill = "#009FAF", alpha = 0.06) +
    ggplot2::annotate("rect", xmin = 9,  xmax = 10, ymin = -Inf, ymax = Inf,
                      fill = "#2D7D46", alpha = 0.06) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data[[score_col]],
                   y = model_ordered, yend = model_ordered),
      colour = "#D8D8D8", linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = display_rating,
        shape  = .data[[tier_col]],
        alpha  = display_alpha
      ),
      size = 4
    ) +
    ggplot2::scale_colour_manual(
      values = rating_colours_ext,
      name   = "Condition"
    ) +
    ggplot2::scale_shape_manual(
      values = tier_shapes,
      name   = "Tier"
    ) +
    ggplot2::scale_alpha_identity(guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = 0:10,
      limits = c(0, 10),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(x = "Condition score (0\u201310)", y = NULL) +
    theme_flood(grid = "x")

  p
}
