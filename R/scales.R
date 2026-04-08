# ============================================================
# Tool:         scales.R
# Description:  Colour and fill scales for QC flags, condition ratings, tier
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       Called as ggplot2 scale layers
# Outputs:      ggplot2 Scale objects
# Dependencies: ggplot2, scales
# ============================================================

# QC flag scales ---------------------------------------------------------------

.QC_COLOURS <- c(
  "1 Good"            = "#2D7D46",
  "2 Estimated"       = "#F9A11B",
  "3 Suspect"         = "#E06B00",
  "4 Rejected"        = "#D4351C",
  "5 No data"         = "#767676",
  "6 Below detection" = "#5694CA"
)

.qc_values <- function() {
  vals <- c("1", "2", "3", "4", "5", "6")
  setNames(.QC_COLOURS, vals)
}

.qc_labels <- function() {
  c("1" = "Good", "2" = "Estimated", "3" = "Suspect",
    "4" = "Rejected", "5" = "No data", "6" = "Below detection")
}

#' Colour scale for QC flag integers (1–6)
#'
#' Maps QC flag codes defined in the Bronze/Silver schema to the EA flood
#' information palette. Intended for use with a `colour` aesthetic mapped to an
#' integer QC flag column.
#'
#' @param na.value Colour for NA values. Default grey.
#' @param name    Legend title. Default `"QC flag"`.
#' @param ...     Additional arguments passed to [ggplot2::scale_colour_manual()].
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' dt <- data.table(
#'   t    = seq.POSIXt(Sys.time(), by = "hour", length.out = 10),
#'   val  = runif(10, 0, 5),
#'   flag = sample(1:4, 10, replace = TRUE)
#' )
#' ggplot(dt, aes(t, val, colour = factor(flag))) +
#'   geom_point() +
#'   scale_colour_qc()
#'
#' @export
scale_colour_qc <- function(na.value = "#767676", name = "QC flag", ...) {
  ggplot2::scale_colour_manual(
    values   = .qc_values(),
    labels   = .qc_labels(),
    na.value = na.value,
    name     = name,
    ...
  )
}

#' Fill scale for QC flag integers (1–6)
#'
#' @inheritParams scale_colour_qc
#' @return A ggplot2 scale object.
#' @export
scale_fill_qc <- function(na.value = "#767676", name = "QC flag", ...) {
  ggplot2::scale_fill_manual(
    values   = .qc_values(),
    labels   = .qc_labels(),
    na.value = na.value,
    name     = name,
    ...
  )
}

# Condition rating scales ------------------------------------------------------

.CONDITION_COLOURS <- c(
  "Good"               = "#2D7D46",
  "Satisfactory"       = "#009FAF",
  "Requires Attention" = "#F9A11B",
  "Poor"               = "#E06B00",
  "Critical"           = "#D4351C"
)

.CONDITION_LEVELS <- c("Critical", "Poor", "Requires Attention",
                        "Satisfactory", "Good")

#' Colour scale for model condition ratings
#'
#' Maps the five condition rating bands (Critical through Good) to a
#' traffic-light colour sequence. Levels are ordered from worst to best so that
#' sorted outputs place Critical at the bottom and Good at the top.
#'
#' @param na.value Colour for NA values.
#' @param name    Legend title. Default `"Condition"`.
#' @param ...     Additional arguments passed to [ggplot2::scale_colour_manual()].
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' dt <- data.table(
#'   model  = paste0("M", 1:5),
#'   score  = c(9, 7, 5, 3, 1),
#'   rating = c("Good", "Satisfactory", "Requires Attention", "Poor", "Critical")
#' )
#' ggplot(dt, aes(model, score, colour = rating)) +
#'   geom_point(size = 4) +
#'   scale_colour_condition()
#'
#' @export
scale_colour_condition <- function(na.value = "#767676", name = "Condition", ...) {
  ggplot2::scale_colour_manual(
    values = .CONDITION_COLOURS,
    limits = .CONDITION_LEVELS,
    name   = name,
    na.value = na.value,
    ...
  )
}

#' Fill scale for model condition ratings
#'
#' @inheritParams scale_colour_condition
#' @return A ggplot2 scale object.
#' @export
scale_fill_condition <- function(na.value = "#767676", name = "Condition", ...) {
  ggplot2::scale_fill_manual(
    values = .CONDITION_COLOURS,
    limits = .CONDITION_LEVELS,
    name   = name,
    na.value = na.value,
    ...
  )
}

# Tier scales ------------------------------------------------------------------

.TIER_COLOURS <- c(
  "Tier 3" = "#1B3A6B",
  "Tier 2" = "#5694CA",
  "Tier 1" = "#767676"
)

#' Colour scale for asset tiers
#'
#' Maps Tier 3 (Operational), Tier 2 (Analytical), and Tier 1 (Experimental)
#' to navy, blue, and grey respectively. Tier 3 receives the most prominent
#' colour to reflect its higher operational significance.
#'
#' @param name    Legend title. Default `"Tier"`.
#' @param ...     Additional arguments passed to [ggplot2::scale_colour_manual()].
#' @return A ggplot2 scale object.
#' @export
scale_colour_tier <- function(name = "Tier", ...) {
  ggplot2::scale_colour_manual(values = .TIER_COLOURS, name = name, ...)
}

#' Fill scale for asset tiers
#'
#' @inheritParams scale_colour_tier
#' @return A ggplot2 scale object.
#' @export
scale_fill_tier <- function(name = "Tier", ...) {
  ggplot2::scale_fill_manual(values = .TIER_COLOURS, name = name, ...)
}

# General flood palette --------------------------------------------------------

.FLOOD_DISCRETE <- c(
  "#009FAF", "#1B3A6B", "#F9A11B", "#2D7D46",
  "#D4351C", "#5694CA", "#767676"
)

#' General discrete colour scale using the EA flood palette
#'
#' Provides up to seven distinct colours drawn from the EA flood information
#' palette. Use for grouping variables that do not correspond to QC flags,
#' condition ratings, or tiers.
#'
#' @param name    Legend title. Default `"Group"`.
#' @param ...     Additional arguments passed to [ggplot2::scale_colour_manual()].
#' @return A ggplot2 scale object.
#' @export
scale_colour_flood <- function(name = "Group", ...) {
  ggplot2::scale_colour_manual(values = .FLOOD_DISCRETE, name = name, ...)
}

#' @rdname scale_colour_flood
#' @export
scale_fill_flood <- function(name = "Group", ...) {
  ggplot2::scale_fill_manual(values = .FLOOD_DISCRETE, name = name, ...)
}
