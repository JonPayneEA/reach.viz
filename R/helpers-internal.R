# ============================================================
# Tool:         helpers-internal.R
# Description:  Non-exported utility functions used across reach.viz
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       Various — see individual function documentation
# Outputs:      Various — see individual function documentation
# Dependencies: data.table
# ============================================================

#' Validate that an object is a data.table with required columns present
#'
#' @param dt Object to check
#' @param required_cols Character vector of required column names
#' @param call_name Name of the calling function for error messages
#' @return Invisibly TRUE; stops with an informative error if validation fails
#' @noRd
.check_dt <- function(dt, required_cols, call_name = sys.call(-1L)[[1L]]) {
  if (!data.table::is.data.table(dt)) {
    stop(
      call_name, "(): `dt` must be a data.table. ",
      "Convert with data.table::as.data.table() or data.table::setDT().",
      call. = FALSE
    )
  }
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0L) {
    stop(
      call_name, "(): required column(s) missing from `dt`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Map integer QC flag to human-readable label
#'
#' @param flag_int Integer vector of QC flags (1–6)
#' @return Character vector of labels
#' @noRd
.qc_label <- function(flag_int) {
  labels <- c(
    "1" = "Good",
    "2" = "Estimated",
    "3" = "Suspect",
    "4" = "Rejected",
    "5" = "No data",
    "6" = "Below detection"
  )
  unname(labels[as.character(flag_int)])
}

#' Map condition score (0–10) to rating string
#'
#' @param score Integer or numeric vector of condition scores
#' @return Ordered factor with levels: Critical, Poor, Requires Attention,
#'   Satisfactory, Good
#' @noRd
.score_to_rating <- function(score) {
  rating <- character(length(score))
  rating[score >= 9]                        <- "Good"
  rating[score >= 7 & score < 9]            <- "Satisfactory"
  rating[score >= 5 & score < 7]            <- "Requires Attention"
  rating[score >= 3 & score < 5]            <- "Poor"
  rating[score < 3]                         <- "Critical"
  rating[is.na(score)]                      <- NA_character_
  factor(
    rating,
    levels = c("Critical", "Poor", "Requires Attention", "Satisfactory", "Good"),
    ordered = TRUE
  )
}

#' Convert data_type code to axis label with units
#'
#' @param code Character; one of "Q", "H", "P", "SM", "SWE"
#' @return Character label suitable for a ggplot2 axis
#' @noRd
.data_type_label <- function(code) {
  labels <- c(
    "Q"   = "Flow (m\u00b3/s)",
    "H"   = "Stage (m AOD)",
    "P"   = "Rainfall (mm/hr)",
    "SM"  = "Soil moisture",
    "SWE" = "Snow water equivalent (mm)"
  )
  out <- labels[toupper(code)]
  out[is.na(out)] <- code[is.na(out)]
  unname(out)
}

#' Compute ensemble quantiles for geom_ribbon
#'
#' Takes a long-format ensemble data.table and returns a wide data.table with
#' one row per timestamp (and optionally site), with columns q_<p> for each
#' probability level.
#'
#' @param dt data.table in long ensemble format
#' @param x_col Name of timestamp column
#' @param value_col Name of value column
#' @param member_col Name of ensemble member column
#' @param group_col Optional additional grouping column (e.g. site_id)
#' @param probs Numeric vector of probabilities
#' @return Wide data.table
#' @noRd
.ensemble_quantiles <- function(dt, x_col, value_col, member_col,
                                group_col = NULL, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  by_cols <- c(x_col, group_col)
  out <- dt[, {
    vals <- get(value_col)
    as.list(stats::quantile(vals, probs = probs, na.rm = TRUE))
  }, by = by_cols]
  q_names <- paste0("q_", gsub("\\.", "", formatC(probs * 100, format = "d", flag = "0")))
  old_names <- paste0(formatC(probs * 100, format = "g"), "%")
  data.table::setnames(out, old_names, q_names, skip_absent = TRUE)
  out
}

#' Check whether a suggested package is available; stop with a helpful message
#'
#' @param pkg Package name
#' @param fn_name Name of the function requiring it (for error message)
#' @noRd
.require_suggested <- function(pkg, fn_name) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      fn_name, "() requires the '", pkg, "' package, which is listed in ",
      "reach.viz Suggests but is not installed.\n",
      "Install it with: install.packages('", pkg, "')",
      call. = FALSE
    )
  }
}
