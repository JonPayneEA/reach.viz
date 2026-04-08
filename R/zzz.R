# ============================================================
# Tool:         zzz.R
# Description:  Package-level setup and global variable declarations
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Dependencies: data.table
# ============================================================

utils::globalVariables(c(
  # data.table column names referenced in NSE contexts
  "timestamp", "qc_value", "qc_flag", "site_id", "data_type",
  "bronze_value", "observed", "simulated", "residual",
  "value", "ensemble_member",
  "metric", "period",
  "model_id", "factor", "score",
  "condition_score", "condition_rating", "tier", "status", "last_assessed",
  "upstream_model", "downstream_model", "connection_type",
  # computed columns created inside functions
  ".N", ".SD", "exceedance_prob", "q_lo", "q_hi", "q_med",
  "flag_label", "n_flag", "prop_flag", "time_period",
  "x", "y", "xend", "yend", "label", "name"
))

.onLoad <- function(libname, pkgname) {
  invisible(NULL)
}

# Set package to being data.table aware
.datatable.aware = TRUE
