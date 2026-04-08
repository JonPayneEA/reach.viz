# ============================================================
# Tool:         plot-cascade.R
# Description:  Model cascade network diagram using ggraph/igraph
#               (suggested dependencies, not required)
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       Two data.tables: connections (edges) and models (nodes)
# Outputs:      ggraph/ggplot2 object, or informative error if ggraph absent
# Dependencies: ggplot2; ggraph + igraph in Suggests
# ============================================================

#' Plot a directed model cascade network
#'
#' Draws the model cascade defined in the Connection Register as a directed
#' acyclic graph. Nodes are flood models; edges are connections. Node colour
#' encodes the model's condition rating; node shape encodes its tier; edge
#' colour encodes the connection type. Suspended models and deprecated
#' connections are shown as faded.
#'
#' Requires the `ggraph` and `igraph` packages, which are listed in
#' `reach.viz` Suggests. If they are not installed the function stops with an
#' informative message rather than throwing a cryptic error.
#'
#' @param connections_dt A data.table of connection register entries. Required
#'   columns: `upstream_model`, `downstream_model`, `connection_type`, `status`.
#' @param models_dt      A data.table of model register entries. Required
#'   columns: `model_id`, `tier`, `condition_score`, `status`.
#' @param upstream_col   Name of the upstream model column in `connections_dt`.
#'   Default `"upstream_model"`.
#' @param downstream_col Name of the downstream model column.
#'   Default `"downstream_model"`.
#' @param type_col       Name of the connection type column.
#'   Default `"connection_type"`.
#' @param layout         igraph layout algorithm. Default `"sugiyama"`, which
#'   produces a hierarchical top-to-bottom layout appropriate for cascading
#'   systems. Other options: `"stress"`, `"fr"`.
#'
#' @return A `ggraph` / `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' # Requires ggraph and igraph
#' library(data.table)
#' connections <- make_connections_dt()
#' models      <- make_register_dt()
#' plot_model_cascade(connections, models)
#' }
#'
#' @export
plot_model_cascade <- function(connections_dt,
                               models_dt,
                               upstream_col   = "upstream_model",
                               downstream_col = "downstream_model",
                               type_col       = "connection_type",
                               layout         = "sugiyama") {
  .require_suggested("ggraph", "plot_model_cascade")
  .require_suggested("igraph", "plot_model_cascade")

  .check_dt(connections_dt, c(upstream_col, downstream_col, type_col, "status"))
  .check_dt(models_dt, c("model_id", "tier", "condition_score", "status"))

  # Build igraph object
  edges <- data.frame(
    from            = connections_dt[[upstream_col]],
    to              = connections_dt[[downstream_col]],
    connection_type = connections_dt[[type_col]],
    status          = connections_dt[["status"]],
    stringsAsFactors = FALSE
  )

  # Collect all node names that appear in edges; left-join with models_dt
  all_nodes <- unique(c(edges$from, edges$to))
  nodes_in  <- models_dt[model_id %in% all_nodes]
  # Any model referenced in connections but absent from models_dt gets defaults
  missing   <- setdiff(all_nodes, nodes_in[["model_id"]])
  if (length(missing) > 0L) {
    nodes_in <- rbind(
      nodes_in,
      data.table::data.table(
        model_id        = missing,
        tier            = "Tier 1",
        condition_score = NA_integer_,
        status          = "Unknown"
      ),
      fill = TRUE
    )
  }
  nodes_in[, condition_rating := as.character(.score_to_rating(condition_score))]
  nodes_in[is.na(condition_rating), condition_rating := "Unknown"]

  g <- igraph::graph_from_data_frame(
    d        = edges,
    directed = TRUE,
    vertices = nodes_in
  )

  tier_shapes <- c("Tier 3" = 21L, "Tier 2" = 22L, "Tier 1" = 24L, "Unknown" = 23L)

  rating_colours_ext <- c(
    .CONDITION_COLOURS,
    "Unknown" = "#D8D8D8"
  )

  edge_type_colours <- c(
    "Upstream inflow"        = "#1B3A6B",
    "Downstream boundary"    = "#009FAF",
    "Intermediate routing"   = "#F9A11B",
    "Ensemble feed"          = "#2D7D46",
    "Unknown"                = "#767676"
  )

  ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        colour = connection_type,
        alpha  = ifelse(status == "Active", 1.0, 0.3)
      ),
      arrow          = grid::arrow(length = grid::unit(3, "mm"),
                                   type   = "closed"),
      end_cap        = ggraph::circle(5, "mm"),
      linewidth      = 0.8,
      show.legend    = TRUE
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        fill   = condition_rating,
        shape  = tier,
        colour = condition_rating,
        alpha  = ifelse(status == "Active", 1.0, 0.35),
        size   = 5
      ),
      stroke = 0.8
    ) +
    ggraph::geom_node_label(
      ggplot2::aes(label = name),
      repel     = TRUE,
      size      = 2.8,
      colour    = "#383838",
      label.size = 0,
      fill       = "#FFFFFF"
    ) +
    ggplot2::scale_fill_manual(
      values = rating_colours_ext,
      name   = "Condition"
    ) +
    ggplot2::scale_colour_manual(
      values = rating_colours_ext,
      guide  = "none"
    ) +
    ggplot2::scale_shape_manual(
      values = tier_shapes,
      name   = "Tier"
    ) +
    ggraph::scale_edge_colour_manual(
      values = edge_type_colours,
      name   = "Connection type"
    ) +
    ggplot2::scale_alpha_identity(guide = "none") +
    ggplot2::scale_size_identity() +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position  = "right",
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      legend.key       = ggplot2::element_rect(fill = "#FFFFFF", colour = NA)
    )
}
