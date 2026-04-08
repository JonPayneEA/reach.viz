# ============================================================
# Tool:         theme.R
# Description:  FlodeTheme S7 class and theme_flood() ggplot2 theme
# Flode Module: reach.viz
# Author:       Jonathan Payne
# Created:      2026-04-07
# Tier:         1 (Experimental)
# Inputs:       Numeric base_size, character base_family, logical grid flags
# Outputs:      ggplot2 theme objects; FlodeTheme S7 objects
# Dependencies: ggplot2, S7
# ============================================================

#' FlodeTheme S7 class
#'
#' Typed configuration object for the reach.viz theme system. Holds palette
#' and sizing parameters that `theme_flood()` and the scale functions read from.
#' Use `flode_theme()` as the constructor rather than calling `new_object()`
#' directly.
#'
#' @slot base_size   Base font size in points. Default 11.
#' @slot base_family Base font family. Default "" (R default sans-serif).
#' @slot palette_name One of `"flood"` (full EA palette) or `"minimal"`
#'   (monochrome with accent).
#' @slot show_grid   Whether to include major gridlines. Default `TRUE`.
#'
#' @export
FlodeTheme <- S7::new_class(
  "FlodeTheme",
  properties = list(
    base_size    = S7::class_numeric,
    base_family  = S7::class_character,
    palette_name = S7::class_character,
    show_grid    = S7::class_logical
  ),
  validator = function(self) {
    if (!self@palette_name %in% c("flood", "minimal")) {
      "`palette_name` must be \"flood\" or \"minimal\""
    }
  }
)

#' Construct a FlodeTheme configuration object
#'
#' @param base_size   Base font size in points. Default `11`.
#' @param base_family Base font family string. Default `""`.
#' @param palette     One of `"flood"` or `"minimal"`. Default `"flood"`.
#' @param show_grid   Include major gridlines. Default `TRUE`.
#'
#' @return A `FlodeTheme` S7 object.
#'
#' @examples
#' ft <- flode_theme()
#' ft@palette_name
#'
#' @export
flode_theme <- function(base_size    = 11,
                        base_family  = "",
                        palette      = "flood",
                        show_grid    = TRUE) {
  FlodeTheme(
    base_size    = base_size,
    base_family  = base_family,
    palette_name = palette,
    show_grid    = show_grid
  )
}

#' S3 print method for FlodeTheme
#'
#' @param x A `FlodeTheme` object.
#' @param ... Unused.
#' @export
print.FlodeTheme <- function(x, ...) {
  cat("<FlodeTheme>\n")
  cat("  palette   :", x@palette_name, "\n")
  cat("  base_size :", x@base_size, "\n")
  cat("  show_grid :", x@show_grid, "\n")
  invisible(x)
}

# Internal palette definition --------------------------------------------------

.FLOOD_PALETTE <- list(
  teal       = "#009FAF",
  amber      = "#F9A11B",
  red        = "#D4351C",
  navy       = "#1B3A6B",
  blue       = "#5694CA",
  offwhite   = "#F4F4F4",
  mid_grey   = "#767676",
  dark_grey  = "#383838",

  qc = c(
    "1" = "#2D7D46",
    "2" = "#F9A11B",
    "3" = "#E06B00",
    "4" = "#D4351C",
    "5" = "#767676",
    "6" = "#5694CA"
  ),

  condition = c(
    "Good"               = "#2D7D46",
    "Satisfactory"       = "#009FAF",
    "Requires Attention" = "#F9A11B",
    "Poor"               = "#E06B00",
    "Critical"           = "#D4351C"
  ),

  tier = c(
    "Tier 3" = "#1B3A6B",
    "Tier 2" = "#5694CA",
    "Tier 1" = "#767676"
  )
)

# Main theme -------------------------------------------------------------------

#' EA flood information ggplot2 theme
#'
#' A clean ggplot2 theme using the EA flood information colour palette. Designed
#' for hydrological time series, ensemble plots, and governance dashboards. Built
#' on `ggplot2::theme_minimal()` with modifications for the team's visual style.
#'
#' @param base_size   Base font size in points. Default `11`.
#' @param base_family Base font family. Default `""`.
#' @param grid        Which gridlines to show. One of `"xy"`, `"x"`, `"y"`,
#'   or `"none"`. Default `"y"`.
#'
#' @return A `ggplot2::theme` object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_flood()
#'
#' @export
theme_flood <- function(base_size = 11, base_family = "", grid = "y") {
  show_x <- grid %in% c("xy", "x")
  show_y <- grid %in% c("xy", "y")

  base <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)

  base +
    ggplot2::theme(
      # Panel
      panel.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "#D8D8D8", linewidth = 0.4),
      panel.grid.major.x = if (show_x) ggplot2::element_line(colour = "#E8E8E8", linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.major.y = if (show_y) ggplot2::element_line(colour = "#E8E8E8", linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),

      # Plot
      plot.background   = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      plot.title        = ggplot2::element_text(colour = "#383838", face = "bold",
                                                size = base_size * 1.15, margin = ggplot2::margin(b = 4)),
      plot.subtitle     = ggplot2::element_text(colour = "#767676", size = base_size * 0.95,
                                                margin = ggplot2::margin(b = 8)),
      plot.caption      = ggplot2::element_text(colour = "#767676", size = base_size * 0.8,
                                                hjust = 0, margin = ggplot2::margin(t = 6)),
      plot.margin       = ggplot2::margin(12, 16, 8, 12),

      # Axes
      axis.title        = ggplot2::element_text(colour = "#383838", size = base_size * 0.9),
      axis.text         = ggplot2::element_text(colour = "#767676", size = base_size * 0.85),
      axis.line         = ggplot2::element_line(colour = "#D8D8D8", linewidth = 0.4),
      axis.ticks        = ggplot2::element_line(colour = "#D8D8D8", linewidth = 0.3),

      # Legend
      legend.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      legend.key        = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      legend.title      = ggplot2::element_text(colour = "#383838", size = base_size * 0.85,
                                                face = "bold"),
      legend.text       = ggplot2::element_text(colour = "#767676", size = base_size * 0.8),
      legend.position   = "right",

      # Strip (facets)
      strip.background  = ggplot2::element_rect(fill = "#F4F4F4", colour = "#D8D8D8", linewidth = 0.3),
      strip.text        = ggplot2::element_text(colour = "#383838", size = base_size * 0.85,
                                                margin = ggplot2::margin(4, 6, 4, 6))
    )
}
