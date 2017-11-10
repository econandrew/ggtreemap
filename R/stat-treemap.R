#' @import ggplot2
#' @export
StatTreemap <- ggproto("StatTreemap", Stat,
  required_aes = c("area"),
  default_aes = aes(layout_area = NA),
  compute_panel = function(self, data, scales, fixed = FALSE, label.position = c(0.5, 0.5), aspect.ratio = 1) {
    data$id <- 1:nrow(data)

    # Generate treemap layout for data
    treemapify_params <- list(
      data = data,
      area = if("layout_area" %in% names(data)) "layout_area" else "area",
      fill = "area", # we ignore this fill and merge with ggplot's own fill
      xlim = c(0, 1*aspect.ratio),
      ylim = c(0, 1),
      label = "id"
    )
    if ("subgroup" %in% names(data)) {
      treemapify_params$group <- "subgroup"
    }
    if (fixed) {
      layout <- do.call(treemapify::treemapify_fixed, treemapify_params)
    } else {
      layout <- do.call(treemapify::treemapify, treemapify_params)
    }

    # Merge layout back into main data
    names(layout)[names(layout) == "label"] <- "id"
    layout <- layout[c("id", "xmin", "xmax", "ymin", "ymax")]
    data <- merge(data, layout, by = "id")

    # If the layout_area aesthetic was used, shrink the rectangles back to
    # the actual area aesthetic
    if ("layout_area" %in% names(data)) {
      side_scale <- sqrt(data$area / data$layout_area)
      width <- data$xmax - data$xmin
      height <- data$ymax - data$ymin
      data$xmin <- data$xmin + width*(1 - side_scale)/2
      data$xmax <- data$xmax - width*(1 - side_scale)/2
      data$ymin <- data$ymin + height*(1 - side_scale)/2
      data$ymax <- data$ymax - height*(1 - side_scale)/2
    }

    # Transform with scales
    if (!is.null(scales$x)) {
      data$xmin <- scales$x$transform(data$xmin)
      data$xmax <- scales$x$transform(data$xmax)
    }
    if (!is.null(scales$y)) {
      data$ymin <- scales$y$transform(data$ymin)
      data$ymax <- scales$y$transform(data$ymax)
    }

    data$x <- data$xmin * (1 - label.position[1]) + data$xmax * label.position[1]
    data$y <- data$ymin * (1 - label.position[2]) + data$ymax * label.position[2]

    data
  }
)

#' Calculate a treemap from data
#'
#' A stat to turn data into treemaps.
#'
#' Use with \code{geom_rect}, mostly.
#'
#' @section Aesthetics:
#' \code{stat_treemap} accepts the following aesthetics:
#' \itemize{
#' \item \strong{area}: the column used to scale the drawn areas of the treemap
#' \item layout_area: an optional column used for areas that will be used for layout but not drawn
#' \item subgroup: a column used for grouping of areas
#' }
#'
#' and applies the layout algorithm to produce
#'
#' \itemize{
#' \item x, xmin, xmax
#' \item y, ymin, ymax
#' }
#'
#' @inheritParams ggplot2::layer
#' @param label.position If used with a geom that uses a single x, y location (
#' most likely geom_text), this determines where in each treemap box that location
#' is set to, using a two element numeric vector. These are used to locate the
#' point within the box.
#' @param aspect.ratio (width / height) The standard 'squarified' algorithm aims to produce
#' square-ish treemap boxes, and this aspect ratio determines what 'square-ish'
#' is. If you set it to the actual aspect ratio of the output device, you will
#' ensure square-ishness.
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#' library(ggplot2)
#' library(ggtreemap)
#'
#' df <- data.frame(
#'   label = c("a1", "a2", "b1", "c1", "c2"),
#'   group = c("a", "a", "b", "c", "c"),
#'   size = c(0.5,4,2,1,0.5)
#' )
#'
#' ggplot(df, aes(area = size, fill = group, subgroup = group)) +
#'   geom_rect(stat = "treemap", mapping = aes(layout_area = size * 2), color = "white") +
#'   geom_rect(stat = "treemap", alpha = 0.5, color = "white") +
#'   theme_minimal() +
#'   theme(axis.text = element_blank(), panel.grid = element_blank())
#'
#' ggplot(df, aes(area = size, fill = group, subgroup = group)) +
#'   geom_rect(stat = "treemap", color = "white") +
#'   geom_text(aes(label = label), stat = "treemap") +
#'   geom_rect(data = aggregate(size ~ group, df, sum), stat="treemap", fill = NA, color = "white", size=2) +
#'   geom_label(
#'     data = aggregate(size ~ group, df, sum),
#'     aes(label = group),
#'     label.position = c(0, 0),
#'     hjust = 0, vjust = 1,
#'     nudge_x = 0.025, nudge_y = -0.025,
#'     fill = "white",
#'     stat = "treemap") +
#'   scale_y_reverse() +
#'   theme_minimal()
#'
#' @export
stat_treemap <- function(mapping = NULL, data = NULL, geom = "rect",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, label.position = c(0.5, 0.5), aspect.ratio = 1, ...) {
  layer(
    stat = StatTreemap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fixed = FALSE, label.position = label.position,
                  aspect.ratio = aspect.ratio, ...)
  )
}
