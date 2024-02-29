#' Sankey edges (flows)
#'
#' `geom_sankeysegment()` draws a straight line between two connected nodes,
#' `geom_sankeyedge()` draws a ribbon between nodes following a Bezier curved path.
#' If you combine the edges with [geom_sankeynode()], make sure that both use the
#' same `position` object.
#'
#' This `ggplot2` layer connects between paired nodes via a Bezier curve. The width
#' of the curve is determined by its `y` aesthetic. It will be attempted to keep the
#' width of the curve constant along its curved path, for the targeted graphics device.
#' When the aspect ratio of the graphics device is altered after the plot is generated,
#' the aspect ratio maybe off. In that case render the plot again.
#'
#' @section Aesthetics:
#' `geom_sankeysegment()` and `geom_sankeyedge()` understand the following
#' aesthetics (required aesthetics are in bold)
#'
#' @template aes_template
#' @section Aesthetics:
#'   * waist: A variable to control the width of an edge in between
#'     two nodes. Small values will create a hour glass shape, whereas large values will
#'     produce an apple shape.
#'
#' @inheritParams ggplot2::geom_segment
#' @param slope Slope parameter (`numeric`) for the Bezier curves used to depict the edges.
#' Any value between 0 and 1 will work nicely. Other non-zero values will also work.
#' @param ncp Number of control points on the Bezier curve that forms the edge. Larger
#' numbers will result in smoother curves, but cost more computational time. Default is
#' 100.
#' @inheritParams position_sankey
#' @return Returns a [ggplot2::layer()] which can be added to a [ggplot2::ggplot()]
#' @examples
#' library(ggplot2)
#' data("ecosystem_services")
#'
#' ggplot(ecosystem_services_pivot1, aes(x = stage, y = RCSES, group = node,
#'                     connector = connector, edge_id = edge_id,
#'                     fill = node)) +
#'   geom_sankeynode(v_space = "auto") +
#'   geom_sankeyedge(v_space = "auto")
#' @name GeomSankeyedge
#' @rdname geom_sankeyedge
#' @author Pepijn de Vries
#' @include scale_waist.r draw_edges.r geom_segment.r
#' @export
GeomSankeyedge <-
  ggplot2::ggproto(
    "GeomSankeyedge", GeomSankeysegment,
    draw_panel   = .draw_edges,
    setup_data   = function(data, params) {
      data <- GeomSankeysegment$setup_data(data, params)
      data <- data |>
        dplyr::mutate(
          slope = params$slope,
          ncp   = params$ncp
        )
      return(data)
    },
    rename_size  = FALSE,
    default_aes  = c(GeomSankeysegment$default_aes, waist = 1),
    draw_key     = draw_key_sankeyedge,
    extra_params = c("na.rm", "slope", "ncp")
  )

#' @name geom_sankeyedge
#' @rdname geom_sankeyedge
#' @export
geom_sankeyedge <-
  function(mapping = NULL, data = NULL, stat = "sankeyedge",
           position = "sankey", na.rm = FALSE, show.legend = NA,
           slope = 0.5, ncp = 100,
           width = "auto", align = c("bottom", "top", "center", "justify"),
           order = c("ascending", "descending", "as_is"),
           h_space = "auto", v_space = 0,
           nudge_x = 0, nudge_y = 0,
           split_nodes = FALSE, split_tol = 1e-3,
           direction   = c("forward", "backward"),
           inherit.aes = TRUE, ...) {

    position <- .check_position_args(match.call(), environment())

    ggplot2::layer(
      geom     = GeomSankeyedge, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = list(na.rm = na.rm, slope = slope, ncp = ncp, ...)
    )
  }
