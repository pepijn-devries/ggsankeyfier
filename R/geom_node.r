#' Bars representing nodes in a Sankey diagram
#'
#' In a Sankey diagram nodes are depicted as stacked bars, possibly with
#' vertical spacing between them. Use `geom_sankeynode()` to add nodes to
#' your Sankey diagram. If you combine the nodes with [geom_sankeyedge()],
#' make sure that both use the same `position` object.
#'
#' This `ggplot2` layer depicts the size of all connected edges as a bar. The height of
#' of each bar is determined by the sum of `y` aesthetic in each `group`. When the sum of edges
#' that flow to a bar differ more than `split_tol` compared to the edges that flow from the
#' same node, a vertical split is introduced in the node.
#'
#' @section Aesthetics:
#' `geom_sankeynode()` understands the following aesthetics (required aesthetics
#' are in bold)
#'
#' @template aes_template
#' @inheritParams ggplot2::geom_segment
#' @inheritParams position_sankey
#' @inherit geom_sankeyedge return examples
#' @author Pepijn de Vries
#' @name GeomSankeynode
#' @rdname geom_sankeynode
#' @include draw_edges.r geom_segment.r geom_edge.r
#' @include position_helpers.r
#' @export
GeomSankeynode <-
  ggplot2::ggproto(
    "GeomSankeynode",
    ggplot2::GeomBar,
    required_aes = c("x", "y", "group", "connector", "edge_id"),
    draw_key     = draw_key_sankeynode,
    rename_size  = FALSE,
    setup_data   = function(data, params) {
      data <- ggplot2::GeomBar$setup_data(data, params)
      # Make sure to unmap the waist scale from nodes:
      data <- data |> dplyr::mutate(waist = NULL)
      return(data)
    },
    draw_panel   = function(self, data, panel_params, coord) {
      ggplot2::GeomRect$draw_panel(data = data, panel_params = panel_params, coord = coord)
    }
  )

#' @name geom_sankeynode
#' @rdname geom_sankeynode
#' @export
geom_sankeynode <-
  function(mapping = NULL, data = NULL,
           stat = "sankeynode",
           position = "sankey", na.rm = FALSE, show.legend = NA,
           width = "auto", align = c("bottom", "top", "center", "justify"),
           order = c("ascending", "descending", "as_is"),
           h_space = "auto", v_space = 0,
           nudge_x = 0, nudge_y = 0,
           split_nodes = FALSE, split_tol = 1e-3,
           direction = c("forward", "backward"),
           inherit.aes = TRUE, ...) {

    position <- .check_position_args(match.call(), environment())

    ggplot2::layer(
      geom     = GeomSankeynode, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = rlang::list2(na.rm = na.rm, ...)
    )
  }
