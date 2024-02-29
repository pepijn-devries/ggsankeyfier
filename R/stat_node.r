#' Sankey stats
#'
#' Aggregates value on the `y` axis per `group` for nodes, and for all used aesthetics for
#' edges.
#'
#' Wrangles data before it can be passed to [`position_sankey()`].
#' @param position A `character` string or function specifying the positioning routine.
#' By default this is `"sankey"`.
#' @param geom a string naming the `ggplot2::proto` Geom subclass. Should be either
#' `"sankeynode"` or `"sankeedge"`.
#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_sankeyedge
#' @param ... Passed to [ggplot2::layer()] function
#' @return Returns a ggplot2 stat layer which can be used in a ggplot.
#' @include stat_node_helpers.r
#' @name StatSankeynode
#' @rdname stat_sankey
#' @examples
#' library(ggplot2)
#' data("ecosystem_services")
#'
#' p <- ggplot(ecosystem_services_pivot1, aes(x = stage, y = RCSES, group = node,
#'                                            connector = connector, edge_id = edge_id,
#'                                            fill = node))
#' p + stat_sankeynode()
#' p + stat_sankeyedge()
#' @author Pepijn de Vries
#' @export
StatSankeynode <-
  ggplot2::ggproto(
    "StatSankeynode", ggplot2::Stat,
    compute_panel = .compute_panel_statnodes,
    required_aes = c("x", "y", "group", "connector"),
  )

#' @name stat_sankeynode
#' @rdname stat_sankey
#' @export
stat_sankeynode <-
  function(mapping = NULL, data = NULL, geom = "sankeynode",
           position = "sankey", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(
      stat     = StatSankeynode, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = rlang::list2(
        na.rm = na.rm, ...)
    )
  }
