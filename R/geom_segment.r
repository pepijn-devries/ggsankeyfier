#' @name GeomSankeysegment
#' @rdname geom_sankeyedge
#' @export
GeomSankeysegment <-
  ggplot2::ggproto(
    "GeomSankeysegment",
    ggplot2::GeomSegment,
    required_aes = c("x", "y", "group", "connector", "edge_id"),
    draw_panel   = function(self, data, panel_params, coord) {
      ggplot2::GeomSegment$draw_panel(data = data, panel_params = panel_params, coord = coord)
    },
    default_aes = list(colour = NA, fill = "grey35",
                       linewidth = 0.5, linetype = 1, alpha = 0.5)
  )

#' @name GeomSankeysegment
#' @rdname geom_sankeyedge
#' @export
geom_sankeysegment <-
  function(mapping = NULL, data = NULL, stat = "sankeyedge",
           position    = "sankey", na.rm = FALSE, show.legend = NA,
           order       = c("ascending", "descending", "as_is"),
           width       = "auto", align = c("bottom", "top", "center", "justify"),
           h_space     = "auto", v_space = 0,
           nudge_x     = 0,      nudge_y = 0,
           split_nodes = FALSE,  split_tol = 1e-3,
           direction   = c("forward", "backward"),
           inherit.aes = TRUE,   ...) {

    position <- .check_position_args(match.call(), environment())

    ggplot2::layer(
      geom     = GeomSankeysegment, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = list(na.rm = na.rm, ...)
    )
  }
