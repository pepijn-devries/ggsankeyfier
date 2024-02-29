#' @name StatSankeyedge
#' @rdname stat_sankey
#' @include position_helpers.r
#' @export
StatSankeyedge <-
  ggplot2::ggproto(
    "StatSankeyedge", ggplot2::Stat,
    compute_panel = function(self, data, scales, params) {

      data |>
        .add_node_id() |>
        .group_across("PANEL", "x", "connector") |>
        tidyr::pivot_wider(id_cols = c("PANEL", "edge_id"), names_from = "connector",
                           values_from = -c("PANEL", "edge_id", "connector")) |>
        dplyr::rename(x = "x_from", xend = "x_to", y = "y_from", yend = "y_to",
                      node_id = "node_id_from", node_id_end = "node_id_to") |>
        dplyr::rename_with(~{gsub(sprintf("_%s$", "from"), "", .)}) |>
        dplyr::mutate(connector = "from") |>
        dplyr::ungroup()
    },

    required_aes = c("x", "y", "group", "connector", "edge_id"),
    optional_aes = "waist"
  )

#' @name stat_sankeyedge
#' @rdname stat_sankey
#' @export
stat_sankeyedge <-
  function(mapping = NULL, data = NULL, geom = "sankeyedge",
           position = "sankey", na.rm = FALSE, slope = 0.5, ncp = 100,
           show.legend = NA, inherit.aes = TRUE, ...) {

    ggplot2::layer(
      stat     = StatSankeyedge, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = list(na.rm = na.rm, slope = slope, ncp = ncp, ...)
    )
  }
