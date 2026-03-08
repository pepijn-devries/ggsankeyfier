.compute_panel_statnodes_from_edgestats <- function(data) {
  dplyr::bind_rows(
    data |>
      dplyr::select(!dplyr::contains("end"), dplyr::any_of("PANEL")) |>
      dplyr::rename_with(\(x) gsub("_?end", "", x)),
    data |>
      dplyr::select(dplyr::contains("end"), dplyr::any_of("PANEL")) |>
      dplyr::rename_with(\(x) gsub("_?end", "", x))
  ) |>
    dplyr::select("PANEL", "group", "node_id", x = "x_raw", y = "y_node", node_size = "y_node") |>
    dplyr::distinct()
}

.compute_panel_statnodes <- function(self, data, params, scales) {

  if (missing(params)) params <- .setup_params_position(self, data)

  data |>
    .add_node_id() |>
    .group_across("PANEL", "x", "group", "connector", "node_id") |>
    dplyr::summarise(
      dplyr::across(!dplyr::any_of(c("y", "edge_id")), ~{
        if (is.numeric(.)) sum(.) else .[[1]]
      }),
      edge_id = list(.data$edge_id),
      y       = sum(.data$y), .groups = "keep") |>
    .group_across("PANEL", "x", "group") |>
    dplyr::mutate(node_size  = .data$y) |>
    dplyr::ungroup()
}

.add_node_id <- function(data) {
  data |>
    .group_across("x", "group") |>
    dplyr::mutate(node_id = dplyr::cur_group_id()) |>
    dplyr::ungroup()
}
