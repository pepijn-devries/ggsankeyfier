.node_summary <- function(self, data) {

  if (!"node_id" %in% names(data) || all(!c("node_size", "y_node_size") %in% names(data))) {
    data |>
      .add_node_id() |>
      tidyr::pivot_longer(dplyr::any_of(c("node_id", "node_id_end")),
                          names_to = "which", values_to = "node_id") |>
      dplyr::mutate(x_fix = ifelse(.data$which == "node_id", .data[["x"]], .data[["xend"]])) |>
      .group_across("PANEL", "x_fix", "node_id", "which") |>
      dplyr::summarise(node_size = sum(.data[["y"]])) |>
      .group_across("PANEL", "node_id") |>
      dplyr::mutate(is_max = max(.data[["node_size"]]) == .data[["node_size"]]) |>
      dplyr::select(-"which") |>
      dplyr::filter(.data[["is_max"]]) |>
      dplyr::distinct() |>
      dplyr::rename(x = "x_fix") |>
      .group_across("PANEL", "x") |>
      dplyr::summarise(n_nodes = dplyr::n(), max_size = sum(.data[["node_size"]]))
  } else if (!"node_size" %in% names(data)) {
    dplyr::bind_rows(
      data |> dplyr::select(c("PANEL", "x", "node_id",
                               node_size = "y_node_size")),
      data |> dplyr::select(c("PANEL", x = "xend", node_id = "node_id_to",
                               node_size = "yend_node_size"))
    ) |>
      dplyr::distinct() |>
      .group_across("PANEL", "x") |>
      dplyr::summarise(n_nodes = dplyr::n(), max_size = sum(.data[["node_size"]])) |>
      dplyr::ungroup()
  } else {
    data |>
      dplyr::select(c("PANEL", "x", "connector", "node_id", "node_size")) |>
      dplyr::distinct() |>
      .group_across("PANEL", "x", "node_id") |>
      dplyr::summarise(node_size = max(.data[["node_size"]])) |>
      .group_across("PANEL", "x") |>
      dplyr::summarise(n_nodes = dplyr::n(), max_size = sum(.data[["node_size"]])) |>
      dplyr::ungroup()
  }
}

.auto_v_space <- function(self, data) {
  node_summary <- .node_summary(self, data)
  y_max        <- max(node_summary$max_size)
  (y_max*0.05)
}

.auto_h_space <- function(w, data) {
  w <- w %||% .auto_width(data)
  w/2
}

.auto_width <- function(data) {
  (ggplot2::resolution(data$x, zero = FALSE)*0.1)
}

.setup_params_position <-

  function(self, data) {

    w <- self$width       %||% .auto_width(data)
    result <- list(
      width       = w,
      align       = self$align       %||% "bottom",
      order       = self$order       %||% "ascending",
      h_space     = self$h_space     %||% .auto_h_space(w, data),
      v_space     = self$v_space     %||% .auto_v_space(self, data),
      split_nodes = self$split_nodes %||% FALSE,
      split_tol   = self$split_tol   %||% 1e-3,
      direction   = self$direction   %||% "forward",
      nudge_x     = self$nudge_x     %||% 0,
      nudge_y     = self$nudge_y     %||% 0
    )

    result$width[result$width == "auto"]     <- .auto_width(data)
    result$width   <- as.numeric(result$width)
    result$h_space[result$h_space == "auto"] <- .auto_h_space(result$width, data)
    result$h_space <- as.numeric(result$h_space)
    result$v_space[result$v_space == "auto"] <- .auto_v_space(self, data)
    result$v_space <- as.numeric(result$v_space)
    return(result)
  }

.compute_layer_positions <- function(self, data, params, scales) {
  ## The stat_sankeyedge function will add an xend column to the data.
  ## The stat_sankeynode won't. We can use this difference
  ## to tell which positioning method to apply:
  if ("xend" %in% names(data)) {
    .compute_layer_edge_positions(self, data, params, scales)
  } else {
    .compute_layer_node_positions(self, data, params, scales) |>
      dplyr::filter(!.data[["duplicated"]])
  }
}

.order_edges <- function(data, order, which = "start") {
  data <- data |>
    .group_across("PANEL",
                  ifelse(which == "start", "x_raw", "xend_raw"),
                  ifelse(which == "start", "group", "group_end")) |>
    dplyr::mutate(temp = {
      if (order == "ascending") {
        dplyr::dense_rank(if (which == "start") .data[["y"]] else .data[["yend"]])
      } else if (order == "descending") {
        dplyr::dense_rank(if (which == "start") -.data[["y"]] else -.data[["yend"]])
      } else {
        seq_len(dplyr::n())
      }
    }) |>
    dplyr::ungroup()
  if (which == "start") {
    data <- data |>
      dplyr::arrange(.data[["PANEL"]], .data[["x_raw"]], .data[["group"]], .data[["temp"]]) |>
      dplyr::rename(edge_order = "temp")
  } else {
    data <- data |>
      dplyr::arrange(.data[["PANEL"]], .data[["xend_raw"]], .data[["group_end"]], .data[["temp"]]) |>
      dplyr::rename(edge_order_end = "temp")
  }
  return(data)
}

.order_nodes <- function(data, order) {
  data |>
    .group_across("PANEL", "x") |>
    dplyr::mutate(
      node_order = {
        if(order == "ascending") {
          dplyr::row_number(.data[["align_offset"]])
        } else if (order == "descending") {
          dplyr::row_number(-.data[["align_offset"]])
        } else {
          seq_len(dplyr::n())
        }
      }
    ) |>
    dplyr::arrange(.data[["PANEL"]], .data[["x"]], .data[["node_order"]])
}

.compute_layer_node_positions <-
  function(self, data, params, scales) {
    if (missing(params)) params <- .setup_params_position(self, data)
    data <- data |> .add_node_id()
    rhs <- .group_across(data, "PANEL", "x", "group") |>
      dplyr::summarise(align_offset = max(.data[["y"]]), .groups = "keep") |>
      .order_nodes(params$order)

    data |>
      dplyr::left_join(.stage_params(data, params), "x") |>
      dplyr::left_join(rhs |>
                         .group_across("PANEL", "x") |>
                         dplyr::mutate(y_cum        = cumsum(.data[["align_offset"]]) -
                                         .data[["align_offset"]]/2,
                                       n_nodes      = dplyr::n_distinct(.data[["group"]]),
                                       ytot         = sum(.data[["align_offset"]]),
                                       align_offset = .data[["ytot"]] +
                                         (.data[["n_nodes"]] - 1)*params$v_space),
                       c("PANEL", "x", "group")) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        y    = .data[["y_cum"]],
        ymin = .data[["y"]] - .data[["node_size"]]/2,
        ymax = .data[["y"]] + .data[["node_size"]]/2,
        xmin = .data[["x"]],
        xmax = .data[["x"]]) |>
      dplyr::select(-"y_cum") |>
      .group_across("PANEL") |>
      dplyr::mutate(
        v_space = if (params$align == "justify") {
          (max(.data[["align_offset"]]) - .data[["ytot"]])/ifelse(.data[["n_nodes"]] > 1,
                                                                  .data[["n_nodes"]] - 1, 1)
        } else params$v_space,
        align_offset = switch(
          params$align,
          bottom  =  0,
          top     =  max(.data[["align_offset"]]) - .data[["align_offset"]],
          center  = (max(.data[["align_offset"]]) - .data[["align_offset"]])/2,
          justify =  0, 0)
      ) |>
      .group_across("PANEL", "x", "node_id") |>
      dplyr::mutate(
        dissimilar = if (dplyr::n() == 1 || max(.data[["node_size"]]) == 0) FALSE else
          (max(abs(diff(.data[["node_size"]])))/max(.data[["node_size"]])) > .data$split_tol[[1]],
        split      = .data$split_nodes[[1]] | .data[["dissimilar"]],
        duplicated = !.data[["split"]] & duplicated(.data[["node_id"]]) & !.data[["dissimilar"]],
        v_space    = max(.data[["v_space"]])
      ) |>
      .group_across("PANEL", "x", "connector") |>
      dplyr::mutate(
        y_offset = (.data[["node_order"]] - 1)*.data[["v_space"]][[1]] +
          .data[["align_offset"]],
        x_offset = ifelse(.data[["split"]], .data[["h_space"]]*
                            ifelse(.data[["connector"]] == "from", .5, -.5), 0),
        y        = .data[["y"]]    + .data[["y_offset"]] + params$nudge_y,
        ymin     = .data[["ymin"]] + .data[["y_offset"]] + params$nudge_y,
        ymax     = .data[["ymax"]] + .data[["y_offset"]] + params$nudge_y,
        x        = .data[["x"]] + .data[["x_offset"]] + params$nudge_x,
        xmin     = .data[["xmin"]] - .data[["width"]]/ifelse(.data[["split"]], 4, 2) +
          .data[["x_offset"]] + params$nudge_x,
        xmax     = .data[["xmax"]] + .data[["width"]]/ifelse(.data[["split"]], 4, 2) +
          .data[["x_offset"]] + params$nudge_x
      )
  }

.stage_params <- function(data, params) {
  n_stages <- dplyr::n_distinct(data$x)
  n_splitn <- length(params$split_nodes)
  n_splitt <- length(params$split_tol)
  n_hsp    <- length(params$h_space)
  n_w      <- length(params$width)
  data.frame(
    x           = sort(unique(data$x)),
    split_nodes =
      params$split_nodes[rep(seq_len(n_splitn), times = ceiling(n_stages/n_splitn))][1:n_stages],
    split_tol   =
      params$split_tol[rep(seq_len(n_splitt), times = ceiling(n_stages/n_splitt))][1:n_stages],
    h_space     =
      params$h_space[rep(seq_len(n_hsp), times = ceiling(n_stages/n_hsp))][1:n_stages],
    width       =
      params$width[rep(seq_len(n_w), times = ceiling(n_stages/n_w))][1:n_stages]
  )
}

.compute_layer_edge_positions <-
  function(self, data, params, scales) {
    params <- .setup_params_position(self, data)

    nodes <- dplyr::bind_rows(
      data |>
        dplyr::select(c("PANEL", "x", "y", "group", "edge_id")) |>
        dplyr::mutate(connector = "from"),
      data |>
        dplyr::select(c("PANEL", x = "xend", y = "yend", group = "group_to", "edge_id")) |>
        dplyr::mutate(connector = "to")
    ) |>
      dplyr::filter(!(is.na(.data[["x"]]) & is.na(.data[["y"]])))
    nodes <- .compute_panel_statnodes(self, nodes, params, scales)
    nodes <- .compute_layer_node_positions(self, nodes, params) |>
      dplyr::ungroup() |>
      dplyr::mutate(x_node = .data[["x"]], x_raw = .data[["x"]] - .data[["x_offset"]]) |>
      tidyr::unnest("edge_id") |>
      dplyr::select(c("PANEL", "connector", "edge_id", "x_node", "x_raw", "split", "width",
                      y_node = "y", y_node_size = "node_size"))

    data |>
      dplyr::rename_with(~gsub("_to$", "_end", .), dplyr::ends_with("_to")) |>
      # dplyr::rename(group_end = "group_to") |>
      dplyr::mutate(connector_end = "to") |>
      dplyr::left_join(nodes, by = c("PANEL", "connector", "edge_id")) |>
      dplyr::left_join(
        nodes |>
          dplyr::filter(.data[["connector"]] == "to") |>
          dplyr::mutate(connector = "from") |>
          dplyr::rename(xend_node = "x_node", xend_raw = "x_raw", splitend = "split", widthend = "width",
                        yend_node = "y_node", yend_node_size = "y_node_size"),
        by = c("PANEL", "connector", "edge_id")) |>
      .order_edges(params$order, "start") |>
      .group_across("PANEL", "x", "group") |>
      dplyr::mutate(
        edge_size = .data[["y"]],
        x         = .data[["x_node"]] + .data[["width"]]/ifelse(.data[["split"]], 4, 2),
        y         = cumsum(.data[["y"]]) - .data[["y"]]/2 + .data[["y_node"]] - .data[["y_node_size"]]/2) |>
      .order_edges(params$order, "end") |>
      .group_across("PANEL", "xend", "group_end") |>
      dplyr::mutate(
        edge_end_size  = .data[["yend"]],
        xend           = .data[["xend_node"]] - .data[["widthend"]]/ifelse(.data[["splitend"]], 4, 2),
        yend           = cumsum(.data[["yend"]]) - .data[["yend"]]/2 + .data[["yend_node"]] - .data[["yend_node_size"]]/2) |>
      .swap_ends_if(params$direction == "backward")
  }

.swap_ends_if <- function(data, condition) {
  if (condition) {
    columns_to_swap_a <- names(data)[grepl("end", names(data))]
    columns_to_swap_a_temp <- paste0(columns_to_swap_a, "temp")
    columns_to_swap_b <- gsub("end|_end", "", columns_to_swap_a)
    data |>
      dplyr::rename_with(~columns_to_swap_a_temp, columns_to_swap_a) |>
      dplyr::rename_with(~columns_to_swap_a, columns_to_swap_b) |>
      dplyr::rename_with(~columns_to_swap_b, columns_to_swap_a_temp)
  } else {
    data
  }
}

.check_position_args <- function(call_match, args) {
  call_match <- as.list(call_match)
  args       <- as.list(args)
  pos_args   <- methods::formalArgs("position_sankey")
  pos_args   <- pos_args[pos_args != "..."]

  if (any(names(call_match) %in% pos_args)) {
    if ("position" %in% names(call_match)) {
      rlang::abort(c(
        "Both `position` and arguments to `position_sankey` are specified",
        i = "Specify either of the arguments not both"))
    } else {
      return(do.call(position_sankey, args[names(args) %in% pos_args]))
    }
  } else {
    return(args[["position"]])
  }
}
