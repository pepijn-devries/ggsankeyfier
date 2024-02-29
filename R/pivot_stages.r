#' Pivot wide data to long for plotting as Sankey
#'
#' Pivot data from a wide to a long format suitable for plotting Sankey diagrams.
#'
#' Typically, data to be displayed as a Sankey, is collected and stored in a
#' wide format, where each stage (i.e., x-axis of a Sankey diagram) is in a
#' column. The `ggplot2` philosophy requires the data to be in a long format,
#' such that diagram decorations (aesthetics) can be mapped to specific
#' columns.
#'
#' This function pivots wide data in an appropriate long format, by indicating
#' which columns contain the stages, and in which order they should appear in
#' the Sankey.
#'
#' For more details see `vignette("data_management")`
#'
#' @param data A `data.frame` (or an object inheriting the `data.frame` class),
#' which needs to be pivoted.
#' @param stages_from A `vector` of column names, which represent the stages.
#' @param values_from A `vector` of column names, which contains `numeric` values
#' that represent the size of the edges in Sankey diagrams. When there are multiple
#' values for a single edge, they are summed.
#' @param additional_aes_from A `vector` of column names of data that you want to
#' use to decorate elements in your Sankey diagram. This argument is optional. See also
#' `vignette("data_management")` and `vignette("decorating")`.
#' @param invert_nodes When pivoting information from `stages_from`, its data is
#' converted into a `factor`. Set `invert_nodes` to `TRUE` if you want to invert the
#' order of the levels of the `factor`.
#' @return Returns a [dplyr::tibble] with all the selected columns from `data` pivoted.
#' The stages will be listed in the column named `stage` and nodes in the column named
#' `node`. The result will contain two new columns: a column named `connector` indicating
#' whether the row in the `tibble` reflects the source of an edge (value `'from'`) or
#' destination of an edge (value `'to'`); and a column named `edge_id`, containing a
#' unique identifier for each edge. The `edge_id` is required for the plotting routine
#' in order to identify which edge source should be connected with which edge destination.
#' @rdname pivot_stages_longer
#' @name pivot_stages_longer
#' @examples
#' data("ecosystem_services")
#'
#' ecosystem_services_p1 <-
#'   pivot_stages_longer(
#'     data        = ecosystem_services,
#'     stages_from = c("activity_type", "pressure_cat",
#'                     "biotic_group", "service_division"),
#'     values_from = "RCSES")
#'
#' ## suppose we want to decorate our Sankey
#' ## with information on the 'section' of the services:
#' ecosystem_services_p2 <-
#'   pivot_stages_longer(
#'     data        = ecosystem_services,
#'     stages_from = c("activity_type", "pressure_cat",
#'                     "biotic_group", "service_division"),
#'     values_from = "RCSES",
#'     additional_aes_from = "service_section")
#'
#' @author Pepijn de Vries
#' @export
pivot_stages_longer <-
  function(data, stages_from, values_from, additional_aes_from, invert_nodes = FALSE) {

    if (missing(additional_aes_from)) additional_aes_from <- character(0)

    result <-
      data |>
      dplyr::ungroup() |>
      dplyr::select(union(union(stages_from, values_from), additional_aes_from)) |>
      dplyr::mutate(dplyr::across(dplyr::any_of(stages_from), ~ {
        if (is.factor(.)) . else {
          factor(., unique(.))
        }
      }))

    lvls <- unname(unlist(lapply(result[,stages_from, drop = FALSE], levels)))
    lvls <- lvls[!duplicated(lvls)]
    if (invert_nodes) lvls <- rev(lvls)

    result <-
      lapply(utils::head(seq_along(stages_from), -1), function(i) {
        result |>
          dplyr::select(stages_from[i + (0:1)]) |>
          dplyr::rename(node_from = stages_from[i], node_to = stages_from[i + 1]) |>
          dplyr::bind_cols(
            result |> dplyr::select(union(values_from, additional_aes_from))
          ) |>
          dplyr::group_by(dplyr::across(dplyr::any_of(c("node_from", "node_to", additional_aes_from)))) |>
          dplyr::summarise(
            dplyr::across(dplyr::any_of(values_from), ~sum(.)),
            .groups = "keep") |>
          dplyr::mutate(
            stage_from = factor(stages_from[i], stages_from),
            stage_to   = factor(stages_from[i + 1], stages_from)
          )
      }) |>
      dplyr::bind_rows() |>
      dplyr::ungroup() |>
      dplyr::mutate(edge_id = dplyr::row_number()) |>
      dplyr::rename(node = "node_from", stage = "stage_from") |>
      tidyr::nest(from = c("node", "stage")) |>
      dplyr::rename(node = "node_to", stage = "stage_to") |>
      tidyr::nest(to = c("node", "stage")) |>
      tidyr::pivot_longer(c("from", "to"), names_to = "connector") |>
      tidyr::unnest("value") |>
      dplyr::mutate(node = factor(as.character(.data[["node"]]), lvls))

    return(result)
  }
