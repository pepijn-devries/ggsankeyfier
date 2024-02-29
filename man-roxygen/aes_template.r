#' @section Aesthetics:
#'
#'  \if{html}{\figure{important_aspects.svg}{Important aspects}}
#'
#'   * **`x`**: Works for variables on a discrete scale. Might work for continuous variables
#'     but is not guaranteed. This variable is used to distinguish between stages in the
#'     Sankey diagram on the x axis.
#'   * **`y`**: A continuous variable representing the width of the edges in a Sankey
#'     diagram.
#'   * **`group`**: A discrete variable used for grouping edges to nodes in each stage.
#'     Essentially it is an identifier for the nodes.
#'   * **`connector`**: Indicates which side of an edge is reflected by the corresponding
#'     record. Should be one of `"from"` or `"to"`.
#'   * **`edge_id`**: A unique identifier value for each edge. This identifier is used
#'     to link specific `"from"` and `"to"` records in an edge (flow).
#'   * fill: see `vignette("ggplot2-specs", "ggplot2")`
#'   * colour: see `vignette("ggplot2-specs", "ggplot2")`
#'   * linetype: see `vignette("ggplot2-specs", "ggplot2")`
#'   * linewidth: see `vignette("ggplot2-specs", "ggplot2")`
#'   * alpha: A variable to control the opacity of an element.
NULL
