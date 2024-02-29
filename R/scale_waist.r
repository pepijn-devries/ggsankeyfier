#' Sankey edge waist line scales
#'
#' The waist scale can be used to control the waist (i.e., the width of the edge at its centre)
#' of edges in Sankey diagrams, in order to put emphasis on specific edges.
#'
#' This scale can be used to scale the centre of a Sankey edge. At one end of the scale
#' the edge will be shaped like an hour glass, at the other end it will be shaped as an
#' apple.
#'
#' @param range A `vector` of two `numeric` values used to scale the waist in between.
#' Should be `>= 0`.
#' @inheritParams ggplot2::scale_alpha_manual
#' @inheritParams ggplot2::scale_alpha_identity
#' @param ... arguments passed onto underpinning scale constructors.
#' @return Returns a [ggplot2::Scale] object which can be added to a [ggplot2::ggplot] to
#' control the waist of Sankey diagram edges.
#' @examples
#' library(ggplot2)
#' data("ecosystem_services")
#'
#' p <-
#'   ggplot(ecosystem_services_pivot2, aes(x = stage, y = RCSES, group = node,
#'                                         connector = connector, edge_id = edge_id,
#'                                         waist = RCSES)) +
#'   geom_sankeynode(v_space = "auto") +
#'   geom_sankeyedge(v_space = "auto", aes(fill = service_section))
#'
#' p + scale_waist_binned(range = c(0.1, 2))
#' p + scale_waist_binned(range = c(2, 0.1))
#' @name scale_waist_continuous
#' @rdname scale_waist
#' @author Pepijn de Vries
#' @export
scale_waist_continuous <- function(..., range = c(0, 1)) {
  rlang::warn(c("Legend is not really suitable for a continuous `waist` scale",
                i = "Apply a binned scale instead"))
  ggplot2::continuous_scale("waist", NULL, palette = scales::rescale_pal(range), ...)
}

#' @name scale_waist_datetime
#' @rdname scale_waist
#' @export
scale_waist_datetime <- function(..., range = c(0, 1)) {
  ggplot2::datetime_scale("waist", NULL, palette = scales::rescale_pal(range), ...)
}

#' @name scale_waist_binned
#' @rdname scale_waist
#' @export
scale_waist_binned <- function(..., range = c(0, 1)) {
  ggplot2::binned_scale("waist", NULL, palette = scales::rescale_pal(range), ...)
}

#' @name scale_waist_discrete
#' @rdname scale_waist
#' @export
scale_waist_discrete <- function(..., range = c(0, 1)) {
  ggplot2::discrete_scale("waist", NULL, palette = function(value) {
    seq(range[[1]], range[[2]], length.out = value)
  }, ...)
}

#' @name scale_waist_manual
#' @rdname scale_waist
#' @export
scale_waist_manual <- function (..., values = NULL, breaks = ggplot2::waiver()) {
  if (rlang::is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }
  if (is.vector(values) && is.null(names(values)) && !inherits(breaks, "waiver") &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }
  pal <- function(n) {
    if (n > length(values)) {
      rlang::abort(
        sprintf("Insufficient values in manual scale. %i needed but only %i provided.",
                n, length(values)))
    }
    values
  }
  ggplot2::discrete_scale("waist", NULL, palette = pal,
                          breaks = breaks, ...)
}

#' @name scale_waist_identity
#' @rdname scale_waist
#' @export
scale_waist_identity <- function (..., guide = "none") {
  ggplot2::continuous_scale("waist", palette = function(x) x,
                            ..., guide = guide, super = ggplot2::ScaleContinuousIdentity)
}
