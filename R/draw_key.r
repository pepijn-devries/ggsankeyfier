#' @inherit ggplot2::draw_key_blank
#' @examples
#' ## The key glyph for sankey diagrams can be applied to different geoms as well.
#' ## In the example below it is applied to a histogram
#'
#' library(ggplot2)
#' ggplot(data.frame(x = rnorm(100), groups = rep(letters[1:2], 2)),
#'        aes(x = x, fill = groups)) +
#'   geom_histogram(key_glyph = draw_key_sankeyedge, binwidth = 0.2, alpha = 1)
#' ggplot(data.frame(x = rnorm(100), groups = rep(letters[1:2], 2)),
#'        aes(x = x, fill = groups)) +
#'   geom_histogram(key_glyph = draw_key_sankeynode, binwidth = 0.2)
#'
#' @name draw_key_sankeyedge
#' @rdname draw_key
#' @author Pepijn de Vries
#' @export
draw_key_sankeyedge <- function(data, params, size) {
  data$linewidth <- data$linewidth %||% 0.5
  lwd    <- min(data$linewidth, min(size)/4)
  fill   <- grDevices::adjustcolor(data$fill %||% "grey20", alpha.f = data$alpha)
  margin <- grid::unit(lwd, "mm")/2
  left   <- grid::unit(.25, "npc")
  right  <- grid::unit(1, "npc") - margin
  half   <- grid::unit(0.5, "npc")
  height <- half - margin

  vwline::offsetXsplineGrob(
    grid::unit.c(left, (left + right)/2, right), rep(half, 3),
    w = grid::unit(c(.5, 0.5*(data$waist %||% 1), .5), "npc"),
    gp = grid::gpar(col = data$colour %||% NA, fill = fill, lty = data$linetype %||%
                      1, lwd = lwd * ggplot2::.pt, linejoin = params$linejoin %||%
                      "mitre", lineend = params$lineend %||% "butt"))
}

#' @name draw_key_sankeynode
#' @rdname draw_key
#' @export
draw_key_sankeynode <- function(data, params, size) {
  data$linewidth <- data$linewidth %||% 0.5
  data$alpha     <- data$alpha %||% 1
  data$alpha[is.na(data$alpha)] <- 1
  lwd    <- min(data$linewidth, min(size)/4)
  fill   <- grDevices::adjustcolor(data$fill %||% "grey20", alpha.f = data$alpha)
  margin <- grid::unit(lwd, "mm")/2
  width  <- grid::unit(.25, "npc") - margin
  height <- grid::unit(1, "npc") - margin

  grid::rectGrob(margin + width/2, margin + height/2, width, height, gp =
                   grid::gpar(col = data$colour %||% NA, fill = fill, lty = data$linetype %||%
                                1, lwd = lwd * ggplot2::.pt, linejoin = params$linejoin %||%
                                "mitre", lineend = params$lineend %||% "butt"))
}
