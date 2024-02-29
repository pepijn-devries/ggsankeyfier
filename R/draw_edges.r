.draw_edges <- function(self, data, panel_params, coord, params) {

  if (!"waist" %in% names(data)) data$waist <- 1
  data <- data |>
    dplyr::mutate(
      res = {
        resolution <- .data[["xend"]] - .data[["x"]]
        resolution[resolution < 0 & resolution > -1] <- -1
        resolution[is.na(resolution)] <- 0
        resolution[abs(resolution) < 0.25] <-
          ifelse(resolution[abs(resolution) < 0.25] >= 0, .25, -.25)
        resolution <-
          coord$transform(rbind(data.frame(x = 0, y = 0),
                                cbind(x = resolution, y = 0)), panel_params)
        resolution[-1, "x"] - resolution[1, "x"]
      }
    )

  # convert from the coordinate system of the original data
  # to Normalised Parent Coordinates (npc) of the plot:

  # Note that edge_end_size is currently ignored.
  data$edge_size <-
    coord$transform(
      dplyr::bind_cols(x = 0, y = data[["edge_size"]]), panel_params)[["y"]] -
    coord$transform(
      dplyr::bind_cols(x = 0, y = rep(0, nrow(data))), panel_params)[["y"]]
  # For very narrow edges take a small value, to avoid errors (it will not be visible to
  # the eye anyway)
  data$edge_size[data$edge_size < 1e-3] <- 1e-3

  data[,c("x", "y")]       <- coord$transform(data[,c("x", "y")], panel_params)
  data[,c("xend", "yend")] <- coord$transform(data[,c("xend", "yend")], panel_params)

  ## aspect ratio correction
  asp_cor <-
    as.numeric(grid::convertHeight(grid::unit(1, "npc"), "npc")) /
      as.numeric(grid::convertHeight(grid::unit(1, "snpc"), "npc"))

  if (asp_cor > 1) rlang::warn(
    c("I'm trying to correct aspect ratio for narrow plot",
      i = "Try widening your plotting device."))

  result <-
    data |>
    dplyr::mutate(
      bez =
        mapply(
          function(x, y, xend, yend, y_size, slope, ncp, fill, colour, linetype, linewidth,
                   alpha, waist, res, connector) {
            gp <- grid::gpar(fill = fill, col = colour,
                             lwd = linewidth*ggplot2::.pt, lty = linetype, alpha = alpha)
            if (is.na(x) || is.na(xend)) return(grid::nullGrob())
            slope2 <- res*slope

            is_feedback <- ifelse(connector == "to", -1, 1)*(xend - x) <= 0
            if (is_feedback) {
              # Note that one might want to change this to node size (instead of edge size):
              top <- max(c(y, yend)) + 1.5*y_size
              between <- (x + xend)/2

              feedback <-
                data.frame(
                  x = c(
                    x - c(0, slope2/4, slope2/2, slope2/2, slope2/2, slope2/2, slope2/4, 0),
                    xend + c(0, slope2/4, slope2/2, slope2/2, slope2/2, slope2/2, slope2/4, 0)
                  ),
                  y = c(
                    y + c(0, 0, (top - y)/4, (top - y)/2, (top - y)/2, 3*(top - y)/4, (top - y), (top - y)),
                    yend + c((top - yend), (top - yend), 3*(top - yend)/4, (top - yend)/2,
                             (top - yend)/2, (top - yend)/4, 0, 0)
                  ),
                  segment = rep(1:4, each = 4)
                ) |>
                dplyr::group_by(.data$segment) |>
                dplyr::reframe(
                  bez =
                    gridBezier::BezierGrob(x, y, default.unit = "in",
                                           stepFn = gridBezier::nSteps(ncp)) |>
                    gridBezier::BezierPoints() |>
                    as.data.frame()
                )
              feedback <- .interpolate_equidistance(feedback$bez$x, feedback$bez$y, ncp)
              vwline::vwlineGrob(x = feedback$x, y = feedback$y, gp = gp,
                w =
                  stats::spline(
                  c(0, .5, 1),
                  c(1, waist, 1)*y_size*asp_cor,
                  n = ncp)[["y"]]
              )
            } else {
              vwline::offsetBezierGrob(
                x      = grid::unit(c(x, x + slope2, xend - slope2, xend), "npc"),
                y      = grid::unit(c(y, y, yend, yend), "npc"),
                w      = grid::unit(c(1, waist, 1)*y_size, "npc")*asp_cor,
                stepFn = gridBezier::nSteps(ncp),
                gp     = gp
              )
            }
          },
          x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]],
          yend = .data[["yend"]], y_size = .data[["edge_size"]], slope = .data[["slope"]],
          ncp = .data[["ncp"]],
          fill = .data[["fill"]], colour = .data[["colour"]], linetype = .data[["linetype"]],
          linewidth = .data[["linewidth"]],
          alpha = .data[["alpha"]], waist = .data[["waist"]], res = .data[["res"]],
          connector = .data[["connector"]],
          SIMPLIFY = F)
    ) |> dplyr::pull("bez")

  return(do.call(grid::gList, result))
}

.interpolate_equidistance <- function(x, y, ncp) {
  dist <-
    c(0, cumsum(sqrt(
      (utils::head(x, -1) - utils::tail(x, -1))^2 +
        (utils::head(y, -1) - utils::tail(y, -1))^2
    )))
  dist_out <- seq(0, max(dist), length.out = ncp)
  data.frame(
    x = stats::approx(dist, x, dist_out, ties = "ordered")[["y"]],
    y = stats::approx(dist, y, dist_out, ties = "ordered")[["y"]]
  )
}
