library(dplyr)
library(gganimate)
library(ggplot2)
library(gridBezier)
library(ragg)
library(vwline)

x <- grid::unit(c(0.001, .25, 0.25, .499), "in")
y <- grid::unit(c(0, 0, 1, 1), "in")
width <- unit(c(.1), "in")
pts <- BezierGrob(
  x, y, default.unit = "in",
  stepFn = gridBezier::nSteps(100)) |>
  gridBezier::BezierPoints() |>
  lapply(as.numeric) |>
  as.data.frame()
pts <- bind_rows(
  data.frame(x = 0, y = 0),
  pts,
  data.frame(x = 0.5, y = 1),
) |>
  mutate(w = width, frame = row_number())

labels <- data.frame(x = 0, y = 1, text = c("Perpendicular\nbrush", "Vertical\nbrush"),
                     what = c("ggsankeyfier", "other packages"))

i_frames <- 3L:100L
frames <- lapply(3L:100L, \(fr) {
  p_slice <- slice_head(pts, prop = fr/max(i_frames))
  lapply(list("perp", pi/2), \(an) {
    lapply(list(p_slice$y, rep(0, nrow(p_slice))), \(dat) {
      result <-
        vwline::vwXsplineGrob(
          p_slice$x, dat, p_slice$w, angle = an, default.units = "in") |>
        grobCoords()
      result[[1]][[1]] |> unclass() |> as.data.frame() |>
        mutate(what  = ifelse(an == "perp", "ggsankeyfier", "other packages"),
               frame = fr,
               ref   = ifelse(all(dat == 0), "Reference", "Curve") |>
                 factor(c("Reference", "Curve")))
    }) |>
      bind_rows()
  }) |>
    bind_rows()
}) |> bind_rows()

anim <-
  ggplot(frames |> mutate(ref = factor(ref, c("Reference", "Curve"))) |>
           arrange(ref)) +
  geom_polygon(aes(x = x, y = y, fill = ref)) +
  geom_segment(aes(x    = x,
                   xend = x,
                   y    = y - as.numeric(width)/2,
                   yend = y + as.numeric(width)/2,
  ), data = pts[i_frames,], col = "red") +
  geom_text(aes(x = x, y = y, label = text), data = labels, hjust = 0) +
  lims(x = c(0, 0.5), y = c(-.06, 1.06)) +
  coord_fixed() +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_light() +
  theme(axis.text = element_blank(), legend.position = "top") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~what) +
  transition_manual(frame)

anim <- animate(anim, device = "ragg_png", fps = 30,
                width = 400, height = 500,
                nframes = length(i_frames), res = 100)
save_animation(anim, "data-raw/brushangle.gif")
