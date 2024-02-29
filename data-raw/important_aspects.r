library(ggsankeyfier)
library(ggplot2)

pos <- ggsankeyfier::position_sankey(
  v_space = "auto", order = "descending", align = "justify")
p <-
  ggplot(ecosystem_services_pivot1,
         aes(x = stage, y = RCSES, group = node,
             connector = connector, edge_id = edge_id, label = node))
edge <- geom_sankeyedge(aes(fill = stage),
                        position = pos,
                        alpha = 0.5)
node <- geom_sankeynode(aes(fill = node),
                        position = pos,
                        alpha = 0.5)

p_build <- ggplot_build(p + edge + node)

data_edge <-
  p_build$data[[1]] |>
  ungroup()

data_edge_sel <-
  data_edge |>
  filter(label == "Fish & Cephalopds") |>
  filter(y == min(y)) |>
  mutate(x_center = (x + xend)/2,
         y_center = (y + yend)/2,
         label2 = "c")

data_node <-
  p_build$data[[2]] |>
  ungroup()

data_node_sel <-
  data_node |>
  filter(label == "Provisioning") |>
  filter(y == min(y)) |>
  mutate(label2 = "n")

connectors <-
  data_edge |>
  filter(x == min(x) & y == sort(y[x == min(x)])[2]) |>
  rename(x_from = x, x_to = xend, y_from = y, y_to = yend) |>
  tidyr::pivot_longer(c("x_from", "x_to", "y_from", "y_to"),
                      names_sep = "_", names_to = c(".value", "which"))

arrows <-
  data_edge |>
  filter(x == min(x) & y == min(y[x == min(x)])) |>
  slice(c(1, 1)) |>
  mutate(y = y[[1]] + 0.95*c(-1, 1)*edge_size[[1]]/2,
         x = x + 0.1)

circle <- dplyr::tibble(
  rad = seq(-pi, pi, length.out = 25),
  x_circ = -cos(rad + pi/2), y_circ = sin(rad + pi/2),
  left = rad <= 0) |>
  mutate(
    rep = 1 + abs(diff(c(left, FALSE)))
  ) |>
  slice(rep(seq(n()), rep)) |>
  mutate(rep = cumsum(rep - 1),
         left = ifelse(rep == 2, FALSE, left)) |>
  select(-rep)

circles <-
  bind_rows(
    data_edge_sel |> select(x = x_center, y = y_center, size = edge_size) |>
      mutate(id = "Edge\n(aes: edge_id)"),
    data_node_sel |> select(x, y, size = node_size) |> mutate(id = "Node\n(aes: group)"),
    tibble(x = unique(data_node$x)[[2]],
           size = max(data_node$y[data_node$x == x]) +
             data_node$node_size[data_node$x == x & data_node$y ==
                                   max(data_node$y[data_node$x == x])]/2, y = size/2,
           id = "Stage\n(aes: x)")
  ) |>
  tidyr::expand_grid(circle) |>
  mutate(x = x + 0.025*x_circ*(size)^(1/4), y = y + y_circ*(size + 0.15)/2) |>
  arrange(id, left, rad)

labs <- circles |>
  group_by(id) |>
  summarise(x = x[which(y == min(y))[[1]]], y = min(y), .groups = "keep")

x_lab_off <- 0.1
y_lab_off <- -0.15

p2 <-
  p +
  theme_light() +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text  = element_blank()) +
  scale_x_discrete(expand = expansion(add = c(.1, .5))) +
  scale_y_continuous(expand = expansion(add = c(.5, .1))) +
  labs(x = NULL, y = NULL) +
  geom_path(aes(x = x, y = y, group = id), data = circles |> filter(!left),
            col = "red", alpha = 0.5,
            inherit.aes = FALSE) +
  edge + node +
  geom_path(aes(x = x, y = y, group = id), data = circles |> filter(left),
            col = "red", lwd = 1.5,
            inherit.aes = FALSE) +
  geom_segment(aes(x = x, xend = x + x_lab_off, y = y, yend = y + y_lab_off),
               data = labs, inherit.aes = F, col = "red") +
  geom_text(aes(x = x, y = y, label = id), data = labs, inherit.aes = FALSE,
            hjust = 0, vjust = 1, position = position_nudge(x_lab_off + .02, y_lab_off),
            size = 3) +
  geom_path(aes(x = x, y = y), data = arrows, inherit.aes = FALSE,
            col = "red", arrow = arrow(type = "closed", ends = "both",
                                       length = unit(0.02, "npc"))) +
  geom_text(aes(x = x, y = y, label = label),
    data = tibble(x = arrows$x[[1]], y = mean(arrows$y),
                  label = "Edge size\n(aes: y)"),
            nudge_x = 0.05, size = 3, hjust = 0, inherit.aes = FALSE) +
  geom_segment(aes(x = x + c(0.02, -0.02),
                   y = y, xend = x + c(0.1, -0.1), yend = y + 0.1),
               data = connectors, inherit.aes = FALSE, col = "red") +
  geom_point(aes(x = x + c(0.02, -0.02), y = y), data = connectors,
             col = "red", inherit.aes = FALSE) +
  geom_text(aes(x = x + c(0.02, -0.02), y = y, label =
                  paste("aes:\nconnector=\n", c("\"from\"", "\"to\""))),
            data = connectors, inherit.aes = FALSE, size = 3,
            nudge_x = c(0.05, -0.05), nudge_y = 0.1, vjust = 0, hjust = c(0, 1))

ggsave(file.path("man", "figures", "important_aspects.svg"), p2,
       width = unit(5, "in"), height = unit(3, "in"))
