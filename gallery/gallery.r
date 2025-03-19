library(ggplot2)
library(ggsankeyfier)
theme_set(theme_light())

es_sub <-
  ecosystem_services |>
  subset(RCSES > 0.005) |>
  pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"),
                      "RCSES", "service_section")

pos     <- position_sankey(v_space = "auto", order = "ascending", align = "justify")
pos_txt <- position_sankey(v_space = "auto", order = "ascending", align = "justify",
                           nudge_x = -.1)

p <-
  ggplot(
    data    = es_sub,
    mapping = aes(x = stage, y = RCSES, group = node,
                  edge_id = edge_id, connector = connector)) +
  geom_sankeyedge(aes(fill = RCSES), position = pos) +
  geom_sankeynode(position = pos) +
  scale_fill_viridis_c(option = "turbo") +
  geom_text(aes(label = node), stat = "sankeynode", pos = pos_txt,
            hjust = 1, angle = -30, cex = 2.5) +
  scale_x_discrete(expand = expansion(add = c(0.65, 0.15))) +
  scale_y_continuous(expand = expansion(add = c(0.15, 0.15))) +
  labs(fill = "risk\nlevel", y = "risk level")

ggsave("gallery/pepijn-devries-ggsankeyfier.png", print(p), width = 350, height = 300, units = "px",
       scale = 5)
