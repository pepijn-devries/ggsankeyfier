test_that(
  "Feedback loop is visualised correctly", {
    feed_back <-
      data.frame(x = "A", y = 1, group = "A", connector = c("from", "to"), edge_id = 1)

    feedback_plot <-
      ggplot2::ggplot(
        feed_back,
        ggplot2::aes(x = x, y = y, group = group, connector = connector, edge_id = edge_id)) +
      geom_sankeynode() + geom_sankeyedge(ncp = 20) + ggplot2::lims(y = c(0,3))
    vdiffr::expect_doppelganger("ggsankeyfier feedback", feedback_plot)
  }
)

test_that(
  "Stacking and vertical spacing is correct when y is identical", {
    dat <-
      data.frame(x = rep(c("A", "B"), each = 4),
                 y = 1, group = LETTERS[c(1:4, 8:5)],
                 connector = rep(c("from", "to"), each = 4),
                 edge_id = rep(1:4, 2))
    pos <- position_sankey(v_space = "auto")
    plot <-
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = x, y = y, group = group, connector = connector, edge_id = edge_id,
                     fill = group)) +
      geom_sankeynode(position = pos) + geom_sankeyedge(position = pos, ncp = 20)
    vdiffr::expect_doppelganger("ggsankeyfier stack ident", plot)
  }
)

test_that(
  "Vectorized stage parameters are handled correctly", {
    pos <- position_sankey(v_space = "auto",
                           split_nodes = c(FALSE, TRUE), h_space = .3, width = c(.1, .2, .4))
    plot <- ggplot2::ggplot(ecosystem_services_pivot1,
                   ggplot2::aes(x = stage, y = RCSES, group = node,
                                connector = connector, edge_id = edge_id)) +
      geom_sankeynode(position = pos) +
      geom_sankeyedge(position = pos, fill = "black", alpha = 0.5, ncp = 20)
    vdiffr::expect_doppelganger("ggsankeyfier stage param", plot)
  }
)
