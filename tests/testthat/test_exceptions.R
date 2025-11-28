test_that("Either `position` or separate arguments are used, not both", {
  expect_error({
    pos <- position_sankey(v_space = "auto")

    ggplot(ecosystem_services_pivot1,
           aes(x = stage, y = RCSES, group = node, connector = connector,
               edge_id = edge_id)) +
      geom_sankeyedge(position = pos, v_space = 0)

  })
})
