## subset data to reduce test time
es <-
  pivot_stages_longer(
    ecosystem_services |> dplyr::filter(RCSES > quantile(RCSES, 0.995)),
    c("activity_realm", "biotic_realm", "service_section"),
    "RCSES")

test_that(
  "Position descending does not throw errors", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      pos <- position_sankey(order = "descending")
      on.exit({grDevices::dev.off(); closeAllConnections()})
      {
        f <- tempfile(fileext = ".pdf")
        p <- ggplot(es,
                    aes(x = stage, y = RCSES, group = node,
                        connector = connector, edge_id = edge_id)) +
          geom_sankeyedge(ncp = 10, position = pos)
        grDevices::pdf(f)
        print(p)
      } |> suppressWarnings()
    })
  })

test_that(
  "Position as_is does not throw errors", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      pos <- position_sankey(order = "as_is")
      on.exit({grDevices::dev.off(); closeAllConnections()})
      {
        f <- tempfile(fileext = ".pdf")
        p <- ggplot(es,
                    aes(x = stage, y = RCSES, group = node,
                        connector = connector, edge_id = edge_id)) +
          geom_sankeyedge(ncp = 10, position = pos)
        grDevices::pdf(f)
        print(p)
      } |> suppressWarnings()
    })
  })

test_that(
  "Position backwards does not throw errors", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      pos <- position_sankey(direction = "backward")
      on.exit({grDevices::dev.off(); closeAllConnections()})
      {
        f <- tempfile(fileext = ".pdf")
        p <- ggplot(es,
                    aes(x = stage, y = RCSES, group = node,
                        connector = connector, edge_id = edge_id)) +
          geom_sankeyedge(ncp = 10, position = pos)
        grDevices::pdf(f)
        print(p)
      } |> suppressWarnings()
    })
  })
