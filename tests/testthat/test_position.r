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

test_that(
  "Nodes are summarised identically for both edges and nodes", {
    dat <-
      data.frame(
        a = rep("A", 4),
        b = c("B", rep("A", 3)),
        c = c(NA, "B", rep("A", 2)),
        d = c(NA, NA, "B", "A"),
        PANEL = 1L,
        y = c(10, 10, 15, 20)
      ) |>
      pivot_stages_longer(
        stages_from = letters[1:4],
        additional_aes_from = "PANEL",
        values_from = "y"
      ) |>
      dplyr::group_by(edge_id) |>
      dplyr::filter(!is.na(.data$node)) |>
      dplyr::mutate(
        x = as.factor(.data$stage),
        node_id = as.factor(paste(.data$stage, .data$node))) |>
      dplyr::ungroup()

    dat2 <-
      dat |>
      tidyr::pivot_wider(
        id_cols = c("PANEL", "edge_id"),
        names_from = c("connector"),
        values_from = !c(c("PANEL", "edge_id", "connector"))
      ) |>
      dplyr::filter(!is.na(.data$node_to)) |>
      dplyr::rename(node_id = "node_id_from",
                    node_id_end = "node_id_to",
                    x = "x_from", xend = "x_to",
                    y = "y_from", yend = "y_to")

    expect_identical({
      ggsankeyfier:::.node_summary(NULL, dat)
    }, {
      ggsankeyfier:::.node_summary(NULL, dat2)
    })
  })

test_that("Layer positions are calculated", {
  expect_no_error({
    p <- ggplot(es,
                aes(x = stage, y = RCSES, group = node, connector = connector,
                    edge_id = edge_id)) +
      geom_sankeyedge() + geom_sankeynode()

    cp <- StatSankeyedge$compute_panel(
      data = p$data |>
        dplyr::mutate(group = .data$node,
                      x = as.numeric(.data$stage),
                      y = .data$RCSES,
                      PANEL = 1)
    )
    ggsankeyfier:::.compute_layer_positions(PositionSankey, cp)
  })
})

test_that("Ordering by aesthetics works", {
  expect_no_error({
    pos <- position_sankey(v_space = "auto", order = "ascending+")

    p <- ggplot(es,
                aes(x = stage, y = RCSES, group = node, connector = connector,
                    edge_id = edge_id)) +
      geom_sankeyedge(position = pos) + geom_sankeynode(position = pos)
    print(p)
  })
})

test_that("Custom ordering works", {
  expect_no_error({

    fun <- function(data) {
      if ("edge_id" %in% names(data)) {

        data$edge_order_end <- data$edge_order <- 1

      } else {

        data$node_order <- 1

      }
      return(data)

    }
    pos <- position_sankey(v_space = "auto", order = fun)

    p <- ggplot(es,
                aes(x = stage, y = RCSES, group = node, connector = connector,
                    edge_id = edge_id)) +
      geom_sankeyedge(position = pos) + geom_sankeynode(position = pos)
    print(p)
  })
})
