## subset data to reduce test time
es <-
  pivot_stages_longer(
  ecosystem_services |> dplyr::filter(RCSES > quantile(RCSES, 0.995)),
  c("activity_realm", "biotic_realm", "service_section"),
  "RCSES")

test_that(
  "No errors when using continuous waist scale", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      on.exit({grDevices::dev.off(); closeAllConnections()})
      {
        f <- tempfile(fileext = ".pdf")
        p <- ggplot(es,
                    aes(x = stage, y = RCSES, group = node,
                        waist = RCSES,
                        connector = connector, edge_id = edge_id)) +
          geom_sankeyedge(ncp = 10) +
          scale_waist_continuous()
        grDevices::pdf(f)
        print(p)
      } |> suppressWarnings()
    })
  })

test_that(
  "No errors when using binned waist scale", {
    expect_no_error({
      library(ggplot2)
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot(es,
                  aes(x = stage, y = RCSES, group = node,
                      waist = RCSES,
                      connector = connector, edge_id = edge_id)) +
        geom_sankeyedge(ncp = 10) +
        scale_waist_binned()
      grDevices::pdf(f)
      print(p)
    })
  })

test_that(
  "No errors when using identity waist scale", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot(es,
                  aes(x = stage, y = RCSES, group = node,
                      waist = RCSES,
                      connector = connector, edge_id = edge_id)) +
        geom_sankeyedge(ncp = 10) +
        scale_waist_identity()
      grDevices::pdf(f)
      print(p)
    })
  })

test_that(
  "No errors when using discrete waist scale", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot(es,
                  aes(x = stage, y = RCSES, group = node,
                      waist = stage,
                      connector = connector, edge_id = edge_id)) +
        geom_sankeyedge() +
        scale_waist_discrete()
      grDevices::pdf(f)
      print(p)
    })
  })

test_that(
  "No errors when using manual waist scale", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot(es,
                  aes(x = stage, y = RCSES, group = node,
                      waist = stage,
                      connector = connector, edge_id = edge_id)) +
        geom_sankeyedge(ncp = 10) +
        scale_waist_manual(values = c(2, 0.5),
                           breaks = c("activity_realm", "biotic_realm"))
      grDevices::pdf(f)
      print(p)
    })
  })

test_that(
  "No errors when using datetime waist scale", {
    expect_no_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot(es |> dplyr::mutate(wst = as.POSIXct(0) +
                                                               1e7*RCSES),
                  aes(x = stage, y = RCSES, group = node,
                      waist = wst |> as.Date(),
                      connector = connector, edge_id = edge_id)) +
        geom_sankeyedge(ncp = 10) +
        scale_waist_datetime(transform = "date")
      grDevices::pdf(f)
      print(p)
    })
  })

test_that(
  "Error when using manual waist scale with insufficient values", {
    expect_error({
      library(ggplot2, quietly = TRUE) |> suppressWarnings()
      on.exit({grDevices::dev.off(); closeAllConnections()})
      f <- tempfile(fileext = ".pdf")
      p <- ggplot(es,
                  aes(x = stage, y = RCSES, group = node,
                      waist = stage,
                      connector = connector, edge_id = edge_id)) +
        geom_sankeyedge(ncp = 10) +
        scale_waist_manual(values = c(activity_realm = 2))
      grDevices::pdf(f)
      print(p)
    })
  })
