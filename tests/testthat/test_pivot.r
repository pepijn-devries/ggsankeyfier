test_that(
  "Pivot 1 can be reproduced",
  {
    expect_identical({
      pivot_stages_longer(
        ecosystem_services,
        c("activity_realm", "biotic_realm", "service_section"),
        "RCSES")
    }, ecosystem_services_pivot1)
  })

test_that(
  "Pivot 1 is not reproduced when nodes are inverted",
  {
    expect_false({
      identical(
        pivot_stages_longer(
          ecosystem_services,
          c("activity_realm", "biotic_realm", "service_section"),
          "RCSES", invert_nodes = TRUE)
        , ecosystem_services_pivot1)
    })
  })

test_that(
  "Pivot 1 can be reproduced",
  {
    expect_identical({
      pivot_stages_longer(
        ecosystem_services,
        c("activity_realm", "biotic_realm", "service_section"),
        "RCSES", "service_section")
    }, ecosystem_services_pivot2)
  })
