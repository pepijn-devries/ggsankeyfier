library(dplyr)
library(ggsankeyfier)

ecosystem_services <- read.csv(file.path("data-raw", "ggsankeyfier.csv"))
bio_realms         <- read.csv(file.path("data-raw", "bio_realms.csv"))
activity_realms    <- read.csv(file.path("data-raw", "activity_realms.csv"))

ecosystem_services <-
  ecosystem_services |>
  as_tibble() |>
  left_join(bio_realms, "biotic_group") |>
  left_join(activity_realms, "activity_type") |>
  select(activity_type, activity_realm = "activity_realm_b", pressure_cat,
         biotic_group, biotic_realm = "biotic_realm_b", service_division,
         service_section, RCSES)

ecosystem_services_pivot1 <-
  pivot_stages_longer(
    ecosystem_services,
    c("activity_realm", "biotic_realm", "service_section"),
    "RCSES")

ecosystem_services_pivot2 <-
  pivot_stages_longer(
    ecosystem_services,
    c("activity_realm", "biotic_realm", "service_section"),
    "RCSES", "service_section")

save(ecosystem_services, ecosystem_services_pivot1, ecosystem_services_pivot2,
     file = file.path("data", "ecosystem_services.rdata"), compress = TRUE)
