# Pivot wide data to long for plotting as Sankey

Pivot data from a wide to a long format suitable for plotting Sankey
diagrams.

## Usage

``` r
pivot_stages_longer(
  data,
  stages_from,
  values_from,
  additional_aes_from,
  invert_nodes = FALSE
)
```

## Arguments

- data:

  A `data.frame` (or an object inheriting the `data.frame` class), which
  needs to be pivoted.

- stages_from:

  A `vector` of column names, which represent the stages.

- values_from:

  A `vector` of column names, which contains `numeric` values that
  represent the size of the edges in Sankey diagrams. When there are
  multiple values for a single edge, they are summed.

- additional_aes_from:

  A `vector` of column names of data that you want to use to decorate
  elements in your Sankey diagram. This argument is optional. See also
  [`vignette("data_management")`](https://pepijn-devries.github.io/ggsankeyfier/articles/data_management.md)
  and
  [`vignette("decorating")`](https://pepijn-devries.github.io/ggsankeyfier/articles/decorating.md).

- invert_nodes:

  When pivoting information from `stages_from`, its data is converted
  into a `factor`. Set `invert_nodes` to `TRUE` if you want to invert
  the order of the levels of the `factor`.

## Value

Returns a
[dplyr::tibble](https://dplyr.tidyverse.org/reference/reexports.html)
with all the selected columns from `data` pivoted. The stages will be
listed in the column named `stage` and nodes in the column named `node`.
The result will contain two new columns: a column named `connector`
indicating whether the row in the `tibble` reflects the source of an
edge (value `'from'`) or destination of an edge (value `'to'`); and a
column named `edge_id`, containing a unique identifier for each edge.
The `edge_id` is required for the plotting routine in order to identify
which edge source should be connected with which edge destination.

## Details

Typically, data to be displayed as a Sankey, is collected and stored in
a wide format, where each stage (i.e., x-axis of a Sankey diagram) is in
a column. The `ggplot2` philosophy requires the data to be in a long
format, such that diagram decorations (aesthetics) can be mapped to
specific columns.

This function pivots wide data in an appropriate long format, by
indicating which columns contain the stages, and in which order they
should appear in the Sankey.

For more details see
[`vignette("data_management")`](https://pepijn-devries.github.io/ggsankeyfier/articles/data_management.md)

## Author

Pepijn de Vries

## Examples

``` r
data("ecosystem_services")

ecosystem_services_p1 <-
  pivot_stages_longer(
    data        = ecosystem_services,
    stages_from = c("activity_type", "pressure_cat",
                    "biotic_group", "service_division"),
    values_from = "RCSES")

## suppose we want to decorate our Sankey
## with information on the 'section' of the services:
ecosystem_services_p2 <-
  pivot_stages_longer(
    data        = ecosystem_services,
    stages_from = c("activity_type", "pressure_cat",
                    "biotic_group", "service_division"),
    values_from = "RCSES",
    additional_aes_from = "service_section")
```
