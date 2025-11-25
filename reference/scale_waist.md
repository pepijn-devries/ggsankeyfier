# Sankey edge waist line scales

The waist scale can be used to control the waist (i.e., the width of the
edge at its centre) of edges in Sankey diagrams, in order to put
emphasis on specific edges.

## Usage

``` r
scale_waist_continuous(..., range = c(0, 1))

scale_waist_datetime(..., range = c(0, 1))

scale_waist_binned(..., range = c(0, 1))

scale_waist_discrete(..., range = c(0, 1))

scale_waist_manual(..., values = NULL, breaks = ggplot2::waiver())

scale_waist_identity(..., guide = "none")
```

## Arguments

- ...:

  arguments passed onto underpinning scale constructors.

- range:

  A `vector` of two `numeric` values used to scale the waist in between.
  Should be `>= 0`.

- values:

  a set of aesthetic values to map data values to. The values will be
  matched in order (usually alphabetical) with the limits of the scale,
  or with `breaks` if provided. If this is a named vector, then the
  values will be matched based on the names instead. Data values that
  don't match will be given `na.value`.

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks (the scale limits)

  - A character vector of breaks

  - A function that takes the limits as input and returns breaks as
    output

- guide:

  Guide to use for this scale. Defaults to `"none"`.

## Value

Returns a
[ggplot2::Scale](https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html)
object which can be added to a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
to control the waist of Sankey diagram edges.

## Details

This scale can be used to scale the centre of a Sankey edge. At one end
of the scale the edge will be shaped like an hour glass, at the other
end it will be shaped as an apple.

## Author

Pepijn de Vries

## Examples

``` r
if (requireNamespace("ggplot2")) {
  library(ggplot2)
  data("ecosystem_services")

  p <-
    ggplot(ecosystem_services_pivot1, aes(x = stage, y = RCSES, group = node,
                                          connector = connector,
                                          edge_id = edge_id,
                                          waist = RCSES)) +
   geom_sankeyedge(v_space = "auto", ncp = 10) +
    geom_sankeynode(v_space = "auto")

  p + scale_waist_binned(range = c(0.1, 2))
  p + scale_waist_binned(range = c(2, 0.1))
}
```
