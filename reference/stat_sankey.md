# Sankey stats

Aggregates value on the `y` axis per `group` for nodes, and for all used
aesthetics for edges.

## Usage

``` r
StatSankeyedge

stat_sankeyedge(
  mapping = NULL,
  data = NULL,
  geom = "sankeyedge",
  position = "sankey",
  na.rm = FALSE,
  slope = 0.5,
  curve_weight = 0.5,
  ncp = 100,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

StatSankeynode

stat_sankeynode(
  mapping = NULL,
  data = NULL,
  geom = "sankeynode",
  position = "sankey",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Format

An object of class `StatSankeyedge` (inherits from `Stat`, `ggproto`,
`gg`) of length 4.

An object of class `StatSankeynode` (inherits from `Stat`, `ggproto`,
`gg`) of length 3.

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  a string naming the `ggplot2::proto` Geom subclass. Should be either
  `"sankeynode"` or `"sankeedge"`.

- position:

  A `character` string or function specifying the positioning routine.
  By default this is `"sankey"`.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- slope:

  Slope parameter (`numeric`) for the Bezier curves used to depict the
  edges. Any value between 0 and 1 will work nicely. Other non-zero
  values will also work.

- curve_weight:

  Places weight on the Bezier curve. Values close to zero will pull the
  inflection point of the curve towards outgoing nodes. Values close to
  one will pull them towards incoming nodes. The default is 0.5, which
  will place the inflection point exactly in the middle of the
  connecting nodes.

- ncp:

  Number of control points on the Bezier curve that forms the edge.
  Larger numbers will result in smoother curves, but cost more
  computational time. Default is 100.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ...:

  Passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  function

## Value

Returns a ggplot2 stat layer which can be used in a ggplot.

## Details

Wrangles data before it can be passed to
[`position_sankey()`](https://pepijn-devries.github.io/ggsankeyfier/reference/position_sankey.md).

## Author

Pepijn de Vries

## Examples

``` r
library(ggplot2)
data("ecosystem_services")

p <- ggplot(ecosystem_services_pivot1, aes(x = stage, y = RCSES, group = node,
                                           connector = connector, edge_id = edge_id,
                                           fill = node))
p + stat_sankeynode()

p + stat_sankeyedge()
```
