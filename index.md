# ggsankeyfier

> Go with the data flow

## Overview

The `ggsankeyfier` packages allows you to visualise your data as Sankey
or Alluvial diagrams. A Sankey diagram is essentially a stacked bar
plot, where the bands connect bars across stages (on the x-axis), to
show how quantities flow between them.

## Why Use `ggsankeyfier`?

`ggsankeyfier` allows you to add Sankey diagram layers to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
The package also provides `stat_*` and `position_*` functions that allow
you to add all sorts of other layers, such as text and labels.

Furthermore, the data model used by the package allows you to visualise
flows that skip stages or even feedback loops.

And last but not least, in contrast to other packages, `ggsankeyfier`
uses proportionate bands. In alluvial and Sankey diagrams data quantity
flows are represented by bands that flow across stages. In order to
effectively visualise quantities, the width of the band needs to be
proportionate to the quantity. Other packages use a vertically oriented
brush to plot the bands, making them narrow when the slope is steep. The
`ggsankeyfier` package uses a brush that is perpendicular to the
direction of the flow. This keeps the width along the band constant and
proportionate with its representing quantity.

This is illustrated in the animation below where both panels visualise
the same quantity. As a reference a horizontal band representing the
same quantity is shown. Note that the orientation of the brush affects
its appearance.

![Effectively visualising data flows requires a brush perpendicular to
its
path](https://raw.githubusercontent.com/pepijn-devries/ggsankeyfier/refs/heads/master/data-raw/brushangle.gif)

Effectively visualising data flows requires a brush perpendicular to its
path

## Installation

> Get CRAN version

``` r
install.packages("ggsankeyfier")
```

> Get development version on github

``` r
devtools::install_github('pepijn-devries/ggsankeyfier')
```

## Important Concepts

As there is some variation in the definition and terminology used in
Sankey diagrams, there are some introduced here for consistency across
the package documentation. Here we try to adhere to common definitions
used in the [graph theory](https://en.wikipedia.org/wiki/Graph_theory).
This theory is used to model pairwise relationships between ‘nodes’
which are connected by ‘edges’. These aspects are circled in the
illustration below.

![Important aspects](reference/figures/important_aspects.svg)

Important aspects

The `ggsankeyfier` package can only visualise structured graphs. Meaning
that each node belongs to a specific stage (arranged along the x-axis).

### Sankey Thesaurus

As there are no standards in Sankey diagrams, there may be different
words representing the same or similar aspects. Therefore, the following
thesaurus is presented to provide an overview and hopefully avoid
confusion. The list starts with the term preferred in the present
package, followed by alternatives.

- Sankey diagram:
  - *Alluvial diagram*. Although arguably not the same as a Sankey
    diagram, they are very similar. Differences ly in the type of data
    (population of facts across categorical dimensions (alluvial) versus
    quantities in different states (Sankey)) Also, alluvial diagrams are
    always structured in stages (where the order does not matter),
    whereas Sankey diagrams are not necessarily structured, but the
    order does matter
  - *Bump diagram*. This is actually a special case of alluvial
    diagrams, where each node flows only to a single next node. Usually,
    the stacking order of nodes in each stage is determined by the size
    of the nodes
- Node:
  - *Vertices*. Another commonly used term in the [graph
    theory](https://en.wikipedia.org/wiki/Graph_theory)
  - *Stratum*. A term coined for alluvial diagrams
- Edge:
  - *Flow*. Sometimes also refers to the interaction between stages. In
    the present package it is used only as a synonym for ‘edge’.
  - *Alluvium*. A term used in alluvial diagrams
  - *Line*. Another commonly used albeit generic term in the [graph
    theory](https://en.wikipedia.org/wiki/Graph_theory)
  - *Link*. Although commonly used in the [graph
    theory](https://en.wikipedia.org/wiki/Graph_theory), we avoid its
    use in this context as it may get confused with a link in a
    cause-effect chain, which is better reflected by the stages
- Connector:
  - *Lode*. A term used in alluvial diagrams
- Stage:
  - *Link*. Not used in the present package to avoid confusion with
    edges (see above)

## Usage

Like any other ggplot, you start by calling
[`ggplot2::ggplot2()`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html),
provide the data for plotting and specify aesthetics (`aes`). Layers
with Sankey edges and nodes are simply added to the plot using the
`+`-operator:

``` r
data("ecosystem_services")

ggplot(ecosystem_services_pivot1,
       aes(x = stage, y = RCSES, group = node,
           connector = connector, edge_id = edge_id)) +
  geom_sankeyedge(v_space = "auto") +
  geom_sankeynode(v_space = "auto")
```

![](reference/figures/README-general_illustration-1.svg)

For consistency with aesthetics used in other
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
layers, the stage variable should be assigned to `x`, the quantity of
the nodes and edges to `y` and the node identifier to `group`. In
addition to these ‘standard’ aesthetics, you also need to specify a
`connector` specifying the direction of an edge (one of `'from'` or
`'to'`); and an `edge_id` which is used to determine which connector
ends should be paired together.

### Data Management

Note that the plotting routines require data organised in a
`data.frame`, with in each row a ‘connector’. A connector is either the
start or an end of an edge. This allows you to provide different
characteristics for each of these ends. However, in most cases this is
not the type of data you will be working with. Check
[`vignette("data_management")`](https://pepijn-devries.github.io/ggsankeyfier/articles/data_management.md),
on how to rearrange your data for displaying it in a Sankey diagram.

### Positioning Nodes and Edges

The package gives you much control on the positioning of elements in the
diagram. Think of:

- spacing between and sizing of nodes and edges
- aligning nodes vertically
- introducing a horizontal split in nodes
- stacking order of nodes and edges

[`vignette("positioning")`](https://pepijn-devries.github.io/ggsankeyfier/articles/positioning.md)
and
[`vignette("stacking_order")`](https://pepijn-devries.github.io/ggsankeyfier/articles/stacking_order.md)
will show you how.

### Decorating Nodes and Edges

When creating your own Sankey diagrams you may want to alter its
appearance. You may want to:

- assign meaningful decorations (such as colours) using aesthetics to
  nodes and edges
- add keys and legends to guide your audience
- add additional layers (such as text)
- change the edge curve shape
- use different themes

Check
[`vignette("decorating")`](https://pepijn-devries.github.io/ggsankeyfier/articles/decorating.md)
to discover how this is done.

## Code of Conduct

Please note that the `ggsankeyfier` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

This package was development as part of the EU GES4SEAS project (EU call
HORIZON-CL6-2021-BIODIV-01-04, grant agreement
[101059877](https://doi.org/10.3030/101059877)) and the WUR Knowledge
Base Research program KB-36-003-022 “The use of ecosystem services to
conserve biodiversity in the North Sea” that is supported by finance
from the Dutch Ministry of Agriculture, Nature and Food Quality

## Resources

- Piet GJ, Bentley JW, Jongbloed RH, Grundlehner A, Tamis JE, De Vries
  P (2024) A Cumulative Impact Assessment on the North Sea Capacity to
  Supply Ecosystem Services. Science of The Total Environment (498)
  [DOI:10.1016/j.scitotenv.2024.174149](https://doi.org/10.1016/j.scitotenv.2024.174149)
