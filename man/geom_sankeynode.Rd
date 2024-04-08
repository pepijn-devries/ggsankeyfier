% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_node.r
\docType{data}
\name{GeomSankeynode}
\alias{GeomSankeynode}
\alias{geom_sankeynode}
\title{Bars representing nodes in a Sankey diagram}
\format{
An object of class \code{GeomSankeynode} (inherits from \code{GeomBar}, \code{GeomRect}, \code{Geom}, \code{ggproto}, \code{gg}) of length 6.
}
\usage{
GeomSankeynode

geom_sankeynode(
  mapping = NULL,
  data = NULL,
  stat = "sankeynode",
  position = "sankey",
  na.rm = FALSE,
  show.legend = NA,
  width = "auto",
  align = c("bottom", "top", "center", "justify"),
  order = c("ascending", "descending", "as_is"),
  h_space = "auto",
  v_space = 0,
  nudge_x = 0,
  nudge_y = 0,
  split_nodes = FALSE,
  split_tol = 0.001,
  direction = c("forward", "backward"),
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{width}{Width of the node (\code{numeric}). When \code{split_nodes} is set to \code{TRUE}
each part of the split node will have half this width. Use \code{"auto"} to automatically
determine a suitable width.}

\item{align}{A \code{character} that indicates how the nodes across the stages are aligned.
It can be any of \code{"top"}, \code{"bottom"}, \code{"center"} or \code{"justify"}.}

\item{order}{A \code{character} indicating the method to be used for the order of stacking
nodes and edges in a plot.
Should be one of: \code{ascending} (default), sorts nodes and edges from large to small
(largest on top); \code{descending} sorts nodes and edges from small to large (smallest
on top); \code{as_is} will leave the order of nodes and edges as they are in \code{data}.}

\item{h_space}{Horizontal space between split nodes (\code{numeric}). This argument is
ignored when \code{split_nodes == FALSE}. Use \code{"auto"} to automatically position split nodes.}

\item{v_space}{Vertical space between nodes (\code{numeric}). When set to zero (\code{0}),
the Sankey diagram becomes an alluvial plot. Use \code{"auto"} to automatically determine
a suitable vertical space.}

\item{nudge_x, nudge_y}{Horizontal and vertical adjustment to nudge items by. Can
be useful for offsetting labels.}

\item{split_nodes}{A \code{logical} value indicating whether the source and destination nodes
should be depicted as separate boxes.}

\item{split_tol}{When the relative node size (resulting source and destination edges)
differs more than this fraction, the node will be displayed as two separate bars.}

\item{direction}{One of \code{"forward"} (default) or \code{"backward"}. When set to \code{"backward"}
the direction of the edges will be inverted. In most cases this
parameter won't affect the plot. It can be helpful when you want to decorate the
end of an edge (instead of the start; see examples).}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\value{
Returns a \code{\link[ggplot2:layer]{ggplot2::layer()}} which can be added to a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}
}
\description{
In a Sankey diagram nodes are depicted as stacked bars, possibly with
vertical spacing between them. Use \code{geom_sankeynode()} to add nodes to
your Sankey diagram. If you combine the nodes with \code{\link[=geom_sankeyedge]{geom_sankeyedge()}},
make sure that both use the same \code{position} object.
}
\details{
This \code{ggplot2} layer depicts the size of all connected edges as a bar. The height of
of each bar is determined by the sum of \code{y} aesthetic in each \code{group}. When the sum of edges
that flow to a bar differ more than \code{split_tol} compared to the edges that flow from the
same node, a vertical split is introduced in the node.
}
\section{Aesthetics}{

\code{geom_sankeynode()} understands the following aesthetics (required aesthetics
are in bold)



\if{html}{\figure{important_aspects.svg}{Important aspects}}
\itemize{
\item \strong{\code{x}}: Works for variables on a discrete scale. Might work for continuous variables
but is not guaranteed. This variable is used to distinguish between stages in the
Sankey diagram on the x axis.
\item \strong{\code{y}}: A continuous variable representing the width of the edges in a Sankey
diagram.
\item \strong{\code{group}}: A discrete variable used for grouping edges to nodes in each stage.
Essentially it is an identifier for the nodes.
\item \strong{\code{connector}}: Indicates which side of an edge is reflected by the corresponding
record. Should be one of \code{"from"} or \code{"to"}.
\item \strong{\code{edge_id}}: A unique identifier value for each edge. This identifier is used
to link specific \code{"from"} and \code{"to"} records in an edge (flow).
\item fill: see \code{vignette("ggplot2-specs", "ggplot2")}
\item colour: see \code{vignette("ggplot2-specs", "ggplot2")}
\item linetype: see \code{vignette("ggplot2-specs", "ggplot2")}
\item linewidth: see \code{vignette("ggplot2-specs", "ggplot2")}
\item alpha: A variable to control the opacity of an element.
}
}

\examples{
library(ggplot2)
data("ecosystem_services")

ggplot(ecosystem_services_pivot1, aes(x = stage, y = RCSES, group = node,
                    connector = connector, edge_id = edge_id,
                    fill = node)) +
  geom_sankeynode(v_space = "auto") +
  geom_sankeyedge(v_space = "auto")
}
\author{
Pepijn de Vries
}
\keyword{datasets}