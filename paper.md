---
title: 'ggsankeyfier: An R package to visualise flows of quantities through stages'
tags:
- R
- data visualisation
- Sankey
- Alluvial
date: 2024-03-01
output:
  pdf_document:
    keep_md: yes
authors:
- given-names: Pepijn
  non-dropping-particle: de
  surname: Vries
  orcid: "0000-0002-7961-6646"
  equal-contrib: true
  affiliation: 1
- given-names: Gerjan
  surname: Piet
  orcid: "0000-0003-0702-1624"
  affiliation: 1
- given-names: Jacqueline
  surname: Tamis
  orcid: "0000-0002-8206-5830"
  affiliation: 1
- given-names: Jongbloed
  surname: Ruud H.
  orcid: "0000-0002-9378-5382"
  affiliation: 1
- given-names: Grundlehner
  surname: Anne
  orcid: "0000-0003-3375-3511"
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Wageningen University and Research, Wageningen Marine Research, P.O. Box 57, 1780 AB Den Helder, The Netherlands
---

# Summary

Visualising flows of quantities across stages is challenging yet highly informative.
Sankey diagrams are, amongst others, a convenient tool for visualising such quantitative flows.
Although there are several alternatives available, the `ggsankeyfier` package
presented here provides several unique and new features for generating and visualizing Sankey
and related diagrams. It is presented here alongside alternatives
allowing the readers to select the right instrument for their visualisation needs.

# Delineation and principles

Diagrams that depict flows between different stages are known under different names, most notably:
Sankey diagrams, alluvial plots, and parallel coordinate plots. These types of plots are not new and
several tools for this purpose have been developed in the past
(see for instance @Hofmann2013; @Riehmann2005; @Schmidt2008; @Rosvall2010)


![\label{fig:important-aspects}Elements and their nomenclature in a Sankey diagram. Between brackets are the aesthetics to which variables need to be mapped in the graphics grammar (ggplot2).](paper_files/figure-latex/important-aspects-1.pdf) 

The difference between the diagrams mentioned above, is mostly based on a discussion about semantics.
Unlike alluvial plots and parallel coordinate plots, Sankey diagrams do not necessarily visualise variables
[@Schonlau2024]. Whether a diagram is called alluvial or Sankey, is irrelevant to the software that generated
it; and their essence is arguably identical. More importantly, data scientists are generally more concerned
whether their message is adequately conveyed
by the visualisation of their data.
As there is no clear consensus and there is no effect on the end-result this discussion is deemed
trivial for this paper. Instead, consistent terminology is used for elements in the diagram (which we
chose to name Sankey). The [graph theory](https://en.wikipedia.org/wiki/Graph_theory) [@Bondy2010]
was used for inspiration.
A [thesaurus is provided with the package](https://pepijn-devries.github.io/ggsankeyfier/)
to avoid any confusion.
Figure \ref{fig:important-aspects} shows a graphical summary of these definitions taken from the manual.

The drive to develop a new tool came from the study described by @PietSub where
a staged acyclic graph was developed to describe how anthropogenic activities through their pressures
affect the marine ecosystem and its components as well as their capacity to supply ecosystem services.
For this case it was desired to visualise how a risk quantity (specifically the amount of Impact Risk) flows through
a network of cause-effect-chains. Such that it is shown how ‘impact risk’ affecting the ecosystem services can be
attributed to human activities and their resulting pressures. For the envisioned diagram it was required that:
edges in the diagram (Figure \ref{fig:important-aspects}) accurately reflect the representing quantity; can be combined with
other graphical layers (e.g., text labels, scatter plots, line plots etc.); positioning of the elements in the diagram
can be controlled;
can distinguish between incoming and outgoing edges; potentially depict cyclic edges; reproduce well
in print; potentially supports interactivity. These and additional desirable features are captured in
Table \ref{tab:features}. As none of the existing packages (in the R environment) adequately address
all these features or could easily be adapted to support them, a new package (`ggsankeyfier`) was
developed.

# Alternatives

To best understand the added value of the `ggsankeyfier` package, its features are compared to
existing packages, here referred to as alternative packages. This will not only justify its right of existence,
it will also help users
to select the right instrument for their needs. To keep this overview concise and comprehensible
it will be limited to packages in the R domain that are available from CRAN and have
an issue tracker (i.e., GitHub). These packages and features are listed in Tables
\ref{tab:features} and \ref{tab:package-features}.


Table: \label{tab:features}A list of potential features, for visualising Sankey diagrams.

|Feature                                          |Feature Code |
|:------------------------------------------------|:------------|
|Plotting routine                                 |A            |
|In control of plotting order / position?         |B            |
|Allows networks without stages?                  |C            |
|Distinguish between incoming and outgoing edges? |D            |
|Interactive?                                     |E            |
|Edge size reflects quantity and is constant?     |F            |
|Cyclic edges possible?                           |G            |

\clearpage


Table: \label{tab:package-features}An overview of evaluated software packages against the features listed in Table \ref{tab:features}. Letters in the table header correspond with the codes used in Table \ref{tab:features}.

|Package      |Reference             |A                       |B   |C   |D   |E   |F   |G                            |
|:------------|:---------------------|:-----------------------|:---|:---|:---|:---|:---|:----------------------------|
|alluvial     |@Bojanowski2016       |base plot               |Yes |No  |No  |No  |No  |No                           |
|easyalluvial |@Koneswarakantha2023a |ggplot2                 |No  |No  |No  |No  |No  |No                           |
|ggsankeyfier |This paper            |ggplot2                 |Yes |No  |Yes |No  |Yes |Yes                          |
|ggalluvial   |@Brunson2020          |ggplot2                 |No  |No  |No  |No  |No  |No                           |
|ggpcp        |@Hofmann2022          |ggplot2                 |No  |No  |No  |No  |No  |No                           |
|networkD3    |@Allaire2017          |javascript / html[^1]   |Yes |Yes |No  |Yes |Yes |Yes                          |
|parcats      |@Koneswarakantha2023b |javascript / html$^{1}$ |Yes |No  |No  |Yes |No  |No                           |
|sankey       |@Csardi2017           |base plot               |Yes |No  |No  |No  |No  |Possible but not implemented |

[^1]: As mentioned in the main text: html / javascript is excellent for interactive applications. For applications in print
it is less suitable.

A more detailed consideration, of why the features in Table \ref{tab:features} and \ref{tab:package-features}
are considered and the performance of the packages is described below.

 A) Most packages either support R `base` plotting routines (where customisability and
    feature richness is limited) or layered Graphical Grammar plots (`ggplot2`, which is
    feature rich and highly customisable, @Wickham2016). Furthermore, the `networkD3` and the `parcats`
    packages render the diagram as html widgets which are made interactive with javascript.
    The latter two are more difficult (if not impossible) to combine with R `base` or `ggplot2`
    routines. It will also be difficult to produce graphics in print with these packages.
 B) All the packages that use `base` plots and those that render html widgets (see Table
    \ref{tab:package-features}) allow the user to control the position of nodes. Either by
    specifying them as function arguments or interactively by dragging them on the display. The
    `ggsankeyfier` is the only `ggplot2` based package that has explicit positioning routines
    that can be fully customised. This gives the user full control over the stacking order
    and or the positioning of and spacing between nodes and edges.
 C) Most packages organise the diagram around stages. Such a structure makes it easier to
    combine the diagram with other graphical layers and include information about those stages.
    The `networkD3` doesn't need any information about stages, allowing it to more freely place
    elements at desirable locations in the plot.
 D) The `ggsankeyfier` package is currently the only package that distinguishes information at the level
    of incoming and outgoing edges. Therewith, it can easily add supporting information in the plot on
    either end of an edge. It also allows for easy definition of feedback loops. Technically, the
    quantity reflected by the edge, could also be attributed to the edge end, such that the quantity
    may grow or shrink along the edge. The latter is not (yet) implemented.
 E) The `networkD3` package utilises the javascript [d3 library](https://github.com/d3/d3)
    and is thus the only package that is inherently interactive. This means that the user can
    interact with the diagram via a web browser by clicking or dragging elements of the diagram.
    Note that with some effort any plot generated with any of the other packages can to some extent
    be turned into an interactive variant, by embedding it in a Shiny app [@Chang2023].
 F) The width of an edge represents a quantity. It is imperative that the band that visualises
    this quantity; and is constant along the edge. However, most packages visualise this band by
    shifting a sigmoid or spline curve horizontally and colouring the space in between. A disadvantage
    of this approach is that the width of the band becomes very narrow at steep parts of the curve.
    In contrast `ggsankeyfier` tries to keep the width of the band (perpendicular to its path) constant.
    This is achieved by applying the `vwline` package by @Murrell2019. A similar approach is implemented
    in the `networkD3` package.
 G) Currently, only `networkD3` and `ggsankeyfier` offer support for the visualisation of feedback
    loops. The `sankey` package, could technically also achieve this, but has not implemented it.
    The other packages organise the data such that feedback loops are per definition impossible.

# Conclusions

The `ggsankeyfier` offers unique features that are not available from any other R package
considered. The `networkD3` comes close, but that package does not allow adding custom graphical
layers   and is not easy to reproduce in print. 

# Acknowledgements

This software presented here was development as part of the EU GES4SEAS project
(EU call HORIZON-CL6-2021-BIODIV-01-04, grant agreement
[101059877](https://doi.org/10.3030/101059877)) and the
WUR Knowledge Base Research program KB-36-003-022 “The use of ecosystem services
to conserve biodiversity in the North Sea” that is supported by finance from the
Dutch Ministry of Agriculture, Nature and Food Quality

# References
