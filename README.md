
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/plotly.png" width="200" />

[![Build
Status](https://travis-ci.org/ropensci/plotly.png?branch=master)](https://travis-ci.org/ropensci/plotly)
[![CRAN
Status](http://www.r-pkg.org/badges/version/plotly)](http://cran.r-project.org/package=plotly)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/plotly)](https://www.rpackages.io/package/plotly)
[![monthly](https://cranlogs.r-pkg.org/badges/plotly)](https://www.rpackages.io/package/plotly)

An R package for creating interactive web graphics via the open source
JavaScript graphing library
[plotly.js](https://github.com/plotly/plotly.js).

## Installation

Install from CRAN:

``` r
install.packages("plotly")
```

Or install the latest development version (on GitHub) via devtools:

``` r
devtools::install_github("ropensci/plotly")
```

## Getting started

### Web-based ggplot2 graphics

If you use [ggplot2](https://github.com/tidyverse/ggplot2), `ggplotly()`
converts your static plots to an interactive web-based version\!

``` r
library(plotly)
g <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") + 
  xlim(1, 6) + ylim(40, 100)
ggplotly(g)
```

![<http://i.imgur.com/G1rSArP.gifv>](http://i.imgur.com/G1rSArP.gif)

By default, `ggplotly()` tries to replicate the static ggplot2 version
exactly (before any interaction occurs), but sometimes you need greater
control over the interactive behavior. The `ggplotly()` function itself
has some convenient “high-level” arguments, such as `dynamicTicks`,
which tells plotly.js to dynamically recompute axes, when appropriate.
The `style()` function also comes in handy for *modifying* the
underlying trace
attributes (e.g. [hoveron](https://plot.ly/r/reference/#scatter-hoveron)) used to generate the plot:

``` r
gg <- ggplotly(g, dynamicTicks = "y")
style(gg, hoveron = "points", hoverinfo = "x+y+text", hoverlabel = list(bgcolor = "white"))
```

![<http://i.imgur.com/qRvLgea.gifv>](http://imgur.com/qRvLgea.gif)

Moreover, since `ggplotly()` returns a plotly object, you can apply
essentially any function from the R package on that object. Some useful
ones include `layout()` (for [customizing the
layout](https://plotly-r.com/improving-ggplotly.html#modifying-layout)),
`add_traces()` (and its higher-level `add_*()` siblings, for example
`add_polygons()`, for [adding new
traces/data](https://plotly-r.com/improving-ggplotly.html#leveraging-statistical-output)),
`subplot()` (for [combining multiple plotly
objects](https://plotly-r.com/arranging-views.html#arranging-plotly-objects)),
and `plotly_json()` (for inspecting the underlying JSON sent to
plotly.js).

The `ggplotly()` function will also respect some “unofficial”
**ggplot2** aesthetics, namely `text` (for [customizing the
tooltip](https://plotly-r.com/controlling-tooltips.html#tooltip-text-ggplotly)),
`frame` (for [creating
animations](https://plotly-r.com/animating-views.html)),
and `ids` (for ensuring sensible smooth transitions).

### Using plotly without ggplot2

The `plot_ly()` function provides a more direct interface to plotly.js
so you can leverage more specialized chart types (e.g., [parallel
coordinates](https://plot.ly/r/parallel-coordinates-plot/) or
[maps](https://plot.ly/r/maps/)) or even some visualization that the
ggplot2 API won’t ever support (e.g., surface,
[mesh](https://plot.ly/r/3d-mesh/),
[trisurf](https://plot.ly/r/trisurf/), etc).

``` r
plot_ly(z = ~volcano, type = "surface")
```

![<https://plot.ly/~brnvg/1134>](https://plot.ly/~brnvg/1134.png)

## Learn more

To learn more about special features that the plotly R package provides (e.g., [client-side linking](https://plotly-r.com/client-side-linking.html), [**shiny** integration](https://plotly-r.com/linking-views-with-shiny.html), [editing and generating static images](https://plotly-r.com/publish.html), [custom events in JavaScript](https://plotly-r.com/javascript.html), and more), see <https://plotly-r.com>. You may already be familiar with existing plotly documentation (e.g., <https://plot.ly/r/>), which is essentially a language-agnostic how-to guide for learning plotly.js, whereas <https://plotly-r.com> is meant to be more wholistic tutorial written by and for the R user. The package itself ships with a number of demos (list them by running `demo(package = "plotly")`) and shiny/rmarkdown examples (list them by running `plotly_example("shiny")` or `plotly_example("rmd")`). [Carson](https://cpsievert.me) also keeps numerous [slide decks](https://talks.cpsievert.me) with useful examples and concepts.

## Contributing

Please read through our [contributing
guidelines](https://github.com/ropensci/plotly/blob/master/CONTRIBUTING.md).
Included are directions for opening issues, asking questions,
contributing changes to plotly, and our code of
conduct.

-----

![<http://ropensci.org>](http://www.ropensci.org/public_images/github_footer.png)
