[![Build Status](https://travis-ci.org/ropensci/plotly.png?branch=master)](https://travis-ci.org/ropensci/plotly)

# plotly

An R package for creating interactive web graphics via the open source JavaScript graphing library [plotly.js](https://github.com/plotly/plotly.js).

## Installation

Install from CRAN:

```r
install.packages("plotly")
```

Or install the latest development version (on GitHub) via devtools:

```r
devtools::install_github("ropensci/plotly")
```

## Getting Started

### Web-based ggplot2 graphics

If you use [ggplot2](https://github.com/hadley/ggplot2), `ggplotly()` converts your plots to an interactive, web-based version! It also provides sensible tooltips, which assists decoding of values encoded as visual properties in the plot.

```r
library(plotly)
g <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") + 
  xlim(1, 6) + ylim(40, 100)
ggplotly(g)
```

![https://plot.ly/~cpsievert/9836](http://i.imgur.com/6G4zv7b.png)

If you'd like to see how `ggplotly()` does in converting different ggplot2 examples, we host a [plotly version](http://ropensci.github.io/plotly/ggplot2/) of the [official ggplot2 documentation](http://docs.ggplot2.org). We also have some of our own examples [here](https://plot.ly/ggplot2/).

### plotly's custom R interface

[plotly.js](https://github.com/plotly/plotly.js) supports some chart types that ggplot2 doesn't (our [cheatsheet](https://images.plot.ly/plotly-documentation/images/r_cheat_sheet.pdf) provides a nice summary of the available chart types). You can create any of these charts via `plot_ly()`.

```r
plot_ly(z = ~volcano, type = "surface")
```

![https://plot.ly/~brnvg/1134](https://plot.ly/~brnvg/1134.png)

We have a number of [vignettes](https://ropensci.github.io/plotly/) which explain the `plot_ly()` interface in depth as well as numerous examples on the [plotly website](https://plot.ly/r/#basic-charts) and [bundled with the package](https://github.com/ropensci/plotly/tree/master/inst/examples).

### Capturing plotly events

[plotly.js](https://github.com/plotly/plotly.js) exposes a number of 'standard' events that work consistently across plot types. It's easy to hook into these events using the `event_data()` function in shiny apps, as these examples demonstrate:

1. [2D events](http://104.131.111.111:3838/plotlyEvents/) ([source](https://github.com/ropensci/plotly/tree/master/inst/examples/plotlyEvents))
2. [Linked Clicks](http://104.131.111.111:3838/plotlyLinkedClick/) ([source](https://github.com/ropensci/plotly/tree/master/inst/examples/plotlyLinkedClick))
3. [Linked Brush](http://104.131.111.111:3838/plotlyLinkedBrush/) ([source](https://github.com/ropensci/plotly/tree/master/inst/examples/plotlyLinkedBrush))

![http://104.131.111.111:3838/plotlyLinkedBrush/](http://i.imgur.com/eVqsZma.gif)

You can also hook into these events without shiny using `htmlwidgets::onRender()` ([example](https://github.com/ropensci/plotly/tree/master/inst/examples/onRenderHover)). This, however, requires JavaScript knowledge and makes it much harder to coordinate views between htmlwidgets.

## Learn more

We have lots of examples on <https://plot.ly/r/> and <https://plot.ly/ggplot2/>, but a more comprehensive review is also available at <https://cpsievert.github.io/plotly_book/>

## Contributing

Please read through our [contributing guidelines](https://github.com/ropensci/plotly/blob/master/CONTRIBUTING.md). Included are directions for opening issues, asking questions, contributing changes to plotly, and our code of conduct. 

---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
