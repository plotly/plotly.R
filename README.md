[![Build Status](https://travis-ci.org/ropensci/plotly.png?branch=master)](https://travis-ci.org/ropensci/plotly)

# plotly

An R package for creating interactive web-based graphs via [plotly](https://plot.ly/)'s JavaScript graphing library. 

## Installation

Install from CRAN:

```r
install.packages("plotly")
```

Or install the latest development version (on GitHub) via devtools:

```r
devtools::install_github("ropensci/plotly")
```

## Introduction

If you use [ggplot2](http://cran.r-project.org/package=ggplot2), use `ggplotly()` to convert your ggplot to an interactive, web-based version!

```r
library(plotly)
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
p <- ggplot(data = d, aes(x = carat, y = price)) + 
  geom_point(aes(text = paste("Clarity:", clarity))) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
(gg <- ggplotly(p))
```

![https://plot.ly/~agvd/1153](http://i.imgur.com/tbKybEb.png)

[Click here](https://plot.ly/~agvd/1153) to interact with the resulting graph (notice the custom hover text!)

__plotly__ also supports certain chart types that ggplot2 doesn't support (such as 3D [surface](https://plot.ly/r/3d-surface-plots/), [point](https://plot.ly/r/3d-scatter-plots/), and [line](https://plot.ly/r/3d-line-plots/) plots). You can easily create these (or any other plotly) charts using the high-level interface. 

```r
plot_ly(z = volcano, type = "surface")
```

![https://plot.ly/~brnvg/1134](https://plot.ly/~brnvg/1134.png)

The `ggplotly()` function converts a ggplot object to a plotly object, so if you like, you may 'post-process' your ggplot graphs to add custom plotly features, for example:

```r
layout(gg, hovermode = "closest")
```

## Documentation

* [An introduction to plotly's R API](https://cran.r-project.org/web/packages/plotly/vignettes/intro.html)
* Examples and vignettes on plotly's R homepage - <https://plot.ly/r>
* The complete figure reference guide - <https://plot.ly/r/reference>

## Contributing

Please read through our [contributing guidelines](https://github.com/ropensci/plotly/blob/master/CONTRIBUTING.md). Included are directions for opening issues, asking questions, contributing changes to plotly, and our code of conduct. 

---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
