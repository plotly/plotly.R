[![Build Status](https://travis-ci.org/ropensci/plotly.png?branch=master)](https://travis-ci.org/ropensci/plotly)

# plotly

An R package for creating (and modifying) interactive web-based graphs via [plotly](https://plot.ly/)'s JavaScript graphing library. 

## Installation

__plotly__ is not (yet) available on CRAN, but you can install it via [devtools](http://cran.r-project.org/web/packages/devtools/):

```r
devtools::install_github("ropensci/plotly")
```

## Getting Started

### Signup

If you don't already have a plotly account, either [signup online](https://plot.ly/how-to-sign-up-to-plotly/) or use the `signup()` function (see the `help(signup)` page for more details).

Note you can check if you have a username and API key with:

```r
plotly:::verify("username")
plotly:::verify("api_key")
```

### Introduction

If you use [ggplot2](http://cran.r-project.org/web/packages/ggplot2/index.html), simply call `ggplotly()` to make your ggplots online and interactive!

```r
library(plotly)
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
p <- ggplot(data = d, aes(x = carat, y = price)) + 
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
(gg <- ggplotly(p))
```

![https://plot.ly/~agvd/1153](http://i.imgur.com/tbKybEb.png)

[Click here](https://plot.ly/~agvd/1153) to interact with the resulting graph (notice the custom hover text!)


The `ggplotly()` function converts a ggplot object to a plotly object, so if you like, you may 'post-process' your ggplot graphs to add custom plotly features, for example:

```r
layout(gg, hovermode = "closest")
```

plotly also supports certain chart types that ggplot2 doesn't support (such as 3D [surface](https://plot.ly/r/3d-surface-plots/), [point](https://plot.ly/r/3d-scatter-plots/), and [line](https://plot.ly/r/3d-line-plots/) plots). You can easily create these (or any other plotly) charts using the high-level interface. 

```r
plot_ly(z = volcano, type = "surface")
```

![https://plot.ly/~brnvg/1134](https://plot.ly/~brnvg/1134.png)

## Learn more

* [An introduction to plotly's R API](https://cdn.rawgit.com/ropensci/plotly/master/vignettes/intro.html)
* [Plotly Offline Mode](https://cdn.rawgit.com/ropensci/plotly/master/vignettes/offline.html)
* Plotly's [R homepage](https://plot.ly/r) and [ggplot2 homepage](https://plot.ly/ggplot2)

## Contributing

- We love collaboration! See the [wiki](https://github.com/ropensci/plotly/wiki/Development-guidelines) and the [code of conduct](https://github.com/ropensci/plotly/blob/master/CONDUCT.md) for more information.

## Stay in touch

- <feedback@plot.ly>
- [@plotlygraphs](https://twitter.com/plotlygraphs)

---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
