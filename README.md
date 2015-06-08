[![Build Status](https://travis-ci.org/ropensci/plotly.png?branch=master)](https://travis-ci.org/ropensci/plotly)

# plotly

An R package for creating (and modifying) interactive web-based graphs via [plotly](https://plot.ly/)'s API. 

## Installation

__plotly__ is not (yet) available on CRAN, but you can easily install it via [devtools](http://cran.r-project.org/web/packages/devtools/):

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

### ggplot2 converter

If you use [ggplot2](http://cran.r-project.org/web/packages/ggplot2/index.html), you can easily convert them to plotly!

```r
library(plotly)
d <- diamonds[sample(nrow(diamonds), 1000), ]
p <- qplot(carat, price, size = I(4), data = d) + facet_wrap(~cut) + 
  geom_smooth(aes(colour = cut, fill = cut))
ggplotly(p)
```

[![](https://plot.ly/~cpsievert/949.png)](https://plot.ly/~cpsievert/949)
Interact with the Plotly graph: [https://plot.ly/~cpsievert/949](https://plot.ly/~cpsievert/949)


### Custom plotlys

See the vignette entry for an overview of the 


## Learn More

- [A declarative DSL for the plotly graphing library in R](http://cpsievert.github.io/plotly/dsl/)
- [Plot with ggplot2, interact, collaborate, and share online « Bayesian Biologist](http://bayesianbiologist.com/2014/07/31/plot-with-ggplot2-interact-collaborate-and-share-online/)
- [A Rosetta Stone for R, ggplot2, Python, MATLAB, and Excel Plotting](http://nbviewer.ipython.org/gist/msund/61cdbd5b22c103fffb84)


## Stay in touch

- <feedback@plot.ly>
- [@plotlygraphs](https://twitter.com/plotlygraphs)

## Contributing

- We love collaboration! See the [code of conduct]() and the [wiki](https://github.com/ropensci/plotly/wiki/Development-guidelines) for more information. 

---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
