---
layout: post
title: Introducing plotly
---

plotly is awesome!

### What's Plot.ly?

Stuff...

* asdfasdf
* asdf
* asdf

### Examples

Let's make it in Plotly. Install:

```r
install.packages("devtools")
library("devtools")
install_github("plotly", "ropensci")
```

Load.

```r
library("plotly")
```

Sign up online, use our public keys below, or sign up like this:
signup("new_username", "your_email@domain.com")
That should have responded with your new key. Use that to create a plotly interface object, or use ours:

```r
py <- plotly("RgraphingAPI", "ektgzomjbx")
```

It just works.

```r
py$ggplotly(ggiris)
```


<iframe height="600" id="igraph" scrolling="no" seamless="seamless"
                src="https://plot.ly/~RgraphingAPI/554" width="600" frameBorder="0"></iframe>
