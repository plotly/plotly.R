[![Build Status](https://travis-ci.org/ropensci/plotly.png?branch=master)](https://travis-ci.org/ropensci/plotly)


![](http://i.imgur.com/gp0muqe.gif)


Interactive R and ggplot2 figures in your web browser
---

```
library(plotly)

# --> your ggplot2 methods <--
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
qplot(carat, price, data=dsamp, colour=clarity)

# --> send your ggplot2 figure to plotly <--
py <- plotly()
py$ggplotly()
```

[![](https://plot.ly/~chris/2703.png)](https://plot.ly/~chris/2703)
Interact with the Plotly graph: [https://plot.ly/~chris/2703](https://plot.ly/~chris/2703)


[Get started](https://plot.ly/ggplot2/)
-------------


Learn More
---
- [Getting started with ggplot2 and plotly](https://plot.ly/ggplot2/getting-started/)
- [A Brief Introduction to Plotly « Bad Hessian](http://badhessian.org/2014/08/a-brief-introduction-to-plotly/)
- [Plot with ggplot2, interact, collaborate, and share online « Bayesian Biologist](http://bayesianbiologist.com/2014/07/31/plot-with-ggplot2-interact-collaborate-and-share-online/)
- [A Rosetta Stone for R, ggplot2, Python, MATLAB, and Excel Plotting](http://nbviewer.ipython.org/gist/msund/61cdbd5b22c103fffb84)


Stay in touch
---
- <feedback@plot.ly>
- [@plotlygraphs](https://twitter.com/plotlygraphs)

Contributing
---
- Contributions welcome! Check out our [wiki](https://github.com/ropensci/plotly/wiki/Development-guidelines) for more information. 

---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
