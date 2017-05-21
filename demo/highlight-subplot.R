library(plotly)
library(crosstalk)

d <- SharedData$new(mtcars)
s <- subplot(
  qplot(data = d, x = mpg, y = wt),
  qplot(data = d, x = mpg, y = vs)
)

highlight(s, "plotly_selected")
