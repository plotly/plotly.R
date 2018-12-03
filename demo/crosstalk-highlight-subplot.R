library(plotly)

d <- highlight_key(mtcars)
s <- subplot(
  qplot(data = d, x = mpg, y = wt),
  qplot(data = d, x = mpg, y = vs)
)

highlight(s, "plotly_selected")
