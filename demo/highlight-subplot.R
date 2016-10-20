library(plotly)
library(crosstalk)

d <- SharedData$new(mtcars)
subplot(
  qplot(data = d, x = mpg, y = wt),
  qplot(data = d, x = mpg, y = vs)
)
