library(plotly)
library(crosstalk)

d <- SharedData$new(txhousing, ~city)
p <- qplot(data = d, x = date, y = median, group = city, geom = "line")
o <- ct_opts(on = "plotly_hover", off = "plotly_doubleclick", color = "red")
ggplotly(p, tooltip = "city", crosstalkOpts = o)
