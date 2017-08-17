library(crosstalk)
library(ggplot2)
library(gapminder)
library(plotly)

sd <- SharedData$new(gapminder)

g <- ggplot(sd, aes(year, lifeExp, color = country, group = country)) +
  geom_line()

bscols(widths = c(12, 12),
  filter_select("country", "Country:", sd, ~ country),
  ggplotly(g)
)
