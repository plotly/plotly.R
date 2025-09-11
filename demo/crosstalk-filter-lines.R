library(crosstalk)
library(ggplot2)
library(plotly)

# Equivalent to data(gapminder, package = "gapminder"), but avoids R CMD check NOTE 
# about `gapminder` not being in DESCRIPTION. Install it `install.packages("gapminder")`
lazyLoad(
  file.path(system.file("data", package = "gapminder"), "Rdata"),
  filter = function(x) x == "gapminder"
)

sd <- highlight_key(gapminder)

g <- ggplot(sd, aes(year, lifeExp, color = country, group = country)) +
  geom_line()

bscols(widths = c(12, 12),
  filter_select("country", "Country:", sd, ~ country),
  ggplotly(g)
)
