library(plotly)
library(tidyr)
library(crosstalk)

m <- gather(mtcars, variable, value, -vs)
msd <- highlight_key(m, ~variable)
gg <- ggplot(msd, aes(factor(vs), value)) + 
  geom_jitter(alpha = 0.3)

bscols(
  widths = c(11, 6, 6),
  filter_select("id", "Select a variable", msd, ~variable, multiple = FALSE),
  ggplotly(gg, dynamicTicks = "y") %>% layout(margin = list(l = 30)),
  plot_ly(msd, x = ~jitter(vs), y = ~value) %>% add_markers(alpha = 0.3)
)
