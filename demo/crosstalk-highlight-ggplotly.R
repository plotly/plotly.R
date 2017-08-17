library(plotly)
library(crosstalk)

d <- SharedData$new(txhousing, ~city)
p <- ggplot(d, aes(date, median, group = city)) + geom_line()
ggplotly(p, tooltip = "city") %>%
  highlight(on = "plotly_hover", color = "red")


# crosstalk keys are automatically added to the group aesthetic...
# if you want to avoid adding the key to group for a layer,
# use the original data
sd <- SharedData$new(txhousing, ~city)
p <- ggplot(sd, aes(month, median)) +
  geom_line(aes(group = city)) + 
  geom_smooth(data = txhousing, method = "gam") + 
  facet_wrap(~ year)
ggplotly(p) %>%
  highlight(on = "plotly_click", color = "red")

# perhaps a more useful example
sd <- SharedData$new(txhousing, ~year)
p <- ggplot(sd, aes(month, median)) +
  geom_line(aes(group = year)) + 
  geom_smooth(data = txhousing, method = "gam") + 
  facet_wrap(~ city)
ggplotly(p, height = 800, width = 1600) %>%
  highlight(on = "plotly_click", color = "red")

# TODO: why doesn't this work?
sd <- SharedData$new(mtcars, ~cyl)
p <- ggplot(sd, aes(factor(vs))) + 
  geom_bar() + facet_wrap(~am)
ggplotly(p) %>%
  highlight(on = "plotly_click", color = "red")
