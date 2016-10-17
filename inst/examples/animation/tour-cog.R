library(tidyr)
library(dplyr)
library(plotly)
library(crosstalk)
library(tourr)


#princomp(tsobj, na.action = na.omit)



# tour of tscognostics...why does the scale explode?

# transform tidy data into a multivariate timeseries object
tsobj <- txhousing %>%
  select(city, date, median) %>%
  spread(city, median) %>%
  select(-date) %>%
  as.matrix() %>%
  ts(frequency = 12, start = c(2000, 1), end = c(2015, 7))

m <- rescale(tscognostics::tsmeasures(tsobj))

tour <- new_tour(m, grand_tour(), NULL)

tour_dat <- function(step_size) {
  step <- tour(step_size)
  proj <- center(m %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], city = colnames(tsobj))
}

steps <- c(0, rep(1/15, 200))
stepz <- cumsum(steps)

# tidy version of tour data
tour_dats <- lapply(steps, tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)

ax <- list(
  title = "",
  range = c(-1, 1),
  zeroline = FALSE
)


pcp <- as.data.frame(m) %>%
  mutate(city = colnames(tsobj)) %>%
  gather(variable, value, -city) %>%
  SharedData$new(~city, "Texan City") %>%
  plot_ly(x = ~variable, y = ~value, color = I("black")) %>%
  group_by(city) %>%
  add_lines() %>%
  add_markers(size = I(1)) %>%
  layout(xaxis = list(title = ""), showlegend = F)
  
tour <- tour_dat %>%
  SharedData$new(~city, "Texan City") %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black")) %>%
  add_markers(text = ~city, hoverinfo = "text") %>%
  hide_legend() %>%
  layout(xaxis = ax, yaxis = ax) %>%
  animationOpts(30, 1000)

# TODO: throw warning/error if trying to do animations in a subplot?
# subplot(pcp, tour)
library(shiny)
pg <- fluidPage(
  fluidRow(column(6, tour), column(6, pcp))
)
htmltools::browsable(pg)
