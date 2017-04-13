# adapted from  https://github.com/rstudio/ggvis/blob/master/demo/tourr.r
library(tourr)
library(plotly)

mat <- rescale(as.matrix(flea[1:6]))
tour <- new_tour(mat, grand_tour(), NULL)

tour_dat <- function(step_size) {
  step <- tour(step_size)
  proj <- center(mat %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], 
             species = flea$species)
}

proj_dat <- function(step_size) {
  step <- tour(step_size)
  data.frame(
    x = step$proj[,1], y = step$proj[,2], measure = colnames(mat)
 )
}

steps <- c(0, rep(1/15, 50))
stepz <- cumsum(steps)

# tidy version of tour data
tour_dats <- lapply(steps, tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)

# tidy version of tour projection data
proj_dats <- lapply(steps, proj_dat)
proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
proj_dat <- dplyr::bind_rows(proj_datz)


ax <- list(
  title = "",
  range = c(-1, 1),
  zeroline = FALSE
)

# for nicely formatted slider labels
options(digits = 2)

proj_dat %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("gray80")) %>%
  add_segments(xend = 0, yend = 0) %>%
  add_text(text = ~measure) %>%
  add_markers(color = ~species, data = tour_dat) %>%
  hide_legend() %>%
  layout(xaxis = ax, yaxis = ax) %>%
  animation_opts(33, redraw = FALSE)
