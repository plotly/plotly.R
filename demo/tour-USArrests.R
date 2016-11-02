# adapted from  https://github.com/rstudio/ggvis/blob/master/demo/tourr.r
library(tourr)
library(plotly)

mat <- rescale(USArrests)
tour <- new_tour(mat, grand_tour(), NULL)

tour_dat <- function(step_size) {
  step <- tour(step_size)
  proj <- center(mat %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], state = rownames(mat))
}

proj_dat <- function(step_size) {
  step <- tour(step_size)
  data.frame(
    x = step$proj[,1], y = step$proj[,2], state = colnames(mat)
  )
}

steps <- c(0, rep(1/15, 200))
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
  title = "", showticklabels = FALSE,
  zeroline = FALSE, showgrid = FALSE,
  range = c(-1.1, 1.1)
)

# for nicely formatted slider labels
options(digits = 2)

tour_dat <- crosstalk::SharedData$new(tour_dat, ~state, group = "A")

p1 <- proj_dat %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black")) %>%
  add_segments(xend = 0, yend = 0, color = I("gray80")) %>%
  add_text(text = ~state) %>%
  add_markers(data = tour_dat, text = ~state, hoverinfo = "text") %>%
  layout(xaxis = ax, yaxis = ax)

p2 <- USArrests %>% 
  dist() %>% 
  hclust() %>%
  as.dendrogram() %>%
  plot_dendro(set = "A", xmin = -100, height = 600, width = 1000)

subplot(p1, p2, shareY = FALSE) %>%
  hide_legend() %>%
  animation_opts(33, easing = "cubic-in-out") %>%
  highlight("plotly_click", NULL, dynamic = T, persistent = T)
