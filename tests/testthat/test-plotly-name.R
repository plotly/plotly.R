
test_that("can create multiple traces from name argument", {
  l <- plot_ly() %>% 
    add_markers(x = 1:10, y = 1:10, name = rep(c("a", "b"), 5)) %>%
    plotly_build()
  
  expect_length(l$x$data, 2)
  expect_equal(l$x$data[[1]]$name, "a")
  expect_equal(l$x$data[[2]]$name, "b")
})


test_that("can override name argument", {
  l <- plot_ly() %>% 
    add_markers(x = 1:10, y = 1:10, split = rep(c("a", "b"), 5), name = "z") %>%
    plotly_build()

  expect_length(l$x$data, 2)
  expect_equal(l$x$data[[1]]$name, "z")
  expect_equal(l$x$data[[2]]$name, "z")
  
  # can get back old behvaior
  l2 <- plot_ly() %>% 
    add_markers(x = 1:10, y = 1:10, split = rep(c("a", "b"), 5), name = paste0(rep(c("a", "b"), 5), "<br>z")) %>%
    plotly_build()
  
  expect_length(l2$x$data, 2)
  expect_equal(l2$x$data[[1]]$name, "a<br>z")
  expect_equal(l2$x$data[[2]]$name, "b<br>z")
  
})


test_that("doesn't break old behavior", {
  # from https://community.plot.ly/t/manual-color-bug/10479
  density1 <- density(diamonds[diamonds$cut %in% "Fair", ]$carat)
  density2 <- density(diamonds[diamonds$cut %in% "Ideal",]$carat)
  
  l <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Fair cut', fill = 'tozeroy',
          fillcolor = 'rgba(168, 216, 234, 0.5)',
          line = list(width = 0.5)) %>%
    add_trace(x = ~density2$x, y = ~density2$y, name = 'Ideal cut', fill = 'tozeroy',
              fillcolor = 'rgba(255, 212, 96, 0.5)') %>%
    plotly_build()
  
  
  expect_equal(l$x$data[[1]]$name, "Fair cut")
  expect_equal(l$x$data[[2]]$name, "Ideal cut")
})


test_that("adding trace name with frame does not throw frameOrder warning", {
  
  dt <- data.frame(source = rep(c(rep("TEL", 2) , rep("WEB", 2), rep("OTH",2)),2), 
                   period = rep(c("AM", "PM"), 6), 
                   y_val = runif(12), 
                   year = c(rep(2020,6), rep(2021,6)))
  
  
  p1 <- plot_ly()
  
  for (yr in unique(dt$year)){
    
    which_lines <- which(dt$year==yr)
    
    p1 <- add_trace(p1, 
                    x = dt$period[which_lines], 
                    y = dt$y_val[which_lines], 
                    frame = dt$source[which_lines],
                    type = "scatter", mode = "lines+markers", 
                    name = yr)
  }
  
  expect_warning(l <- plotly_build(p1), NA) 
  
  expect_equal(l$x$data[[1]]$name, 2020)
  expect_equal(l$x$data[[2]]$name, 2021)
  
  
})

test_that("adding trace name does not throw error", {
  
  #From plotly/plotly.R/issues/1618
  df <- data.frame(category=c('a', 'b', 'c', 'd', 'a', 'b', 'c', 'd'),
                   year=c(2000, 2000, 2000, 2000, 2001, 2001, 2001, 2001),
                   val_a=c(1,2,2,1,2,5,6,8),
                   val_b=c(3,5,4,7,1,9,2,12))
  
  
  p1 <- plot_ly(data = df, frame = ~year) %>%
    add_markers(x = ~val_a, y = ~category, name = "Val_A", color = I("red")) %>%
    add_markers(x = ~val_b, y = ~category, name = "Val_B", color = I("blue")) %>%
    add_segments(x = ~val_a, xend = ~val_b, y = ~category, yend = ~category, showlegend=F) %>%
    layout(
      title = "Val A v Val B",
      xaxis = list(title = "Value"), 
      yaxis = list(title = ""),
      margin = list(l = 65)
      )  
  
  
  expect_error(l <- plotly_build(p1), NA) 
  
  expect_equal(l$x$data[[1]]$name, "Val_A")
  expect_equal(l$x$data[[2]]$name, "Val_B")
  
  
  #From plotly/plotly.R/issues/1903
  df1 <- data.frame(frame = 1:10, x = 1:10, y = 0)
  df2 <- data.frame(frame = rep(1:10, 1:10), 
                    x = unlist(lapply(1:10, function(x) 1:x)),
                    y = 1)
  
  p2 <-  plot_ly() %>%
    add_trace(data = df1, type = "scatter", mode = "markers", x = ~x, y = ~y, frame = ~frame, name= "A") %>%
    add_trace(data = df2, type = "scatter", mode = "lines", x = ~x, y = ~y, frame = ~frame, name = "B")
  
  expect_error(l1 <- plotly_build(p2), NA) 
  
})
