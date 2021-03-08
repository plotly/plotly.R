context("name-mapping")

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
