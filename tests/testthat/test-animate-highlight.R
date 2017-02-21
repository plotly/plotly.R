context("highlighting and animation")

m <- crosstalk::SharedData$new(mtcars, ~vs)

test_that("SharedData produces key/set in plot_ly", {
  m <- crosstalk::SharedData$new(mtcars, ~vs)
  p <- plot_ly(m, x = ~wt, y = ~mpg) %>% add_markers()
  tr <- plotly_build(p)$x$data[[1]]
  
  expect_true(all(tr$key == m$key()))
  expect_identical(tr$set, m$groupName())
  expect_false(tr$`_isNestedKey` %||% FALSE)
  expect_false(tr$`_isSimpleKey` %||% FALSE)
})

test_that("SharedData produces key/set in ggplotly", {
  p <- ggplot(m, aes(x = wt, y = mpg)) + geom_point()
  tr <- plotly_build(p)$x$data[[1]]
  
  expect_true(all(tr$key == m$key()))
  expect_type(tr$set, "character")
  expect_length(tr$set, 1)
  expect_false(tr$`_isNestedKey` %||% FALSE)
  expect_false(tr$`_isSimpleKey` %||% FALSE)
})

test_that("SharedData produces key/set in ggpairs", {
  p <- GGally::ggpairs(m, columns = 1:3)
  l <- plotly_build(p)$x
  
  for (i in seq_along(l$data)) {
    tr <- l$data[[i]]
    if (tr$mode != "markers") next
    expect_true(all(tr$key == m$key()))
    expect_identical(tr$set, m$groupName())
    expect_false(tr$`_isNestedKey` %||% FALSE)
    expect_false(tr$`_isSimpleKey` %||% FALSE)
  }
  
})


test_that("When key is equivalent to group, produce simple keys", {
  gg <- ggplot(m, aes(wt, mpg, color = factor(vs))) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  # for interactive testing -- `highlight(gg, "plotly_click")`
  
  l <- plotly_build(gg)$x
  
  for (i in seq_along(l$data)) {
    tr <- l$data[[i]]
    expect_false(tr$`_isNestedKey` %||% FALSE)
    
    if (tr$mode == "markers") {
      # clicking on a single point should select the whole group in a efficient
      # (i.e., no trace subsetting occurs for simple keys) manner
      expect_true(tr$key == tr$name)
      expect_true(tr$`_isSimpleKey`)
    } else {
      # TODO: shouldn't key be a length 1 here?
      expect_true(tr$name %in% tr$key)
      expect_true(tr$`_isSimpleKey`)
    }
  }
  
})


m2 <- crosstalk::SharedData$new(mtcars)

test_that("When key is nested within group, produce simple key", {
  gg <- ggplot(m2, aes(wt, mpg, color = factor(vs))) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  # for interactive testing -- `highlight(gg, "plotly_click")`
  
  l <- plotly_build(gg)$x
  
  for (i in seq_along(l$data)) {
    tr <- l$data[[i]]
    
    key <- m2$key()[mtcars$vs == tr$name]
    expect_true(all(tr$key == key))
    
    if (tr$mode == "markers") {
      expect_false(tr$`_isSimpleKey` %||% FALSE)
      expect_false(tr$`_isNestedKey` %||% FALSE)
    } else {
      expect_true(tr$`_isSimpleKey`)
      expect_false(tr$`_isNestedKey` %||% FALSE)
    }
  }
  
})


test_that("Key structure is passed along to frame data", {
  p <- ggplot(m2, aes(wt, mpg, color = factor(vs), frame = am)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
  
  # TODO: why doesn't the highlight update on the second frame?
  # animation_opts(p, 0, redraw = T) %>% highlight("plotly_click")
  
  l <- suppressWarnings(plotly_build(p)$x)
  
  for (i in seq_along(l$data)) {
    tr <- l$data[[i]]
    
    key <- m2$key()[mtcars$vs == tr$name & mtcars$am == tr$frame]
    expect_true(all(tr$key == key))
  }
  
  # the fitted line of every frame should have a simple key
  for (i in seq_along(l$frames)) {
    fr <- l$frames[[i]]
    for (j in seq_along(fr$data)) {
      tr <- fr$data[[j]]
      if (tr$mode != "lines") next
      expect_true(tr$`_isSimpleKey`)
    }
  }
  
})
