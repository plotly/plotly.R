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

test_that("crosstalk keys are inherited in a layer with inherit = FALSE", {
  
  p <- txhousing %>%
    group_by(city) %>%
    crosstalk::SharedData$new(~city, "Select a city") %>%
    plot_ly(x = ~date, y = ~median) %>%
    add_lines(alpha = 0.2) %>%
    add_ribbons(
      x = c(2016, 2017), ymin = c(150000, 160000), ymax = c(200000, 190000),
      inherit = FALSE
    )
  
  b <- plotly_build(p)
  # second trace should have key/set info
  expect_null(b$x$data[[2]][["key"]])
  expect_null(b$x$data[[2]][["set"]])
  # first trace should
  k <- unique(b$x$data[[1]]$key)
  expect_equal(sort(k[!is.na(k)]), sort(unique(txhousing$city)))
  expect_true(b$x$data[[1]][["set"]] == "Select a city")
})

test_that("Simple scatterplot brushing with plot_ly() and subplot()", {
  
  p <- mtcars %>%
    crosstalk::SharedData$new(group = "testing") %>%
    plot_ly(x = ~mpg, y = ~wt)
  
  b <- subplot(p, p) %>% 
    highlight("plotly_selected") %>%
    plotly_build()
  
  expect_true(all(b$x$data[[1]]$key == row.names(mtcars)))
  expect_true(all(b$x$data[[2]]$key == row.names(mtcars)))
  expect_true(b$x$data[[1]]$set == "testing")
  expect_true(b$x$layout$dragmode == "select")
})



# Ignore for now https://github.com/ggobi/ggally/issues/264
#test_that("SharedData produces key/set in ggpairs", {
#  p <- GGally::ggpairs(m, columns = 1:3)
#  l <- plotly_build(p)$x
#  
#  for (i in seq_along(l$data)) {
#    tr <- l$data[[i]]
#    if (tr$mode != "markers") next
#    expect_true(all(tr$key == m$key()))
#    expect_identical(tr$set, m$groupName())
#    expect_false(tr$`_isNestedKey` %||% FALSE)
#    expect_false(tr$`_isSimpleKey` %||% FALSE)
#  }
#  
#})


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



test_that("can handle inconsistent # of traces across frames & supply default colors", {
  d <- data.frame(
    y = rnorm(20),
    score = c(1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2),
    population = c(rep(1, 12), rep(2, 8))
  )
  
  p <- plot_ly(d, y = ~y, split = ~as.factor(score), frame = ~population) %>%
    add_boxplot()
  
  l <- plotly_build(p)$x
  
  expect_length(l$data, 3)
  
  # default colors are the plotly.js defaults
  cols <- sapply(l$data, function(x) x$line$color)
  defaultCols <- toRGB(colorway()[1:3])
  expect_equivalent(cols, defaultCols)
  
  # trace names reflect the split/score (i.e., frames are removed)
  nms <- sapply(l$data, "[[", "name")
  expect_equivalent(nms, levels(as.factor(d$score)))
  
  # 2 frames: both with 3 traces
  expect_length(l$frames, 2)
  expect_length(l$frames[[1]]$data, 3)
  expect_length(l$frames[[2]]$data, 3)
  
  # make sure the frames are targetting the right traces
  expect_equivalent(l$frames[[1]]$traces, 0:2)
  expect_equivalent(l$frames[[2]]$traces, 0:2)
  
  # 1st frame has all 3 traces visible; 2nd frame has 2 visible
  expect_true(
    unique(sapply(l$frames[[1]]$data, "[[", "visible"))
  )
  expect_identical(
    sapply(l$frames[[2]]$data, "[[", "visible"),
    c(TRUE, TRUE, FALSE)
  )
  
  # ensure the default colors remain consistent throughout the animation
  cols <- sapply(l$frames[[1]]$data, function(x) x$line$color)
  expect_equivalent(cols, defaultCols)
  cols <- sapply(l$frames[[2]]$data, function(x) x$line$color)
  expect_equivalent(cols, defaultCols)
  
  # ensure the animation defaults are supplied
  buttonArgs <- l$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]
  defaults <- animation_opts_defaults()
  expect_identical(
    buttonArgs[names(defaults)], defaults
  )
  
  # step values reflect the frame values
  steps <- l$layout$sliders[[1]]$steps
  expect_equivalent(
    unlist(lapply(steps, function(s) s$args[[1]])),
    c("1", "2")
  )
  
  # all the slider steps reflect the animation default
  res <- lapply(steps, function(s) {
    expect_identical(s$args[[2]], defaults)
  })
  
})

test_that("can change animation defaults", {
  
  data(mtcars)
  
  p <- plot_ly(mtcars, x = ~wt, y = ~mpg, frame = ~cyl)  %>%
    animation_opts(frame = 1200, transition = 1000, easing = "elastic") %>%
    animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "YEAR ", font = list(color="red"))
    )
  
  l <- plotly_build(p)$x
  
  expect_length(l$data, 1)
  expect_length(l$frames, 3)
  
  cyl <- as.character(unique(sort(mtcars$cyl)))
  for (i in seq_along(l$frames)) {
    f <- l$frames[[i]]
    expect_equivalent(f$name, cyl[[i]])
    expect_length(f$data, 1)
  }
  
  # the expectation for animation option values
  aniOpts <- modify_list(
    rapply(animation_opts_defaults(), unclass, how = "list"), 
    list(
      frame = list(duration = 1200), 
      transition = list(duration = 1000, easing = "elastic")
    )
  )
  
  # ensure the animation options are supplied
  buttonArgs <- l$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]
  expect_equivalent(
    buttonArgs[names(aniOpts)], aniOpts
  )
  
  # step values reflect the frame values
  steps <- l$layout$sliders[[1]]$steps
  expect_equivalent(
    unlist(lapply(steps, function(s) s$args[[1]])), cyl
  )
  
  # all the slider steps reflect the animation options
  res <- lapply(steps, function(s) {
    expect_identical(
      s$args[[2]], aniOpts
    )
  })
  
})

test_that("simple animation targeting works", {
  
  df <- data.frame(
    x = c(1, 2, 2, 1, 1, 2),
    y = c(1, 2, 2, 1, 1, 2),
    z = c(1, 1, 2, 2, 3, 3)
  )
  p <- plot_ly(df) %>%
    add_markers(x = 1.5, y = 1.5) %>%
    add_markers(x = ~x, y = ~y, frame = ~z)
  
  l <- plotly_build(p)$x
  
  
  expect_length(l$data, 2)
  for (i in seq_along(l$data)) {
    tr <- l$data[[i]]
    # trace names are empty
    expect_equivalent(tr$name %||% "no-name", "no-name")
    # color defaults are retained
    expect_true(tr$marker$color == toRGB(colorway()[[i]]))
  }
  
  # frame trace names are empty
  expect_length(l$frames, 3)
  for (i in seq_along(l$frames)) {
    f <- l$frames[[i]]
    for (j in seq_along(f$data)) {
      tr <- f$data[[j]]
      # trace names are empty
      expect_equivalent(tr$name %||% "no-name", "no-name")
      # color defaults are retained
      expect_true(tr$marker$color == toRGB(colorway()[[2]]))
    }
  }
  
  # since all trace types are scatter, redraw = FALSE
  buttonArgs <- l$layout$updatemenus[[1]]$buttons[[1]]$args
  expect_false(buttonArgs[[2]]$frame$redraw)
  
  steps <- l$layout$sliders[[1]]$steps
  res <- lapply(steps, function(s) {
    expect_false(s$args[[2]]$frame$redraw)
  })
})

test_that("animation frames are boxed up correctly", {
  dallas <- subset(txhousing, city == "Dallas" & month == 1)
  p <- ggplot(dallas) +
    geom_point(aes(x = volume, y = sales, frame = year))
  l <- plotly_build(p)$x
  
  for (i in seq_along(l$frames)) {
    traces <- l$frames[[i]]$data
    for (j in seq_along(traces)) {
      x <- traces[[j]]$x
      y <- traces[[j]]$y
      expect_true(length(x) > 1 || inherits(x, "AsIs"))
      expect_true(length(y) > 1 || inherits(y, "AsIs"))
    }
  }
  
})

test_that("animation button can be customized", {
  
  p <- plot_ly(mtcars, x = ~mpg, y = ~wt, frame = ~vs) %>% 
    animation_button(label = "Custom", bgcolor = "red", font = list(color = "white"))
  
  f <- plotly_build(p)$x
  
  menu <- f$layout$updatemenus[[1]]
  expect_true(menu$bgcolor == "red")
  expect_true(menu$font$color == "white")
  expect_true(menu$buttons[[1]]$label == "Custom")
})


test_that("sf works with crosstalk", {
  skip_if_not_installed("sf")
  
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  # shared data will make the polygons "query-able"
  ncsd <- crosstalk::SharedData$new(nc)
  p <- ggplot(ncsd) +
    geom_sf(aes(fill = AREA, text = paste0(NAME, "\n", "FIPS: ", FIPS))) +
    ggthemes::theme_map()
  gg <- ggplotly(p, tooltip = "text")
  d <- gg$x$data
  for (i in seq_along(d)) {
    if (!isTRUE(d[["_isGraticule"]])) next
    expect_false(is.null(d[[i]]$key))
    expect_false(is.null(d[[i]]$set))
  }
})
