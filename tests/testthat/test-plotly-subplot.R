context("subplot")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- expect_doppelganger_built(p, paste0("plotly-subplot-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}

test_that("simple subplot works", {
  p1 <- plot_ly(x = c(1, 2))
  p2 <- plot_ly(x = c(1, 2))
  s <- expect_traces(subplot(p1, p2), 2, "simple")
  expect_identical(s$data[[2]]$xaxis, s$layout[["yaxis2"]][["anchor"]])
  expect_identical(s$data[[2]]$yaxis, s$layout[["xaxis2"]][["anchor"]])
  doms <- lapply(s$layout[grepl("^xaxis", names(s$layout))], "[[", "domain")
  expect_true(doms$xaxis[2] <= doms$xaxis2[1])
})

test_that("nrows argument works", {
  p1 <- plot_ly(x = c(1, 2))
  p2 <- plot_ly(x = c(1, 2))
  s <- expect_traces(subplot(p1, p2, nrows = 2), 2, "simple2")
  expect_identical(s$data[[2]]$xaxis, s$layout[["yaxis2"]][["anchor"]])
  expect_identical(s$data[[2]]$yaxis, s$layout[["xaxis2"]][["anchor"]])
  doms <- lapply(s$layout[grepl("^[x-y]axis", names(s$layout))], "[[", "domain")
  expect_true(doms$yaxis[2] > doms$yaxis[1])
  expect_true(doms$yaxis[1] > doms$yaxis2[2])
  expect_true(doms$yaxis2[2] > doms$yaxis2[1])
})

test_that("group + [x/y]axis works", {
  iris$id <- as.integer(iris$Species)
  p <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width, color = ~Species,
               xaxis = ~paste0("x", id), mode = "markers")
  s <- expect_traces(subplot(p, margin = 0.05), 3, "group")
  ax <- s$layout[grepl("^[x-y]axis", names(s$layout))]
  doms <- lapply(ax, "[[", "domain")
  # make sure y domain is [0, 1] on every axis
  ydom <- doms[grepl("^y", names(doms))]
  expect_equivalent(sort(unique(unlist(ydom))), c(0, 1))
  xdom <- doms[grepl("^x", names(doms))]
  expect_true(all(1/3 > xdom[[1]] & xdom[[1]] >= 0))
  expect_true(all(2/3 > xdom[[2]] & xdom[[2]] > 1/3))
  expect_true(all(1 >= xdom[[3]] & xdom[[3]] > 2/3))
})

test_that("shareX produces one x-axis and a legend", {
  s <- subplot(plot_ly(x = 1), plot_ly(x = 1), nrows = 2, shareX = TRUE)
  l <- expect_traces(s, 2, "shareX")
  expect_true(sum(grepl("^xaxis", names(l$layout))) == 1)
  expect_true(l$data[[1]]$showlegend %||% TRUE)
  expect_true(l$data[[2]]$showlegend %||% TRUE)
  expect_true(l$layout$showlegend %||% TRUE)
})

test_that("shareY produces one y-axis", {
  s <- subplot(plot_ly(x = 1), plot_ly(x = 1), shareY = TRUE)
  l <- expect_traces(s, 2, "shareY")
  expect_true(sum(grepl("^yaxis", names(l$layout))) == 1)
})

test_that("share both axes", {
  s <- subplot(
    plot_ly(x = 1), plot_ly(x = 1), plot_ly(x = 1), plot_ly(x = 1), 
    nrows = 2, shareX = TRUE, shareY = TRUE
  )
  l <- expect_traces(s, 4, "shareBoth")
  expect_true(sum(grepl("^yaxis", names(l$layout))) == 2)
  expect_true(sum(grepl("^xaxis", names(l$layout))) == 2)
})

# https://github.com/ropensci/plotly/issues/376
d <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)
hist_top <- ggplot(d) + geom_histogram(aes(x = x))
empty <- ggplot() + geom_blank()
scatter <- ggplot(d) + geom_point(aes(x = x, y = y))
hist_right <- ggplot(d) + geom_histogram(aes(x = y)) + coord_flip()
s <- subplot(
  hist_top, empty, scatter, hist_right, 
  nrows = 2, widths = c(0.8, 0.2), heights = c(0.2, 0.8),
  margin = 0.005, shareX = TRUE, shareY = TRUE
)

test_that("Row/column height/width", {
  l <- expect_traces(s, 3, "width-height")
  expect_equivalent(diff(l$layout$xaxis$domain), 0.8 - 0.005)
  expect_equivalent(diff(l$layout$xaxis2$domain), 0.2 - 0.005)
  expect_equivalent(diff(l$layout$yaxis$domain), 0.2 - 0.005)
  expect_equivalent(diff(l$layout$yaxis2$domain), 0.8 - 0.005)
})

test_that("recursive subplots work", {
  p1 <- plot_ly(economics, x = ~date, y = ~unemploy)
  p2 <- plot_ly(economics, x = ~date, y = ~uempmed)
  s1 <- subplot(p1, p1, shareY = TRUE)
  s2 <- subplot(p2, p2, shareY = TRUE)
  s <- subplot(s1, s2, nrows = 2, shareX = TRUE)
  l <- expect_traces(s, 4, "recursive")
  xaxes <- l$layout[grepl("^xaxis", names(l$layout))]
  yaxes <- l$layout[grepl("^yaxis", names(l$layout))]
  expect_true(length(xaxes) == 2)
  expect_true(length(yaxes) == 2)
  # both x-axes are anchored on the same y-axis
  yanchor <- unique(unlist(lapply(xaxes, "[[", "anchor")))
  expect_true(length(yanchor) == 1)
  # both y-axes are anchored on the same x-axis
  xanchor <- unique(unlist(lapply(yaxes, "[[", "anchor")))
  expect_true(length(xanchor) == 1)
  # x/y are anchored on the bottom/left
  expect_true(l$layout[[sub("x", "xaxis", xanchor)]]$domain[1] == 0)
  expect_true(l$layout[[sub("y", "yaxis", yanchor)]]$domain[1] == 0)
  # every trace is anchored on a different x/y axis pair
  xTraceAnchors <- sapply(l$data, "[[", "xaxis")
  yTraceAnchors <- sapply(l$data, "[[", "yaxis")
  expect_true(length(unique(paste(xTraceAnchors, yTraceAnchors))) == 4)
})

test_that("subplot accepts a list of plots", {
  vars <- setdiff(names(economics), "date")
  plots <- lapply(vars, function(var) {
    plot_ly(x = economics$date, y = economics[[var]], name = var)
  })
  s <- subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)
  l <- expect_traces(s, 5, "plot-list")
  xaxes <- l$layout[grepl("^xaxis", names(l$layout))]
  yaxes <- l$layout[grepl("^yaxis", names(l$layout))]
  expect_true(length(xaxes) == 1)
  expect_true(length(yaxes) == 5)
  # x-axis is anchored at the bottom
  expect_true(l$layout[[sub("y", "yaxis", xaxes[[1]]$anchor)]]$domain[1] == 0)
})

test_that("ggplotly understands ggmatrix", {
  skip_if_not_installed("GGally")
  L <- expect_doppelganger_built(GGally::ggpairs(iris), "plotly-subplot-ggmatrix")
})

test_that("annotation paper repositioning", {
  p1 <- plot_ly() %>%
    add_annotations(text = "foo", x = 0.5, y = 0.5, xref = "paper", yref = "paper")
  p2 <- plot_ly(mtcars) %>%
    add_annotations(text = "bar", x = 0.5, y = 0.5, xref = "paper", yref = "paper")
  
  s <- subplot(p1, p2, margin = 0)
  ann <- expect_doppelganger_built(s, "subplot-reposition-annotation")$layout$annotations
  expect_length(ann, 2)
  
  text <- sapply(ann, "[[", "text")
  x <- sapply(ann, "[[", "x")
  y <- sapply(ann, "[[", "y")
  xref <- sapply(ann, "[[", "xref")
  yref <- sapply(ann, "[[", "yref")
  
  expect_equal(x, c(0.25, 0.75))
  expect_equal(y, c(0.5, 0.5))
  expect_equal(xref, rep("paper", 2))
  expect_equal(yref, rep("paper", 2))
})

test_that("shape paper repositioning", {
  
  p1 <- plot_ly(mtcars) %>%
    layout(
      shapes = ~list(
        type = "rect",
        x0 = 0.25,
        x1 = 0.75,
        y0 = 0.25,
        y1 = 0.75,
        xref = "paper",
        yref = "paper",
        fillcolor = "red"
      )
    )
  p2 <- plot_ly(mtcars) %>%
    layout(
      shapes = ~list(
        type = "line",
        type = "rect",
        x0 = 0.25,
        x1 = 0.75,
        y0 = 0.25,
        y1 = 0.75,
        xref = "paper",
        yref = "paper",
        line = list(color = "blue")
      )
    )
  
  s <- subplot(p1, p2)
  shapes <- expect_doppelganger_built(s, "subplot-reposition-shape")$layout$shapes
  expect_length(shapes, 2)
  
  x0 <- sapply(shapes, "[[", "x0")
  x1 <- sapply(shapes, "[[", "x1")
  y0 <- sapply(shapes, "[[", "y0")
  y1 <- sapply(shapes, "[[", "y1")
  xref <- sapply(shapes, "[[", "xref")
  yref <- sapply(shapes, "[[", "yref")
  
  expect_equal(x0, c(0.12, 0.64))
  expect_equal(x1, c(0.36, 0.88))
  expect_equal(y0, rep(0.25, 2))
  expect_equal(y1, rep(0.75, 2))
  expect_equal(xref, rep("paper", 2))
  expect_equal(yref, rep("paper", 2))
  
  # now with a fixed height/width
  p1 <- plot_ly() %>%
    layout(
      shapes = list(
        type = "rect",
        x0 = 0.25,
        x1 = 0.75,
        xref = "paper",
        y0 = 0,
        y1 = 30,
        yanchor = 0.5,
        ysizemode = "pixel",
        yref = "paper",
        fillcolor = "red"
      )
    )
  p2 <- plot_ly() %>%
    layout(
      shapes = list(
        type = "rect",
        y0 = 0.25,
        y1 = 0.75,
        yref = "paper",
        x0 = 0,
        x1 = 30,
        xanchor = 0.5,
        xsizemode = "pixel",
        xref = "paper",
        line = list(color = "blue")
      )
    )
  
  s <- subplot(p1, p2)
  shapes <- expect_doppelganger_built(s, "subplot-reposition-shape-fixed")$layout$shapes
  expect_length(shapes, 2)
  
  xanchor <- lapply(shapes, "[[", "xanchor")[[2]]
  yanchor <- lapply(shapes, "[[", "yanchor")[[1]]
  x0 <- sapply(shapes, "[[", "x0")
  x1 <- sapply(shapes, "[[", "x1")
  y0 <- sapply(shapes, "[[", "y0")
  y1 <- sapply(shapes, "[[", "y1")
  
  expect_equal(xanchor, 0.76)
  expect_equal(yanchor, 0.5)
  expect_equal(x0, c(0.12, 0))
  expect_equal(x1, c(0.36, 30))
  expect_equal(y0, c(0, 0.25))
  expect_equal(y1, c(30, 0.75))
})


test_that("image paper repositioning", {
  skip_if_not_installed("png")
  
  r <- as.raster(matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5))
  
  # embed the raster as an image
  p <- plot_ly(x = 1, y = 1) %>% 
    layout(
      images = list(list(
        source = raster2uri(r),
        sizing = "fill",
        xref = "paper", 
        yref = "paper", 
        x = 0, 
        y = 0, 
        sizex = 0.5, 
        sizey = 0.5, 
        xanchor = "left", 
        yanchor = "bottom"
      ))
    ) 
  
  s <- subplot(p, p, nrows = 1, margin = 0.02)
  imgs <- expect_doppelganger_built(s, "subplot-reposition-image")$layout$images
  
  expect_length(imgs, 2)
  
  x <- sapply(imgs, "[[", "x")
  y <- sapply(imgs, "[[", "y")
  sizex <- sapply(imgs, "[[", "sizex")
  sizey <- sapply(imgs, "[[", "sizey")
  
  expect_equal(x, c(0, 0.52))
  expect_equal(y, c(0, 0))
  expect_equal(sizex, rep(0.24, 2))
  expect_equal(sizey, rep(0.5, 2))
})

test_that("annotation xref/yref bumping", {
  
  p1 <- plot_ly(mtcars) %>%
    add_annotations(text = ~cyl, x = ~wt, y = ~mpg)
  p2 <- plot_ly(mtcars) %>%
    add_annotations(text = ~am, x = ~wt, y = ~mpg)
  s <- subplot(p1, p2)
  ann <- expect_doppelganger_built(s, "subplot-bump-axis-annotation")$layout$annotations
  
  txt <- sapply(ann, "[[", "text")
  xref <- sapply(ann, "[[", "xref")
  yref <- sapply(ann, "[[", "yref")
  
  expect_length(ann, 64)
  expect_equal(txt, c(mtcars$cyl, mtcars$am))
  expect_equal(xref, rep(c("x", "x2"), each = 32))
  expect_equal(yref, rep(c("y", "y2"), each = 32))
  
  s2 <- subplot(p1, p2, shareY = TRUE)
  ann2 <- expect_doppelganger_built(s2, "subplot-bump-axis-annotation-shared")$layout$annotations
  
  xref2 <- sapply(ann2, "[[", "xref")
  yref2 <- sapply(ann2, "[[", "yref")
  expect_equal(xref2, rep(c("x", "x2"), each = 32))
  expect_equal(yref2, rep(c("y", "y"), each = 32))
  
  # now, with more traces than annotations
  # https://github.com/ropensci/plotly/issues/1444
  p1 <- plot_ly() %>%
    add_markers(x = 1, y = 1) %>%
    add_markers(x = 2, y = 2) %>%
    add_annotations(text = "foo", x = 1.5, y = 1.5)
  p2 <- plot_ly() %>%
    add_markers(x = 1, y = 1) %>%
    add_markers(x = 2, y = 2) %>%
    add_annotations(text = "bar", x = 1.5, y = 1.5)
  s <- subplot(p1, p2)
  ann <- expect_doppelganger_built(s, "subplot-bump-axis-annotation-traces")$layout$annotations
  
  txt <- sapply(ann, "[[", "text")
  xref <- sapply(ann, "[[", "xref")
  yref <- sapply(ann, "[[", "yref")
  
  expect_length(ann, 2)
  expect_equal(txt, c("foo", "bar"))
  expect_equal(xref, c("x", "x2"))
  expect_equal(yref, c("y", "y2"))
  
  s2 <- subplot(p1, p2, shareY = TRUE)
  ann2 <- expect_doppelganger_built(s2, "subplot-bump-axis-annotation-traces-shared")$layout$annotations
  
  xref2 <- sapply(ann2, "[[", "xref")
  yref2 <- sapply(ann2, "[[", "yref")
  expect_equal(xref2, c("x", "x2"))
  expect_equal(yref2, c("y", "y"))
})

test_that("shape xref/yref bumping", {
  
  p1 <- plot_ly(mtcars) %>%
    layout(
      shapes = ~list(
        type = "rect",
        x0 = min(cyl),
        x1 = max(cyl),
        y0 = min(am),
        y1 = max(am),
        fillcolor = "red"
      )
    )
  p2 <- plot_ly(mtcars) %>%
    layout(
      shapes = ~list(
        type = "line",
        x0 = min(cyl),
        x1 = max(cyl),
        y0 = min(am),
        y1 = max(am),
        line = list(color = "blue")
      )
    )
  
  s <- subplot(p1, p2)
  shapes <- expect_doppelganger_built(s, "subplot-bump-axis-shape")$layout$shapes
  expect_length(shapes, 2)
  
  types <- sapply(shapes, "[[", "type")
  expect_equal(types, c("rect", "line"))
  
  xref <- sapply(shapes, "[[", "xref")
  yref <- sapply(shapes, "[[", "yref")
  expect_equal(xref, c("x", "x2"))
  expect_equal(yref, c("y", "y2"))
  
  s2 <- subplot(p1, p2, shareY = TRUE)
  shapes2 <- expect_doppelganger_built(s2, "subplot-bump-axis-shape-shared")$layout$shapes
  
  xref2 <- sapply(shapes2, "[[", "xref")
  yref2 <- sapply(shapes2, "[[", "yref")
  expect_equal(xref2, c("x", "x2"))
  expect_equal(yref2, c("y", "y"))
})

test_that("image xref/yref bumping", {
  skip_if_not_installed("png")
  
  r <- as.raster(matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5))
  
  # embed the raster as an image
  p <- plot_ly(x = 1, y = 1) %>% 
    layout(
      images = list(list(
        source = raster2uri(r),
        sizing = "fill",
        xref = "x", 
        yref = "y", 
        x = 0, 
        y = 0, 
        sizex = 1, 
        sizey = 1, 
        xanchor = "left", 
        yanchor = "bottom"
      ))
    )
  
  s <- subplot(p, p, nrows = 1, margin = 0.02)
  imgs <- expect_doppelganger_built(s, "subplot-bump-axis-image")$layout$images
  
  expect_length(imgs, 2)
  
  x <- sapply(imgs, "[[", "x")
  y <- sapply(imgs, "[[", "y")
  xref <- sapply(imgs, "[[", "xref")
  yref <- sapply(imgs, "[[", "yref")
  
  expect_equal(x, c(0, 0))
  expect_equal(y, c(0, 0))
  expect_equal(xref, c("x", "x2"))
  expect_equal(yref, c("y", "y2"))
})



test_that("geo+cartesian behaves", {
  # specify some map projection/options
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    lakecolor = toRGB('white')
  )
  # create a map of population density
  density <- state.x77[, "Population"] / state.x77[, "Area"]
  map <- plot_geo(
    z = ~density, text = state.name, 
    locations = state.abb, locationmode = 'USA-states'
  ) %>% layout(geo = g)
  # create a bunch of horizontal bar charts 
  vars <- colnames(state.x77)
  barcharts <- lapply(vars, function(var) {
    plot_ly(x = state.x77[, var], y = state.name, type = "bar", 
            orientation = "h", name = var) %>%
      layout(showlegend = FALSE, hovermode = "y",
             yaxis = list(showticklabels = FALSE))
  })
  s <- subplot(
    subplot(barcharts, margin = 0.01), map, 
    nrows = 2, heights = c(0.3, 0.7)
  )
  l <- expect_traces(s, 9, "geo-cartesian")
  geoDom <- l$layout[[grep("^geo", names(l$layout))]]$domain
  expect_equivalent(geoDom$x, c(0, 1))
  expect_equivalent(geoDom$y, c(0, 0.68))
})



test_that("May specify legendgroup with through a vector of values", {
  
  # example adapted from https://github.com/ropensci/plotly/issues/817
  df <- dplyr::bind_rows(
    data.frame(x = rnorm(100,2), Name = "x1"),
    data.frame(x = rnorm(100,6), Name = "x2"),
    data.frame(x = rnorm(100,4), Name = "x3")
  )
  df$y <- rnorm(300)
  
  # marker definition...
  m <- list(
    size = 10, 
    line = list(
      width = 1, 
      color = "black"
    )
  )
  
  base <- plot_ly(
    df, 
    marker = m, 
    color = ~factor(Name), 
    legendgroup = ~factor(Name)
  ) 
  
  s <- subplot(
    add_histogram(base, x = ~x, showlegend = FALSE),
    plotly_empty(), 
    add_markers(base, x = ~x, y = ~y),
    add_histogram(base, y = ~y, showlegend = FALSE),
    nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
    shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
  ) %>% layout(barmode = "stack")
  
  # one trace for the empty plot
  l <- expect_traces(s, 10, "subplot-legendgroup")
  
  # really this means show three legend items (one is blank)
  expect_equivalent(
    sum(sapply(l$data, function(tr) tr$showlegend %||% TRUE)), 4
  )
  
  expect_length(
    unlist(lapply(l$data, "[[", "legendgroup")), 9
  )
  
})

