test_that("api() returns endpoints", {
  skip_cloud_tests()

  res <- api()
  expect_true(length(res) > 1)
  expect_true(all(c("plots", "grids", "folders") %in% names(res)))
})

test_that("Can search with white-space", {
  skip_cloud_tests()

  res <- api("search?q=overdose drugs")
  expect_true(length(res) > 1)
})

test_that("Changing a filename works", {
  skip_cloud_tests()

  id <- plotly:::new_id()
  f <- api("files/cpsievert:14680", "PATCH", list(filename = id))
  expect_equivalent(f$filename, id)
})


test_that("Downloading plots works", {
  skip_cloud_tests()

  # https://plot.ly/~cpsievert/200
  p <- api_download_plot(200, "cpsievert")
  expect_is(p, "htmlwidget")
  expect_is(p, "plotly")

  l <- plotly_build(p)$x
  expect_length(l$data, 1)

  # This file is a grid, not a plot https://plot.ly/~cpsievert/14681
  expect_error(
    api_download_plot(14681, "cpsievert"), "grid"
  )
})


test_that("Downloading grids works", {
  skip_cloud_tests()

  g <- api_download_grid(14681, "cpsievert")
  expect_is(g, "api_file")
  expect_is(
    tibble::as_tibble(g$preview), "data.frame"
  )

  # This file is a plot, not a grid https://plot.ly/~cpsievert/14681
  expect_error(
    api_download_grid(200, "cpsievert"), "plot"
  )
})


test_that("Creating produces a new file by default", {
  skip_cloud_tests()

  expect_new <- function(obj) {
    old <- api("folders/home?user=cpsievert")
    new_obj <- api_create(obj)
    Sys.sleep(3)
    new <- api("folders/home?user=cpsievert")
    n <- if (plotly:::is.plot(new_obj)) 2 else 1
    expect_equivalent(old$children$count + n, new$children$count)
  }

  expect_new(mtcars)
  # even if plot has multiple traces, only one grid should be created
  p1 <- plot_ly(mtcars, x = ~mpg, y = ~wt)
  p2 <- add_markers(p1, color = ~factor(cyl))
  p3 <- add_markers(p1, color = ~factor(cyl), frame = ~factor(vs))
  expect_new(p1)
  expect_new(p2)
  expect_new(p3)
})


test_that("Can overwrite a grid", {
  skip_cloud_tests()

  id <- new_id()
  m <- api_create(mtcars, id)
  m2 <- api_create(palmerpenguins::penguins, id)
  expect_true(identical(m$embed_url, m2$embed_url))
  expect_false(identical(m$cols, m2$cols))
})

test_that("Can overwrite a plot", {
  skip_cloud_tests()

  id <- new_id()
  p <- plot_ly()
  m <- api_create(p, id)
  m2 <- api_create(layout(p, title = "test"), id)
  expect_true(identical(m$embed_url, m2$embed_url))
  expect_false(identical(m$figure$layout$title, m2$figure$layout$title))
})

test_that("Can create plots with non-trivial src attributes", {
  skip_cloud_tests()

  expect_srcified <- function(x) {
    expect_length(strsplit(x, ":")[[1]], 3)
  }

  expect_not_srcified <- function(x) {
    expect_true(is.null(x))
  }

  # src-ifies data arrays, but not arrayOk of length 1
  p <- plot_ly(x = 1:10, y = 1:10, marker = list(color = "red"))
  res <- api_create(p)
  trace <- res$figure$data[[1]]
  expect_srcified(trace$xsrc)
  expect_srcified(trace$ysrc)
  expect_true(trace$marker$color == "red")

  # can src-ify data[i].marker.color
  p <- plot_ly(x = 1:10, y = 1:10, color = 1:10)
  res <- api_create(p)
  trace <- res$figure$data[[1]]
  expect_srcified(trace$marker$colorsrc)

  # can src-ify frames[i].data[i].marker.color
  res <- p %>%
    add_markers(frame = rep(1:2, 5)) %>%
    api_create()
  trace <- res$figure$frames[[1]]$data[[1]]
  expect_srcified(trace$marker$colorsrc)

  # doesn't src-ify layout arrays (layout.xaxis.tickvals)
  res <- api_create(ggplot() + geom_bar(aes(1:10)))
  expect_not_srcified(res$figure$layout$xaxis$tickvalssrc)

})


test_that("filename supports names with paths included ", {
  skip_cloud_tests()
  
  p <- plot_ly(mtcars, x = ~wt, y = ~vs)
  filename <- "directory/awesome"
  # trash the file if it already exists
  file <- api_lookup_file(filename)
  if (is.file(file)) {
    endpt <- sprintf("files/%s/trash", file$fid)
    res <- api(endpt, "POST")
  }
  f <- api_create(p, filename = filename)
  expect_match(f$filename, "awesome")
  expect_true(f$parented)
})




test_that("requests made by a user who doesn't exist error a 404", {
  skip_cloud_tests()
  
  expect_error({
    get_figure("klmadslfjdfljdsf", 0)
  }, ".*404.*")
})

test_that("requests made to retrieve a figure that doesn't exist returns a 404", {
  skip_cloud_tests()
  
  expect_error({
    get_figure("get_test_user", 18324823)
  }, ".*404.*")
})

test_that("requests made to retrieve some elses private file errors", {
  skip_cloud_tests()
  
  expect_error(get_figure("get_test_user", 1))
})

test_that("retrieving a public figure ... works.", {
  skip_cloud_tests()
  
  fig <- get_figure("get_test_user", 0)
  # get the data behind the hash
  p <- plotly_build(fig)$x
  expect_equivalent(p$data[[1]]$x, c("1", "2", "3"))
})

test_that("can add traces to a subplot figure", {
  skip_cloud_tests()
  
  fig <- get_figure('chelsea_lyn', 6366)
  p <- add_lines(fig, x = c(1, 2, 3), y = c(4, 2, 4))
  expect_equivalent(
    length(plotly_build(fig)$x$data) + 1, 
    length(plotly_build(p)$x$data)
  )
})

test_that("posting a hidden plot returns a secret key", {
  skip_cloud_tests()
  
  res <- api_create(plot_ly(), sharing = "secret")
  expect_true(res$share_key_enabled)
  expect_true(nchar(res$share_key) > 1)
})


