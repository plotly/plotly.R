context("Filename")

test_that("filepath with directories is returned as passed", {
  nm <- "directory/awesome"
  p <- print(plot_ly(mtcars, x = wt, y = vs, filename = nm))
  usr <- sub("https://plot.ly/~(.*)/[0-9]+", "\\1", p$url)
  id <- sub("https://plot.ly/~.*/([0-9]+)", "\\1", p$url)
  fig <- get_plot(get_figure(usr, id))
  expect_identical(fig$data[[1]]$filename, nm)
})
