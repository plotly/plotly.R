context("geom_point")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("smooth-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

test_that("geom_point size & alpha translate to a single trace", {
  gg <- ggplot(mtcars, aes(cyl, wt)) + 
    geom_point(aes(size = gear, alpha = cyl)) 
  info <- save_outputs(gg, "point-size-alpha")
  expect_equal(length(info$data), 1)
  mkr <- info$data[[1]]$marker
  expect_equal(length(mkr$size), nrow(mtcars))
  expect_equal(length(mkr$opacity), nrow(mtcars))
})

test_that("can plot on sub-second time scale", {
  d <- data.frame(
    x = Sys.time() + 1e-3 * c(1:9, 5000), 
    y = rnorm(10)
  )
  g <- ggplot(d, aes(x, y)) + geom_point()
  info <- save_outputs(g, "point-size-alpha2")
  expect_equivalent(info$data[[1]]$x, as.numeric(d$x))
})
