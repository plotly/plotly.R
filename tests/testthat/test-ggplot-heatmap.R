context("Heatmap")

wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dtimes <- c("Morning", "Afternoon", "Evening")
workweek <- matrix(
  c(1, 20, 30, 20, 1, 60, 30, 60, 1, 50, 80, -10, 1, 30, 20),
  nrow = 5, ncol = 3, byrow = TRUE,
  dimnames = list(day = wdays, time = dtimes)
)

test_that("geom_tile is translated to type=heatmap", {
  skip_if_not_installed("reshape2")
  
  ww <- getFromNamespace("melt", "reshape2")(workweek)
  ww$day <- factor(ww$day, wdays)
  ww$time <- factor(ww$time, dtimes)
  # Plot a heatmap using geom_tile
  hm <- ggplot(ww) + geom_tile(aes(x = day, y = time, fill = value))
  
  L <- save_outputs(hm, "heatmap")
  # one trace is for the colorbar
  expect_equivalent(length(L$data), 2)
  expect_equivalent(L$data[[1]]$type, "heatmap")
  expect_equivalent(L$layout$xaxis$ticktext, wdays)
  expect_equivalent(L$layout$yaxis$ticktext, dtimes)
  # show bin value on hover (but without x/y since they are discrete)
  expect_true(
    L$data[[1]]$hoverinfo == "text"
  )
  expect_true(
    all(grepl("value:\\s+[-]?[0-9]+$", c(L$data[[1]]$text)))
  )
})

d <- expand.grid(
  x = seq(0, 1, .005), 
  y = seq(0, 1, .005)
)
d$z <- with(d, (1 - y) * x / ((1 - y) * x + y * (1 - x)))
p <- ggplot(data = d, aes(x, y)) + 
  geom_tile(aes(fill = z)) + 
  scale_fill_gradient2(low = '#67001f', mid = 'white', high = '#053061', midpoint = .5)

test_that("geom_tile() scale_fill_gradient2()", {
  L <- save_outputs(p, "heatmap-midpoint")
  # one trace is for the colorbar
  expect_equivalent(length(L$data), 2)
  expect_equivalent(L$data[[1]]$type, "heatmap")
})

tidy_cor <- function(x) {
  co <- as.data.frame(cor(x[vapply(x, is.numeric, logical(1))]))
  co$var1 <- row.names(co)
  tidyr::gather(co, var2, cor, -var1)
}
d <- tidy_cor(mtcars)
p <- ggplot(d, aes(var1, var2, fill = cor)) + geom_tile()

test_that("geom_tile() with discrete x/y", {
  L <- save_outputs(p, "heatmap-discrete")
  # one trace is for the colorbar
  expect_equivalent(length(L$data), 2)
  expect_equivalent(L$data[[1]]$type, "heatmap")
})

