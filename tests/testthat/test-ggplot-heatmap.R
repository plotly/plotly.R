context("Heatmap")

wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dtimes <- c("Morning", "Afternoon", "Evening")
workweek <- matrix(
  c(1, 20, 30, 20, 1, 60, 30, 60, 1, 50, 80, -10, 1, 30, 20),
  nrow = 5, ncol = 3, byrow = TRUE,
  dimnames = list(day = wdays, time = dtimes)
)
ww <- reshape2::melt(workweek)
ww$day <- factor(ww$day, wdays)
ww$time <- factor(ww$time, dtimes)
# Plot a heatmap using geom_tile
hm <- ggplot(ww) + geom_tile(aes(x = day, y = time, fill = value))

test_that("geom_tile is translated to type=heatmap", {
  L <- save_outputs(hm, "heatmap")
  # one trace is for the colorbar
  expect_equal(length(L$data), 2)
  expect_identical(L$data[[1]]$type, "heatmap")
  expect_identical(L$layout$xaxis$ticktext, wdays)
  expect_identical(L$layout$yaxis$ticktext, dtimes)
  # show bin value on hover (but without x/y since they are discrete)
  expect_true(
    L$data[[1]]$hoverinfo == "text"
  )
  expect_true(
    all(grepl("^value: [-]?[0-9]+$", c(L$data[[1]]$text)))
  )
})
