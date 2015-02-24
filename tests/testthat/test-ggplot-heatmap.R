context("Heatmap")

wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dtimes <- c("Morning", "Afternoon", "Evening")
workweek <- matrix(c(1, 20, 30, 20, 1, 60, 30, 60, 1, 50, 80, -10, 1, 30, 20),
             nrow=5, ncol=3, byrow=TRUE,
             dimnames=list(day=wdays, time=dtimes))
ww <- reshape2::melt(workweek)
ww$day <- factor(ww$day, wdays)
ww$time <- factor(ww$time, dtimes)
# Plot a heatmap using geom_tile
hm <- ggplot(ww) + geom_tile(aes(x=day, y=time, fill=value))

test_that("geom_tile is translated to type=heatmap", {
    L <- gg2list(hm)
    expect_equal(length(L), 2)
    expect_identical(L[[1]]$type, "heatmap")
    expect_identical(as.character(L[[1]]$x), wdays)
    expect_identical(as.character(L[[1]]$y), dtimes)
})

save_outputs(hm, "heatmap")
