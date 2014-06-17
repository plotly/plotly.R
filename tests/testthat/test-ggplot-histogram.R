context("Histogram")

# Non-numeric data
noram <- data.frame(country=c("MEX", "CDN", "USA", "CDN", "MEX", "MEX"))

test_that("default position is translated to barmode=stack", {
  hist <- ggplot(noram, aes(country)) + geom_bar()
  L <- gg2list(hist)
  expect_equal(length(L), 2)
  expect_identical(L$kwargs$layout$barmode, "stack")
})

# Numeric data
x <- rnorm(50)
df <- data.frame(index=seq(1:length(x)), x=x)
# Binwidth
bw <- 0.8
gg <- ggplot(df, aes(x))

test_that("binwidth is translated into xbins.size", {
  hist <- gg + geom_histogram(binwidth=bw)
  L <- gg2list(hist)
  expect_equal(length(L), 2)
  expect_equal(L[[1]]$xbins$size, bw)
})
