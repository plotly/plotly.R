context("Histogram")

# Non-numeric data
noram <- data.frame(country=c("MEX", "CDN", "USA", "CDN", "MEX", "MEX"))

test_that("default position is translated to barmode=stack", {
  hist <- ggplot(noram, aes(country)) + geom_bar()
  L <- gg2list(hist)
  expect_equal(length(L), 2)
  expect_identical(L$kwargs$layout$barmode, "stack")

  save_outputs(hist, "histogram-barmodestack")
})

# Numeric data
x <- c(-0.7392909,-0.1433534,0.458901,-1.288281,1.548516,-2.388749,-2.210839,-0.1724795,-1.573152,-0.7600643,-0.3611827,-0.8990402,-1.970716,1.056986,-0.833159,-0.2324272,-2.094518,1.478515,-0.7656415,-0.3660834,1.821793,-1.271924,-0.3413464,0.4588009,-2.838673,-0.2176166,0.3438984,-1.304567,1.133631,0.462299,0.2105919,0.9017204,-0.5982157,-0.5799123,-0.7730307,0.5052771,-0.02328334,-0.3153552,0.4962177,0.4669228,-1.440982,0.2828748,-0.8115607,0.1936876,-0.7211877,0.8330693,1.27252,-0.1995907,1.127246,1.406967)
df <- data.frame(index=seq(1:length(x)), x=x)
# Binwidth
bw <- 0.8
gg <- ggplot(df, aes(x))

test_that("binwidth is translated into xbins.size", {
  hist <- gg + geom_histogram(binwidth=bw)
  L <- gg2list(hist)
  expect_equal(length(L), 2)
  expect_equal(L[[1]]$xbins$size, bw)

  save_outputs(hist, "histogram-binwidth")
})

# Non-numeric (date) data
noram <- data.frame(month=c("2012-01-01", "2012-02-01", "2012-01-01",
                            "2012-01-01", "2012-03-01", "2012-02-01"))
noram$month <- as.Date(noram$month)

test_that("dates work well with histograms", {
  hist <- ggplot(noram, aes(month)) + geom_histogram()
  L <- gg2list(hist)
  expect_equal(length(L), 2)  # 1 trace + layout
  expect_identical(L$kwargs$layout$barmode, "stack")
  expect_identical(L$kwargs$layout$xaxis$type, "date")
  expect_identical(L[[1]]$x[1], "2012-01-01 00:00:00")
  expect_identical(L[[1]]$x[2], "2012-02-01 00:00:00")

  save_outputs(hist, "histogram-dates")
})
