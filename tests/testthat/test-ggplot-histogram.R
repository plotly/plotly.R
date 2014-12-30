context("Histogram")

# Non-numeric data
noram <- data.frame(country=c("MEX", "CDN", "USA", "CDN", "MEX", "MEX"))

test_that("default position is translated to barmode=stack", {
  hist <- ggplot(noram, aes(country)) + geom_bar()
  L <- gg2list(hist)
  expect_equal(length(L), 2)
  expect_identical(L$kwargs$layout$barmode, "stack")
  expect_identical(L$kwargs$layout$xaxis$type, "category")
  expect_identical(L[[1]]$type, "histogram")
  expect_true(L[[1]]$x[1] %in% c("CDN", "MEX", "USA"))
  
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

# Non-numeric (date) data, specifying binwidth
killed <- data.frame(date=c("2014-12-24",
                            "2014-12-23",
                            "2014-12-22",
                            "2014-12-22",
                            "2014-12-22",
                            "2014-12-18",
                            "2014-12-22",
                            "2014-12-21",
                            "2014-12-21",
                            "2014-12-21",
                            "2014-12-20",
                            "2014-12-19",
                            "2014-12-18",
                            "2014-12-18",
                            "2014-12-17",
                            "2014-12-17",
                            "2013-12-20",
                            "2014-04-25",
                            "2014-12-01",
                            "2014-12-17",
                            "2014-12-17",
                            "2014-12-17",
                            "2014-12-17",
                            "2014-12-17",
                            "2014-12-17",
                            "2014-12-15",
                            "2014-12-15",
                            "2014-12-15",
                            "2014-12-14",
                            "2014-12-14",
                            "2014-12-14",
                            "2014-12-13",
                            "2014-12-13",
                            "2013-05-18",
                            "2014-12-13",
                            "2014-12-12",
                            "2014-12-12",
                            "2014-12-11",
                            "2014-12-10",
                            "2014-12-10",
                            "2014-12-10",
                            "2014-12-10",
                            "2014-12-09",
                            "2014-12-09",
                            "2014-12-09",
                            "2014-12-09",
                            "2014-12-08",
                            "2014-12-08",
                            "2014-12-08",
                            "2014-12-07",
                            "2014-12-08",
                            "2014-12-07",
                            "2014-05-01",
                            "2014-12-05",
                            "2014-12-05",
                            "2014-12-05",
                            "2014-12-04",
                            "2014-12-04",
                            "2014-12-04",
                            "2014-07-13",
                            "2014-12-02",
                            "2014-12-03",
                            "2014-12-03",
                            "2014-12-02",
                            "2014-12-02",
                            "2014-12-01",
                            "2014-12-01",
                            "2014-12-01",
                            "2014-04-02",
                            "2014-11-30",
                            "2014-11-30",
                            "2014-11-29",
                            "2014-11-28",
                            "2014-11-29",
                            "2014-11-27",
                            "2014-11-28",
                            "2014-11-27",
                            "2014-11-26",
                            "2014-11-25",
                            "2014-11-26",
                            "2014-11-25",
                            "2014-11-25",
                            "2014-11-24",
                            "2014-11-24",
                            "2014-11-23",
                            "2014-11-23",
                            "2014-11-24",
                            "2014-11-23",
                            "2014-11-22",
                            "2014-11-23",
                            "2014-11-22",
                            "2014-11-22",
                            "2014-11-21",
                            "2014-11-21",
                            "2014-11-21",
                            "2014-11-20",
                            "2014-11-20",
                            "2014-11-20",
                            "2014-11-19"))

test_that("datetime binning for class POSIXt works in histograms", {
  kP <- killed
  kP$date <- as.POSIXlt(kP$date)
  histP <- ggplot(kP, aes(x=date)) + geom_histogram(binwidth=2592000)
  
  L <- gg2list(histP)
  expect_equal(length(L), 2)  # 1 trace + layout
  expect_false(L[[1]]$autobinx)  # No auto-binning
  expect_identical(L$kwargs$layout$xaxis$type, "date")
  expect_equal(L[[1]]$xbins$size, 2592000000)  # Bin size in ms
  
  save_outputs(hist, "histogram-POSIXt-bins")
})

test_that("datetime binning for class Date works in histograms", {
  kD <- killed
  kD$date <- as.Date(kD$date)
  histD <- ggplot(kD, aes(x=date)) + geom_histogram(binwidth=30)
  
  L <- gg2list(histD)
  expect_equal(length(L), 2)  # 1 trace + layout
  expect_false(L[[1]]$autobinx)  # No auto-binning
  expect_identical(L$kwargs$layout$xaxis$type, "date")
  expect_equal(L[[1]]$xbins$size, 2.592e+09)  # Number of ms in 30 days
  
  save_outputs(hist, "histogram-Date-bins")
})