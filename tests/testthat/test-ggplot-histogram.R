context("Histogram")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(gg, paste0("histogram-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

base <- ggplot(mtcars, aes(wt))

test_that("geom_histogram() is a bar chart of counts with no bargap", { 
  info <- expect_traces(base + geom_histogram(), 1, "counts")
  tr <- info$data[[1]]
  expect_identical(tr$type, "bar")
  expect_equivalent(sum(tr$y), nrow(mtcars))
  expect_equivalent(info$layout$barmode, "relative")
})

test_that("geom_histogram(aes(y = ..density..)) displays a density", { 
  info <- expect_traces(base + geom_histogram(aes(y=..density..)), 1, "density")
  tr <- info$data[[1]]
  expect_identical(tr$type, "bar")
  #default binwidth
  bw <- (max(tr$x) - min(tr$x))/30
  area <- sum(tr$y) * bw
  # the "area" of the plot (should be 1).
  # note this also serves as a check for the default binwidth
  expect_equal(area, 1, tolerance = 0.1)
})

test_that("geom_histogram(aes(fill = ..count..)) works", {
  info <- expect_traces(base + geom_histogram(aes(fill = ..count..)), 6, "fill")
  # grab just the bar traces (there should also be a colorbar)
  bars <- info$data[sapply(info$data, "[[", "type") == "bar"]
  # each traces should have the same value of y
  for (i in seq_along(bars)) {
    ys <- bars[[i]]$y
    expect_equivalent(length(unique(ys)), 1)
  }
})

test_that("Histogram with fixed colour/fill works", {
  gg <- base + geom_histogram(colour = "darkgreen", fill = "white")
  info <- expect_traces(gg, 1, "fixed-fill-color")
  tr <- info$data[[1]]
  expect_true(tr$marker$color == "rgba(255,255,255,1)")
  expect_true(tr$marker$line$color == "rgba(0,100,0,1)")
})

test_that("Specify histogram binwidth", {
  gg <- base + geom_histogram(aes(y=..density..), binwidth = 0.3)
  info <- expect_traces(gg, 1, "density-binwidth")
  tr <- info$data[[1]]
  area <- sum(tr$y) * 0.3
  expect_equivalent(area, 1, 0.1)
})

test_that("geom_histogram(aes(fill = factor(...))) is a stacked by default", {
  gg <- base + geom_histogram(aes(fill = factor(vs)))
  info <- expect_traces(gg, 2, "fill-factor")
  expect_equivalent(info$layout$barmode, "relative")
})

test_that("geom_histogram(aes(fill = factor(...))) respects position_identity()", {
  gg <- base + geom_histogram(
    aes(fill = factor(vs)), alpha = 0.3, position = "identity"
  )
  info <- expect_traces(gg, 2, "fill-factor-identity")
  expect_equivalent(info$layout$barmode, "relative")
})

test_that("geom_histogram(aes(fill = factor(...))) respects position_dodge()", {
  gg <- base + geom_histogram(
    aes(fill = factor(vs)), alpha = 0.3, position = "dodge"
  )
  info <- expect_traces(gg, 2, "fill-factor-dodge")
  expect_equivalent(info$layout$barmode, "relative")
})

test_that("geom_histogram() with facets", {
  gg <- base + geom_histogram(aes(fill = factor(vs)), alpha = 0.3) + 
    facet_wrap(~am)
  info <- expect_traces(gg, 4, "fill-factor-facets")
  trs <- info$data
  type <- unique(sapply(trs, "[[", "type"))
  gap <- unique(sapply(trs, "[[", "bargap"))
  barmode <- unique(sapply(trs, "[[", "barmode"))
  expect_identical(type, "bar")
  expect_equivalent(info$layout$barmode, "relative")
})

test_that("vline overlaid histogram", {
  gg <- base + geom_histogram() +
    geom_vline(aes(xintercept=mean(wt)), color="red", linetype="dashed", size=1)
  info <- expect_traces(gg, 2, "vline")
  trs <- info$data
  type <- unique(sapply(trs, "[[", "type"))
  expect_identical(sort(type), c("bar", "scatter"))
})

# Non-numeric (date) data
noram <- data.frame(
  month = c("2012-01-01", "2012-02-01", "2012-01-01", "2012-01-01", 
            "2012-03-01", "2012-02-01")
)
noram$month <- as.Date(noram$month)

test_that("dates work well with histograms", {
  hist <- ggplot(noram, aes(month)) + geom_histogram()
  info <- expect_traces(hist, 1, "dates")
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
  kP$date <- as.POSIXct(kP$date)
  histP <- ggplot(kP, aes(x = date)) + geom_histogram(binwidth = 2592000)
  info <- expect_traces(histP, 1, "POSIXt-bins")
})

test_that("datetime binning for class Date works in histograms", {
  kD <- killed
  kD$date <- as.Date(kD$date)
  histD <- ggplot(kD, aes(x = date)) + geom_histogram(binwidth = 30)
  info <- expect_traces(histD, 1, "Date-bins")
})
