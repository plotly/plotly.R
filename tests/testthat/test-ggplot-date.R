context("date")

test_that("datetimes are converted to e.g. 2013-01-02 05:00:00", {
  in.str <- c("17 Mar 1983 06:33:44 AM",
                "17 Mar 1984 01:59:55 PM")
  time.obj <- strptime(in.str, "%d %b %Y %I:%M:%S %p")
  out.str <- strftime(time.obj, "%Y-%m-%d %H:%M:%S")
  df <- rbind(data.frame(who="me", time.obj, dollars=c(1.1, 5.6)),
              data.frame(who="you", time.obj, dollars=c(10.2, 0)))
  gg <- qplot(time.obj, dollars, data=df, color=who, geom="line")
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_identical(info$kwargs$layout$xaxis$type, "date")
  for(trace in info[1:2]){
    expect_identical(trace$x, out.str)
  }
})

test_that("Class Date is supported", {
  df <- data.frame(x=c("2013-01-01", "2013-01-02", "2013-01-03"),
                   y=c(2, 3, 2.5))
  df$x <- as.Date(df$x)
  gg <- ggplot(df) + geom_line(aes(x=x, y=y))
  info <- gg2list(gg)
  expect_equal(length(info), 2)
  expect_identical(info$kwargs$layout$xaxis$type, "date")
  expect_identical(info[[1]]$x[1], "2013-01-01 00:00:00")
})
