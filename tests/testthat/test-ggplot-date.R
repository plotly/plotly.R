context("date")

test_that("datetimes are converted to e.g. 2013-01-02 05:00:00", {
  in.str <- c("17 Mar 1983 06:33:44 AM",
                "17 Mar 1984 01:59:55 PM")
  time.obj <- strptime(in.str, "%d %b %Y %I:%M:%S %p")
  out.str <- strftime(time.obj, "%Y-%m-%d %H:%M:%S")
  df <- rbind(data.frame(who = "me", time.obj, dollars = c(1.1, 5.6)),
              data.frame(who = "you", time.obj, dollars = c(10.2, 0)))
  gg <- qplot(time.obj, dollars, data = df, color = who, geom = "line")
  info <- save_outputs(gg, "date-strings")
  expect_equivalent(length(info$data), 2)
  for(trace in info$data[1:2]){
    expect_equivalent(as.numeric(time.obj), trace$x)
  }
})

test_that("class Date is supported", {
  df <- data.frame(
    x = c("2013-01-01", "2013-01-02", "2013-01-03"),
    y = c(2, 3, 2.5)
  )
  df$x <- as.Date(df$x)
  gg <- ggplot(df) + geom_line(aes(x = x, y = y))
  info <- save_outputs(gg, "date-class-Date")
  expect_equivalent(length(info$data), 1)
})

test_that("scale_x_date and irregular time series work", {
  df <- data.frame(
    date = seq(as.Date("2121-12-12"), len = 100, by = "1 day")[sample(100, 50)],
    price = runif(50)
  )
  df <- df[order(df$date), ]
  dt <- qplot(date, price, data = df, geom = "line") + theme(aspect.ratio = 1/4)
  info <- save_outputs(dt, "date-irregular-time-series")
})
