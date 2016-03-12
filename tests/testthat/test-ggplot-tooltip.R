context("tooltip")

test <- data.frame(
  time = strptime("2016-03-12 16:32:56", format = "%Y-%m-%d %X") + 60 * 1:100, 
  x = cos(1:100)
)
p <- ggplot(test, aes(time, x)) + geom_point()

test_that("datetimes are displayed in tooltip properly", {
  l <- save_outputs(p, "tooltip-datetime")
  txt <- strsplit(l$data[[1]]$text, "<br>")
  expect_identical(
    paste0("time: ", test$time), sapply(txt, "[[", 1)
  )
})

test <- data.frame(
  time = strptime("2016-03-12", format = "%Y-%m-%d") + 1:100, 
  x = sin(1:100)
)
p <- ggplot(test, aes(time, x)) + geom_point()

test_that("dates are displayed in tooltip properly", {
  l <- save_outputs(p, "tooltip-date")
  txt <- strsplit(l$data[[1]]$text, "<br>")
  expect_identical(
    paste0("time: ", test$time), sapply(txt, "[[", 1)
  )
})

test_that("tooltip argument respects ordering", {
  p <- qplot(mpg, fill = factor(cyl), data = mtcars, geom = "density")
  p <- ggplotly(p, tooltip = c("y", "x"))
  info <- plotly_build(p)
  txt <- strsplit(info$data[[1]]$text, "<br>")
  expect_true(all(grepl("^density", sapply(txt, "[[", 1))))
  expect_true(all(grepl("^mpg", sapply(txt, "[[", 2))))
})

test_that("can hide x values in tooltip", {
  gg2 <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) + geom_violin()
  p <- ggplotly(gg2, tooltip = "y")
  l <- plotly_build(p)
  expect_equal(sum(grepl("cyl", l$data[[1]]$text)), 0)
})
