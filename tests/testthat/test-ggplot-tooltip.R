context("tooltip")

test <- data.frame(
  time = strptime("2016-03-12 16:32:56", format = "%Y-%m-%d %X") + 60 * 1:100, 
  x = cos(1:100)
)
p <- ggplot(test, aes(time, x)) + geom_point()

test_that("datetimes are displayed in tooltip properly", {
  l <- save_outputs(p, "tooltip-datetime")
  txt <- strsplit(l$data[[1]]$text, br())
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
  txt <- strsplit(l$data[[1]]$text, br())
  expect_identical(
    paste0("time: ", test$time), sapply(txt, "[[", 1)
  )
})

test_that("tooltip argument respects ordering", {
  # qplot() is broken in ggplot2 (as of c0a99a8)
  #p <- qplot(mpg, fill = factor(cyl), data = mtcars, geom = "density")
  p <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) + geom_density()
  p <- ggplotly(p, tooltip = c("density", "x"))
  info <- plotly_build(p)$x
  txt <- strsplit(info$data[[1]]$text, br())
  expect_true(all(grepl("^density", sapply(txt, "[[", 1))))
  expect_true(all(grepl("^mpg", sapply(txt, "[[", 2))))
})

test_that("can hide x values in tooltip", {
  gg2 <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) + geom_violin()
  p <- ggplotly(gg2, tooltip = "y")
  l <- plotly_build(p)$x
  expect_equivalent(sum(grepl("cyl", l$data[[1]]$text)), 0)
})

cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
p <- cars + stat_bin2d(aes(fill = ..density..), binwidth = c(3,1))

test_that("geom_tile() displays correct info in tooltip with discrete y", {
  L <- save_outputs(p, "heatmap-discrete-tooltip")
  expect_equivalent(length(L$data), 2)
  expect_equivalent(L$data[[1]]$type, "heatmap")
  txt <- c(L$data[[1]]$text)
  txt <- txt[!is.na(txt)]
  # tooltip should show y-values on the _data_ scale
  expect_true(all(grepl("factor\\(cyl\\): [4,6,8]", txt)))
})

p <- ggplot(txhousing, aes(x = date, y = median, group = city)) +
  geom_line(alpha = 0.3)

test_that("group domain is included in hovertext", {
  L <- save_outputs(p, "group-lines-hovertext")
  expect_equivalent(length(L$data), 1)
  txt <- L$data[[1]]$text
  txt <- txt[!is.na(txt)]
  pattern <- paste(unique(txhousing$city), collapse = "|")
  expect_true(all(grepl(pattern, txt)))
})

test_that("tooltip elements are not crossed", {
  # Tooltips with y == 10 should belong to Sample2 in this example
  mydata <- data.frame(id = paste0("Sample", rep(1:2, times = 4)),
                       x = rep(1:4, each = 2),
                       y = rep(c(1, 10), times = 4),
                       stringsAsFactors = FALSE)
  #        id x  y
  # 1 Sample1 1  1
  # 2 Sample2 1 10
  # 3 Sample1 2  1
  # 4 Sample2 2 10
  # 5 Sample1 3  1
  # 6 Sample2 3 10
  # 7 Sample1 4  1
  # 8 Sample2 4 10
  gplt <- ggplot(mydata) + geom_line(aes(x=x, y = y, group = id))
  pltly <- plotly::ggplotly(gplt)
  y_equal_ten <- grepl("y: 10", pltly$x$data[[1]]$text)
  sample_2 <- grepl("id: Sample2", pltly$x$data[[1]]$text)
  expect_equivalent(y_equal_ten, sample_2)
})

labelDF <- data.frame(
  label = paste0(("label"), c(1:10)), 
  x = runif(10, 1, 10), 
  y = runif(10, 1, 10)
)
# Create data frame for 10 edges
edgeDF <- data.frame(
  x = runif(10, 1, 10), 
  y = runif(10, 1, 10), 
  xend = runif(10, 1, 10), 
  yend = runif(10, 1, 10)
)

myPlot <- ggplot(data = labelDF, aes(x = x, y = y)) +
  geom_segment(data = edgeDF, aes(x = x, y = y, xend = xend, yend = yend), 
               colour = "pink") +
  geom_text(data = labelDF, aes(x = x, y = y, label = label), size = 10)

test_that("Hoverinfo is only displayed if no tooltip variables are present", {
  L <- save_outputs(p, "hovertext-display")
  L <- plotly_build(ggplotly(myPlot, tooltip = "label"))[["x"]]
  expect_equivalent(length(L$data), 2)
  expect_equivalent(sum(nchar(L$data[[1]]$text)), 0)
  expect_true(all(grepl("^label", L$data[[2]]$text)))
})

