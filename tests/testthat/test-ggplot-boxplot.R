context("Boxplot")

test_that("geom_boxplot gives a boxplot", {
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
  
  L <- save_outputs(gg, "boxplot")

  # right nb. traces
  expect_equal(length(L$data), 3)
  # right type for 1st trace
  expect_identical(L$data[[1]]$type, "box")
  # right data for 1st trace
  expect_identical(sort(L$data[[1]]$y),
                   sort(mtcars$mpg[mtcars$cyl == 4]))
})

test_that("geom_violin is equated to geom_boxplot for now", {
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin()
  
  L <- save_outputs(gg, "violin")
  
  # right nb. traces
  expect_equal(length(L$data), 3)
  # right type for 1st trace
  expect_identical(L$data[[1]]$type, "box")
  # right data for 1st trace
  expect_identical(sort(L$data[[1]]$y),
                   sort(mtcars$mpg[mtcars$cyl == 4]))
})

test_that("you can make a boxplot for a distribution of datetimes", {
  dist <- c(10, 20, 33, 40, 11, 12, 11)
  dist <- as.POSIXct(paste0("2014-09-19 10:00:", dist))
  df <- data.frame(y=dist)
  df$x <- 0
  
  bp <- ggplot(df) + geom_boxplot(aes(x, y))
  
  L <- save_outputs(bp, "boxplot-datetime")
  
  expect_equal(length(L$data), 1)  # 1 trace
  expect_identical(L$data[[1]]$type, "box")
  expect_identical(L$data[[1]]$y, as.character(df$y))
})

dat <- data.frame(
  cond = factor(rep(c("A", "B", "C", "D"), each = 200)), 
  col = factor(rep(c("C1", "C2"), each = 400)), 
  rating = c(rnorm(200), rnorm(200, mean=.8), rnorm(200, mean=.4), rnorm(200, mean=.2))
)
g <- ggplot(dat, aes(x = cond, y = rating)) + 
  geom_boxplot(outlier.shape = NA, aes(fill = col))

test_that("correct # of unique fillcolors", {
  L <- save_outputs(g, "boxplot-fillcolor")
  expect_equal(length(L$data), 4)
  expect_identical(L$data[[1]]$type, "box")
  fills <- sapply(L$data, "[[", "fillcolor")
  expect_equal(length(unique(fills)), length(unique(dat$col)))
})

