context("Boxplot")

test_that("geom_boxplot gives a boxplot", {
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
  
  L <- gg2list(gg)

  # right nb. traces
  expect_equal(length(L), 4)
  # right type for 1st trace
  expect_identical(L[[1]]$type, "box")
  # right data for 1st trace
  expect_identical(sort(L[[1]]$y),
                   sort(mtcars$mpg[mtcars$cyl == 4]))
})

test_that("geom_violin is equated to geom_boxplot for now", {
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin()
  
  L <- gg2list(gg)
  
  # right nb. traces
  expect_equal(length(L), 4)
  # right type for 1st trace
  expect_identical(L[[1]]$type, "box")
  # right data for 1st trace
  expect_identical(sort(L[[1]]$y),
                   sort(mtcars$mpg[mtcars$cyl == 4]))
})

test_that("you can make a boxplot for a distribution of datetimes", {
  dist <- c(10, 20, 33, 40, 11, 12, 11)
  dist <- as.POSIXct(paste0("2014-09-19 10:00:", dist))
  df <- data.frame(y=dist)
  df$x <- 0
  
  bp <- ggplot(df) + geom_boxplot(aes(x, y))
  
  L <- gg2list(bp)
  
  expect_equal(length(L), 2)  # 1 trace + layout
  expect_identical(L[[1]]$type, "box")
  expect_identical(L[[1]]$y, as.character(df$y))
})
