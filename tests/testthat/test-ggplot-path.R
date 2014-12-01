context("path")

test_that("lines are different from paths", {
  df <- data.frame(x=c(1, 3, 2),
                   y=c(0, 0, 1))
  p <- qplot(x, y, data=df, geom="path")
  info <- gg2list(p)
  p.tr <- info[[1]]
  expect_identical(p.tr$x[1:3], c(1, 3, 2))
  expect_identical(p.tr$y[1:3], c(0, 0, 1))
  l <- qplot(x, y, data=df, geom="line")
  l.tr <- gg2list(l)[[1]]
  expect_identical(l.tr$x[1:3], c(1, 2, 3))
  expect_identical(l.tr$y[1:3], c(0, 1, 0))

  save_outputs(p, "path-lines-diff-from-paths")
})

two.paths <- data.frame(x=c(1, 2, 1, 2),
                        y=c(1, 1, 2, 2))

test_that("paths with different colors become different traces", {
  ## Numeric color.
  gg <- ggplot()+
    geom_path(aes(x, y, group=y, color=y), data=two.paths)
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  trace.names <- sapply(info[1:2], "[[", "name")
  expect_identical(as.character(trace.names), c("1", "2"))
  expect_identical(info[[1]]$x[1:2], c(1,2))
  expect_identical(info[[2]]$x[1:2], c(1,2))
  expect_identical(info[[1]]$y[1:2], c(1,1))
  expect_identical(info[[2]]$y[1:2], c(2,2))
  ## Categorical color.
  gg <- ggplot()+
    geom_path(aes(x, y, group=y, color=paste0("FOO", y)), data=two.paths)
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  trace.names <- sapply(info[1:2], "[[", "name")
  expect_identical(as.character(trace.names), c("FOO1", "FOO2"))
  expect_identical(info[[1]]$x[1:2], c(1,2))
  expect_identical(info[[2]]$x[1:2], c(1,2))
  expect_identical(info[[1]]$y[1:2], c(1,1))
  expect_identical(info[[2]]$y[1:2], c(2,2))

  save_outputs(gg, "path-colors")
})

four.paths <- rbind(data.frame(two.paths, g="positive"),
                    data.frame(-two.paths, g="negative"))

test_that("paths with the same color but different groups stay together", {
  gg <- ggplot()+
    geom_path(aes(x, y, group=y, color=g), data=four.paths)
  info <- gg2list(gg)
  expect_equal(length(info), 3)
  expect_identical(info[[1]]$name, "positive")
  expect_identical(info[[2]]$name, "negative")
  expect_true(any(is.na(info[[1]]$x)))
  expect_true(any(is.na(info[[1]]$y)))
  expect_true(any(is.na(info[[2]]$x)))
  expect_true(any(is.na(info[[2]]$y)))

  save_outputs(gg, "path-colored-groups-stay-together")
})

test_that("lines work with aesthetic shape", {
  df1 <- data.frame(sex = factor(c("Female", "Female", "Male", "Male")),
                    time = factor(c("Lunch", "Dinner", "Lunch", "Dinner"),
                                  levels=c("Lunch", "Dinner")),
                    total_bill = c(13.53, 16.81, 16.24, 17.42))
  gg <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
    geom_line() +
    geom_point()
  info <- gg2list(gg)
  expect_equal(length(info), 3)  # 2 traces + layout
  expect_identical(info[[1]]$name, "Female")
  expect_identical(info[[1]]$marker$symbol, "circle")
  expect_identical(info[[2]]$name, "Male")
  expect_identical(info[[2]]$marker$symbol, "triangle-up")
  # Layout
  expect_identical(info[[3]]$layout$xaxis$title, "time")
  expect_identical(info[[3]]$layout$xaxis$type, "category")
  
  save_outputs(gg, "path-line-symbols")
})
