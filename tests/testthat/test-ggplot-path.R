context("path")

test_that("lines are different from paths", {
  df <- data.frame(
    x = c(1, 3, 2),
    y = c(0, 0, 1)
  )
  p <- qplot(x, y, data = df, geom = "path")
  info <- save_outputs(p, "path-lines-diff-from-paths")
  expect_identical(info$data[[1]]$x[1:3], c(1, 3, 2))
  expect_identical(info$data[[1]]$y[1:3], c(0, 0, 1))
})

two.paths <- data.frame(
  x = c(1, 2, 1, 2),
  y = c(1, 1, 2, 2)
)

test_that("paths with different colors become different traces", {
  ## Numeric color.
  gg <- ggplot() +
    geom_path(aes(x, y, group = y, color = y), data = two.paths)
  info <- save_outputs(gg, "path-colors")
  # one trace is for the colorbar
  expect_equivalent(length(info$data), 3)
  expect_identical(info$data[[1]]$x[1:2], c(1,2))
  expect_identical(info$data[[2]]$x[1:2], c(1,2))
  expect_identical(info$data[[1]]$y[1:2], c(1,1))
  expect_identical(info$data[[2]]$y[1:2], c(2,2))
  ## Categorical color.
  gg <- ggplot() +
    geom_path(aes(x, y, group = y, color = paste0("FOO", y)), data = two.paths)
  info <- save_outputs(gg, "path-colors2")
  expect_equivalent(length(info$data), 2)
  expect_identical(info$data[[1]]$x[1:2], c(1,2))
  expect_identical(info$data[[2]]$x[1:2], c(1,2))
  expect_identical(info$data[[1]]$y[1:2], c(1,1))
  expect_identical(info$data[[2]]$y[1:2], c(2,2))
})

four.paths <- rbind(
  data.frame(two.paths, g = "positive"),
  data.frame(-two.paths, g = "negative")
)

test_that("paths with the same color but different groups stay together", {
  gg <- ggplot() +
    geom_path(aes(x, y, group = y, color = g), data = four.paths)
  info <- save_outputs(gg, "path-colored-groups-stay-together")
  expect_equivalent(length(info$data), 2)
  expect_identical(info$data[[1]]$name, "positive")
  expect_identical(info$data[[2]]$name, "negative")
  expect_true(any(is.na(info$data[[1]]$x)))
  expect_true(any(is.na(info$data[[1]]$y)))
  expect_true(any(is.na(info$data[[2]]$x)))
  expect_true(any(is.na(info$data[[2]]$y)))
})

test_that("lines & points are merged into markers+lines traces", {
  df1 <- data.frame(
    sex = factor(c("Female", "Female", "Male", "Male")),
    time = factor(c("Lunch", "Dinner", "Lunch", "Dinner"),
                  levels = c("Lunch", "Dinner")),
    total_bill = c(13.53, 16.81, 16.24, 17.42)
  )
  gg <- ggplot(data = df1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
    geom_line() +
    geom_point()
  info <- save_outputs(gg, "path-line-symbols")
  expect_equivalent(length(info$data), 2)  # 2 traces
  expect_equivalent(info$data[[1]]$name, "Female")
  expect_equivalent(info$data[[1]]$marker$symbol, "circle")
  expect_equivalent(info$data[[2]]$name, "Male")
  expect_equivalent(info$data[[2]]$marker$symbol, "triangle-up")
  expect_match(info$data[[1]]$mode, "lines")
  expect_match(info$data[[1]]$mode, "markers")
  expect_match(info$data[[2]]$mode, "lines")
  expect_match(info$data[[2]]$mode, "markers")
})
