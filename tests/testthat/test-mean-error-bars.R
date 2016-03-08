context("means and error bars")

one.line.df <- data.frame(
  x = c(1, 2, 3, 4), 
  y = c(2, 1, 3, 4), 
  array = c(0.1, 0.2, 0.1, 0.1), 
  arrayminus = c(0.2, 0.4, 1, 0.2)
)

test_that("only asymmetric error bars", {
  error.gg <- ggplot(one.line.df, aes(x, y)) +
    geom_errorbar(aes(ymin = y - arrayminus, ymax = y + array))
  L <- save_outputs(error.gg, "error-simple")
})

test_that("asymmetric error bars, geom_errorbar last", {
  one.line.gg <- ggplot(one.line.df, aes(x, y)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = y - arrayminus, ymax = y + array))
  L <- save_outputs(one.line.gg, "error-simple-line")
})

test_that("asymmetric error bars, geom_errorbar first", {
  one.line.gg <- ggplot(one.line.df, aes(x, y)) +
    geom_errorbar(aes(ymin = y - arrayminus, ymax = y + array)) +
    geom_line() +
    geom_point()
  L <- save_outputs(one.line.gg, "error-simple-line-point")
})

test_that("different colors for error bars, points, and lines", {
  one.line.gg <- ggplot(one.line.df, aes(x, y)) +
    geom_errorbar(aes(ymin = y - arrayminus, ymax = y + array), color = "red") +
    geom_line(color = "violet") +
    geom_point(color = "blue", size = 14)
  L <- save_outputs(one.line.gg, "error-simple-line-point-crazy")
})
