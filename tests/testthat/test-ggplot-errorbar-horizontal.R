context("geom_errorbarh")

test_that("geom_errorbarh gives horizontal errorbars", {

  df <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    se = c(0.1, 0.3, 0.3, 0.4)
  )
  g <- ggplot(df, aes(resp, trt, colour=group)) + geom_point()
  # Define the limits of the horizontal errorbars
  g <- g + geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))

  L <- save_outputs(g, "errorbar-horizontal")

  # Expect scatter plot and its error bars to have the same color
  expect_equivalent(L$data[[1]]$marker$color, L$data[[3]]$error_x$color)
  expect_equivalent(L$data[[2]]$marker$color, L$data[[4]]$error_x$color)
  # Expect given errorbar values
  expect_equivalent(L$data[[3]]$error_x$array, c(0.1, 0.3))
  expect_equivalent(L$data[[4]]$error_x$array, c(0.3, 0.4))
})
