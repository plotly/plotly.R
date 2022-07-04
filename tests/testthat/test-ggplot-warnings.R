test_that("ggplotly does not issue partial-argument-match warning", {
  opts <- options(warnPartialMatchArgs = TRUE)
  on.exit(options(opts), add = TRUE)
  p <- ggplot(data.frame())
  expect_warning(ggplotly(p), regexp = NA)
})
