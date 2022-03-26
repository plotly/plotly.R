

test_that("ggplotly does not issue partial-argument-match warning", {
  p <- ggplot(data.frame())
  rlang::local_options(warnPartialMatchArgs = TRUE)
  expect_warning(ggplotly(p), regexp = NA)
})
