context("Filename")

test_that("filepath with directories is returned as passed", {
  dat <- list(x = rnorm(30), type = "histogramx")
  nm <- "directory/coolest-plot"
  l <- list(autosize = FALSE, width = 600, height = 400, showlegend = FALSE,
            filename = nm, fileopt = "overwrite")
  resp <- plotly_POST(dat, l)
  # why does directory get prepended?
  expect_identical(resp[["filename"]], "directorydirectory/coolest-plot")
})
