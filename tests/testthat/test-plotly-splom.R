context("splom")


test_that("No cartesian axes are supplied to a splom chart", {
  
  p <- plot_ly(
    type = 'splom',
    dimensions = list(
      list(values=c(1,2,3), label="A"),
      list(values=c(2,5,6), label="B")
    )
  )
  
})
