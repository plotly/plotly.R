context("unimplemented geoms")

library(proto)
geom_unimplemented <- function(...){
  GeomUnimplemented <- proto(ggplot2:::GeomLine,{
    objname <- "unimplemented"
  })
  GeomUnimplemented$new(...)
}

test_that("un-implemented geoms are ignored with a warning", {
  gg <- ggplot(iris, aes(Sepal.Width, Petal.Length))
  expect_error({
    gg2list(gg)
  }, "No layers in plot")

  un <- gg+geom_unimplemented()
  expect_error({
    gg2list(un)
  }, "No exportable traces")

  ok <- un+geom_point()
  expect_warning({
    info <- gg2list(ok)
  }, "conversion not implemented")
  expect_equal(length(info), 2)
})
