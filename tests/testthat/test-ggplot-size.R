context("size")

test_that("size is not a vector if it is not specified", {
  iplot <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width))
  L <- gg2list(iplot)
  m <- L[[1]]$marker
  expect_that(m, is_a("list"))
  expect_true(length(m$size) <= 1)

  save_outputs(gg, "size-not-a-vector")
})

test_that("size is a vector if it is specified", {
  iplot <- ggplot(iris)+
    geom_point(aes(Petal.Width, Sepal.Width, size=Petal.Length))
  L <- gg2list(iplot)
  m <- L[[1]]$marker
  expect_that(m, is_a("list"))
  expect_true(length(m$size) > 1)

  save_outputs(gg, "size-is-a-vector")
})

countrypop <- data.frame(country=c("Paraguay", "Peru", "Philippines"),
                         population=c(7, 31, 101),
                         edu=c(4.2, 1.75, 1.33),
                         illn=c(0.38, 1.67, 0.43))

gg <- ggplot(countrypop, aes(edu, illn, colour=country, size=population)) +
  geom_point()

test_that("global scaling works for sizes over different traces", {
  L <- gg2list(gg)
  expect_equal(length(L), 4)  # 1 trace per country (3) + layout
  expect_true(as.numeric(L[[1]]$marker$size) < as.numeric(L[[2]]$marker$size))
  expect_true(as.numeric(L[[2]]$marker$size) < as.numeric(L[[3]]$marker$size))
})

save_outputs(gg, "size-global-scaling")
