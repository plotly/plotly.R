context("bar")

researchers <-
  data.frame(country=c("Canada", "Canada", "Germany", "USA"),
             name=c("Warren", "Andreanne", "Stefan", "Toby"),
             papers=c(23, 14, 37, 20),
             field=c("Math", "Bio", "Bio", "Math"))

gg <- ggplot(researchers, aes(country, papers, fill=field))

test_that("position_dodge is translated to barmode=group", {
  gg.dodge <- gg+geom_bar(stat="identity", position="dodge")
  L <- gg2list(gg.dodge)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "group")
})

test_that("position_stack is translated to barmode=stack", {
  gg.stack <- gg+geom_bar(stat="identity", position="stack")
  gg2list(gg.stack)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "stack")
})

test_that("position_identity is translated to barmode=overlay", {
  gg.identity <- gg+geom_bar(stat="identity", position="identity")
  gg2list(gg.identity)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "overlay")
})

