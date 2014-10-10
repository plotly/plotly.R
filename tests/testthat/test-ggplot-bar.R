context("bar")

researchers <-
  data.frame(country=c("Canada", "Canada", "Germany", "USA"),
             name=c("Warren", "Andreanne", "Stefan", "Toby"),
             papers=c(23, 14, 37, 20),
             field=c("Math", "Bio", "Bio", "Math"))

gg <- ggplot(researchers, aes(country, papers, fill=field))

test_that("position_dodge is translated to barmode=group", {
  gg.dodge <- gg + geom_bar(stat="identity", position="dodge")
  L <- gg2list(gg.dodge)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "group")
  
  save_outputs(gg.dodge, "bar-dodge")
})

test_that("position_stack is translated to barmode=stack", {
  gg.stack <- gg + geom_bar(stat="identity", position="stack")
  L <- gg2list(gg.stack)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "stack")
  
  save_outputs(gg.stack, "bar-stack")
})

test_that("position_identity is translated to barmode=overlay", {
  gg.identity <- gg + geom_bar(stat="identity", position="identity")
  L <- gg2list(gg.identity)
  expect_equal(length(L), 3)
  trace.names <- sapply(L[1:2], "[[", "name")
  expect_true(all(c("Math", "Bio") %in% trace.names))
  expect_identical(L$kwargs$layout$barmode, "overlay")
  
  save_outputs(gg.identity, "bar-identity")
})

test_that("dates work well with bar charts", {
  
  researchers$month <- c("2012-01-01", "2012-01-01", "2012-02-01", "2012-02-01")
  researchers$month <- as.Date(researchers$month)
  
  gd <- ggplot(researchers, aes(month, papers, fill=field)) +
    geom_bar(stat="identity")
  
  L <- gg2list(gd)
  
  expect_equal(length(L), 3)  # 2 traces + layout
  expect_identical(L$kwargs$layout$xaxis$type, "date")
  expect_identical(L[[1]]$x[1], "2012-01-01 00:00:00")
  expect_identical(L[[1]]$x[2], "2012-02-01 00:00:00")
  
  save_outputs(gd, "bar-dates")
})
