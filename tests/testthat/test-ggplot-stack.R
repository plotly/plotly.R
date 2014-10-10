context("Stack")

# Data for test is taken from
# http://software-carpentry.org/blog/2014/04/instructor-survey-processed-2014-04-04.csv"
instructors <-
  data.frame(topic=c("Python", "Python", "Python", "R", "R", "R"),
             level=c("0) None", "1) Novice", "2) Intermediate",
                     "0) None", "1) Novice", "2) Intermediate"),
             number=c(4, 27, 51, 50, 18, 14))

test_that("y value is non-cumulative in stacked bar charts", {
  gg <- ggplot(instructors, aes(x=topic, y=number, fill=level)) +
    geom_bar(stat="identity")
  L <- gg2list(gg)
  expect_equal(length(L), 4)
  expect_identical(L$kwargs$layout$barmode, "stack")
  trace.names <- sapply(L[1:3], "[[", "name")
  expect_true(all(c("1) Novice", "2) Intermediate") %in% trace.names))
  expect_equal(L[[2]]$y[1], instructors$number[2])
  expect_equal(L[[3]]$y[1], instructors$number[3])
  expect_equal(L[[2]]$y[2], instructors$number[5])
  expect_equal(L[[3]]$y[2], instructors$number[6])

  save_outputs(gg, "stack")
})
