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
  L <- save_outputs(gg, "stack")
  expect_equal(length(L$data), 3)
  expect_identical(L$layout$barmode, "stack")
  trace.names <- sapply(L$data[1:3], "[[", "name")
  expect_true(all(c("1) Novice", "2) Intermediate") %in% trace.names))
  expect_equal(L$data[[2]]$y[1], instructors$number[2])
  expect_equal(L$data[[3]]$y[1], instructors$number[3])
  expect_equal(L$data[[2]]$y[2], instructors$number[5])
  expect_equal(L$data[[3]]$y[2], instructors$number[6])
})

testd <- data.frame(
  group1 = rep(1:3, each = 3),
  group2 = LETTERS[1:3],
  # We have two 0.2 within each group.
  count = c(0.2, 0.2, 0.6)
) 

test_that("can stack identical traces", {
  p1 <- ggplot(testd, aes(x = factor(group1), y = count, 
                          fill = factor(group2))) + 
    geom_bar(stat = 'identity', width = 1, position = "stack")
  L <- save_outputs(p1, "stack-identical")
  expect_equal(length(L$data), 3)
})

