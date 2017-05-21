context("Text")

gg <- ggplot(mtcars, aes(x = wt, y = mpg, label = rownames(mtcars))) +
  geom_text(size = 18)
info <- save_outputs(gg, "text")

test_that("label is translated correctly", {
  greps <- Map(function(x, y) grepl(x, y), rownames(mtcars), info$data[[1]]$text)
  expect_true(all(unlist(greps)))
})

test_that("position is translated correctly", {
  expect_equivalent(info$data[[1]]$x, mtcars$wt)
  expect_equivalent(info$data[[1]]$y, mtcars$mpg)
})

test_that("geom_text splits along colour", {
  mds <- data.frame(
    State = c("Alabama", "Alabama", "Alabama", "Alabama",
              "Arizona", "Arizona"),
    City = c("HUNTSVILLE", "MOBILE", "BIRMINGHAM", "MONTGOMERY",
             "TUCSON", "PEORIA"),
    coord.1 = c(1.561284, 6.088862, 9.978292, 15.454877,
                23.225289, -7.283954),
    coord.2 = c(0.2228790, 0.8343259, -3.6507234, -4.8520206,
                -0.4438650, 9.1252792),
    Division = c("East South Central", "East South Central",
                 "East South Central", "East South Central",
                 "Mountain", "Mountain")
  )
  gg <- ggplot(mds) +
    geom_text(aes(x = coord.1, y = coord.2, label = City, colour = Division))
  
  L <- save_outputs(gg, "text-colour")
  
  expect_equivalent(length(L$data), 2)  # 2 traces
  # Proper type and mode conversion
  expect_identical(L$data[[1]]$type, "scatter")
  expect_identical(L$data[[1]]$mode, "text")
  expect_identical(L$data[[2]]$type, "scatter")
  expect_identical(L$data[[2]]$mode, "text")
  # Right colour for each trace
  expect_true(L$data[[1]]$textfont$color != L$data[[2]]$textfont$color)
})
