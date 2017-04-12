context("Text")

gg <- ggplot(mtcars, aes(x = wt, y = mpg, label = rownames(mtcars))) +
  geom_text(size = 18)
info <- save_outputs(gg, "text")

test_that("label is translated correctly", {
  greps <- Map(function(x, y) grepl(x, y), rownames(mtcars), info$data[[1]]$text)
  expect_true(all(unlist(greps)))
})

test_that("position is translated correctly", {
  expect_identical(info$data[[1]]$x, mtcars$wt)
  expect_identical(info$data[[1]]$y, mtcars$mpg)
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
  
  expect_equal(length(L$data), 2)  # 2 traces
  # Proper type and mode conversion
  expect_identical(L$data[[1]]$type, "scatter")
  expect_identical(L$data[[1]]$mode, "text")
  expect_identical(L$data[[2]]$type, "scatter")
  expect_identical(L$data[[2]]$mode, "text")
  # Right colour for each trace
  expect_true(L$data[[1]]$textfont$color != L$data[[2]]$textfont$color)
})

gg1 = ggplot(data.frame(x = seq(5, 25, 5), y = 60)) +
  geom_point(aes(x = x, y = y)) +
  geom_text(x = 5, y = 60, label = "nothing") +
  geom_text(x = 10, y = 60, label = "bold", fontface = "bold", hjust = 0, vjust = 0) +
  geom_text(x = 15, y = 60, label = "italic", fontface = "italic", hjust = 1, vjust = 1) +
  geom_text(x = 20, y = 60, label = "bold italic", fontface = "bold.italic", hjust = 0, vjust = 1) +
  geom_text(x = 25, y = 60, label = "plain", fontface = "plain", hjust = 1, vjust = 0)
info1 <- save_outputs(gg1, "text-fontjust")

test_that("fontface is translated correctly", {
  expect_identical(info1$data[[2]]$text, rep("nothing", 5))
  expect_identical(info1$data[[3]]$text, rep("<b>bold</b>", 5))
  expect_identical(info1$data[[4]]$text, rep("<i>italic</i>", 5))
  expect_identical(info1$data[[5]]$text, rep("<b><i>bold italic</i></b>", 5))
  expect_identical(info1$data[[6]]$text, rep("plain", 5))
})

test_that("hjust/vjust is translated correctly", {
  expect_identical(info1$data[[2]]$textposition, rep("center", 5))
  expect_identical(info1$data[[3]]$textposition, rep("top right", 5))
  expect_identical(info1$data[[4]]$textposition, rep("bottom left", 5))
  expect_identical(info1$data[[5]]$textposition, rep("bottom right", 5))
  expect_identical(info1$data[[6]]$textposition, rep("top center", 5))
})
