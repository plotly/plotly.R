context("Text")

gg <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) +
  geom_text(size=18)
info <- gg2list(gg)

test_that("label is translated correctly", {
  expect_identical(as.character(info[[1]]$text), rownames(mtcars))
})

test_that("position is translated correctly", {
  expect_identical(info[[1]]$x, mtcars$wt)
  expect_identical(info[[1]]$y, mtcars$mpg)
})

test_that("textsize is translated correctly", {
  expect_identical(info[[1]]$textfont$size, 18)
})

save_outputs(gg, "text")

test_that("geom_text splits along colour", {
  mds <- data.frame(State=c("Alabama", "Alabama", "Alabama", "Alabama",
                            "Arizona", "Arizona"),
                    City=c("HUNTSVILLE", "MOBILE", "BIRMINGHAM", "MONTGOMERY",
                           "TUCSON", "PEORIA"),
                    coord.1=c(1.561284, 6.088862, 9.978292, 15.454877,
                              23.225289, -7.283954),
                    coord.2=c(0.2228790, 0.8343259, -3.6507234, -4.8520206,
                              -0.4438650, 9.1252792),
                    Division=c("East South Central", "East South Central",
                               "East South Central", "East South Central",
                               "Mountain", "Mountain"))
  
  gg <- ggplot(mds) +
    geom_text(aes(x=coord.1, y=coord.2, label=City, colour=Division))
  
  L <- gg2list(gg)
  
  expect_equal(length(L), 3)  # 2 traces + layout
  # Proper type and mode conversion
  expect_identical(L[[1]]$type, "scatter")
  expect_identical(L[[1]]$mode, "text")
  expect_identical(L[[2]]$type, "scatter")
  expect_identical(L[[2]]$mode, "text")
  # Right colour for each trace
  expect_identical(L[[1]]$textfont$color, "#F8766D")
  expect_identical(L[[2]]$textfont$color, "#00BFC4")
  
  save_outputs(gg, "text-colour")
})
