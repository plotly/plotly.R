context("labels")

test_that("ggtitle is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ggtitle("My amazing plot!")
  info <- save_outputs(ggiris, "labels-ggtitle")
  # TODO: change me to annotation to support justification
  expect_identical(info$layout$title, "My amazing plot!")
})

test_that("ylab is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ylab("sepal width")
  info <- save_outputs(ggiris, "labels-ylab")
  labs <- c(info$layout$xaxis$title, info$layout$yaxis$title)
  expect_identical(labs, c("Petal.Width", "sepal width"))
})

test_that("scale_x_continuous(name) is translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    scale_x_continuous("petal width")
  info <- save_outputs(ggiris, "labels-scale_x_continuous_name")
  labs <- c(info$layout$xaxis$title, info$layout$yaxis$title)
  expect_identical(labs, c("petal width", "Sepal.Width"))
})

test_that("angled ticks are translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    theme(axis.text.x = element_text(angle = 45))
  info <- save_outputs(ggiris, "labels-angles")
  expect_identical(info$layout$xaxis$tickangle, -45)
})

test_that("labels are translated correctly", {
  ggiris <- ggplot(iris) +
    geom_point(aes(Petal.Width, Sepal.Width)) +
    ggtitle("My amazing plot!") +
    labs(
      subtitle = "Some loooooooooooooooooooooooooooooooooooooooooooooong text",
      caption = "Some loooooooooooooooooooooooooooong text"
    )
  info <- save_outputs(ggiris, "labels-ggtitle")
  # TODO: change me to annotation to support justification
  expect_identical(info$layout$title, "My amazing plot!")
})

# TODO: why is the right plot margin off?
ggplotly(qplot(data = mtcars, vs, mpg))

# TODO: 
# (1) how to handle text being clipped to the width?
# (2) test lineheight once we have a ability to set it

qplot(data = mtcars, vs, mpg, color = factor(am)) +
  labs(
    title = "sadlknewldknewflkcewelkmcdewdlm;dscklmcdslkmcds",
    subtitle = "Some looooooooooooo0000000000000000oooooooooooooooooooooooooooong text",
    caption = "Some loooooooooooooooooooooooooooong text",
    x = "Silly",
    y = "Yar har har"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, debug = TRUE),
    plot.subtitle = element_text(hjust = 0.5, size = 20, debug = TRUE),
    plot.caption = element_text(debug = TRUE),
    axis.title.x = element_text(hjust = 0.5, vjust = 0.75, angle = 90, debug = TRUE),
    axis.title.y = element_text(hjust = 0.5, vjust = 0.25, angle = 45, debug = TRUE),
    # TODO: why doesn't debug get respected here?
    legend.title = element_text(size = 7, debug = TRUE),
    # TODO: 
    legend.title.align = 0.5
  )
