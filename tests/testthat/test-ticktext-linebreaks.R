
# Compute margin
comp_margin <- function(gg, axisTickText) {
  plot <- ggfun("plot_clone")(gg)
  theme <- calculated_theme_elements(plot)
  
  pm <- unitConvert(theme$plot.margin, "pixels")
  gglayout <- list(
    margin = list(t = pm[[1]], r = pm[[2]], b = pm[[3]], l = pm[[4]])
  )
  axisTitle <- theme[["axis.title.y"]]
  axisObj <- list(
    ticktext = "djdfkjdfdklj",
    tickfont = text2font(theme[["axis.text"]], "width"),
    ticklen = unitConvert(theme$axis.ticks.length, "pixels", "width")
  )
  
  gglayout$margin[["l"]] + axisObj$ticklen +
    bbox(axisTickText, 0, axisObj$tickfont$size)[["width"]] +
    bbox("y", 90, unitConvert(axisTitle, "pixels", "width"))[["width"]]
}

expect_margin <- function(L, gg, ticktext) {
  margin_l <- comp_margin(gg, ticktext)
  expect_equal(round(L$layout$margin$l, 10), round(margin_l, 10))
}

# Linebreaks
d <- data.frame(x = c(1, 2, 3), y = c("ticktext\nlong_ticktext\nticktext", "ticktext", "ticktext"))
gg <- ggplot(d, aes(x, y)) + geom_bar(stat = "identity")

test_that("ggplotly takes account of linebreaks in ticktext", {
  # Visual Test
  L <- expect_doppelganger_built(gg, "ticktext-linebreaks")
  # ggplotly returns correct margin 
  expect_margin(L, gg, "long_ticktext")
})

# Linebreaks one category
d <- data.frame(x = c(1), y = c("ticktext\nlong_ticktext\nticktext"))
gg <- ggplot(d, aes(x, y)) + geom_bar(stat = "identity")

test_that("ggplotly takes account of linebreaks in ticktext with only one category", {
  # Visual Test
  L <- expect_doppelganger_built(gg, "ticktext-linebreaks-one-cat")
  # ggplotly returns correct margin 
  expect_margin(L, gg, "long_ticktext")
})

# No linebreaks
d <- data.frame(x = c(1, 2, 3), y = c("ticktext long_ticktext ticktext", "ticktext", "ticktext"))
gg <- ggplot(d, aes(x, y)) + geom_bar(stat = "identity")

test_that("ggplotly works with no linebreaks in ticktext", {
  # Visual Test
  L <- expect_doppelganger_built(gg, "ticktext-linebreaks-no-linebreaks")
  # ggplotly returns correct margin 
  expect_margin(L, gg, "ticktext long_ticktext ticktext")
})
