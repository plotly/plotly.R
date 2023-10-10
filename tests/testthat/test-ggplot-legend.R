

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- expect_doppelganger_built(gg, paste0("legend-", name))
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equivalent(length(has.data), n.traces)
  list(data = has.data, layout = L$layout)
}

p <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(vs), shape = factor(cyl))) + 
  geom_point()

test_that("Discrete colour and shape get merged into one legend", {
  info <- expect_doppelganger_built(p, "scatter_legend")
  expect_equivalent(length(info$data), 5)
  expect_true(info$layout$showlegend)
  # 5 legend entries
  expect_equivalent(sum(sapply(info$data, "[[", "showlegend")), 5)
  # verify entries are sorted correctly
  nms <- sapply(info$data, "[[", "name")
  d <- unique(mtcars[c("vs", "cyl")])
  d <- d[order(d$vs, d$cyl), ]
  expect_identical(
    nms, paste0("(", d$vs, ",", d$cyl, ")")
  )
  legend_title <- info$layout$legend$title$text
  expect_match(legend_title, "^factor\\(vs\\)")
  expect_match(legend_title, "factor\\(cyl\\)$")
})


test_that("legend vanishes when theme(legend.position = 'none'')", {
  info <- expect_traces(p + theme(legend.position = "none"), 5, "hide")
  expect_identical(info$layout$showlegend, FALSE)
})

test_that("aesthetics can be discarded from legend with guide(aes = 'none')", {
  df1 <- data.frame(
    Date = seq(as.Date("2021-01-01"), as.Date("2021-01-10"), "days"),
    Series = c(rep("SeriesA", 10), rep("SeriesB", 10)),
    Values = rnorm(n = 20),
    Mean = 0, V1 = 2, V2 = -2
  )
  
  p <- ggplot(df1, aes(x=Date, y=Values, color = Series, linetype = Series, shape = Series)) +
    geom_line(aes(x = Date, y = Mean, color = "Mean", linetype = "Mean")) +
    geom_line(aes(x = Date, y = V1, color = "QC", linetype = "QC")) +
    geom_line(aes(x = Date, y = V2, color = "QC", linetype = "QC")) +
    geom_line() + 
    geom_point() + 
    guides(shape = "none", linetype = "none")
  
  expect_doppelganger(p, "guide-aes-none")
})

test_that("legend can be manipulated via guides(aes = guide_xxx())", {
  # Issue posted on Stackoverflow
  # https://stackoverflow.com/questions/75365694/plotly-did-not-show-legend-when-converted-from-ggplot
  data <- data.frame(
    stringsAsFactors = FALSE,
    Level = c("Fast","Fast","Fast","Fast",
              "Fast","Fast","Slow","Slow","Slow",
              "Slow","Slow","Slow"),
    Period = c("1Year","3Month","1Year","3Month",
               "1Year","3Month","1Year","3Month",
               "1Year","3Month","1Year","3Month"),
    X = c(0.002,0.002,0.1,0.1,0.9,0.9,
          0.002,0.002,0.1,0.1,0.9,0.9),
    Y = c(1.38,1.29,1.61,1.61,1.74,0.98,
          1.14,0.97,1.09,1.1,0.94,0.58)
  )
  
  gg <- ggplot(data = data,
               aes(x = X,
                   y = Y,
                   shape = Period,
                   color = Level)) +
    geom_point(alpha = 0.6, size = 3) +
    labs(x = " ",
         y = "Value") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
    guides(color = guide_legend(title = "Period", order = 1),
           shape = guide_legend(title = "", order = 2)) +
    theme(axis.text.x = element_text(angle = 90))
  
  info <- expect_doppelganger_built(gg, "respect-guides")
  
  expect_equivalent(sum(sapply(info$data, "[[", "showlegend")), 4)
})

p <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(vs))) + 
  geom_point()

# TODO: better support for scale_*_discrete()
#test_that("trace order respects scale_color_discrete()", {
#  g <- p + scale_color_discrete(breaks = c(1, 0))
#  info <- expect_traces(g, 2, "iris-default")
#  nms <- unlist(lapply(info$data, "[[", "name"))
#  expect_true(all(nms == c("factor(vs): 1", "factor(vs): 0")))
#})
#
#test_that("missing breaks translates to showlegend=FALSE", {
#  g <- p + scale_color_discrete(breaks = 1)
#  info <- expect_traces(two.legend.entries, 3, "iris-trace-showlegend-FALSE")
#  expect_equivalent(sum(sapply(info, "[[", "showlegend")), 1)
#})

# test of legend position
test_that("very long legend items", {
  long_items <- data.frame(
    cat1 = sample(x = LETTERS[1:10], 
                  size = 100, replace = TRUE),
    cat2 = sample(x = c("AAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
                        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
                        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCC"),
                  size = 100, replace = TRUE)
  )
  p_long_items <- ggplot(long_items, aes(cat1, fill = cat2)) + 
    geom_bar(position = "dodge")
  info <- expect_traces(p_long_items, 3, "very-long-legend-items")
})

penguins <- palmerpenguins::penguins
penguins$All <- "All species"
p <- qplot(data = penguins, x = bill_length_mm, y = bill_depth_mm, color = All)

test_that("legend is created with discrete mapping regardless of unique values", {
  info <- expect_traces(p, 1, "one-entry")
  expect_true(info$data[[1]]$showlegend)
  expect_true(info$layout$showlegend)
  expect_true(nzchar(info$layout$legend$title$text))
})

test_that("can hide legend", {
  info <- expect_traces(hide_legend(p), 1, "hide-legend")
  expect_false(info$layout$showlegend)
  expect_null(info$layout$annotations %||% NULL)
})


# test of legend position
test_that("many legend items", {
  p <- ggplot(midwest, aes(category, fill = category)) + geom_bar()
  info <- expect_traces(p, length(unique(midwest$category)), "many legend items")
})


# Make sure we can support the bugfix made in tidyverse/ggplot2#5425
test_that("can handle varying aesthetics/scales", {
  df <- data.frame(x = (1:3)/3, z = c("red", "blue", "green"))
  
  p <- ggplot(df) +
    aes(x, z, colour = z, fill = z, size = x) +
    geom_point() +
    scale_discrete_identity(aesthetics = c("colour", "fill")) +
    scale_size_identity()
  
  expect_traces(p, 1, "varying-aes-guide")
})
