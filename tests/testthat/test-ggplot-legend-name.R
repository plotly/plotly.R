context("legend names")

expect_traces <- function(gg, n.traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("legend_name-", name))
  L <- gg2list(gg)
  all.traces <- L$data
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, layout=L$layout)
}

# scatterplot: R Cookbook example
set.seed(955)
# Make some noisily increasing data
dat <- data.frame(cond1 = rep(c("A", "B"), each = 10),
                  cond2 = rep(rep(c("C", "D"), each = 5), 2),
                  xvar = 1:20 + rnorm(20, sd = 3),
                  yvar = 1:20 + rnorm(20, sd = 3))

# ggplot
p <- ggplot(dat, aes(x = xvar, y = yvar, color = cond1, shape = cond1)) + 
  geom_point()

# tests
test_that("Color and shape, no user generated legend name", {
  info <- expect_traces(p, 2, "scatter_no_user_generated_legend_name")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, "<b>cond1</b>")
})

test_that("Color and shape, labs same legend name", {
  p1 <- p + labs(shape = "Group") + labs(color = "Group")
  info <- expect_traces(p1, 2, "scatter_same_legend_name")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, "<b>Group</b>")
})

test_that("Color and shape, labs different legend name", {
  p2 <- p + labs(shape = "Group Shape") + labs(color = "Group Color")
  info <- expect_traces(p2, 2, "scatter_labs_different_legend_names")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, 
                   "<b>Group Color / Group Shape</b>")
})

test_that("Color and shape, discrete scales, same legend name", {
  p3 <- p + scale_shape_discrete(name = "Group") + 
    scale_color_discrete(name = "Group")
  info <- expect_traces(p3, 2, "scatter_scale_same_legend_name")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, "<b>Group</b>")
})

test_that("Color and shape, discrete scales, different legend names", {
  p4 <- p + scale_shape_discrete(name = "Group Shape") + 
    scale_color_discrete(name = "Group Color")
  info <- expect_traces(p4, 2, "scatter_scale_different_legend_names")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, 
                   "<b>Group Color / Group Shape</b>")
})

test_that("Color and shape, labs and discrete scales, same name", {
  p6 <- p + labs(shape = "Group") + 
    scale_color_discrete(name = "Group")
  info <- expect_traces(p6, 2, "scatter_scale_lab_same_legend_name")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, 
                   "<b>Group</b>")
})

test_that("Color and shape, labs and discrete scales, same name", {
  p7 <- p + labs(shape = "Group Shape") + 
    scale_color_discrete(name = "Group Color")
  info <- expect_traces(p7, 2, "scatter_scale_lab_same_legend_name")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, 
                   "<b>Group Shape / Group Color</b>")
})

# ggplot: two different factors
q <- ggplot(dat, aes(x = xvar, y = yvar, color = cond1, shape = cond2)) + 
  geom_point()

test_that("Color and shape, no user generated legend name, 2 factors", {
  info <- expect_traces(q, 4, "scatter_no_user_generated_legend_name_2_factors")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, "<b>cond1 / cond2</b>")
})

test_that("Color and shape, different legend names, 2 factors", {
  q1 <- q + labs(shape = "Group Shape") + 
    scale_color_discrete(name = "Group Color")
  info <- expect_traces(q1, 4, "scatter_different_legend_names_2_factors")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, 
                   "<b>Group Shape / Group Color</b>")
})

# points, lines plot: R Cookbook example
df <- read.table(header=T, text='
  cond xval yval
     A    1  2.0
     A    2  2.5
     B    1  3.0
     B    2  2.0
')
lp <- ggplot(df, aes(x=xval, y=yval, group = cond)) +
  geom_line(aes(linetype=cond), # Line type depends on cond
            size = 1.5) +       # Thicker line
  geom_point(aes(shape=cond),   # Shape depends on cond
             size = 4) +        # Large points
  scale_shape_manual(values=c(6,5)) +                  # Change shapes
  scale_linetype_manual(values=c("dotdash", "dotted")) # Change linetypes

# tests
test_that("points and line, same legend name", {
  info <- expect_traces(lp, 2, "lines_points_same_legend_name2")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, "<b>cond</b>")
})

test_that("points and line, different legend names", {
  lp1 <- lp + labs(linetype = "Group Linetype", shape = "Group Shape")
  info <- expect_traces(lp1, 2, "lines_points_different_legend_name")
  layout <- info$layout
  expect_identical(layout$annotations[[1]]$text, 
                   "<b>Group Linetype / Group Shape</b>")
})
