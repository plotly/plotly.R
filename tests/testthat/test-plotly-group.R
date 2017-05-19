context("plotly-group")

expect_traces <- function(p, n.traces, name){
  stopifnot(is.numeric(n.traces))
  L <- save_outputs(p, paste0("plotly-", name))
  expect_equivalent(length(L$data), n.traces)
  L
}

# oh no, June is missing!
e <- economics
isMissing <- format(economics$date, "%m") == "06"
e[isMissing, ] <- NA

p <- plot_ly(e, x = ~date, y = ~pce)

test_that("Missing values are preserved for lines", {
  l <- expect_traces(add_lines(p), 1, "NAs")
  expect_equivalent(sum(is.na(l$data[[1]]$x)), sum(isMissing))
})

test_that("Can connectgaps without groups", {
  l <- expect_traces(add_lines(p, connectgaps = TRUE), 1, "NAs-connect")
  expect_true(l$data[[1]]$connectgaps)
  expect_equivalent(sum(is.na(l$data[[1]]$x)), sum(isMissing))
})

test_that("Warning is thrown when missing values are ignored", {
  expect_warning(
    plotly_build(add_markers(p)), 
    paste("Ignoring", sum(isMissing))
  )
})

e2 <- tidyr::gather(e, variable, value, -date)

p2 <- e2 %>% 
  group_by(variable) %>% 
  plot_ly(x = ~date, y = ~value)

test_that("Missing values are preserved for lines within a group variable", {
  l <- expect_traces(add_lines(p2), 1, "NAs-within-group")
  expect_true(sum(is.na(l$data[[1]]$x)) >= sum(is.na(e2$date)))
  p2b <- add_lines(p2, connectgaps = TRUE)
  expect_error(plotly_build(p2b), "connectgaps")
})

p3 <- plot_ly(e2, x = ~date, y = ~value, color = ~variable)

test_that("Missing values are preserved for lines within a color variable", {
  l <- expect_traces(add_lines(p3), 5, "NAs-within-color")
  # connectgaps makes sense in this case
  l <- expect_traces(add_lines(p3, connectgaps = TRUE), 5, "NAs-within-color2")
})

m <- mtcars
m$rowname <- rownames(mtcars)
p <- m %>% 
  dplyr::group_by_("rowname") %>%
  plot_ly(x = ~wt, y = ~mpg) %>% 
  add_markers()

test_that("Groups are ignored if grouping is irrelevant for the geom", {
  l <- expect_traces(p, 1, "no-NAs-for-irrelevant-group")
  expect_length(l$data[[1]][["x"]], 32)
  expect_length(l$data[[1]][["y"]], 32)
})
