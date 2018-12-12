context("customdata")

# TODO: use shinytest to make sure we can access the right value in shiny
test_that("ggplotly relays customdata", {
  nms <- row.names(mtcars)
  p <- ggplot(mtcars, aes(x = mpg, y = wt, customdata = nms)) + geom_point()
  l <- plotly_build(p)
  trace <- l$x$data[[1]]
  expect_equivalent(trace$customdata, nms)
})


test_that("Can provide list-columns to customdata", {
  l <- txhousing %>%
    group_by(city) %>%
    highlight_key(~city) %>%
    plot_ly(x = ~date, y = ~median, hoverinfo = "name") %>%
    add_lines(customdata = ~purrr::map2(date, median, ~list(.x, .y))) %>%
    plotly_build()
  
  trace <- l$x$data[[1]]
  expect_true(length(trace$customdata) == length(trace$x))
  
  # make sure customdata have been arranged properly
  customx <- unlist(lapply(trace$customdata, function(x) x[1] %||% NA))
  expect_equivalent(customx, trace$x)
  
  # check there is no customdata where x values are null
  nullcd <- trace$customdata[which(is.na(trace$x))]
  expect_true(unique(lengths(nullcd)) == 0)
})

