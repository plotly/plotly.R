# Tests for specific issue fixes

# Issue #2458: bargroupgap and other layout attributes should not warn
test_that("Cross-trace layout attributes do not produce warnings", {
  p <- plot_ly(x = 1:3, y = 1:3, type = "bar")

  # Bar attributes
  expect_silent(plotly_build(layout(p, bargroupgap = 0.1)))
  expect_silent(plotly_build(layout(p, barnorm = "fraction")))

  # Box attributes
  expect_silent(plotly_build(layout(p, boxmode = "group")))
  expect_silent(plotly_build(layout(p, boxgap = 0.1)))
  expect_silent(plotly_build(layout(p, boxgroupgap = 0.1)))

  # Violin attributes
  expect_silent(plotly_build(layout(p, violinmode = "group")))
  expect_silent(plotly_build(layout(p, violingap = 0.1)))
  expect_silent(plotly_build(layout(p, violingroupgap = 0.1)))

  # Funnel attributes
  expect_silent(plotly_build(layout(p, funnelmode = "group")))
  expect_silent(plotly_build(layout(p, funnelgap = 0.1)))
  expect_silent(plotly_build(layout(p, funnelgroupgap = 0.1)))

  # Waterfall attributes
  expect_silent(plotly_build(layout(p, waterfallmode = "group")))
  expect_silent(plotly_build(layout(p, waterfallgap = 0.1)))
  expect_silent(plotly_build(layout(p, waterfallgroupgap = 0.1)))
})

# Issue #2420: ggplotly legend should use scale labels
test_that("ggplotly legend uses custom scale labels", {
  d <- data.frame(X = 1:5, Y = 1:5)

  # Test with scale_color_manual labels

  gg <- ggplot(d, aes(x = X, y = Y, col = "value1")) +
    geom_point() +
    scale_color_manual(values = c("blue"), labels = c("Custom Label"))

  p <- ggplotly(gg)
  built <- plotly_build(p)

  # The trace name should be "Custom Label", not "value1"
  expect_equal(built$x$data[[1]]$name, "Custom Label")
  expect_equal(built$x$data[[1]]$legendgroup, "Custom Label")
})

test_that("ggplotly legend uses custom labels with multiple values", {
  d <- data.frame(X = 1:10, Y = (1:10)^2, grp = rep(c("a", "b"), 5))

  gg <- ggplot(d, aes(x = X, y = Y, col = grp)) +
    geom_point() +
    scale_color_manual(
      values = c("a" = "red", "b" = "blue"),
      labels = c("a" = "Group A", "b" = "Group B")
    )

  p <- ggplotly(gg)
  built <- plotly_build(p)

  # Get trace names
  trace_names <- sapply(built$x$data, function(tr) tr$name)
  trace_names <- trace_names[!is.na(trace_names)]

  expect_true("Group A" %in% trace_names)
  expect_true("Group B" %in% trace_names)
  expect_false("a" %in% trace_names)
  expect_false("b" %in% trace_names)
})

# Issue #2462: dynamicTicks with grouped geom_line should not error
test_that("dynamicTicks works with grouped geom_line", {
  df <- data.frame(
    time = factor(rep(c("t1", "t2"), 4)),
    value = c(1.25, 1.5, 2, 1.75, 1.25, 0.25, 3, 3.5),
    grp = factor(rep(1:4, each = 2))
  )

  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line(aes(group = grp))

  # This should not error (previously failed with "attempt to select less than one element")
  expect_silent(built <- plotly_build(ggplotly(p, dynamicTicks = TRUE)))

  # Verify the data contains NA values (from group2NA) that are preserved
  trace_x <- built$x$data[[1]]$x
  expect_true(any(is.na(trace_x)))

  # Non-NA values should be categorical labels
  non_na_x <- trace_x[!is.na(trace_x)]
  expect_true(all(non_na_x %in% c("t1", "t2")))
})

# Issue #2446: Date class should be preserved in colorbar trace
test_that("Date class is preserved in colorbar trace", {
  df <- data.frame(
    y = 1:10,
    rank = sample(1:100, 10),
    datetime = seq(as.Date("2022-01-01"), by = "day", length.out = 10)
  )

  p <- plot_ly(df, type = "scatter", mode = "markers") %>%
    add_trace(x = ~datetime, y = ~y, color = ~rank)

  built <- plotly_build(p)


  # Find the main data trace (not the empty first trace or colorbar)
  data_trace <- NULL
  for (tr in built$x$data) {
    if (!is.null(tr[["x"]]) && length(tr[["x"]]) > 2) {
      data_trace <- tr
      break
    }
  }

  expect_false(is.null(data_trace))
  expect_s3_class(data_trace[["x"]], "Date")

  # The x values should be in 2022, not 1970
  expect_true(all(data_trace[["x"]] >= as.Date("2022-01-01")))
  expect_true(all(data_trace[["x"]] <= as.Date("2022-12-31")))
})

test_that("POSIXct class is preserved in colorbar trace", {
  df <- data.frame(
    y = 1:10,
    rank = sample(1:100, 10),
    datetime = seq(as.POSIXct("2022-01-01"), by = "day", length.out = 10)
  )

  p <- plot_ly(df, type = "scatter", mode = "markers") %>%
    add_trace(x = ~datetime, y = ~y, color = ~rank)

  built <- plotly_build(p)

  # Find the main data trace
  data_trace <- NULL
  for (tr in built$x$data) {
    if (!is.null(tr[["x"]]) && length(tr[["x"]]) > 2) {
      data_trace <- tr
      break
    }
  }

  expect_false(is.null(data_trace))
  expect_s3_class(data_trace[["x"]], "POSIXt")
})
