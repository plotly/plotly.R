# Tests for hard difficulty issue fixes
# Following TDD: these tests are written FIRST before implementing fixes

# Issue #2419: Two NAs per category cause incorrect line connection
# When exactly 2 NA values exist per category with a hovertemplate,
# lines incorrectly connect across the NAs instead of creating gaps.

test_that("Issue #2419: exactly 2 NAs per category create gaps, not connected lines", {
  df <- data.frame(
    Category = rep(c("A", "B"), each = 6),
    Date = c(2020, 2021, 2022, 2023, 2024, 2025, 2020, 2021, 2022, 2023, 2024, 2025),
    Value = c(10, 15, NA, NA, 20, 25, 12, 14, NA, 22, NA, 27)
  )
  df$Date <- factor(df$Date, levels = unique(df$Date), ordered = TRUE)

  p <- plot_ly(
    df,
    x = ~Date,
    y = ~Value,
    color = ~Category,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~Category,
    hovertemplate = paste0("Date: %{x}<br>Category: %{text}")
  )

  built <- plotly_build(p)

  # There should be 2 traces (one per category)
  expect_equal(length(built$x$data), 2)

  # For category A: values are 10, 15, NA, NA, 20, 25
  # After NA handling, the y values should have NAs inserted to create gaps
  traceA <- built$x$data[[1]]

  # The key test: NAs should be present in the y data to create gaps

  # If exactly 2 NAs are being connected incorrectly, this would fail
  # We should see NA values in the output that separate the groups
  expect_true(any(is.na(traceA$y)))

  # For category B: values are 12, 14, NA, 22, NA, 27
  traceB <- built$x$data[[2]]
  expect_true(any(is.na(traceB$y)))
})

test_that("Issue #2419: single NA per category creates gaps correctly", {
  df <- data.frame(
    Category = rep(c("A", "B"), each = 6),
    Date = c(2020, 2021, 2022, 2023, 2024, 2025, 2020, 2021, 2022, 2023, 2024, 2025),
    Value = c(10, 15, NA, 18, 20, 25, 12, 14, NA, 22, 24, 27)
  )
  df$Date <- factor(df$Date, levels = unique(df$Date), ordered = TRUE)

  p <- plot_ly(
    df,
    x = ~Date,
    y = ~Value,
    color = ~Category,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~Category,
    hovertemplate = paste0("Date: %{x}<br>Category: %{text}")
  )

  built <- plotly_build(p)

  # There should be 2 traces (one per category)
  expect_equal(length(built$x$data), 2)

  # Both traces should have NA values to create gaps
  traceA <- built$x$data[[1]]
  traceB <- built$x$data[[2]]
  expect_true(any(is.na(traceA$y)))
  expect_true(any(is.na(traceB$y)))
})


# Issue #2468: Pie chart color mapping doesn't work properly when aggregating data
# When plotly.js aggregates pie chart data (duplicate labels), the marker.colors
# don't apply correctly to the first slice.

test_that("Issue #2468: pie chart colors apply correctly with aggregated data", {
  # When there are 3 unique labels but more rows (so plotly aggregates),
  # marker.colors should apply to all slices correctly
  p <- plot_ly(
    mtcars[, c("cyl", "drat")],
    labels = ~cyl,
    values = ~drat,
    type = 'pie',
    marker = list(colors = c("cyan", "magenta", "black"))
  )

  built <- plotly_build(p)

  # The colors should be present in the marker (as-is, values preserved)
  colors <- as.character(built$x$data[[1]]$marker$colors)
  expect_equal(length(colors), 3)
  expect_equal(colors, c("cyan", "magenta", "black"))
})

test_that("Issue #2468: pie chart colors work without aggregation", {
  # Without aggregation (unique labels), colors should still work
  p <- plot_ly(
    mtcars[c(1, 3, 5), c("cyl", "drat")],
    labels = ~cyl,
    values = ~drat,
    type = 'pie',
    marker = list(colors = c("cyan", "magenta", "black"))
  )

  built <- plotly_build(p)

  # The colors should be present in the marker (as-is, values preserved)
  colors <- as.character(built$x$data[[1]]$marker$colors)
  expect_equal(length(colors), 3)
  expect_equal(colors, c("cyan", "magenta", "black"))
})


# Issue #2437: subplot() with bar and pie chart creates NA layout attribute
# When combining bar and pie charts in a subplot, an NA attribute is created
# in the layout, causing a warning.

test_that("Issue #2437: subplot with bar and pie does not create NA layout attribute", {
  bar_info <- data.frame(
    Group = rep(c("first", "second", "third"), 2),
    values_monthly = c(100, 200, 300, 400, 500, 600),
    month = factor(rep(c("April", "May"), each = 3))
  )
  pie_info <- aggregate(values_monthly ~ Group, data = bar_info, sum)
  names(pie_info)[2] <- "values_total"

  colors <- c("red", "blue", "yellow")

  bar_chart <- plot_ly(
    bar_info,
    type = "bar",
    x = ~month,
    y = ~values_monthly,
    color = ~Group,
    colors = colors
  )

  pie_chart <- plot_ly(
    pie_info,
    type = "pie",
    labels = ~Group,
    values = ~values_total,
    marker = list(colors = colors),
    domain = list(x = c(0.9, 1), y = c(0, 1)),
    showlegend = FALSE
  )

  # Should not produce warnings about NA attributes
  expect_no_warning({
    combined_chart <- subplot(bar_chart, pie_chart, nrows = 1, widths = c(0.9, 0.1))
  })

  built <- plotly_build(combined_chart)

  # Layout should not have any attributes with NA names
  layout_names <- names(built$x$layout)
  expect_false(any(is.na(layout_names)))
  expect_false(any(grepl("^NA", layout_names)))
})

test_that("Issue #2437: subplot warnings about discrete/non-discrete data", {
  bar_info <- data.frame(
    Group = rep(c("first", "second", "third"), 2),
    values_monthly = c(100, 200, 300, 400, 500, 600),
    month = factor(rep(c("April", "May"), each = 3))
  )
  pie_info <- aggregate(values_monthly ~ Group, data = bar_info, sum)
  names(pie_info)[2] <- "values_total"

  colors <- c("red", "blue", "yellow")

  bar_chart <- plot_ly(
    bar_info,
    type = "bar",
    x = ~month,
    y = ~values_monthly,
    color = ~Group,
    colors = colors
  )

  pie_chart <- plot_ly(
    pie_info,
    type = "pie",
    labels = ~Group,
    values = ~values_total,
    marker = list(colors = colors),
    domain = list(x = c(0.9, 1), y = c(0, 1)),
    showlegend = FALSE
  )

  # Specifically check that no warning about NA attributes is thrown
  warnings_caught <- character(0)
  withCallingHandlers({
    combined_chart <- subplot(bar_chart, pie_chart, nrows = 1, widths = c(0.9, 0.1))
  }, warning = function(w) {
    warnings_caught <<- c(warnings_caught, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  # Should not have warning about 'NA' attribute
  expect_false(any(grepl("NA", warnings_caught)))
})
