# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

plotly is an R package for creating interactive web graphics via the plotly.js JavaScript library. It provides two main interfaces:
- `ggplotly()`: Converts ggplot2 objects to interactive plotly visualizations
- `plot_ly()`: Direct interface to plotly.js for specialized chart types

## Common Commands

### Running Tests
```r
# Run all tests
devtools::test()

# Run visual tests (requires kaleido via reticulate)
Sys.setenv("VISUAL_TESTS" = "true")
devtools::test()

# Run a single test file
devtools::test(filter = "ggplot-bar")
```

### Package Check
```r
rcmdcheck::rcmdcheck()
```

### Building Documentation
```r
devtools::document()
```

### Visual Testing via Docker
For consistent visual test results:
```shell
docker run -v $(pwd):/home/plotly --privileged -p 3838:3838 cpsievert/plotly-orca
```
Access the validation Shiny app at http://0.0.0.0:3838

CI-only visual test run:
```shell
docker run -e VMODE="ci" -v $(pwd):/home/plotly --privileged cpsievert/plotly-orca
```

## Architecture

### ggplot2 to plotly Conversion Pipeline

The conversion from ggplot2 to plotly follows this flow:

1. **`ggplotly()`** (`R/ggplotly.R`): Entry point that dispatches on input type
2. **`gg2list()`** (`R/ggplotly.R`): Main conversion function that:
   - Builds the ggplot object to extract computed data
   - Processes each layer through `layers2traces()`
   - Processes layout through `layers2layout()`
3. **`layers2traces()`** (`R/layers2traces.R`): Converts ggplot2 geom layers to plotly trace objects
4. **`layers2layout()`** (`R/layers2layout.R`): Converts ggplot2 theme/coordinate settings to plotly layout

### Direct plotly Interface

1. **`plot_ly()`** (`R/plotly.R`): Creates a plotly object with trace attributes
2. **`add_trace()`** and `add_*()` functions (`R/add.R`): Add traces to existing plots
3. **`layout()`** (`R/layout.R`): Modify plot layout
4. **`plotly_build()`** (`R/plotly_build.R`): Evaluates lazy attributes and creates final JSON for plotly.js

### Key Modules

- `R/shiny.R`: Shiny integration and event handling
- `R/subplots.R`: Combining multiple plots
- `R/highlight.R`: Linked brushing/crosstalk support
- `R/animate.R`: Animation support
- `R/kaleido.R`, `R/orca.R`: Static image export

## Testing Patterns

Tests should check the return value of `plotly_build()`:
```r
test_that("example test", {
  p <- plot_ly(x = 1:10, y = 1:10)
  built <- plotly_build(p)
  expect_equal(built$x$data[[1]]$x, 1:10)
})
```

Visual tests use `expect_doppelganger()` from `tests/testthat/helper-vdiffr.R`:
```r
test_that("visual test", {
  p <- plot_ly(x = 1:10, y = 1:10)
  expect_doppelganger(p, "scatter-basic")
})
```

## Code Style

Follow the tidyverse style guide: http://style.tidyverse.org/
