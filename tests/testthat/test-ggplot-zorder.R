# Test zorder assignment and layering

test_that("boxplot layering without zorder", {
  # Simulate old behavior: no automatic zorder assignment
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, alpha = 0.4, 
                fill = "lightblue", color = "blue") +
    geom_boxplot(alpha = 0.8, fill = "orange", color = "darkred", 
                 linewidth = 2, width = 0.6)
  
  # Build with current version then remove zorder to simulate old behavior
  p <- plotly_build(ggplotly(gg))
  
  # Remove zorder to simulate old plotly behavior
  for (i in seq_along(p$x$data)) {
    p$x$data[[i]]$zorder <- NULL
  }
  
  # Generate snapshot without zorder
  expect_doppelganger(p, "boxplot-without-zorder")
  
  expect_equivalent(length(p$x$data), 3)
  
  # Verify no zorder values (old behavior)
  for (i in seq_along(p$x$data)) {
    expect_null(p$x$data[[i]]$zorder)
  }
})

test_that("boxplot layering with zorder", {
  # Test current fix: automatic zorder assignment ensures proper layering
  gg <- ggplot(mtcars, aes(factor(cyl), mpg)) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, alpha = 0.4, 
                fill = "lightblue", color = "blue") +
    geom_boxplot(alpha = 0.8, fill = "orange", color = "darkred", 
                 linewidth = 2, width = 0.6)
  
  L <- expect_doppelganger_built(gg, "boxplot-with-zorder")
  
  expect_equivalent(length(L$data), 3)
  
  # Verify automatic zorder assignment (the fix)
  expect_equivalent(L$data[[1]]$zorder, 1)  # regression line
  expect_equivalent(L$data[[2]]$zorder, 2)  # confidence interval  
  expect_equivalent(L$data[[3]]$zorder, 3)  # boxplot (highest, on top)
  
  # Verify trace types
  expect_equivalent(L$data[[1]]$type, "scatter")  # regression line
  expect_equivalent(L$data[[2]]$type, "scatter")  # confidence interval  
  expect_equivalent(L$data[[3]]$type, "box")      # boxplot
  
  # Most important: boxplot has highest zorder (renders on top)
  expect_true(L$data[[3]]$zorder > L$data[[1]]$zorder)
  expect_true(L$data[[3]]$zorder > L$data[[2]]$zorder)
})
