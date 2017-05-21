context("spoke")

df <- expand.grid(x = 1:10, y = 1:10)
df$angle <- runif(100, 0, 2*pi)
df$speed <- runif(100, 0, sqrt(0.1 * df$x))

p <- ggplot(df, aes(x, y)) +
  geom_spoke(aes(angle = angle), radius = 0.5)

test_that("Basic geom_spoke()", {
  
  l <- plotly_build(p)$x
  expect_length(l$data, 1)
  expect_equivalent(l$data[[1]]$type, "scatter")
  expect_equivalent(l$data[[1]]$mode, "lines")
  
  txt <- strsplit(l$data[[1]]$text, br())
  angle <- unlist(lapply(txt, function(x) x[grepl("angle", x, fixed = T)]))
  radius <- unlist(lapply(txt, function(x) x[grepl("radius", x, fixed = T)]))
  expect_equivalent(
    angle, rep(paste0("angle: ", format(df$angle)), each = 2)
  )
  expect_equivalent(
    unique(radius), "radius: 0.5"
  )
  
})

df$color <- rep(1:2, each = 50)
p <- ggplot(df, aes(x, y, color = color)) +
  geom_spoke(aes(angle = angle), radius = 0.5)

test_that("Basic geom_spoke() with color", {
  
  l <- plotly_build(p)$x
  # has to 3 traces since plotly.js doesn't (currently) support 
  # more than one `line.color`
  expect_length(l$data, 3)
  for (i in seq_along(l$data)) {
    expect_equivalent(l$data[[i]]$type, "scatter")
    expect_match(l$data[[i]]$mode, "markers|lines")
  }
  
})
