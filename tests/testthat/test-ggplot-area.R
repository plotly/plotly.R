context("Area")

huron <- data.frame(year=1875:1972, level=as.vector(LakeHuron))
huron$decade <- plyr::round_any(huron$year, 10, floor)

ar <- ggplot(huron) + geom_area(aes(x=year, y=level))
L <- gg2list(ar)

test_that("sanity check for geom_area", {
  expect_equal(length(L$data), 1)
  expect_identical(L$data[[1]]$type, "scatter")
  expect_equal(L$data[[1]]$x, c(huron$year[1], huron$year, tail(huron$year, n=1)))
  expect_equal(L$data[[1]]$y, c(0, huron$level, 0))
  expect_identical(L$data[[1]]$line$color, "transparent")
})

save_outputs(ar, "area")

# Test alpha transparency in fill color
gg <- ggplot(huron) + geom_area(aes(x=year, y=level), alpha=0.4)
L <- gg2list(gg)

test_that("transparency alpha in geom_area is converted", {
  expect_identical(L$data[[1]]$line$color, "transparent")
  expect_identical(L$data[[1]]$fillcolor, "rgba(51,51,51,0.4)")
})

save_outputs(gg, "area-fillcolor")

# Test that the order of traces is correct
# Expect traces function
expect_traces <- function(gg, n_traces, name) {
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n_traces))
  save_outputs(gg, paste0("coord_fixed-", name))
  L <- gg2list(gg)
  all_traces <- L$data
  no_data <- sapply(all_traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has_data <- all_traces[!no_data]
  expect_equal(length(has_data), n_traces)
  list(traces = has_data, layout = L$layout)
}
# Generate ggplot object
library(dplyr)
df <- diamonds %>% group_by(carat, cut) %>% 
  summarise(n = n()) %>% mutate(freq = n/sum(n))
df$r.cut <- factor(df$cut, levels = rev(levels(df$cut)))
p <- ggplot(data = df, aes(x = carat, y = freq, fill = cut)) + 
  geom_area() 
# Test 
test_that("traces are ordered correctly in geom_area", {
  info <- expect_traces(p, 5, "geom_area_traces_order")
  tr <- info$traces[[1]]
  la <- info$layout
  expect_identical(tr$type, "scatter")
  # check trace order
  trace.names <- rev(levels(df$cut))
  for (i in 1:5){
    expect_identical(info$traces[[i]]$name, trace.names[i])
  }
})
# Save output
save_outputs(p, "area_traces_order")

