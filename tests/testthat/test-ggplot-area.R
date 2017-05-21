context("Area")

# Test that the order of traces is correct
# Expect traces function
expect_traces <- function(gg, n_traces, name) {
  stopifnot(is.numeric(n_traces))
  save_outputs(gg, paste0("area-", name))
  L <- gg2list(gg)
  all_traces <- L$data
  no_data <- sapply(all_traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has_data <- all_traces[!no_data]
  expect_equivalent(length(has_data), n_traces)
  list(data = has_data, layout = L$layout)
}

huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
# like getAnywhere(round_any.numeric)
huron$decade <- floor(huron$year / 10) * 10 

ar <- ggplot(huron) + geom_area(aes(x = year, y = level))

test_that("sanity check for geom_area", {
  L <- expect_traces(ar, 1, "simple")
  expect_identical(L$data[[1]]$type, "scatter")
  expect_identical(L$data[[1]]$mode, "lines")
  expect_identical(L$data[[1]]$fill, "toself")
  expect_true(
    L$data[[1]]$fillcolor ==
    toRGB(GeomArea$default_aes$fill, GeomArea$default_aes$alpha)
  )
})

# Test alpha transparency in fill color
gg <- ggplot(huron) + geom_area(aes(x = year, y = level), alpha = 0.4)

test_that("transparency alpha in geom_area is converted", {
  L <- expect_traces(gg, 1, "area-fillcolor")
  expect_true(L$data[[1]]$line$color == "transparent")
  expect_true(
    L$data[[1]]$fillcolor == 
    toRGB(GeomArea$default_aes$fill, 0.4)
  )
})


# Generate data
df <- aggregate(price ~ cut + carat, data = diamonds, FUN = length)
names(df)[3] <- "n"
temp <- aggregate(n ~ carat, data = df, FUN = sum)
names(temp)[2] <- "sum.n"
df <- merge(x = df, y = temp, all.x = TRUE)
df$freq <- df$n / df$sum.n
# Generate ggplot object
p <- ggplot(data = df, aes(x = carat, y = freq, fill = cut)) + 
  geom_area() 
# Test 
test_that("traces are ordered correctly in geom_area", {
  info <- expect_traces(p, 5, "traces_order")
  tr <- info$data[[1]]
  la <- info$layout
  expect_identical(tr$type, "scatter")
  # check trace order
  trace.names <- levels(df$cut)
  for (i in 1:5){
    expect_true(grepl(trace.names[i], info$data[[i]]$name))
  }
})

