context("Facets")

# test_that("6 facets becomes 6 panels", {
#   require(lattice)
#   gg <- qplot(yield, variety, data=barley, color=year, facets=site~., pch=I(1))+
#     theme_bw()+
#     theme(panel.margin=grid::unit(0, "cm"))
#   info <- gg2list(gg)
#   traces <- info[names(info)==""]
#   trace.axes <- list()
#   for(N in c("xaxis", "yaxis")){
#     trace.axes[[N]] <- axes.vec <- 
#       sapply(traces, function(t){
#         if(N %in% names(t)){
#           t[[N]]
#         }else{
#           NA
#         }
#       })
#     expect_true(all(!is.na(axes.vec)))
#   }
#   trace.axes.df <- as.data.frame(trace.axes)
#   u <- unique(trace.axes.df)
#   expect_identical(nrow(u), 6L)
# })

test_that("3 facets becomes 3 panels", {
  df <- data.frame(x=runif(99), y=runif(99), z=rep(c('a','b','c'), 33))
  gg <- qplot(x, y, data=df, facets=z~., pch=I(1)) +
    theme_bw() +
    theme(panel.margin=grid::unit(0, "cm"))
  info <- gg2list(gg)
  traces <- info$data
  trace.axes <- list()
  for(N in c("xaxis", "yaxis")){
    trace.axes[[N]] <- axes.vec <- 
      sapply(traces, function(t) {
        if(N %in% names(t)) {
          t[[N]]
        } else {
          NA
        }
      })
    expect_true(all(!is.na(axes.vec)))
  }
  trace.axes.df <- as.data.frame(trace.axes)
  u <- unique(trace.axes.df)
  expect_identical(nrow(u), 3L)
})

# expect a certain number of _unique_ [x/y] axes
expect_axes <- function(info, n, axis = "x") {
  pattern <- paste0("^", axis, "axis([0-9]+)?$")
  axes <- with(info, layout[grepl(pattern, names(layout))])
  n.axes <- length(axes)
  ranges <- do.call("rbind", lapply(axes, function(x) x$range))
  expect_identical(nrow(unique(ranges)), as.integer(n))
}

no_panels <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

test_that("facet_wrap(..., scales = 'free') creates interior scales", {
  free_both <- no_panels + facet_wrap(~am+vs, scales = "free")
  info <- save_outputs(free_both, "facet_wrap_free")
  expect_axes(info, 4L)
  expect_axes(info, 4L, "y")
  
  free_y <- no_panels + facet_wrap(~am+vs, scales = "free_y")
  info <- save_outputs(free_y, "facet_wrap_free_y")
  expect_axes(info, 1L)
  expect_axes(info, 4L, "y")
  
  free_x <- no_panels + facet_wrap(~am+vs, scales = "free_x")
  info <- save_outputs(free_x, "facet_wrap_free_x")
  expect_axes(info, 4L)
  expect_axes(info, 1L, "y")
})

test_that("facet_grid(..., scales = 'free') doesnt create interior scales.", {
  free_both <- no_panels + facet_grid(vs~am, scales = "free")
  info <- save_outputs(free_both, "facet_grid_free")
  expect_axes(info, 2L)
  expect_axes(info, 2L, "y")
  
  free_y <- no_panels + facet_grid(vs~am, scales = "free_y")
  info <- save_outputs(free_y, "facet_grid_free_y")
  expect_axes(info, 1L)
  expect_axes(info, 2L, "y")
  
  free_x <- no_panels + facet_grid(vs~am, scales = "free_x")
  info <- save_outputs(free_x, "facet_grid_free_x")
  expect_axes(info, 2L)
  expect_axes(info, 1L, "y")
})

gg <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point() + geom_line() + 
  facet_wrap(~vs, scales = "free")

test_that("facet_wrap(..., scales = 'free') can handle multiple traces on each panel", {
  info <- save_outputs(gg, "facet_wrap_free_mult")
  yaxes <- sapply(info$data, "[[", "yaxis")
  modes <- sapply(info$data, "[[", "mode")
  expect_true(length(unique(paste(yaxes, modes))) == 4)
})
