context("ribbon")

expect_traces <- function(gg, n.traces, name){
  stopifnot(is.ggplot(gg))
  stopifnot(is.numeric(n.traces))
  save_outputs(gg, paste0("ribbon-", name))
  L <- gg2list(gg)
  is.trace <- names(L) == ""
  all.traces <- L[is.trace]
  no.data <- sapply(all.traces, function(tr) {
    is.null(tr[["x"]]) && is.null(tr[["y"]])
  })
  has.data <- all.traces[!no.data]
  expect_equal(length(has.data), n.traces)
  list(traces=has.data, kwargs=L$kwargs)
}

huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
huron$decade <- with(huron, round(year/10) * 10)
huron$diff <- huron$year - huron$decade

p1 <- ggplot(data = huron) + 
  geom_ribbon(aes(x = year, ymin = level-1, ymax = level+1), 
              alpha = 0.1)

test_that("geom_ribbon() creates 1 trace & respects alpha transparency", {
  info <- expect_traces(p1, 1, "alpha")
  tr <- info$traces[[1]]
  expect_match(L2[[1]]$fillcolor, "0.1)", fixed=TRUE)
})

p2 <- ggplot(data = huron, aes(group = factor(decade))) + 
  geom_ribbon(aes(x = diff, ymin = level-0.1, ymax = level+0.1))

test_that("geom_ribbon() group aesthetic", {
  info <- expect_traces(p2, 1, "group")
})

p3 <- ggplot(data = huron, aes(colour = factor(decade))) + 
  geom_ribbon(aes(x = diff, ymin = level-0.1, ymax = level+0.1))

test_that("geom_ribbon() colour aesthetic", {
  info <- expect_traces(p3, 1, "colour")
})

p4 <- ggplot(data = huron, aes(fill = factor(decade))) + 
  geom_ribbon(aes(x = diff, ymin = level-0.1, ymax = level+0.1))

test_that("geom_ribbon() fill aesthetic", {
  info <- expect_traces(p4, 1, "fill")
})
