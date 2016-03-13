context("maps")

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- tidyr::gather(crimes, variable, value, -state)
states_map <- map_data("state")
g <- ggplot(crimesm, aes(map_id = state)) +
  geom_map(aes(fill = value), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  facet_wrap( ~ variable)


test_that("basic geom_map works",  {
  l <- save_outputs(g, "map-facet")
  expect_true(length(l$data) > 1)
})

