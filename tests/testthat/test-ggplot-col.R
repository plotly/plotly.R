context("geom_col")

# https://gist.github.com/jsonbecker/0cc702804512fdf29c7f9067adfc17d0#gistcomment-1935427
df <- data.frame(
  type = c(rep('Elementary', 2), rep('Middle', 2), rep('High', 2)),
  included = rep(c('included','excluded'), 3),
  dollars = c(1000, 2500, 4000, 1000, 3000, 2800)
)

p <- df %>% 
  group_by(type) %>% 
  mutate(prop = dollars / sum(dollars)) %>% 
  ggplot(aes(type, prop, fill = included)) + 
  geom_col() +
  geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5))  

test_that("geom_col is supported", {
  l <- save_outputs(p, "col")
  barDat <- l$data[sapply(l$data, "[[", "type") %in% "bar"]
  expect_equivalent(
    unlist(lapply(barDat, "[[", "x")),
    c(1, 2, 3, 1, 2, 3)
  )
  expect_equal(
    unlist(lapply(barDat, "[[", "y")), 
    c(0.7142857, 0.4827586, 0.2, 0.2857143, 0.5172414, 0.8),
    tolerance = 0.0001
  )
})
