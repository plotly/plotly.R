test_that("ggplotly does not break discrete x-axis with facet_yyyy in panels > 1 with only one category", {
  d <- data.frame(cat = c("A", "A", "A"), pan = paste("Panel", c(1, 2, 2)))
  gp <- ggplot(d, aes(cat)) +
    geom_bar() +
    facet_wrap(~pan)
  L <- plotly_build(ggplotly(gp))
  # tickvals, ticktext and categoryarray are sent to plotly.js as arrays
  json <- jsonlite::fromJSON(plotly_json(L, jsonedit = FALSE), simplifyVector = FALSE)
  lapply(json$layout[c("xaxis", "xaxis2")], function(axis) {
    expect_type(axis$tickvals, "list")
    expect_type(axis$ticktext, "list")
    expect_true(axis$ticktext[[1]] == "A")
    expect_type(axis$categoryarray, "list")
    expect_true(axis$categoryarray[[1]] == "A")
  })
})
