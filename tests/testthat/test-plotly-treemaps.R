context("treemap")

labels <- c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura")
parents <- c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")

test_that("treemap renders", {
  plot_ly(
    type = "treemap",
    labels = labels,
    parents =  parents
  ) %>%
    expect_doppelganger("basic")
})


test_that("treemap advanced", {
  
  plot_ly(
    labels = labels,
    parents = parents,
    textinfo = "label+value+percent parent+percent entry",
    outsidetextfont = list(size = 20, color = "#377eb8"),
    marker = list(line = list(width = 2)),
    pathbar = list(visible = FALSE)
  ) %>%
    add_trace(
      type = "treemap",
      values =  c(10, 14, 12, 10, 2, 6, 6, 1, 4),
      domain = list(x = c(0, 0.48))
    ) %>%
    add_trace(
      type = "treemap",
      branchvalues = "total",
      domain = list(x = c(0.52, 1)),
      values = c(65, 14, 12, 10, 2, 6, 6, 1, 4)
    ) %>%
    layout(
      annotations = list(
        list(
          showarrow = FALSE,
          text = "branchvalues = <b>remainder</b>",
          x = 0.25,
          xanchor = "center",
          y = 1.1,
          yanchor = "bottom"
        ),
        list(
          showarrow = FALSE,
          text = "branchvalues = <b>total</b>",
          x = 0.75,
          xanchor = "center",
          y = 1.1,
          yanchor = "bottom"
        )
      )
    ) %>%
     expect_doppelganger("advanced") 
})
  
