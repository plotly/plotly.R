context("Errorbar")

test_that("geom_errobar is rendered with flipped aes", {
  
  df <- dplyr::group_by(iris, Species)
  df <- dplyr::summarise_if(df, is.numeric, list(m = mean, q1 = ~ quantile(.x, .25), q3 = ~ quantile(.x, .75)))
  gp <- ggplot(df, aes(y = Species, xmin = Sepal.Width_q1, xmax = Sepal.Width_q3)) +
    geom_errorbar()
  
  L <- plotly_build(gp)
  
  # Tests
  # errobar is rendered
  expect_doppelganger(L, "errobar-flipped-aes")
  # xmin and xmax equal to ggplot 
  expect_equivalent(L[["x"]][["data"]][[1]][["x"]] + L[["x"]][["data"]][[1]][["error_x"]][["array"]],
                    ggplot_build(gp)$data[[1]]$xmax)
  
  expect_equivalent(L[["x"]][["data"]][[1]][["x"]] - L[["x"]][["data"]][[1]][["error_x"]][["arrayminus"]],
                    ggplot_build(gp)$data[[1]]$xmin)
  # xmin and xmax equal to data
  expect_equivalent(L[["x"]][["data"]][[1]][["x"]] + L[["x"]][["data"]][[1]][["error_x"]][["array"]],
                    df$Sepal.Width_q3)
  
  expect_equivalent(L[["x"]][["data"]][[1]][["x"]] - L[["x"]][["data"]][[1]][["error_x"]][["arrayminus"]],
                    df$Sepal.Width_q1)
  
})
