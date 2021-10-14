p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 

test_that("ggplotly correctly handles stat_function", {
  pl <- p + 
    stat_function(fun = function(x) x^2+3) + 
    xlim(1, 5)

    expect_doppelganger(ggplotly(pl), "stat-function")
})

test_that("ggplotly correctly handles stat_function", {
  pl <- p + 
    geom_function(fun = function(x) x^2+3) + 
    stat_function(fun = function(x) 5) + 
    xlim(1, 5)

    expect_doppelganger(ggplotly(pl), "geomfunction")
})