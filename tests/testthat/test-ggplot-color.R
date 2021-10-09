df = data.frame(width = 1:3, height = 1:3, col = letters[1:3])
test_that("ggplotly automatically converts `color` aes to `colour`", {
    ggpenguins <- qplot(width, height, 
                        data = df, color = col)
    # color variable is not shown
    color <- plotly_build(ggplotly(ggpenguins, tooltip = c("color")))
    # colour (with u!) variable is shown
    expect_identical(color$x$data, plotly_build(ggplotly(ggpenguins, tooltip = c("colour")))$x$data)
})

