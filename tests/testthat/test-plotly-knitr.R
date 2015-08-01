context("knitr")

txt <- "
Simple knitr demo
```{r}
p <- qplot(rnorm(50))
ggplotly(p)
```
"
test_that("plotly embeds inside knitr", {
  html <- knitr::knit2html(text = txt)
  expect_true(grepl("<iframe", html))
})

# If you want to interactively see the result
#tmp <- tempfile(fileext = ".html")
#writeLines(html, tmp)
#browseURL(tmp)
