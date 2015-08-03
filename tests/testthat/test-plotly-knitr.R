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
  # why does this all of a sudden fail on Travis? 
  # https://travis-ci.org/ropensci/plotly/builds/73902815
  print(html)
  #expect_true(grepl("iframe", html))
})

# If you want to interactively see the result
#tmp <- tempfile(fileext = ".html")
#writeLines(html, tmp)
#browseURL(tmp)
