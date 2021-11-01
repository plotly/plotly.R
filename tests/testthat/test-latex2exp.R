library(latex2exp)

test_that("latex2exp expressions render correctly", {
  p <- qplot(1, "A")+
    ylab(TeX("$\\frac{2hc^2}{\\lambda^\\beta}$"))+
    xlab(TeX("$\\alpha$"))

  expect_doppelganger(config(ggplotly(p), mathjax="cdn"), "latex2exp-rendering")
})
