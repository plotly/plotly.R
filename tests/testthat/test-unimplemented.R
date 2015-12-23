context("Unimplemented geoms")


test_that("un-implemented geoms are ignored with a warning", {
  
  dmod <- lm(price ~ cut, data=diamonds)
  cuts <- data.frame(
    cut = unique(diamonds$cut), 
    predict(dmod, data.frame(cut = unique(diamonds$cut)), se=TRUE)[c("fit","se.fit")]
  )
  se <- ggplot(cuts, aes(cut, fit, ymin = fit - se.fit, ymax = fit + se.fit, colour = cut))
  
  expect_warning({
    info <- gg2list(se + geom_linerange())
  }, "geom_linerange() has yet to be implemented in plotly")
})
