# context("thematic")
# 
# test_that("ggplotly() works with thematic", {
#   skip_if_not_installed("thematic")
#   library(thematic)
#   thematic_on(bg = "black", fg = "white", accent = "purple")
#   on.exit(thematic_off(), add = TRUE)
#   expect_doppelganger_built(
#     qplot(1:10, 1:10), 
#     "thematic-geom-and-theme-defaults"
#   )
#   expect_doppelganger_built(
#     qplot(1:10, 1:10, color = 1:10), 
#     "thematic-sequential"
#   )
#   expect_doppelganger_built(
#     ggplot(economics_long, aes(date, value01, color = variable)) + geom_line(), 
#     "thematic-qualitative"
#   )
# })
