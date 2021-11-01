skip_if_not_installed("ggalluvial")
library(ggalluvial)

test_that("using both of `geom_alluvium` and `geom_stratum` gives the correct output", {
  p <- ggplot(as.data.frame(Titanic),
              aes(y = Freq, axis1 = Survived, axis2 = Sex, axis3 = Class)) +
    geom_alluvium(aes(fill = Class),
                  width = 0, knot.pos = 0, reverse = FALSE) +
    guides(fill = "none") +
    geom_stratum(width = 1/8, reverse = FALSE) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              reverse = FALSE) +
    scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
    coord_flip() +
    ggtitle("Titanic survival by class and sex")
  expect_doppelganger(ggplotly(p), "stratum-alluvium")
})

test_that("color aesthetic works", {
  p <- ggplot(as.data.frame(Titanic),
              aes(y = Freq, axis1 = Survived, axis2 = Sex, axis3 = Class)) +
    geom_alluvium(aes(color = Class), width = 0, knot.pos = 0, reverse = FALSE, alpha = 0.3) +
    geom_stratum(width = 1/8, reverse = FALSE) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              reverse = FALSE) +
    scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
    coord_flip() +
    ggtitle("Titanic survival by class and sex")
  expect_doppelganger(ggplotly(p), "stratum-alluvium-color")
})

