context("Errorbar")

test_that("geom_errobar is rendered when y aes is not set", {

  # Example from issue #1751
  d <- data.frame(
    auc = c(0.268707482993197, 0.571428571428571),
    sup = c(0.407680628614317, 0.648343533190079),
    inf = c(0.129734337372078, 0.494513609667063),
    Names = c("Firmicutes", "Spirochaetes")
  )

  # Plot with y aes set
  p <- ggplot(d, aes(Names)) +
    geom_errorbar(aes(y = auc, ymin = inf, ymax = sup))
  L <- plotly_build(p)

  # Plot with y aes not set
  p1 <- ggplot(d, aes(Names)) +
    geom_errorbar(aes(ymin = inf, ymax = sup))
  L1 <- plotly_build(p1)

  # Tests
  # errobar is rendered
  expect_doppelganger(L1, "errobar-no-aes-y")
  ## array and arrayminus of L and L1 are equivalent
  lapply(c("array", "arrayminus"), function(x) {
    expect_equivalent(
      L[["x"]][["data"]][[1]][["error_y"]][[x]],
      L1[["x"]][["data"]][[1]][["error_y"]][[x]]
    )
  })
  ## array equals difference between sup and auc, array equals difference between auc and inf
  expect_equivalent(L1[["x"]][["data"]][[1]]$error_y$array, d$sup - d$auc)
  expect_equivalent(L1[["x"]][["data"]][[1]]$error_y$arrayminus, d$auc - d$inf)
})
