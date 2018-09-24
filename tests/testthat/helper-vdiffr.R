
save_outputs <- function(gg, name) {
  p <- plotly_build(gg)$x[c("data", "layout")]
  vdiffr::expect_doppelganger(name, p)
  invisible(p)
}
