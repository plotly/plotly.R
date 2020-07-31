.onLoad <- function(...) {
  # These generics are defined in R/plotly_data.R
  dplyr_generics <- c(
    "groups", "ungroup", "group_by", "summarise", "mutate", "do", "arrange",
    "select", "filter", "distinct", "slice", "rename", "transmute"
  )
  dplyr_generics <- paste0("dplyr::", dplyr_generics)
  for (generic in dplyr_generics) {
    vctrs::s3_register(generic, "plotly")
    if (generic %in% c("dplyr::groups", "dplyr::ungroup")) {
      next
    }
    vctrs::s3_register(paste0(generic, "_"), "plotly")
  }
}
