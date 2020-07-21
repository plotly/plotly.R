.onLoad <- function(...) {
  # These generics are defined in R/plotly_data.R
  dplyr_generics <- c(
    "groups", "ungroup", "group_by", "summarise", "mutate", "do", "arrange",
    "select", "filter", "distinct", "slice", "rename", "transmute"
  )
  for (generic in dplyr_generics) {
    register_s3_method("dplyr", generic, "plotly")
    if (generic %in% c("groups", "ungroup")) {
      next
    }
    register_s3_method("dplyr", paste0(generic, "_"), "plotly")
  }
}

# copy/pasta from shiny:::register_s3_method
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
