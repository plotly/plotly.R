# highlight() and attrs_selected() are plotly.core's (re-exported in
# imports.R): they set the same `highlight` options, which plotly_build()
# completes in supply_highlight_attrs().

# ----------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------


highlight_defaults <- function() {
  args <- formals(highlight)[-1]
  # have to evaluate args now that some of them are functions...
  compact(lapply(args, function(x) tryNULL(eval(x))))
}
