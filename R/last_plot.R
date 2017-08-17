# Same as here, thanks Hadley -- 
# https://github.com/hadley/ggplot2/blob/8aa578/R/plot-last.r

.plot_store <- function() {
  .last_plot <- NULL
  
  list(
    get = function() .last_plot,
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()

# Set last plot created or modified
set_last_plot <- function(value) .store$set(value)

#' Retrieve the last plot to be modified or created.
#'
#' @seealso [ggplot2::last_plot()]
#' @export
last_plot <- function() .store$get()
