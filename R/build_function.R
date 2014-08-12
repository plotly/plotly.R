#' ggplot build function with enhanced return
#'
#' This function builds on top of ggplot2::ggplot_build by
#' Hadley Wickham <h.wickham@@gmail.com> and Winston Chang <winston@@stdout.org>
#' (http://ggplot2.org, https://github.com/hadley/ggplot2).
#'
#' @param plot ggplot2 plot
#' @return List with (data, panel, plot, prestats.data) where prestats.data
#' is the data as it is prior to calculate_stats() call
#' @keywords internal
ggplot_build2 <- local({
  # Get body of the original function, in list form
  ggplot_build2 <- ggplot2::ggplot_build
  g_b <- as.list(body(ggplot_build2))
  
  # Find line where we want to insert new code
  line_after <- quote(data <- calculate_stats(panel, data, layers))
  idx <- vapply(g_b, identical, line_after, FUN.VALUE=TRUE)
  idx <- which(idx)
  
  # Insert new code before that line
  new_line <- quote(prestats.data <- data)
  return_value <- quote(list(data=data, panel=panel, plot=plot,
                             prestats.data=prestats.data))
  g_b2 <- c(g_b[seq(1, idx-1)], list(new_line), g_b[seq(idx, length(g_b)-1)],
            return_value)
  
  # Assign the modified body back into the function
  body(ggplot_build2) <- as.call(g_b2)
  ggplot_build2
})
