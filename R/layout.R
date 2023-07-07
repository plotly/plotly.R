#' Modify the layout of a plotly visualization
#' 
#' @param p A plotly object.
#' @param ... Arguments to the layout object. For documentation,
#' see \url{https://plotly.com/r/reference/#Layout_and_layout_style_objects}
#' @param data A data frame to associate with this layout (optional). If not 
#' provided, arguments are evaluated using the data frame in [plot_ly()].
#' @author Carson Sievert
#' @export
layout <- function(p, ..., data = NULL) {
  UseMethod("layout")
}

#' @export
layout.matrix <- function(p, ..., data = NULL) {
  # workaround for the popular graphics::layout() function
  # https://github.com/ropensci/plotly/issues/464
  graphics::layout(p, ...)
}

#' @export
layout.shiny.tag.list <- function(p, ..., data = NULL) {
  idx <- which(vapply(p, is.plotly, logical(1)))
  for (i in idx) {
    p[[i]] <- layout.plotly(p[[i]], ..., data = NULL)
  }
  p
}

#' @export
layout.plotly <- function(p, ..., data = NULL) {
  p <- add_data(p, data)
  attrs <- list(...)
  if (!is.null(attrs[["height"]]) || !is.null(attrs[["width"]])) {
    warning("Specifying width/height in layout() is now deprecated.\n", 
            "Please specify in ggplotly() or plot_ly()", call. = FALSE)
  }
  # similar to add_trace()
  p$x$layoutAttrs <- c(
    p$x$layoutAttrs %||% list(), 
    setNames(list(attrs), p$x$cur_data)
  )
  p
}

#' Add a range slider to the x-axis
#'
#' @param p plotly object.
#' @param start a start date/value.
#' @param end an end date/value.
#' @param ... these arguments are documented here 
#' \url{https://plotly.com/r/reference/#layout-xaxis-rangeslider}
#' @export
#' @author Carson Sievert
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' plot_ly(x = time(USAccDeaths), y = USAccDeaths) %>% 
#'   add_lines() %>%
#'   rangeslider()
#'   
#' d <- tibble::tibble(
#'   time = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by = "days"),
#'   y = rnorm(seq_along(time))
#'  )
#'  
#' plot_ly(d, x = ~time, y = ~y) %>%
#'   add_lines() %>%
#'   rangeslider(d$time[5], d$time[50])
#'   
#' 
rangeslider <- function(p, start = NULL, end = NULL, ...) {
  if (sum(grepl("^xaxis", names(p$x$layout))) > 1) {
    stop("Can only add a rangeslider to a plot with one x-axis", call. = FALSE)
  }
  
  p$x$layout$xaxis$range <- c(
    to_milliseconds(start),
    to_milliseconds(end)
  )
  
  p$x$layout$xaxis$rangeslider <- list(visible = TRUE, ...)
  p
}


#' Set the default configuration for plotly
#' 
#' @param p a plotly object
#' @param ... these arguments are documented at 
#' \url{https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js}
#' @param cloud deprecated. Use `showSendToCloud` instead.
#' @param showSendToCloud include the send data to cloud button?
#' @param locale locale to use. See [here](https://github.com/plotly/plotly.js/tree/master/dist#to-include-localization) for more info.
#' @param mathjax add [MathJax rendering support](https://github.com/plotly/plotly.js/tree/master/dist#to-support-mathjax).
#' If `"cdn"`, mathjax is loaded externally (meaning an internet connection is needed for 
#' TeX rendering). If `"local"`, the PLOTLY_MATHJAX_PATH environment variable must be
#' set to the location (a local file path) of MathJax. IMPORTANT: **plotly** uses SVG-based 
#' mathjax rendering which doesn't play nicely with HTML-based rendering 
#' (e.g., **rmarkdown** documents and **shiny** apps). To leverage both types of rendering, 
#' you must `<iframe>` your plotly graph(s) into the larger document 
#' (see [here](https://github.com/plotly/plotly.R/blob/master/inst/examples/rmd/MathJax/index.Rmd) 
#' for an **rmarkdown** example and 
#' [here](https://github.com/plotly/plotly.R/blob/master/inst/examples/rmd/MathJax/index.Rmd) for a **shiny** example).
#' @author Carson Sievert
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' # remove the plotly logo and collaborate button from modebar
#' config(plot_ly(), displaylogo = FALSE, collaborate = FALSE)
#' 
#' # enable mathjax
#' # see more examples at https://plotly.com/r/LaTeX/
#' plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16)) %>%
#'   layout(title = TeX("\\text{Some mathjax: }\\alpha+\\beta x")) %>%
#'   config(mathjax = "cdn")
#' 
#' # change the language used to render date axes and on-graph text 
#' # (e.g., modebar buttons)
#' today <- Sys.Date()
#' x <- seq.Date(today, today + 360, by = "day")
#' p <- plot_ly(x = x, y = rnorm(length(x))) %>%
#'   add_lines()
#' 
#' # japanese
#' config(p, locale = "ja")
#' # german
#' config(p, locale = "de")
#' # spanish
#' config(p, locale = "es")
#' # chinese
#' config(p, locale = "zh-CN")
#' 

config <- function(p, ..., cloud = FALSE, showSendToCloud = cloud, locale = NULL, mathjax = NULL) {
  
  if (!is.null(locale)) {
    p$x$config$locale <- locale
    # Plotly.js defaults to US English (en-US) and includes 
    # British English (en) in the standard bundle.
    if (!locale %in% c("en", "en-US")) {
      p$dependencies <- c(
        p$dependencies,
        list(locale_dependency(locale))
      )
    }
  }
  
  if (!is.null(mathjax)) {
    mj <- switch(
      match.arg(mathjax, c("cdn", "local")),
      cdn = mathjax_cdn(),
      local = mathjax_local()
    )
    # if mathjax is already supplied overwrite it; otherwise, prepend it
    depNames <- sapply(p$dependencies, "[[", "name")
    if (any(idx <- depNames %in% "mathjax")) {
      p$dependencies[[which(idx)]] <- mathjax
    } else {
      p$dependencies <- c(list(mj), p$dependencies)
    }
  }
  
  args <- list(...)
  if ("collaborate" %in% names(args)) warning("The collaborate button is no longer supported")
  
  args$modeBarButtonsToAdd <- unique(c(
    args$modeBarButtonsToAdd, c("hoverclosest", "hovercompare")
  ))
  
  p$x$config <- modify_list(p$x$config, args)
  if (cloud) warning("The `cloud` argument is deprecated. Use `showSendToCloud` instead.")
  p$x$config$showSendToCloud <- showSendToCloud

  p
}

#' Construct a plot with a `layout.updatemenus` element programmatically
#' 
#' @param plot_list a named list of `plotly` objects
#' @param ... Arguments to the layout.updatemenus object. For documentation,
#' see \url{https://plotly.com/r/reference/layout/updatemenus/}
#' @param show_legend Should the legend always be visible? Set to `TRUE` or `FALSE` to 
#' always display or hide the legend, respectively. If `NA` (the default), legend visibility will mimic 
#' plotly's default settings (for example, legends with only one trace will be hidden)
#' @param active_plot the index of the plot from `plot_list` that is active by default. 
#' By default, the first plot is considered active unless otherwise specified.
#' @returns When given a named list of plotly objects, this function will return a single plotly object 
#' with a `layout.updatemenus` element embedded within it. This plot element can be used to cycle between
#' the plots provided by the user.
#' @export
#' @examplesIf interactive() || !identical(.Platform$OS.type, "windows")
#' 
#' plotly_merge(
#'   plot_list = list(
#'     Petal = iris %>% 
#'       plot_ly(
#'         type = 'scatter',
#'         mode = 'markers',
#'         x = ~Petal.Length,
#'         y = ~Petal.Width,
#'         color = ~Species
#'       ),
#'     Sepal = iris %>% 
#'       plot_ly(
#'         type = 'scatter',
#'         mode = 'markers',
#'         x = ~Sepal.Length,
#'         y = ~Sepal.Width,
#'         color = ~Species
#'       ) %>% layout(yaxis = list(range = c(0, 5))),
#'     mtbars = mtcars %>% 
#'       group_by(cyl) %>% 
#'       summarise(med_mpg = median(mpg)) %>% 
#'       ungroup() %>% 
#'       mutate(cyl = as.character(cyl)) %>% 
#'       plot_ly(
#'         type = 'bar',
#'         x = ~cyl,
#'         y = ~med_mpg
#'       )
#'   )
#' )

plotly_merge = function(plot_list, ..., show_legend = NA, active_plot = 1) {
  stopifnot(
    'plot_list must be a list of plotly objects' = is.list(plot_list),
    'all elements of plot_list must be plotly objects' = vapply(plot_list, function(x) {
      all(c('plotly', 'htmlwidget') %in% class(x))
    }, logical(1)) %>% all(),
    'show_legend must be a logical scalar; if it is NA (the default), legends with a single trace will be hidden' = 
      is.vector(show_legend) && is.logical(show_legend) && length(show_legend) == 1,
    'active_plot must be an integer scalar greater than zero; numerics will be truncated to integers' = is.vector(active_plot) && 
      length(active_plot) == 1 && 
      is.numeric(active_plot) && 
      active_plot >= 1 &&
      is.finite(active_plot),
    'active_plot must be less than or equal to the length of the list of plotly objects provided' = 
      length(active_plot) <= length(plot_list)
  )
  
  # If there is only one plot in the list, return just that plot
  if(length(plot_list) == 1) {return(plot_list[[1]])}
  
  # Truncate active_plot
  active_plot = trunc(active_plot)
  
  # Build out the plots for manipulation
  plot_list = lapply(plot_list, plotly_build) %>% 
    # Examine the first trace of each plot; if legend visibility is not specified,
    # guess whether the user wants to show the legend or not, or use show_legend if a value has been provided
    lapply(function(p) {
      if(is.null(p$x$data[[1]]$showlegend) && length(p$x$data) == 1) {
        p$x$data[[1]]$showlegend = ifelse(!is.na(show_legend), show_legend, FALSE)
      }
      
      return(p)
    })
  
  # Note how many traces each plot contains
  trace_count = vapply(plot_list, function(p_traces) {
    length(p_traces$x$data)
  }, integer(1))
  max_trace = max(trace_count)
  max_traces = seq_len(max_trace)
  
  # If necessary, increase the trace count and set the starting visibility of each plot so that there are enough traces to switch between
  for(p in seq_along(plot_list)) {
    trace_deficit = max_trace - trace_count[p]
    if(trace_deficit > 0) {
      for(traces in max_traces[! max_traces %in% seq_along(plot_list[[p]]$x$data)]) {
        plot_list[[p]]$x$data[[traces]] = plot_list[[p]]$x$data[[1]]
        plot_list[[p]]$x$data[[traces]]$visible = FALSE
      }
    }
  }
  
  # Note the visibility, data, and layout settings each button in the dropdown should have
  p_viz = seq_along(trace_count) %>% 
    setNames(rep('visible', length(.))) %>% 
    lapply(function(p) {
      max_traces <= trace_count[p]
    })
  
  p_data = plot_list %>% 
    p_button_data(n_traces = max(max_traces))
  
  p_layout = plot_list %>% 
    p_button_layout() %>% 
    setNames(names(plot_list)) %>% 
    # If no axis type is specified, assume the axis is linear
    lapply(function(p) {
      p$xaxis$type = ifelse(is.null(p$xaxis$type), 'linear', p$xaxis$type)
      p$yaxis$type = ifelse(is.null(p$yaxis$type), 'linear', p$yaxis$type)
      
      return(p)
    })
  
  # Append the `visible` element to the plot
  p_data = seq_along(p_data) %>% 
    setNames(names(p_data)) %>% 
    lapply(function(p) {
      p_data[[p]]$visible = p_viz[[p]]
      
      return(p_data[[p]])
    })
  
  # Create the dropdown to return the final plot
  plot_list[[active_plot]] %>% 
    layout(
      updatemenus = list(
        list(
          active = active_plot - 1,
          ...,
          # For each plot provided, make a button in the dropdown to switch to that plot
          buttons = seq_along(trace_count) %>% 
            lapply(function(p_trace) {
              list(
                method = 'update',
                args = list(
                  p_data[[p_trace]],
                  p_layout[[p_trace]]
                ),
                label = names(trace_count[p_trace])
              )
            })
        )
      )
    )
}

p_button_data = function(p_list, n_traces) {
  stopifnot(
    'p_list must be a list of plotly objects' = is.list(p_list),
    'all elements of p_list must be plotly objects' = vapply(p_list, function(x) {
      c('plotly', 'htmlwidget') %in% class(x)
    }, logical(2)) %>% all(),
    'All elements of p_list must be named' = !is.null(names(p_list)) && !any(names(p_list) == '')
  )
  
  p_list = p_list %>% lapply(plotly_build)
  
  # Gather the names of the trace elements to restructure
  p_data_elements = p_list %>% 
    lapply(function(p) {
      p$x$data %>% lapply(names) %>% unlist() %>% unique()
    }) %>% 
    unlist() %>% 
    unique() %>% 
    setNames(nm = .)
  
  # For each trace element, return the data in a format suitable for plotly update buttons
  p_list %>% 
    lapply(function(p) {
      p_data_elements %>% lapply(function(d) {
        a = p$x$data %>% lapply(function(p_data) p_data[[d]])
        
        if(length(a) < n_traces) {
          a[(length(a)+1):n_traces] = NA
          # a[(length(a)+1):n_traces] = a[1]
        }
        
        return(a)
      })
    })
}
p_button_layout = function(p_list) {
  stopifnot(
    'p_list must be a list of plotly objects' = is.list(p_list),
    'all elements of p_list must be plotly objects' = vapply(p_list, function(x) {
      c('plotly', 'htmlwidget') %in% class(x)
    }, logical(2)) %>% all(),
    'All elements of p_list must be named' = !is.null(names(p_list)) && !any(names(p_list) == '')
  )
  
  p_list = p_list %>% lapply(plotly_build)
  
  # Gather the names of the trace elements to restructure
  p_data_elements = p_list %>% 
    lapply(function(p) {
      # p$x$layout %>% lapply(names) %>% unlist() %>% unique()
      p$x$layout %>% names()
    }) %>% 
    unlist() %>% 
    unique() %>% 
    setNames(nm = .)
  
  # For each trace element, return the data in a format suitable for plotly update buttons
  p_data_elements %>% lapply(function(l) {
    p_list %>% unname() %>% lapply(function(p) {
      p$x$layout[[l]]
    })
  })
  
  # For each plot, extract the layout
  p_list %>% 
    lapply(function(p) {
      p_data_elements %>% lapply(function(l) {
        p$x$layout[[l]]
      })
    }) %>% 
    # Try to manually add logic to determine the axis type
    lapply(function(p) {
      p$xaxis$type = ifelse(is.null(p$xaxis$type), '-', p$xaxis$type)
      p$yaxis$type = ifelse(is.null(p$yaxis$type), '-', p$yaxis$type)
      
      return(p)
    })
}
