#' @import ggplot2
#' @importFrom grDevices col2rgb
#' @importFrom graphics layout
#' @importFrom utils getFromNamespace modifyList data packageVersion browseURL
#' @importFrom stats setNames complete.cases quantile
#' @importFrom tidyr gather gather_ unnest
#' @importFrom viridisLite viridis
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr GET POST PATCH content config add_headers stop_for_status
#' @importFrom htmlwidgets createWidget sizingPolicy saveWidget
#' @importFrom lazyeval f_eval is_formula all_dots is_lang
NULL

# ------------------------------------------------------------------
# Import dplyr generics and re-export them
# ------------------------------------------------------------------

#' Add new variables.
#'
#' See \link[dplyr]{mutate}.
#'
#' @importFrom dplyr mutate 
#' @name mutate
#' @export
NULL

#' Add new variables.
#'
#' See \link[dplyr]{mutate_}.
#'
#' @importFrom dplyr mutate_
#' @name mutate_
#' @export
NULL

#' Add new variables.
#'
#' See \link[dplyr]{transmute}.
#'
#' @importFrom dplyr transmute
#' @name transmute
#' @export
NULL

#' Add new variables.
#'
#' See \link[dplyr]{transmute_}.
#'
#' @importFrom dplyr transmute_
#' @name transmute_
#' @export
NULL

#' Select/rename variables by name.
#'
#' See \link[dplyr]{select}.
#'
#' @importFrom dplyr select
#' @name select
#' @export
NULL

#' Select/rename variables by name.
#'
#' See \link[dplyr]{select_}.
#'
#' @importFrom dplyr select_
#' @name select_
#' @export
NULL

#' Select/rename variables by name.
#'
#' See \link[dplyr]{rename}.
#'
#' @importFrom dplyr rename
#' @name rename
#' @export
NULL

#' Select/rename variables by name.
#'
#' See \link[dplyr]{rename_}.
#'
#' @importFrom dplyr rename_
#' @name rename_
#' @export
NULL

#' Group a tbl by one or more variables.
#'
#' See \link[dplyr]{group_by}.
#'
#' @importFrom dplyr group_by 
#' @name group_by
#' @export
NULL

#' Group a tbl by one or more variables.
#' 
#' See \link[dplyr]{group_by}.
#'
#' @importFrom dplyr group_by_
#' @name group_by_
#' @export
NULL

#' Get/set the grouping variables for tbl.
#' 
#' See \link[dplyr]{groups}.
#'
#' @importFrom dplyr groups
#' @name groups
#' @export
NULL

#' Get/set the grouping variables for tbl.
#' 
#' See \link[dplyr]{ungroup}.
#'
#' @importFrom dplyr ungroup
#' @name ungroup
#' @export
NULL

#' Summarise multiple values to a single value.
#' 
#' See \link[dplyr]{summarise}.
#'
#' @importFrom dplyr summarise
#' @name summarise
#' @export
NULL

#' Summarise multiple values to a single value.
#' 
#' See \link[dplyr]{summarise_}.
#'
#' @importFrom dplyr summarise_
#' @name summarise_
#' @export
NULL

#' Do arbitrary operations on a tbl.
#' 
#' See \link[dplyr]{do}.
#'
#' @importFrom dplyr do
#' @name do
#' @export
NULL

#' Do arbitrary operations on a tbl.
#' 
#' See \link[dplyr]{do_}.
#'
#' @importFrom dplyr do_
#' @name do_
#' @export
NULL

#' Arrange rows by variables.
#' 
#' See \link[dplyr]{arrange}.
#'
#' @importFrom dplyr arrange
#' @name arrange
#' @export
NULL

#' Arrange rows by variables.
#' 
#' See \link[dplyr]{arrange_}.
#'
#' @importFrom dplyr arrange_
#' @name arrange_
#' @export
NULL

#' Select distinct/unique rows.
#' 
#' See \link[dplyr]{distinct}.
#'
#' @importFrom dplyr distinct
#' @name distinct
#' @export
NULL

#' Select distinct/unique rows.
#' 
#' See \link[dplyr]{distinct_}.
#'
#' @importFrom dplyr distinct_
#' @name distinct_
#' @export
NULL

#' Select rows by position.
#' 
#' See \link[dplyr]{slice}.
#'
#' @importFrom dplyr slice
#' @name slice
#' @export
NULL

#' Select rows by position.
#' 
#' See \link[dplyr]{slice_}.
#'
#' @importFrom dplyr slice_
#' @name slice_
#' @export
NULL

#' Return rows with matching conditions.
#' 
#' See \link[dplyr]{filter}.
#'
#' @importFrom dplyr filter
#' @name filter
#' @export
NULL

#' Return rows with matching conditions.
#' 
#' See \link[dplyr]{filter_asw}.
#'
#' @importFrom dplyr filter_
#' @name filter_
#' @export
NULL
