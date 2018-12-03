#' @import ggplot2
#' @importFrom grDevices col2rgb extendrange dev.list dev.off rgb as.raster
#' @importFrom graphics layout
#' @importFrom utils getFromNamespace modifyList data packageVersion browseURL str
#' @importFrom stats setNames complete.cases quantile is.leaf
#' @importFrom tidyr unnest
#' @importFrom viridisLite viridis
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr GET POST PATCH content config add_headers stop_for_status warn_for_status
#' @importFrom htmlwidgets createWidget sizingPolicy saveWidget onRender prependContent
#' @importFrom lazyeval f_eval is_formula all_dots is_lang f_new
#' @importFrom tibble as_tibble
#' @importFrom htmltools browsable tagList tags div
#' @importFrom purrr transpose
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom data.table as.data.table setorderv
#' @importFrom rlang eval_tidy
NULL


#' @importFrom dplyr mutate 
#' @name mutate
#' @rdname reexports
#' @export
dplyr::mutate

#' @importFrom dplyr mutate_
#' @name mutate_
#' @rdname reexports
#' @export
dplyr::mutate_

#' @importFrom dplyr transmute
#' @name transmute
#' @rdname reexports
#' @export
dplyr::transmute

#' @importFrom dplyr transmute_
#' @name transmute_
#' @rdname reexports
#' @export
dplyr::transmute_

#' @importFrom dplyr select
#' @name select
#' @rdname reexports
#' @export
dplyr::select

#' @importFrom dplyr select_
#' @name select_
#' @rdname reexports
#' @export
dplyr::select_

#' @importFrom dplyr rename
#' @name rename
#' @rdname reexports
#' @export
dplyr::rename

#' @importFrom dplyr rename_
#' @name rename_
#' @rdname reexports
#' @export
dplyr::rename_

#' @importFrom dplyr group_by 
#' @name group_by
#' @rdname reexports
#' @export
dplyr::group_by

#' @importFrom dplyr group_by_
#' @name group_by_
#' @rdname reexports
#' @export
dplyr::group_by_

#' @importFrom dplyr groups
#' @name groups
#' @rdname reexports
#' @export
dplyr::groups

#' @importFrom dplyr ungroup
#' @name ungroup
#' @rdname reexports
#' @export
dplyr::ungroup

#' @importFrom dplyr summarise
#' @name summarise
#' @rdname reexports
#' @export
dplyr::summarise

#' @importFrom dplyr summarise_
#' @name summarise_
#' @rdname reexports
#' @export
dplyr::summarise_

#' @importFrom dplyr do
#' @name do
#' @rdname reexports
#' @export
dplyr::do

#' @importFrom dplyr do_
#' @name do_
#' @rdname reexports
#' @export
dplyr::do_

#' @importFrom dplyr arrange
#' @name arrange
#' @rdname reexports
#' @export
dplyr::arrange

#' @importFrom dplyr arrange_
#' @name arrange_
#' @rdname reexports
#' @export
dplyr::arrange_

#' @importFrom dplyr distinct
#' @name distinct
#' @rdname reexports
#' @export
dplyr::distinct

#' @importFrom dplyr distinct_
#' @name distinct_
#' @rdname reexports
#' @export
dplyr::distinct_

#' @importFrom dplyr slice
#' @name slice
#' @rdname reexports
#' @export
dplyr::slice

#' @importFrom dplyr slice_
#' @name slice_
#' @rdname reexports
#' @export
dplyr::slice_

#' @importFrom dplyr filter
#' @name filter
#' @rdname reexports
#' @export
dplyr::filter

#' @importFrom dplyr filter_
#' @name filter_
#' @rdname reexports
#' @export
dplyr::filter_

# waiting on https://github.com/tidyverse/tidyr/pull/229
#
# #' @importFrom tidyr gather
# #' @name gather
# #' @rdname reexports
# #' @export
# tidyr::gather
# 
# #' @importFrom tidyr gather_
# #' @name gather_
# #' @rdname reexports
# #' @export
# tidyr::gather_
