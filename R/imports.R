#' @import ggplot2
#' @importFrom grDevices col2rgb extendrange dev.list dev.off rgb as.raster
#' @importFrom graphics layout
#' @importFrom utils getFromNamespace modifyList data packageVersion browseURL str file.edit
#' @importFrom stats setNames complete.cases quantile is.leaf
#' @importFrom tidyr unnest
#' @importFrom viridisLite viridis
#' @importFrom jsonlite toJSON parse_json read_json
#' @importFrom httr RETRY content config add_headers authenticate stop_for_status warn_for_status write_disk
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

#' @importFrom dplyr transmute
#' @name transmute
#' @rdname reexports
#' @export
dplyr::transmute

#' @importFrom dplyr select
#' @name select
#' @rdname reexports
#' @export
dplyr::select

#' @importFrom dplyr rename
#' @name rename
#' @rdname reexports
#' @export
dplyr::rename

#' @importFrom dplyr group_by 
#' @name group_by
#' @rdname reexports
#' @export
dplyr::group_by

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

#' @importFrom dplyr do
#' @name do
#' @rdname reexports
#' @export
dplyr::do

#' @importFrom dplyr arrange
#' @name arrange
#' @rdname reexports
#' @export
dplyr::arrange
#' @importFrom dplyr distinct
#' @name distinct
#' @rdname reexports
#' @export
dplyr::distinct

#' @importFrom dplyr slice
#' @name slice
#' @rdname reexports
#' @export
dplyr::slice

#' @importFrom dplyr filter
#' @name filter
#' @rdname reexports
#' @export
dplyr::filter

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
