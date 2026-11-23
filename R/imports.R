#' @import ggplot2
#' @importFrom grDevices col2rgb extendrange dev.list dev.off rgb as.raster
#' @importFrom graphics layout
#' @importFrom utils getFromNamespace modifyList data packageVersion browseURL str file.edit
#' @importFrom stats setNames complete.cases quantile is.leaf
#' @importFrom tidyr unnest
#' @importFrom viridisLite viridis
#' @importFrom jsonlite toJSON parse_json read_json
#' @rawNamespace import(plotly.core, except = c(schema))
#' @rawNamespace export(add_trace, as_widget, config)
#' @evalNamespace sprintf("S3method(add_%s,plotly)", setdiff(plotly.core::available_trace_types(), c("table", "image", "pie", "histogram", "histogram2d", "histogram2dcontour", "heatmap", "contour", "surface", "scattergeo", "choropleth")))
#' @importFrom htmlwidgets createWidget sizingPolicy saveWidget onRender prependContent
#' @importFrom tibble as_tibble
#' @importFrom htmltools browsable tagList tags div
#' @importFrom purrr transpose
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom data.table as.data.table setorderv
#' @importFrom rlang eval_tidy !! !!!
NULL


#' @importFrom plotly.core highlight
#' @name highlight
#' @rdname reexports
#' @export
plotly.core::highlight

#' @importFrom plotly.core attrs_selected
#' @name attrs_selected
#' @rdname reexports
#' @export
plotly.core::attrs_selected

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


# plotly.core functions --------------------------------------------------------
# Rendering, Shiny and static image export come from plotly.core, as do the
# add_<trace type>() functions, which add a layer of that type to a plot_ly()
# object (see R/go_figure.R).

#' @export
plotly.core::plotlyOutput
#' @export
plotly.core::renderPlotly
#' @export
plotly.core::event_data
#' @export
plotly.core::event_register
#' @export
plotly.core::event_unregister
#' @export
plotly.core::plotlyProxy
#' @export
plotly.core::plotlyProxyInvoke
#' @export
plotly.core::save_image
#' @export
plotly.core::partial_bundle
#' @export
plotly.core::TeX
#' @export
plotly.core::add_bar
#' @export
plotly.core::add_barpolar
#' @export
plotly.core::add_box
#' @export
plotly.core::add_candlestick
#' @export
plotly.core::add_carpet
#' @export
plotly.core::add_choropleth
#' @export
plotly.core::add_choroplethmap
#' @export
plotly.core::add_cone
#' @export
plotly.core::add_contour
#' @export
plotly.core::add_contourcarpet
#' @export
plotly.core::add_densitymap
#' @export
plotly.core::add_funnel
#' @export
plotly.core::add_funnelarea
#' @export
plotly.core::add_heatmap
#' @export
plotly.core::add_histogram
#' @export
plotly.core::add_histogram2d
#' @export
plotly.core::add_histogram2dcontour
#' @export
plotly.core::add_icicle
#' @export
plotly.core::add_image
#' @export
plotly.core::add_indicator
#' @export
plotly.core::add_isosurface
#' @export
plotly.core::add_mesh3d
#' @export
plotly.core::add_ohlc
#' @export
plotly.core::add_parcats
#' @export
plotly.core::add_parcoords
#' @export
plotly.core::add_pie
#' @export
plotly.core::add_quiver
#' @export
plotly.core::add_sankey
#' @export
plotly.core::add_scatter
#' @export
plotly.core::add_scatter3d
#' @export
plotly.core::add_scattercarpet
#' @export
plotly.core::add_scattergeo
#' @export
plotly.core::add_scattergl
#' @export
plotly.core::add_scattermap
#' @export
plotly.core::add_scatterpolar
#' @export
plotly.core::add_scatterpolargl
#' @export
plotly.core::add_scattersmith
#' @export
plotly.core::add_scatterternary
#' @export
plotly.core::add_splom
#' @export
plotly.core::add_streamtube
#' @export
plotly.core::add_sunburst
#' @export
plotly.core::add_surface
#' @export
plotly.core::add_table
#' @export
plotly.core::add_treemap
#' @export
plotly.core::add_violin
#' @export
plotly.core::add_volume
#' @export
plotly.core::add_waterfall
