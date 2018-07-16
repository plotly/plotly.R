# 4.8.0

## NEW FEATURES & IMPROVEMENTS

### plotly.js and `plot_ly()` specific improvements

* Upgraded to plotly.js v1.39.2. A _huge_ amount of features and improvements have been made since v1.29.2 (i.e., the version included in the last CRAN release of the R package - v4.7.1). Highlights include a complete re-write of `scattergl` to make it nearly feature complete with `scatter`, localization of text rendering (i.e., international translations), and six new trace types (`cone`, `scatterpolar`, `scatterpolargl`, `splom`, `table`, & `violin`)! See [here](https://github.com/plotly/plotly.js/releases) for a complete list of plotly.js-specific improvements.
* Support for **sf** (simple feature) data structures was added to `plot_ly()`, `plot_mapbox()`, and `plot_geo()` (via the new `add_sf()` function). See [this blog post](https://blog.cpsievert.me/2018/03/30/visualizing-geo-spatial-data-with-sf-and-plotly) for an overview.
* Better control over the stroke (i.e., outline) appearance of various filled graphical marks via the new "special arguments" (`stroke`, `strokes`, `alpha_stroke`, `span`, and `spans`). For an overview, see the **sf** blog post linked to in the bullet point above and the new package demos (list all demos with `demo(package = "plotly")`).

### `ggplotly()` specific improvements

* `ggplotly()` now supports conversion of **ggplot2**'s `geom_sf()`.
* One may now inform `ggplotly()` about the relevant **shiny** output size via `session$clientData`. This ensures `ggplotly()` sizing is closer to **ggplot2** sizing, even on window resize. For an example, run `plotly_example("shiny", "ggplotly_sizing")`.

### Other improvements relevant for all **plotly** objects

* LaTeX rendering via MathJax is now supported and the new `TeX()` function may be used to flag a character vector as LaTeX (#375). Use the new `mathjax` argument in `config()` to specify either external (`mathjax="cdn"`) or local (`mathjax="local"`) MathJaX. If `"cdn"`, mathjax is loaded externally (meaning an internet connection is needed for TeX rendering). If `"local"`, the PLOTLY_MATHJAX_PATH environment variable must be set to the location (a local file path) of MathJax. IMPORTANT: **plotly** uses SVG-based mathjax rendering which doesn't play nicely with HTML-based rendering (e.g., **rmarkdown** documents and **shiny** apps). To leverage both types of rendering, you must `<iframe>` your plotly graph(s) into the larger document (see [here](https://github.com/ropensci/plotly/blob/master/inst/examples/rmd/MathJax/index.Rmd) for an **rmarkdown** example  and [here](https://github.com/ropensci/plotly/blob/master/inst/examples/rmd/MathJax/index.Rmd) for a **shiny** example).
* The selection (i.e., linked-brushing) mode can now switch from 'transient' to 'persistent' by holding the 'shift' key. It's still possible to _force_ persistent selection by setting `persistent = TRUE` in `highlight()`, but `persistent = FALSE` (the default) is now recommended since it allows one to switch between [persistent/transient selection](https://plotly-book.cpsievert.me/linking-views-without-shiny.html#transient-versus-persistent-selection) in the browser, rather than at the command line.
* The `highlight()` function gains a `debounce` argument for throttling the rate at which `on` events may be fired. This is mainly useful for improving user experience when `highlight(on = "plotly_hover")` and mousing over relevant markers at a rapid rate (#1277)
* The new `partial_bundle()` function makes it easy to leverage [partial bundles of plotly.js](https://github.com/plotly/plotly.js#partial-bundles) for reduced file sizes and faster render times.
* The `config()` function gains a `locale` argument for easily changing localization defaults (#1270). This makes it possible localize date axes, and in some cases, modebar buttons (#1270).
* The `plot_geo()` function gains a `offline` argument for rendering `"scattergeo"` traces with or without an internet connection (#356). Leveraging this argument requires the new **plotlyGeoAssets** package.
* Support for async rendering of inside **shiny** apps using the [promises](https://rstudio.github.io/promises/) package (#1209). For an example, run `plotly_example("shiny", "async")`.
* Instead of an error, `ggplotly(NULL, "message")` and `plotly_build(NULL, "message")` now returns `htmltools::div("message")`, making it easier to relay messages in shiny when data isn't yet ready to plot (#1116).
* The `animation_button()` function gains a `label` argument, making it easier to control the label of an animation button generated through the `frame` API (#1205).
* The new `highlight_key()` function provides a wrapper around `crosstalk::SharedData$new()`, making it easier to teach others how to leverage `SharedData` objects with **plotly** and **crosstalk**.

## CHANGES

### `plot_ly()` specific changes

* The `name` attribute is now a "special `plot_ly()` argument" and behaves similar to `split` (it ensures a different trace for every unique value supplied). Although this leads to a breaking change (`name` was previously appended to an automatically generated trace name), it leads to a more flexible and transparent API. Those that wish to have the old behavior back should provide relevant mappings to the `name` attributes (e.g. `plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(vs), name = "a")` should become `plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~factor(vs), name = ~paste(vs, "\na"))`)
* The `color` argument now maps to `fillcolor`, making it much easier to use polygon fills to encode data values (e.g., choropleth maps). For backwards-compatibilty reasons, when `color` maps to `fillcolor`, `alpha` defaults to 0.5 (instead of 1). For an example, `plot_mapbox(mn_res, color = ~INDRESNAME)` or `plot_mapbox(mn_res, split = ~INDRESNAME, color = ~AREA, showlegend = FALSE, stroke = I("black"))`.
* The `color` argument no longer automatically add `"markers"` to the `mode` attribute for scatter/scattergl trace types. Those who wish to have the old behavior back, should add `"markers"` to the `mode` explicity (e.g., change `plot_ly(economics, x = ~pce, y = ~pop, color = ~as.numeric(date), mode = "lines")` to `plot_ly(economics, x = ~pce, y = ~pop, color = ~as.numeric(date), mode = "lines+markers")`).
* The `size` argument now informs a default [error_[x/y].width](https://plot.ly/r/reference/#scatter-error_x-width) (and `span` informs [error_[x/y].thickness](https://plot.ly/r/reference/#scatter-error_x-thickness)). Note you can override the default by specifying directly (e.g. `plot_ly(x = 1:10, y = 1:10, size = I(10), error_x = list(value = 5, width = 0))`).
* `layout.showlegend` now defaults to `TRUE` for a *single* pie trace. This is a more sensible default and matches pure plotly.js behavior.

### Other changes relevant for all **plotly** objects

* All axis objects now default to `automargin = TRUE`. The majority of the time this should make axis labels more readable, but may have un-intended consequences in some rare cases (#1252). 
* The `elementId` field is no longer populated, which fixes the "Ignoring explicitly provided widget ID" warning in shiny applications (#985).

## BUG FIXES

### `ggplotly()` specific fixes

* The default `height`/`width` that `ggplotly()` assumes is now more consistently correct in various context, but it also now requires access to one of the following devices: `Cairo::Cairo()`, `png()`, or `jpg()`. 
* In RStudio, `ggplotly()` was ignoring a specified `height`/`width` (#1190).
* `ggplotly()` now uses fixed heights for facet strips meaning that their height is still correct after a window resize (#1265).

### `plot_ly()` specific fixes

* The `limits` argument of `colorbar()` wasn't being applied to `line.color`/`line.cmin`/`line.cmax` (#1236).
* The `legendgroup` can now properly map data values (#1148).

### Other fixes relevant for all **plotly** objects

* Marker sizes (i.e., `marker.size`) are now _always_ based on the area when `marker.sizemode='area'` (which is the default sizemode when using the `size` argument). Previously, traces with one just one value supplied to `marker.size` were being sized by their diameter (#1133).
* Bug fix for linking views with crosstalk where the source of the selection is an aggregated trace (#1218).
* Resizing behavior, after updating `height`/`width` via **shiny** reactive values, is now correct (#1068).
* Fixed algorithm for coercing the proposed layout to the plot schema (#1156).
* `add_*()` no longer inherits `crosstalk::SharedData` key information when `inherit = FALSE` (#1242).


# 4.7.1

## NEW FEATURES & IMPROVEMENTS

* It is now possible to modify (i.e., update without a full redraw) plotly graphs inside of a shiny app via the new `plotlyProxy()` and `plotlyProxyInvoke()` functions. For examples, see `plotly_example("shiny", "proxy_relayout")` and `plotly_example("shiny", "proxy_mapbox")`. Closes #580. 
* Added a new `plotly_example()` function to make it easier to run shiny/rmarkdown examples included with the package under the `inst/examples` directory.
* The `schema()` function now returns the plot schema (rather just printing it), making it easier to acquire/use values from the official plot schema. See `help(schema)` for an example. Fixes #1038.

## CHANGES

* Upgraded to plotly.js v1.29.2 -- https://github.com/plotly/plotly.js/releases/tag/v1.29.2

## BUG FIXES

* The default sizing in `ggplotly()` is no longer fixed to the device size inside RStudio. Fixes #1033.
* Removed use of `ArrayBuffer.isView()`, which should fix rendering issues on plaforms that don't have a typed array polyfill (e.g., RStudio on Windows). Fixes #1055.
* `event_data("plotly_relayout")` no longer fires `NULL` for any event. Fixes #1039.
* Fixed a bug when using `color` with scattermapbox/scattergeo. Fixes #1038.
* Fixed a highlighting bug when brushing multiple points with `marker.color` as an array. Fixes #1084.


# 4.7.0

## NEW FEATURES & IMPROVEMENTS

* Added support for fixed coordinates (i.e., the aspect ratio component of `coord_equal()`, `coord_fixed()`, `coord_map()`, `coord_quickmap()`).
* Added support for `geom_sf()` and `coord_sf()`.
* The (previously internal) `group2NA()` function is now exported and its performance has been greatly improved thanks to the new **data.table** dependency. Essentially any geom conversion that reduces to a polygon/path should see speed improvements. Similarly, any `plot_ly()` graph that use `group_by()` in conjunction with `add_lines()`, `add_paths()`, `add_segments()`, etc will also see improvements, especially when there is a large number of groups. For details on the speed improvements, see #1022 and #996 (thanks @msummersgill).
* The `api_create()` function gains a new `fileopt` argument, which is inspired from the `fileopt` argument in the (deprecated) `plotly_POST()` function (fixes #976). It currently supports to values: `"new"` and `"overwrite"`. The default, `"overwrite"`, will overwrite existing file(s) with a matching `filename`.
* The `filename` argument in `api_create()` now accepts a character vector of length 2, the first string is used to name the plot, and the second is used to name the grid (i.e., data behind the plot).

## CHANGES

* Upgraded to plotly.js v1.27.1 -- https://github.com/plotly/plotly.js/releases/tag/v1.27.1
* The `traces` argument in the `style()` function now defaults to `NULL` (instead of 1). Meaning that, by default, supplied attributes now modify _every_ trace (instead of the first one).

## Bug fixes 

* Fixes numerous problems with `coord_flip()` (fixes #1012).
* The typed array polyfill is now included *before* the plotly.js bundle, which should fix some rendering issues in some browsers, including RStudio (fixes #1010).
* When creating private plots (via `api_create()`), both the plot and the data behind the plot are private (fixes #976).
* Creating a plot with multiple traces (or frames) via (via `api_create()`) no longer creates multiple grids (fixes #1004).
* The `api_create()` function should now create grid references for all data array attributes (fixes #1014).
* `ggplotly()` no longer opens an (off-screen) graphics device in RStudio for sizing. It will now correctly use the size of the viewer panel when querying the size of the graphics device.
* Margins are no longer always set to `NULL` for pie charts (fixes #1002)
* Fixed bug when highlight multiple 'simple key' traces (fixes #974).

# 4.6.0

## NEW FEATURES & IMPROVEMENTS

* Added a significant amount of support for "multiple linked views". For some relatively basic examples, see the demos (the ones prefixed with "highlight" are most relevant) -- `demo(package = "plotly")`. For a more comprehensive overview, see <https://cpsievert.github.io/plotly_book/linking-views-without-shiny.html>. For some more complex examples, see <https://cpsievert.github.io/pedestrians/>
* Added the `highlight()` function for configuring selection modes/sequences/options.
* Added support for animation. For some relatively basic examples, see the examples section of `help(animation)`. For a more thorough overview, see <https://cpsievert.github.io/plotly_book/key-frame-animations.html>
* Added a `frame` argument to `plot_ly()` for creating animations. Also added the `animation_opts()`, `animation_slider()`, and `animation_button()` functions for configuring animation defaults.
* Added a new interface to [v2 of the REST API](https://api.plot.ly/v2). This new interface makes the  `plotly_POST()` and `get_figure()` functions obsolete (use `api_create()` and `api_download_plot()` instead), and thus, are now deprecated, but remain around for backwards-compatibility. For more details, see `help(api)`.
* Added support for conversion of more **ggplot2** geoms via `ggplotly()`: `GeomCol`, `GeomRug`, `GeomCrossbar`, `GeomQuantile`, `GeomSpoke`, `GeomDotplot`, `GeomRasterAnn` (i.e., `annotation_raster()`), and `GeomAnnotationMap` (i.e., `annotation_map()`).
* Added a new function `raster2uri()` which makes it easier to embed raster objects as [images](https://plot.ly/r/reference/#layout-images) via data URIs. For examples, see `help(raster2uri)`.
* `ggplotly()` gains a new argument, `dynamicTicks`, which allows axis ticks to update upon zoom/pan interactions (fixes #485).
* Sensible sizing and positioning defaults are now provided for subplots multiple colorbars.
* R linebreaks are translated to HTML linebreaks (i.e., '\n' translates to '<br />') (fixes #851).
* Added a `plot_dendro()` function for a quick and dirty interactive dendrogram with support for hierarchial selection. For more, see -- <https://cpsievert.github.io/plotly_book/linking-views-without-shiny.html#nested-selections>
* The `export()` function gains a `selenium` argument for rendering/exporting WebGL plots and exporting to 'svg'/'webp' file formats (via the plotly.js function [Plotly.downloadImage()](https://plot.ly/javascript/plotlyjs-function-reference/#plotlydownloadimage)).
* Better type checking of trace attributes will now automatically reduce a single-valued vector to a constant (when appropriate). This is particularly useful for anchoring multiple traces to a single legend entry via `legendgroup` (see #675, #817, #826).
* The `plotlyOutput()` function gains a `inline` argument which makes it easier to place multiple plots in the same row (in a shiny application).

## CHANGES

* Upgraded to plotly.js v1.26.1 -- https://github.com/plotly/plotly.js/releases/tag/v1.26.1
* `ggplotly()` now applies `format()` to automatically generated hoverinfo. This will allow for finer control over the text displayed (for instance, `options(digits = 4)` can now be used to choose the number of significant digits used). See #834 for an example.
* `HTMLwidgets.renderValue()` should now avoid creating too many active WebGL contexts (thanks @AleksandrIanevski).
* A TypedArray polyfill is now included by default, and the function `remove_typedarray_polyfill()` was added to make it easy to remove it. Fixes #824, #717, #825.
* If no graphics device is already open, `ggplotly()` now tries to open/close a Cairo graphics device, then a bitmap (png/jpeg) device. If neither is available, it errors. This helps to ensure that a *screen* device is never opened by `ggplotly()` (which fixes #829). Furthermore, if `width`/`height` is not specified *and* no graphics device is currently open, a default of 640/480 is used for width/height of the device. 

## BUG FIXES


* Placement of bars (in all cases, even when representing a negative count) should now be correct (applies to `geom_bar()`, `geom_histogram()`, `geom_col()`). Fixes #560, #874, #901, #831.
* Fix for hoverinfo displaying the heights of bars in the translation `geom_bar()` via `ggplotly()`. Fixes #557 and #662.
* `embed_notebook()` now works in *nteract* notebooks (see #768). 
* Axis categories are no longer reordered for matrices (see #863).
* Fix for hoverinfo displaying values after scale transformations (in `ggplotly()`). Fixes #804.
* Font faces for axis titles are now translated (in `ggplotly()`). Fixes #861.

# 4.5.6

## NEW FEATURES

* Added support for the `timezone` argument in __ggplot2__'s `scale_datetime()`. Fixes (#743, thanks @earowang).

## CHANGES

* Now requires  a version of __ggplot2__ higher than 2.1.0 because the new ggproto faceting infrastructure introduced breaking changes.
* A book icon is added to the mode bar, by default, which links to the plotly book. If you want to remove this icon from a plot `p`, do `config(p, modeBarButtonsToRemove = "Collaborate")`
* Specifying height/width in `layout()` is now deprecated. Specify in `ggplotly()` or `plot_ly()`.
* The `ggplotly()` function now preserves all information about the layer mapping. This makes it possible to access input/output data from any layer.

## BUG FIXES

* HTMLwidget.resize() used to ignore specified `layout.width`/`layout.height`.
* When `height`/`width` are specified in `ggplotly()`, relative sizes are now translated correctly. Fixes #489 and #510.
* More careful handling of font when expanding annotation arrays. Fixes #738.
* Ignore data arrays of non-tidy traces. Fixes #737.
* When using `ggplotly()` on a plot with `geom_line` and `group` aesthetic wrong tooltip information was shown. Fixes #774.

# 4.5.5 -- 28 September 2016

## NEW FEATURES

* histogram2d/histogram2dcontour traces now respect the `colors` argument.

## BUG FIX

* Don't traceify by non-existant levels, fixes #735.

# 4.5.4 -- 27 September 2016

## BUG FIX

* Only insert missing values to differentiate groups when it's relevant.

# 4.5.3 -- 27 September 2016

## NEW FEATURES

* The `colorbar()` function gains a new `limits` arguments for controlling the colorscale
limits.

## BUG FIX

* The `z` is now required in `add_heatmap()`. If you want a `z` to be computed, use `add_histogram()`.

# 4.5.2 -- 23 September 2016

## NEW FEATURES

* The new argument, `split`, replaces the old functionality of the now deprecated `group` argument by creating one trace per value.

## BUG FIXES

* Passing plots to `subplot()` without a specified color (once again) match the coloring defaults supplied by plotly.js (see #724).

# 4.5.1 -- 23 September 2016

## NEW FEATURES

* A tibble with a list-column of plotly objects can now be input directly into `subplot()`

## BUG FIXES

* The `colorbar()` function now works on colorbars generated via `z` mapping.

# 4.5.0 -- 22 September 2016

## NEW FEATURES

* Added the `plot_mapbox()` and `plot_geo()` functions, which make it easier to work with the "scattermapbox", "scattergeo", and "choropleth" trace types. See the maps chapter of the plotly book for some examples -- <https://cpsievert.github.io/plotly_book/maps.html> 
* `subplot()` now accepts, and correctly scales mapbox objects.
* Added the `add_mesh3d()` and `add_pie()` functions as wrappers around the "mesh3d", and "pie" trace types.

## CHANGES

* The `add_scattergeo()` and `add_choropleth()` functions have been deprecated in favor of `plot_geo()`. 
* The `add_area(...)` function changed it's meaning from `add_lines(..., fill = 'tozeroy')` to a wrapper around the area trace <https://plot.ly/r/reference/#area>. This is more consistent with the naming conventions already in place for other `add_()` functions.
* `add_ribbons()` now shows points (instead of fill) on hover.

# 4.4.5 -- 19 September 2016

## NEW FEATURES

* Added a `rangeslider()` function to make it easier to add a range slider to the x-axis.
* Added a `colorbar()` function to make it easier to modify an automatically generated colorbar.

## BUG FIXES

* Bug fix for data arranging (introduced in 4.4.2).
* If the same (discrete) variable is mapped to two different aesthetics, redundant text is no longer generated in the legend entries (e.g., `plot_ly(mpg, x = ~cty, y = ~hwy, symbol = ~factor(cyl), color = ~factor(cyl))`)

# 4.4.4 -- 15 September 2016

## NEW FEATURES

* Added `inherit` argument for all `add_()` functions to avoid inheriting attributes from `plot_ly()`.
* Added the `add_fun()` function to add layers to a plot without modifying the original data associated with a plotly object.
* Added the `add_annotations()` function to make it easier to add annotations.
* Added the `layerData` argument to `ggplotly()` to make it possible to retrieve the data from a particular __ggplot2__ layer.

# 4.4.3 -- 15 September 2016

## CHANGES

* Downgrade to plotly.js v1.16.3 (which is proven to be a stable release and avoids #717) -- https://github.com/plotly/plotly.js/releases/tag/v1.17.2

# 4.4.2 -- 14 September 2016

## BUG FIXES

* Arrange data by trace index _before_ computing groups, fixes #716.

# 4.4.1 -- 14 September 2016

## BUG FIXES

* Restrict to atomic vectors when gathering data for training; otherwise, formulas referencing variables that don't exist in the data, but reference a function can cause problems.

# 4.4.0 -- 13 September 2016

## CHANGES 

* To enhance visibility of small markers, `marker.line.color` is now transparent by default.
* Upgraded to plotly.js v1.17.2 -- https://github.com/plotly/plotly.js/releases/tag/v1.17.2

## BUG FIXES

* It is now possible (again) to set/change attributes of autogenerated `marker.colorbar`.
* The `add_choropleth()` previously wasn't relaying the `z` attribute.
* Factors were being treated as characters in `add_segments()` (resulting in incorrect axis category order).
* No more error in `plot_ly()` when the number of traces is a multiple of ten.

# 4.3.7 -- 11 September 2016

## BUG FIXES

* `event_data()` now works inside shiny modules (#659). For an example, see <https://github.com/ropensci/plotly/tree/master/inst/examples/shiny/event_data_modules>

# 4.3.6 -- 9 September 2016

## CHANGES

* Upgraded to plotly.js v1.17.0 -- https://github.com/plotly/plotly.js/releases/tag/v1.17.0

## BUG FIXES

* Fix for error handling in `add_bars()`.
* More careful logic for inferring data variables. Fixes #712

# 4.3.5 -- 5 September 2016

## NEW FEATURES

* The internal `as_widget()` function was exported to make it easier to convert a list
(adhering to the plotly spec) to a plotly htmlwidget object. This should only be needed when "manually" editing the underlying list object.
* Warnings about missing attributes now supply information about the relevant trace.

## CHANGES

* vignettes were removed and that documentation will now be hosted at <https://cpsievert.github.io/plotly_book/>

## BUG FIXES

* Get event_data() working with subplots. Fixes #663

# 4.3.4 -- 31 August 2016

## CHANGES

* Expressions yielding a ggplot2 object can now, once again, be provided to `plotlyOutput()`. In order to make this possible, `ggplotly()` now has a method for plotly objects (the identity function), and `ggplotly()` called on any expression provided to `plotlyOutput()`.

# 4.3.3 -- 29 August 2016

## BUG FIXES

* Bug fix for translation of ggplot2's `geom_text()`.

# 4.3.2 -- 26 August 2016

## NEW FEATURES

* The function `last_plot()` can now be used to retrieve the most recently _printed_ plotly graph. Thanks to this new feature, when `plotly_POST()` is called with no plotly object supplied, the most recently _printed_ plotly graph is sent to the users plotly account.

# 4.3.1 -- 23 August 2016

## CHANGES

* Upgraded to plotly.js v1.16.3 -- https://github.com/plotly/plotly.js/releases/tag/v1.16.3

# 4.3.0 -- 22 August 2016

## NEW FEATURES

* The `colors`/`symbols`/`linetypes` arguments now accept _named_ character vectors.
The names specify the domain (i.e., data values) and the values specify the range
(i.e., visual encodings). This is mainly useful to ensure a particular 
(discrete) data value is mapped to a particular visual attribute (yes, this is similar, in spirit, to ggplot2's `scale_*_manual()`).

## CHANGES

* Symbol and linetype palette defaults are now determined by `scales::shape_pal()` and `scales::linetype_pal()`.
* viridis is the default color scale for ordered factors.
* When mapping a character string to `color`/`symbol`/`linetype`, domain values are 
sorted alphabetically before scales are applied. Also, when mapping a factor to `color`/`symbol`/`linetype`, domain values are sorted according to their factor levels before scales are applied. This leads to more consistent (categorical axis ordering behaves similarly) and predictable (compared to having values sorted in the order in which they appear) behavior.

## BUG FIXES

# 4.2.1 -- 22 August 2016

## BUGFIX

* `alpha` is now applied when `color` isn't specified (fixes #658).

# 4.2.0 -- 11 August 2016

## CHANGES

* `plot_ly()` now orders the categories of a discrete x/y axis according the level ordering (if a factor) or alphabetical order (if a character string). Fixes #578.

# 4.1.1 -- 8 August 2016

## CHANGES

* Upgraded to plotly.js v1.16.1 -- https://github.com/plotly/plotly.js/releases/tag/v1.16.1

# 4.1.0 -- 27 June 2016

## NEW FEATURES

* `ggplotly()` gains a new `originalData` argument which allows one to attach either the original (global) data, or a "scaled"/"trained" version of the data used by __ggplot2__ to draw the graph (for a quick example, `ggplotly(qplot(1:10), originalData = FALSE) %>% plotly_data()`). 
* Hoverinfo is now shown for fill, instead of points, for several geoms (`geom_polygon()`/`geom_hex()`/`geom_rect()`/`geom_map()`). 
* If `stat_identity()` is used, group domain values are preserved and displayed in hoverinfo.
* New functions `hide_guides()`/`hide_legend()` were added (these work similarly to the existing `hide_colorbar()`) to simply the hiding of guides (i.e., legends/colorbars).

## BUG FIXES

* Legend titles (annotations) are no longer generated when no legend is displayed (#635, #607)
* Hoverinfo is no longer displayed if no tooltip variables are present in a layer (#563).
* Facets with 10 or more columns/rows should now render correctly (#640).
* x-axis anchors in `facet_wrap()` should now be correct.

## OTHER CHANGES

* Upgraded to plotly.js v1.15.0 -- https://github.com/plotly/plotly.js/releases/tag/v1.15.0

# 4.0.2 -- 25 June 2016

## BUG FIXES

* Bug fix for formulas evaluating to a logical vector (#650)

# 4.0.1 -- 14 June 2016

## BUG FIXES

* Duplicated values of positional attributes are no longer removed (bug was introduced in v4.0.0).

## OTHER CHANGES

* Upgraded to plotly.js v1.14.2 -- https://github.com/plotly/plotly.js/releases/tag/v1.14.2

# 4.0.0 -- 13 June 2016

## BREAKING CHANGES & IMPROVEMENTS:

* Formulas (instead of plain expressions) are now required when using variable mappings. For example, `plot_ly(mtcars, x = wt, y = mpg, color = vs)` should now be `plot_ly(mtcars, x = ~wt, y = ~mpg, color = ~vs)`. This is a major breaking change, but it is necessary to ensure that evaluation is correct in all contexts (as a result, `evaluate` argument is now deprecated as it is no longer needed). It also has the benefit of being easier to program with (i.e., writing your own custom functions that wrap `plot_ly()`) since it preserves [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency). For more details, see the [lazyeval vignette](https://github.com/hadley/lazyeval/blob/master/vignettes/lazyeval.Rmd)
* The data structure used to represent plotly objects is now an htmlwidget object (instead of a data frame with a special attribute tracking visual mappings). As a result, the `as.widget()` function has deprecated, and [serialization/memory leak problems](https://github.com/rstudio/shiny/issues/1151) are no longer an issue. This change also implies that arbitrary data manipulation functions can no longer be intermingled inside a plot pipeline, but plotly methods for dplyr's data manipulation verbs are now provided (see `?plotly_data` for examples).
* The `group` variable mapping no longer create multiple traces, but instead defines "gaps" within a trace (fixes #418, #381, #577). Groupings should be declared via the new `group_by()` function (see `help(plotly_data)` for examples) instead of the `group` argument (which is now deprecated).
* `plot_ly()` now _initializes_ a plotly object (i.e., won't add a scatter trace by default), meaning that something like `plot_ly(x = 1:10, y = 1:10) %>% add_trace(y = 10:1)` creates one trace, instead of two. That being said, if you manually specify a trace type in `plot_ly()`, it will add a layer with that trace type (e.g. `plot_ly(x = 1:10, y = 1:10, type = "scatter") %>% add_trace(y = 10:1)` draws two scatter traces). If no trace type is provided, a sensible type is inferred from the supplied data, and automatically added (i.e., `plot_ly(x = rnorm(100))` now creates a histogram).
* The `inherit` argument is deprecated. Any arguments/attributes specified in `plot_ly()` will automatically be passed along to additional traces added via `add_trace()` (or any of it's `add_*()` siblings).
* Aesthetic scaling (e.g., `color`, `symbol`, `size`) is applied at the plot-level, instead of the trace level. 
* Size is no longer automatically included in hovertext (closes #549).

## NEW FEATURES & IMPROVEMENTS:

* Added `linetype`/`linetypes` arguments for mapping discrete variables to line types (works very much like the `symbol`/`symbols`).
* Scaling for aesthetics can be avoided via `I()` (closes #428). This is mainly useful for changing default appearance (e.g. `plot_ly(x = 1:10, y = 1:10, color = I("red"))`).
* Symbols and linetypes now recognize `pch` and `lty` values (e.g. `plot_ly(x = 1:25, y = 1:25, symbol = I(0:24))`)
* A new `alpha` argument controls the alpha transparency of `color` (e.g. `plot_ly(x = 1:10, y = 1:10, color = I("red"), alpha = 0.1)`).
* Added a `sizes` argument for controlling the range of marker size scaling.
* New `add_polygons()`/`add_ribbons()`/`add_area()`/`add_segments()`/`add_lines()`/`add_markers()`/`add_paths()`/`add_text()` functions provide a shorthand for common special cases of `add_trace()`.
* New `toWebGL()` function for easy conversion from SVG to WebGL.
* New `export()` function makes it easy to save plots as png/jpeg/pdf (fixes #311).
* Misspecified trace/layout attributes produce a warning.
* New `plotly_data()` function for returning/inspecting data frame(s) associated with a plotly object.
* New `plotly_json()` function for inspecting the data sent to plotly.js (as an R list or JSON).
* `layout()` is now a generic function and uses method dispatch to avoid conflicts with `graphics::layout()` (fixes #464).

## OTHER CHANGES:

* Upgraded to plotly.js v1.14.1 -- https://github.com/plotly/plotly.js/releases/tag/v1.14.1

3.6.5 -- 10 June 2016

IMPROVEMENT:

Multiple rows of facet strips will now be separated by <br> (i.e., line breaks) instead of ,. See #593.

3.6.4 -- 31 May 2016

BUG FIX:

embed_notebook() will no longer use a '.embed' extension in the iframe src attribute. See #613.

3.6.3 -- 24 May 2016

CHANGES:

Provided a better way of reexporting magrittr::`%>%`. See #597.

3.6.2 -- 24 May 2016

CHANGES: 

Removed unnecessary plyr dependency.

3.6.1 -- 23 May 2016

BUG FIX: 

Add a default method for plotly_build. Fixes #592.

3.6.0 -- 16 May 2016

NEW FEATURES & CHANGES:

* Many improvements to the subplot() function:
  * ggplot2 objects are now officially supported (#520).
  * Several new arguments allow one to synchronize x/y axes (#298), height/width (#376), hide/show x/y axis titles.
  * A list of plots can now be passed to the first argument.
  * A new vignette with examples and more explanation can be accessed via `vignette("subplot")`.

* ggplotly() is now a generic function with a method for ggmatrix objects.
* plotly_build() is now a generic function. 

BUG FIX: 

Column facet strips will no longer be drawn when there is only one column.

3.5.7 -- 13 May 2016

CHANGES:

Better defaults for defaultWidth/defaultHeight in the htmlwidget's sizing policy.

BUG FIX:

Pass knitr options to the named argument options. Fixes #582.

3.5.6 -- 12 May 2016

BUG FIX:

Use .embed suffix in iframe src attribute. Fixes #581.

3.5.5 -- 5 May 2016

CHANGES:

ggplotly() will now use plotly's layout.axisid.title (instead of 
layout.annotations) for axis titles on non-faceted plots. 
This will make for a better title placement experience (see #510).

BUG FIX:

Space for interior facet_wrap() strips are now accounted for.

3.5.4 -- 5 May 2016

BUG FIX:

gg2list() now returns an object of class "plotly_built" instead of "plotly"
to ensure a sensible print method is invoked.

3.5.3 -- 3 May 2016

CHANGES:

Upgrade to plotlyjs v1.10.1 -- https://github.com/plotly/plotly.js/releases/tag/v1.10.1

3.5.2 -- 2 May 2016

BUG FIX:

Added missing key properties in ggplotly() converter so selections can be accessible via event_data().

3.5.1 -- 26 Apr 2016

CHANGES:

Upgrade to plotlyjs v1.10.0 -- https://github.com/plotly/plotly.js/releases/tag/v1.10.0

Distinguish between "built" (plotly_built) and "non-built" (plotly_hash) plotly objects. See #562


3.5.0 -- 19 Apr 2016

NEW FEATURES:

The toRGB() function will now respect alpha channels in hex color codes and can recursively apply alpha. 

CHANGES:

The toRGB() function will always output color codes with an alpha channel (e.g. toRGB('black') is now 'rgba(0,0,0,1)' instead of 'rgb(0,0,0)')

3.4.15 -- 18 Apr 2016

BUGFIX:

The alpha in geom_smooth was incorrectly inheriting from other layers. See #551.

3.4.14 -- 15 Apr 2016

CHANGES:

Upgrade to plotlyjs v1.9.0 -- https://github.com/plotly/plotly.js/releases/tag/v1.9.0

3.4.13 -- 6 Apr 2016

BUGFIX:

In some cases, marker color was inheriting from the marker line color when
it shouldn't have. See ##537.

3.4.12 -- 5 Apr 2016

CHANGES:

Upgrade to plotlyjs v1.8.0 -- https://github.com/plotly/plotly.js/releases/tag/v1.8.0

3.4.11 -- 2 Apr 2016

BUGFIX:

Fix bug when altering modebar button defaults

3.4.10 -- 1 Apr 2016

BUGFIX:

Fix a geom_errorbar bug introduced in 3.4.9. See #513.

3.4.9 -- 25 Mar 2016

BUGFIX:

Upgrade to plotlyjs 1.7.0. Fixes #513

3.4.8 -- 23 Mar 2016

BUGFIX:

* Safeguard against null fields in selections. See #530.

3.4.7 -- 19 Mar 2016

BUGFIX:

* Added custom CSS which allows plotly to work nicely in ioslides.

3.4.6 -- 17 Mar 2016

NEW FEATURES:

The 'plotly_relayout' event is now accessible via the event_data() function.

Fixed #514.

3.4.5 -- 17 Mar 2016

BUGFIX:

Fixed #514.

3.4.4 -- 17 Mar 2016

BUGFIX:

Show discrete positional values in tooltip (see #515); better GeomTile conversion; pass plot object into layers2traces.

3.4.3 -- 14 Mar 2016

BUGFIX:

Custom facet labeller functions will now translate correctly. See #507.

3.4.2 -- 14 Mar 2016

BUGFIX:

Automatic resizing will now occur only when layout.autosize is true (the default). See #403.

3.4.1 -- 13 Mar 2016

BUGFIX:

Legend titles are now supported.

3.4.0 -- 12 Mar 2016

NEW FEATURES:

* geom_map() and geom_hex() are now supported.

CHANGES:

* The default value of the fileopt argument was changed from "new" to "overwrite".

BUGFIX:

* Made a number of bugfixes/improvements to hoverinfo & conversion of geom_tile()/geom_point().

3.3.1 -- 10 Mar 2016

CHANGES:

* Changed the mapping argument name to tooltip (which seems like a better name).

BUGFIX:

* Redundant legend entries are no longer shown.

3.2.1 -- 10 Mar 2016

BUGFIX:

* Proper formatting for date tooltips.

3.2.0 -- 10 Mar 2016

CHANGES:

* Legend titles no longer appear in legend entries.
* Tooltips now reflect aesthetic mappings. This makes it easier to decode 
data values from a given visual marking.

NEW FEATURES:

* geom_violin() is now supported.
* ggplotly() gains a mapping argument to control the set of aesthetics to appears in the tooltip as well as their order.

3.1.0 -- 8 Mar 2016

CHANGES:

* The "hidden" sharing option in plotly_POST() was renamed to "secret".
* The default value in the scale argument in plotly_IMAGE() is now 1.

3.0.0 -- 8 Mar 2016

NEW FEATURES:

* ggplotly() is now about 20x faster (it avoids calling ggplot_build() 20+ times). In some cases, it might be even faster since a lot of other redundant computation is avoided.

CHANGES:

* Instead of (trying to) translate both major and minor grid lines, we now translate only major grid lines. This generally produces a result closer to the actual ggplot2 result since ggplot2 doesn't draw ticks on minor grid lines.

BUG FIXES:

* ggplotly() now supports most of scale_*()/theme()/guides(). As a result, this fixes a lot of issues (#482, #481, #479, #476, #473, #460, #456, #454, #453, #447, #443, #434, #422, #421, #399, #379, #378, #357, #318, #316, #242, #232, #211, #203, #185, #184, #161). In order to support all of scale_x_*() an scale_y_*(), we always use linear axis types, and supply ticktext/tickvals to plotly.js. This has some unfortunate consequences on hoverformatting, which may be addressed in future releases of plotly.js -- https://github.com/plotly/plotly.js/issues/320

2.5.0 -- 1 Mar 2016

NEW FEATURES

* New event_data() function provides easy access to plotly events in shiny.
For an example, see https://github.com/ropensci/plotly/tree/master/inst/examples/plotlyEvents

* plot_ly() and ggplotly() gain a source argument to differentiate between 
plotly events in shiny apps with multiple plots. ggplotly() also gains width 
and height arguments.

CHANGES

The arguments filename, fileopt, world_readable in ggplotly() were removed as
they should be provided to plotly_POST() instead. 

2.4.4 -- 13 Feb 2016

as.widget() now returns htmlwidget objects untouched. See #449.

2.4.3 -- 11 Feb 2016

Ensure that we always return HTTPS links. Fixes #455

2.4.2 -- 9 Feb 2016

Fix for on-premise domain configuration. 

2.4.1 -- 2 Feb 2016

Attach base_url in as.widget() so it works in multiple contexts

2.4.0 -- 1 Feb 2016

* Pass plot configuration using ... to avoid conflicts in defaults/documentation
* Upgrade to plotly.js 1.5.1

2.3.4 -- 1 Feb 2016

Added a plotly_api_domain environment variable for configuring the API domain. Fixes #441

2.3.3 -- 27 Jan 2016

Bump axis number for each trace matching a panel number. fixes #318

2.3.2 -- 25 Jan 2016

More accurate list of data_array properties. Fixes #415

2.3.1 -- 25 Jan 2016

More accurate conversion of path width. Fixes #373.

2.3.0 -- 19 Jan 2016

Add sharing argument and deprecate world_readable. Fixes #332

2.2.4 -- 18 Jan 2016

Fix for error in embed_notebook(). See #409.

2.2.3 -- 18 Jan 2016

Fix for geom_vline(). See #402.

2.2.2 -- 18 Jan 2016

Fix bar orientation when we detect geom_bar() + coord_flip() in ggplotly(). Fixes #390.

2.2.1 -- 18 Jan 2016

Search for axis title in scene object. fixes #393.

2.2.0 -- 13 Jan 2016

The default for layout.hovermode is now 'closest' for non-line scatter traces

2.1.3 -- 12 Jan 2016

Fix size and alpha translation for geom_point. Fixes #386

2.1.2 -- 11 Jan 2016

Upgraded to plotlyjs 1.4.1. For a list of changes, see https://github.com/plotly/plotly.js/releases/tag/v1.4.1

2.1.1 -- 11 Jan 2016

Upgraded to plotlyjs 1.4. For a list of changes, see https://github.com/plotly/plotly.js/releases/tag/v1.4.0

2.1.0 -- 29 Dec 2015

plot_ly() now defaults to inherit=FALSE and plotly_build() is now idempotent. Fixes #280 and #277. See #368 for details.

2.0.19 -- 23 Dec 2015

Added as.widget() function for conveniency in converting plotly object to htmlwidget objects. See #294.

2.0.18 -- 22 Dec 2015

Fix #365

2.0.17 -- 22 Dec 2015

Fix #358

2.0.16 -- 18 Dec 2015

Require ggplot2 2.0.0 or higher. For details, see #269.

2.0.15 -- 13 Dec 2015

Fix #346

2.0.14 -- 13 Dec 2015

Fix #212

2.0.13 -- 12 Dec 2015

Fix #286

2.0.12 -- 11 Dec 2015

Fix #221

2.0.11 -- 11 Dec 2015

Fix #250

2.0.10 -- 10 Dec 2015

Fix #225

2.0.9 -- 10 Dec 2015

Fix #333

2.0.8 -- 10 Dec 2015

Fix a bug with geom_segment (see #321 & #228) 

2.0.7 -- 10 Dec 2015

Fix #233

2.0.6 -- 2 Dec 2015

Upgrade to plotlyjs 1.1.1. Fixes #319.

2.0.5 -- 1 Dec 2015

Fix for legend names. See #236.

2.0.4 -- 28 Nov 2015

Fix #313.

2.0.3 -- 18 Nov 2015

Fixed bug causing knitr options to be ignored. Also added VignetteBuilder to DESCRIPTION to vignette is available.

2.0.2 -- 17 Nov 2015

Using plotly_build() on a ggplot object should always return a plotly object

2.0.1 -- 17 Nov 2015

Better printing of server figures. Documentation and other fixes for initial CRAN release!

2.0.0 -- 2 Nov 2015

Added a dependency on htmlwidgets and 'offline' plots are now the default. If you want to create a figure on a plotly server, you need to use `plotly_POST()`. Also added a `config()` function to control the default appearance of the interactive plot

1.0.10 -- 3 Nov 2015

Fixed #292.

1.0.9 -- 28 Sep 2015

Fixed filename, fileopt arguments in plot_ly. Specifying the same filename will now overwrite the plot if it exists.

1.0.8 -- 14 Sep 2015

Added the plotly_IMAGES() function which interfaces to the images endpoint https://api.plot.ly/v2/#images

Details -> https://github.com/ropensci/plotly/pull/279

1.0.7 -- 26 Aug 2015

See https://github.com/ropensci/plotly/pull/275

1.0.6 -- 25 Aug 2015

Fix a bug with subplot domain calculations (see https://github.com/ropensci/plotly/pull/274)

1.0.5 -- 20 Aug 2015

Fix issue converting plotly offline markdown documents to HTML when using `markdown::markdownToHTML`

1.0.4 -- 14 Aug 2015

Bug fix for subplot. See #265

1.0.3 -- 7 Aug 2015

Improved legend positioning. See #241

1.0.2 -- 2 Aug 2015

* last_plot() will now look for the last plotly object; if not found, it will try to find the last ggplot object.
* Officially added the filename, fileopt, and world_readable arguments to plot_ly() and ggplotly().
* If plotly offline is not available, the shiny.launch.browser option is changed to open a web brower. See #245.
* Various namespace/documentation improvements for R CMD check.

1.0.1 -- 2 Aug 2015

Removed the stream() function as it wasn't ready to be included.

1.0.0 -- 31 July 2015

A major reworking of package internals which includes a few backwards incompatible changes.

Major changes include:

(1) New high-level grammar for expressing Plotly graphs from R (see the `plot_ly()`, `add_trace()`, `layout()`, and `style()` functions).
(2) New print methods which make it easier to create, modify, and embed Plotly graphs.
(3) Added a `subplot()` function for putting several graphs on a single page.
(4) Added the `renderPlotly()` and `plotlyOutput()` functions for embedding plotly graphs in shiny applications.
(5) Added `offline()` function for creating standalone HTML pages via Plotly Offline (see http://purchasing.plot.ly/)

For more details, see the new vignettes with `browseVignettes(package = "plotly")` and/or the pull request -> https://github.com/ropensci/plotly/pull/226

0.6.3 -- 2 June 2015

Add new tests inspired by the R Cookbook distributions #214

0.6.2 -- 19 May 2015

In geom_bar(stat = "identity"), sum y values if multiple for a given x.

0.6.1 -- 5 May 2015

Add test-cookbook-lines.R and fix bugs that showed up in those tests.

0.6 -- 4 May 2015

Let gg2list() return a figure object (backwards incompatible change).

0.5.29 -- 16 April 2015

geom_density() as filled area chart #202

0.5.28 -- 15 April 2015

Let ggplot handle histogram binning. Fix #198

0.5.27 -- 19 Mar 2015

Reimplement geom_ribbon as a basic polygon. Fix #191. Fix #192.

0.5.26 -- 18 Mar 2015

Implemented geom_rect #178

0.5.25 -- 10 March 2015

Implemented geom_smooth() #183

0.5.24 -- 10 March 2015

Implemented facet_wrap(scales="free") #167

0.5.23 -- 10 March 2015.

geom_ribbon() now respects alpha transparency

0.5.22 -- 2 March 2015.

Fixes for ylim() #171.

0.5.21 -- 23 February 2015.

Fixes for error bars and tick marks.

0.5.20 -- 9 February 2015.

Add alpha transparency to fill conversion.
Let geom_area support colour and fill aesthetics.

0.5.19 -- 23 January 2015.

Support class conversion such as as.Date() within ggplot code.

0.5.18 -- 22 January 2015.

Return proper filepath when filename contains directories.

0.5.17 -- 30 December 2014.

Support date-time binning in histograms.

0.5.16 -- 29 December 2014.

Support colour aesthetic in geom_text().

0.5.15 -- 19 December 2014.

Use proper RCurlOptions in get_figure() method.

0.5.14 -- 1 December 2014.

Make layers geom_line + geom_point only one trace in Plotly.

0.5.13 -- 27 November 2014.

Rename translation file and server endpoint parameter to be hip.

0.5.12 -- 12 November 2014.

Improve legend title position.

0.5.11 -- 11 November 2014.

Show legend title.

0.5.10 -- 7 November 2014.

Improve showlegend and fix legend’s `x` position.

0.5.9 -- 3 November 2014.

Default colours for geom_polygon().

0.5.8 -- 30 October 2014.

Support hline over a factor x range.
Default colours for geom_boxplot().

0.5.7 -- 29 October 2014.

Default colours for geom_area() and geom_ribbon().

0.5.6 -- 28 October 2014.

Convert line size faithfully.

0.5.5 -- 24 October 2014.

Support category histograms (with factors).

0.5.4 -- 22 October 2014.

Support conversion of geom_vline().

0.5.3 -- 21 October 2014.

Support conversion of geom_bar() with position_dodge().

0.5.2 -- 18 October 2014.

Support aesthetic shape in geom_path() and, hence, geom_line() (conversion).

0.5.1 -- 15 October 2014.

Do not show zero lines by default (as in ggplot2 plots).

0.5.0 -- 15 October 2014.

From now on, version numbers are meaningful again...
Many changes meanwhile, especially support for more geoms.

0.4 -- 7 April 2014.

Re-write geom to trace conversion code.

0.3.8 -- 21 March 2014.

ggplotly takes the last_plot() by default.

Support for ggplotly layout elements title, tickcolor, gridcolor,
showlegend, plot_bgcolor, paper_bgcolor, tickangle, axis titles, plot
border colors.

0.3.7 -- 14 March 2014.

For ggplotly:

- if on the command line, open a web browser (as before).

- if in knitr/Rmd in a chunk with plotly=TRUE, embed the plot.

0.3.6 -- 10 March 2014.

Merge ggplotly code.

0.3.5
