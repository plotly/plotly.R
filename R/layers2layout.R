# convert layout-specific geoms/layers 
layers2layout <- function(gglayout, layers, layout) {
  geoms <- sapply(layers, function(x) class(x[["geom"]])[1])
  RasterGeom <- which(geoms %in% "GeomRasterAnn")
  for (i in RasterGeom) {
    params <- layers[[i]]$geom_params
    for (j in seq_len(nrow(layout))) {
      lay <- layout[j, ]
      
      img <- list(
        source = raster2uri(params$raster),
        # TODO: ask plotly.js to implement explicit placement between traces?
        layer = if (RasterGeom / length(geoms) > 0.5) "above" else "below",
        xref = sub("axis", "", lay[["xaxis"]]), 
        yref = sub("axis", "", lay[["yaxis"]]), 
        x = params$xmin, 
        xanchor = "left",
        sizex = with(params, abs(xmax - xmin)),
        y = params$ymin, 
        yanchor = "bottom",
        sizey = with(params, abs(ymax - ymin)),
        sizing = "stretch"
      )
      gglayout$images <- c(gglayout$images, list(img))
    }
  }
  # TODO: maybe we could support a subset of grobs in GeomCustomAnn?
  gglayout
}
