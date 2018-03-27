library(sf)

f <- tempfile(fileext = ".zip")
download.file("https://www.dot.state.mn.us/maps/gdma/data/datafiles/statewide/indian_res.zip", f)
unzip(f, exdir = dirname(f))
res <- st_read(file.path(dirname(f), "reservtn.shp"))

# assuming this based on epsg provided from other shapefiles on 
# https://www.dot.state.mn.us/maps/gdma/gis-data.html
st_crs(res) <- 26915

res_mn <- st_transform(res, 4326)

# plot_mapbox(res_mn, text = ~INDRESNAME, hoverinfo = "text")
devtools::use_data(res_mn, overwrite = TRUE)


# f <- tempfile(fileext = ".zip")
# download.file("https://www.dot.state.mn.us/maps/gdma/data/datafiles/statewide/county.zip", f)
# unzip(f, exdir = dirname(f))
# mn <- st_read(file.path(dirname(f), "MNCounties_MNDOT.shp"))
# 
# mn <- mn %>%
#   st_simplify(TRUE, 2000) %>%
#   st_transform(4326) 
# 
# # plot_mapbox(mn)
# 
# devtools::use_data(mn, overwrite = TRUE)
