# This script grabs argument names from plotly documentation and provides some
# convenience functions for translating all those arguments to the R package

library(rvest)
ref <- read_html("https://plot.ly/javascript-graphing-library/reference")
# complete set of args
argz <- ref %>% html_nodes(".gamma .link--impt") %>% html_text() %>%
  gsub("\\n", "", .) %>% stringr::str_trim()


# function arguments
fun_args <- function(x) paste(x, "= NULL", collapse = ", ") 
# argument documentation (put inside @param)
doc_args <- function(x) paste(x, collapse = ", ") 
# put inside function body
in_args <- function(x) paste(x, "=", x, collapse = ", ") 

# scatter args
fun_args(argz[1:24])
doc_args(argz[1:24])
in_args(argz[1:24])

# bar args
fun_args(argz[26:43])
doc_args(argz[26:43])
in_args(argz[26:43])

# histogram args
fun_args(argz[45:68])
doc_args(argz[45:68])
in_args(argz[45:68])

# box args
fun_args(argz[70:89])
doc_args(argz[70:89])
in_args(argz[70:89])

# heatmap args
fun_args(argz[91:116])
doc_args(argz[91:116])
in_args(argz[91:116])

# contour args
fun_args(argz[118:145])
doc_args(argz[118:145])
in_args(argz[118:145])

# histogram2d args
fun_args(argz[147:173])
doc_args(argz[147:173])
in_args(argz[147:173])

# histogram2dcontour args
fun_args(argz[175:204])
doc_args(argz[175:204])
in_args(argz[175:204])

# area args
fun_args(argz[206:214])
doc_args(argz[206:214])
in_args(argz[206:214])

# scatter3d args
fun_args(argz[216:230])
doc_args(argz[216:230])
in_args(argz[216:230])

# surface args
fun_args(argz[232:240])
doc_args(argz[232:240])
in_args(argz[232:240])

# ----------------------------------------------------------------------
# Trace auxiliary objects (in R/aux.R)
# ----------------------------------------------------------------------

# error_y args
fun_args(argz[241:251])
doc_args(argz[241:251])
in_args(argz[241:251])

# error_x args
fun_args(argz[252:263])
doc_args(argz[252:263])
in_args(argz[252:263])

# xbins args
fun_args(argz[264:266])
doc_args(argz[264:266])
in_args(argz[264:266])

# ybins args
fun_args(argz[267:269])
doc_args(argz[267:269])
in_args(argz[267:269])

# marker args
fun_args(argz[270:282])
doc_args(argz[270:282])
in_args(argz[270:282])

# line args
fun_args(argz[283:290])
doc_args(argz[283:290])
in_args(argz[283:290])

# contour args
fun_args(argz[291:295])
doc_args(argz[291:295])
in_args(argz[291:295])

# colorbar args
fun_args(argz[296:326])
doc_args(argz[296:326])
in_args(argz[296:326])

# stream args
fun_args(argz[327:328])
doc_args(argz[327:328])
in_args(argz[327:328])

# error_z args
fun_args(argz[329:339])
doc_args(argz[329:339])
in_args(argz[329:339])

# ----------------------------------------------------------------------
# Axis objects (in R/axis.R)
# ----------------------------------------------------------------------

# xaxis/yaxis/zaxis args are identical
x <- fun_args(argz[340:378])
y <- fun_args(argz[379:417])
z <- fun_args(argz[444:482])
identical(x, y) && identical(y, z)

x
doc_args(argz[340:378])
in_args(argz[340:378])


# radialaxis/angularaxis args
fun_args(argz[418:437])
doc_args(argz[418:437])
in_args(argz[418:437])

# scene args
fun_args(argz[438:443])
doc_args(argz[438:443])
in_args(argz[438:443])


# ----------------------------------------------------------------------
# Layout objects (in R/layout.R)
# ----------------------------------------------------------------------

# layout args
fun_args(argz[483:512])
doc_args(argz[483:512])
in_args(argz[483:512])

# font args
fun_args(argz[513:516])
doc_args(argz[513:516])
in_args(argz[513:516])

# legend args
fun_args(argz[517:527])
doc_args(argz[517:527])
in_args(argz[517:527])

# annotation args
fun_args(argz[528:549])
doc_args(argz[528:549])
in_args(argz[528:549])

# margin args
fun_args(argz[550:557])
doc_args(argz[550:557])
in_args(argz[550:557])
