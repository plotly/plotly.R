library("testthat")
library("plotly")
# crendentials for the test bot
Sys.setenv(`plotly-username` = "TestBot")
Sys.setenv(`plotly-apikey` = "r1neazxo9w")
# find the hash of the currently installed plotly package
pkg_info <- devtools::session_info()$packages
src <- subset(pkg_info, package == "plotly")$source
hash <- sub("\\)", "", strsplit(src, "@")[[1]][2])
# placement of outputs depend on this hash
Sys.setenv(`plotly-hash` = hash)
source(system.file("testscripts/save_outputs.R", package = "plotly"))
test_check("plotly")
