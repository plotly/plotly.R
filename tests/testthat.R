library(testthat)
library(plotly)
# crendentials for the test bot
Sys.setenv(`plotly-username` = "TestBot")
Sys.setenv(`plotly-apikey` = "r1neazxo9w")
source(system.file("testscripts/save_outputs.R", package = "plotly"))
test_check("plotly")
