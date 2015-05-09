library("testthat")
library("plotly")
library("devtools")
# crendentials for the test bot
Sys.setenv(`plotly-username` = "TestBot")
Sys.setenv(`plotly-apikey` = "r1neazxo9w")
source(system.file("testscripts/save_outputs.R", package = "plotly"))
# avoid weird errors if this function is called via testhat::check()
# https://github.com/hadley/testthat/issues/144
Sys.setenv("R_TESTS" = "")
test_check("plotly")
