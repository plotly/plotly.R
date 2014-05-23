context("get_figure")

dir.create("tests/testthat/test_savefigure_images")

py <- plotly("PlotlyImageTest", "786r5mecv0")
figure = py$get_figure("PlotlyImageTest", 0);

images = c(0, 1, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 27, 28, 30, 31, 32, 34, 35, 37, 38, 41, 51, 52, 29, 33, 40, 42, 43, 44, 45, 50, 53, 55, 56)

for(i in images){
    figure = py$get_figure("PlotlyImageTest", i);
    py$save_image(figure, paste("tests/testthat/test_savefigure_images/PlotlyImageTest_", i, sep=""))
}