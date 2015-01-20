context("Filename")

test_that("Filepath with directories is returned as passed", {
  x <- c(-1.50548425849621, 0.023267831354017, -1.38460390550496,
         -0.805552814226363, 1.59651736643461, 0.936302685370894,
         0.512729504994891, -0.24492573745161, -0.465348603632604,
         0.173523456651353, 0.389491211182137, -0.275308705542518,
         -0.132866228059449, -0.336255877656944, 0.916535489109209,
         -0.936870130264329, 0.363137478307925, -1.26433467241078,
         -0.388804188531171, 0.785842426281935)
  data = list(x=x,
              type="histogramx")
  l <- list(autosize=FALSE, width=600, height=400, showlegend=FALSE)
  
  py <- plotly("get_test_user_2", "0f9es4r6tm")
  response <- py$plotly(data, kwargs=list(layout=l, filename="directory/hist",
                                          fileopt="overwrite"))
  
  expect_identical(response$filename, "directory/hist")
  
})
