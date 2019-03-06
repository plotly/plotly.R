context("ggplot sec_axis")

test_that("Yaxis2 in layout", {
  p = ggplotly(ggplot(mtcars,aes(x=drat,y=hp))+geom_point()+
    geom_point(aes(y=qsec,color='red',axis='sec'))+
    scale_y_continuous(sec.axis = sec_axis(~./6,'2nd Axis')))

  expect_identical(TRUE,'yaxis2' %in% names(p$x$layout))
})

test_that("Yaxis2 labelled correctly", {
  p = ggplotly(ggplot(mtcars,aes(x=drat,y=hp))+geom_point()+
    geom_point(aes(y=qsec,color='red',axis='sec'))+
    scale_y_continuous(sec.axis = sec_axis(~./6,'2nd Axis')))
    
  expect_identical('2nd Axis',p$x$layout$yaxis2$title)
})

test_that("Second trace attributed to y2", {
  p = ggplotly(ggplot(mtcars,aes(x=drat,y=hp))+geom_point()+
                 geom_point(aes(y=qsec,color='red',axis='sec'))+
                 scale_y_continuous(sec.axis = sec_axis(~./6,'2nd Axis')))
  
  expect_identical('y2',p$x$data[[2]]$yaxis)
})
