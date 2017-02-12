library(testthat)


test_that("Test Month1", {
  setwd("C:/repo/week4/data")
  yrMoCount <-  fars_summarize_years(c(2013,2014,2015))
  yrMoCount
  class(yrMoCount)
  v <- head(yrMoCount,1)
  expected <-  data.frame(  MONTH=as.integer(1),  
                            `2013`=as.integer(2230),  
                            `2014`=as.integer(2168),  
                            `2015`=as.integer(2368)  
                )
  class(v)
  class(expected)
  print ('testing Test Month1')
  expect_that( v[1,][1], equals(expected[1,][1]))
  expect_that( as.integer(v[1,][2]), equals(as.integer(expected[1,][2])))
  expect_that( as.integer(v[1,][3]), equals(as.integer(expected[1,][3])))
  expect_that( as.integer(v[1,][4]), equals(as.integer(expected[1,][4])))
  expect_that( as.integer(v[1,][2]), equals(as.integer(expected[1,][2])))
} )

