library(testthat)
library(ARCOS)

testthat::test_that("clipping works", {

  vCalc = ARCOS:::rcpp_clip(1:10, 3, 7)
  vTrue = c(3,3,3,4,5,6,7,7,7,7)

  expect_equal(vCalc, vTrue)
})
