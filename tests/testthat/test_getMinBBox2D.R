library(testthat)
library(data.table)
library(ARCOS)

testthat::test_that("bounding box: rectangle vertical", {

  mIn = cbind(c(0,2,2,0,1),
              c(0,0,3,3,1))

  lCalc = getMinBBox2D(mIn)

  lTrue = list(w = 2, h = 3)

  expect_equal(lCalc, lTrue)
})


testthat::test_that("bounding box: rectangle horizontal", {

  mIn = cbind(c(0,3,3,0,1),
              c(0,0,2,2,1))

  lCalc = getMinBBox2D(mIn)

  lTrue = list(w = 3, h = 2)

  expect_equal(lCalc, lTrue)
})

testthat::test_that("bounding box: square", {

  mIn = cbind(c(0,2,2,0,1),
              c(0,0,2,2,1))

  lCalc = getMinBBox2D(mIn)

  lTrue = list(w = 2, h = 2)

  expect_equal(lCalc, lTrue)
})
