library(testthat)
library(data.table)
library(ARCOS)

testthat::test_that("shift numeric vector left", {

  vIn = 1:10
  vCalc = shifter(vIn, 2)
  vTrue = c(3:10, 1, 2)

  expect_equal(vCalc, vTrue)
})

testthat::test_that("shift numeric vector right", {

  vIn = 1:10
  vCalc = shifter(vIn, -2)
  vTrue = c(9, 10, 1:8)

  expect_equal(vCalc, vTrue)
})
