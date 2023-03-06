library(testthat)
library(data.table)
library(ARCOS)

testthat::test_that("block shuffle vector: run 0 single 1", {

  set.seed(7)
  vIn = c(rep(0, 10), 1)
  vCalc = shuffBlockVecAlt(vIn)
  vTrue = c(rep(0, 10), 1)

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: run 1 single 0", {

  set.seed(7)
  vIn = c(rep(1, 10), 0)
  vCalc = shuffBlockVecAlt(vIn)
  vTrue = c(rep(1, 10), 0)

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: 2 edge runs of 1", {

  set.seed(7)
  vIn = c(rep(1, 3), rep(0, 5), rep(1, 2))
  vCalc = shuffBlockVecAlt(vIn)
  vTrue = c(rep(1, 2), rep(0, 5), rep(1, 3))

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: random blocks", {

  set.seed(7)
  vIn = c(0,1,0,0,1,1,0,0,0,1,1,1)
  vCalc = shuffBlockVecAlt(vIn)
  vTrue = c(0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1)

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: periodic", {

  set.seed(7)
  vIn = rep(c(0,0,1,1),5)
  vCalc = shuffBlockVecAlt(vIn)
  vTrue = vIn

  expect_equal(vCalc, vTrue)
})
