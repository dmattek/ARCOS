library(testthat)
library(data.table)
library(ARCOS)

testthat::test_that("block shuffle vector: run 0 single 1", {

  vIn = c(rep(0, 10), 1)

  set.seed(7)
  vCalc = shuffBlockVec(vIn)

  vTrue = c(1, rep(0, 10))

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: run 1 single 0", {

  vIn = c(rep(1, 10), 0)

  set.seed(7)
  vCalc = shuffBlockVec(vIn)

  vTrue = c(0, rep(1, 10))

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: 2 edge runs of 1: keep split", {

  vIn = c(rep(1, 3), rep(0, 5), rep(1, 2))

  set.seed(13)
  vCalc = shuffBlockVec(vIn)

  vTrue = c(rep(1, 2), rep(0, 5), rep(1, 3))

  expect_equal(vCalc, vTrue)
})

testthat::test_that("block shuffle vector: 2 edge runs of 1: merge blocks", {

  vIn = c(rep(1, 3), rep(0, 5), rep(1, 2))

  set.seed(7)
  vCalc = shuffBlockVec(vIn)

  vTrue = c(rep(0, 5), rep(1, 2), rep(1, 3))

  expect_equal(vCalc, vTrue)
})


testthat::test_that("block shuffle vector: random blocks", {

  vIn = c(0,1,0,0,1,1,0,0,0,1,1,1)

  set.seed(7)
  vCalc = shuffBlockVec(vIn)

  vTrue = c(1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0)

  expect_equal(vCalc, vTrue)
})
