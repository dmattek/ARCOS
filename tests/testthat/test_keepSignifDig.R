library(testthat)
library(data.table)
library(ARCOS)

testthat::test_that("clipping works", {

  dtIn = data.table(id = LETTERS[1:6],
                    x = c(1.23456, 2.3456, 3.456, 4.56, 5.6, 6))

  dtCalc = ARCOS::keepSignifDig(dtIn, 2)
  dtTrue = data.table(id = LETTERS[1:6],
                      x = c(1.2, 2.3, 3.5, 4.6, 5.6, 6.0))

  dtCalc[,
         xChar := as.character(x)]

  dtTrue[,
         xChar := as.character(x)]


  expect_equal(dtCalc, dtTrue)
})
