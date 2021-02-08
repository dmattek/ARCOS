library(testthat)
library(data.table)
library(ARCOS)

testthat::test_that("interpolate middle", {

  dtIn = data.table(t = c(1,2,3,5,6,7),
                    y = c(1,2,3,5,6,7),
                    id = rep(1, 6))

  dtCalc = interpolateTS(inDT = dtIn,
                         inColID = "id",
                         inColFN = "t",
                         inColY = "y",
                         inFNfreq = 1)

  dtTrue = structure(list(t = 1:7,
                          y = 1:7,
                          id = rep(1, 7)),
                     sorted = c("id", "t"),
                     class = c("data.table", "data.frame"), row.names = c(NA, -7L))

  expect_equal(dtCalc, dtTrue)
})
