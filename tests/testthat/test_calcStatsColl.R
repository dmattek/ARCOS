library(testthat)
library(ARCOS)
library(data.table)


# empty table; no events
testthat::test_that("synthetic2D_Gradient20", {

  locTS <- ARCOS::loadDataFromFile(fname = file.path(system.file('synthetic2D', package = 'ARCOS'), 'Gradient20.csv.gz'),
                                     colFrame = 'frame',
                                     colIDobj = 'id',
                                     colPos = c('x', 'y'),
                                     colMeas = 'meas')

  ARCOS::binMeas(locTS,
                 biasMet = 'none',
                 smoothK = 1,
                 binThr = 0.)

  locColl <- ARCOS::trackColl(locTS[meas > 0],
                              eps = 1.5,
                              minClSz = 3L)

  locCollStatsCalc <- ARCOS::calcStatsColl(locColl)

  locCollStatsTrue <- fread(input = file.path(system.file('synthetic2D', package = 'ARCOS'), 'Gradient20_collStats.csv.gz'))

  expect_equal(locCollStatsCalc,
               locCollStatsTrue,
               ignore_attr = TRUE)

})
