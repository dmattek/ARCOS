library(testthat)
library(ARCOS)
library(data.table)


# empty table; no events
testthat::test_that("empty", {

  locDTin <- data.table(
    id = rep(1:10, 10),
    time = rep(1:10, each = 10),
    m = rep(0, 10 * 10),
    x = rep(1:10, 10)
  )

  expect_warning(
    trackCollEvents(locDTin[m > 0],
                    eps = 1.01,
                    minClSz = 1L,
                    cols = list(
                      frame = "time",
                      id = "trackID",
                      collid = "clTrackID"
                    ),
                    posCols = c("x"),
                    deb = F),
    "*NULL"
  )
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central2prev_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 2L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 3D", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central3D_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central3D_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("1 central growing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1centralGrowing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1centralGrowing_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 central growing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2centralGrowing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2centralGrowing_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 with 1 common symmetric", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonSym_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonSym_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 with 1 common asymmetric", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonAsym_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonAsym_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 merging", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2clustersMerging_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2clustersMerging_out.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.5,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "collid"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, collid)],
               locDTtrueRes)
})

testthat::test_that("2 crossing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2clustersCrossing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2clustersCrossing_out.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.5,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "collid"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, collid)],
               locDTtrueRes)
})

testthat::test_that("3 spreading 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("3 spreading 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading2prev_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 2L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 clusters, 1 delayed", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2clusters1delayed_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2clusters1delayed_out.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.5,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "collid"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, collid)],
               locDTtrueRes)
})

# 5 events overlapping in time
testthat::test_that("5 overlapping 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("5 overlapping 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping2prev_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 2L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 6 events overlapping in time
testthat::test_that("6 overlapping", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "6overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "6overlapping_res.csv"))
  locDTcalcRes <- trackCollEvents(locDTin[m > 0],
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "time",
                                    id = "trackID",
                                    clid = "clTrackID"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})


testthat::test_that("split from single", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1objSplit_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1objSplit_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("split from 2 objects", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objSplit_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objSplit_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("cross 2 objects", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCross_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCross_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("cross 2 objects with common", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCrossCommon_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCrossCommon_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("merge & split 2 objects with common", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCommon_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCommon_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("merge & split 2 objects crossing", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCross_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCross_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("merge & split 2 objects near", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitNear_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitNear_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "t",
                                    id = "id",
                                    clid = "collid"
                                  ),
                                  posCols = c("pos"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})

testthat::test_that("4 objects in 2 events", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "4obj2events_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "4obj2events_res.csv"))

  locDTcalcRes <- trackCollEvents(locDTin,
                                  eps = 1.0,
                                  minClSz = 1L,
                                  nPrev = 1L,
                                  cols = list(
                                    frame = "frame",
                                    id = "id",
                                    clid = "collId"
                                  ),
                                  posCols = c("x"),
                                  deb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(frame, id, collId)],
               locDTtrueRes)
})

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

  locCollCalc <- ARCOS::trackColl(locTS[meas > 0],
                              eps = 1.5,
                              minClSz = 3L)

  locCollTrue <- fread(input = file.path(system.file('synthetic2D', package = 'ARCOS'), 'Gradient20_coll.csv.gz'))

  expect_equal(locCollCalc,
               locCollTrue,
               ignore_attr = TRUE)

})
