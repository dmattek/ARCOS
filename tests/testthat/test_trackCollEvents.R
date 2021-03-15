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
    ARCOS::trackCollEvents(locDTin[m > 0],
                           inEps = 1.01,
                           inMinPts = 1L,
                           inCols = list(
                             frame = "time",
                             x = "x",
                             y = NULL,
                             z = NULL,
                             id = "trackID",
                             collid = "clTrackID"
                           ),
                           inDeb = F),
    "*NULL"
  )
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central2prev_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 2L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 3D", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central3D_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central3D_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("1 central growing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1centralGrowing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1centralGrowing_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 central growing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2centralGrowing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2centralGrowing_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 with 1 common symmetric", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonSym_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonSym_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("2 with 1 common asymmetric", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonAsym_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonAsym_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("3 spreading 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("3 spreading 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading2prev_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 2L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 5 events overlapping in time
testthat::test_that("5 overlapping 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

testthat::test_that("5 overlapping 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping2prev_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 2L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})

# 6 events overlapping in time
testthat::test_that("6 overlapping", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "6overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "6overlapping_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           id = "trackID",
                                           clid = "clTrackID"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(time, trackID, clTrackID)],
               locDTtrueRes)
})


testthat::test_that("split from single", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1objSplit_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1objSplit_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("split from 2 objects", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objSplit_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objSplit_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("cross 2 objects", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCross_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCross_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("cross 2 objects with common", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCrossCommon_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCrossCommon_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("merge & split 2 objects with common", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCommon_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCommon_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("merge & split 2 objects crossing", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCross_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCross_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})


testthat::test_that("merge & split 2 objects near", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitNear_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitNear_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "t",
                                           id = "id",
                                           clid = "collid"
                                         ),
                                         inPosCols = c("pos"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(t, id, collid)],
               locDTtrueRes)
})

testthat::test_that("4 objects in 2 events", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "4obj2events_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "4obj2events_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.0,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "frame",
                                           id = "id",
                                           clid = "collId"
                                         ),
                                         inPosCols = c("x"),
                                         inDeb = F)

  attr(locDTcalcRes, "sorted") = NULL
  expect_equal(locDTcalcRes[,
                            .(frame, id, collId)],
               locDTtrueRes)
})
