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
                                  inEps = 1.01,
                                  inMinPts = 1L,
                                  inNprev = 1L,
                                  inCols = list(
                                    frame = "time",
                                    x = "x",
                                    y = NULL,
                                    z = NULL,
                                    id = "trackID",
                                    collid = "clTrackID"
                                  ),
                                  inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central2prev_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 2L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

# 3 events at position 3 that last 1, 2, 3, respectively, with 1 frame separation
testthat::test_that("1 central 3D", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central3D_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1central3D_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("1 central growing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1centralGrowing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1centralGrowing_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("2 central growing", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2centralGrowing_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2centralGrowing_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("2 with 1 common symmetric", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonSym_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonSym_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("2 with 1 common asymmetric", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonAsym_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2with1commonAsym_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("3 spreading 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("3 spreading 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "3spreading2prev_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 2L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

# 5 events overlapping in time
testthat::test_that("5 overlapping 1 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("5 overlapping 2 prev", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "5overlapping2prev_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 2L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

# 6 events overlapping in time
testthat::test_that("6 overlapping", {

  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "6overlapping_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "6overlapping_res.csv"))
  locDTcalcRes <- ARCOS::trackCollEvents(locDTin[m > 0],
                                         inEps = 1.01,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(
                                           frame = "time",
                                           x = "x",
                                           y = NULL,
                                           z = NULL,
                                           id = "trackID",
                                           collid = "clTrackID"
                                         ),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("split from single", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1objSplit_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "1objSplit_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                              inEps = 1.1,
                              inMinPts = 1L,
                              inNprev = 1L,
                              inCols = list(frame = "t",
                                            x = "pos",
                                            id = "id",
                                            collid = "collid"),
                              inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("split from 2 objects", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objSplit_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objSplit_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.1,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "t",
                                                       x = "pos",
                                                       id = "id",
                                                       collid = "collid"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("cross 2 objects", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCross_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCross_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.1,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "t",
                                                       x = "pos",
                                                       id = "id",
                                                       collid = "collid"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("cross 2 objects with common", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCrossCommon_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objCrossCommon_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.1,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "t",
                                                       x = "pos",
                                                       id = "id",
                                                       collid = "collid"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("merge & split 2 objects with common", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCommon_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCommon_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.1,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "t",
                                                       x = "pos",
                                                       id = "id",
                                                       collid = "collid"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("merge & split 2 objects crossing", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCross_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitCross_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.1,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "t",
                                                       x = "pos",
                                                       id = "id",
                                                       collid = "collid"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("merge & split 2 objects near", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitNear_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "2objMergeSplitNear_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 1.1,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "t",
                                                       x = "pos",
                                                       id = "id",
                                                       collid = "collid"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("4 objects in 2 events", {
  locDTin <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "4obj2events_in.csv"))
  locDTtrueRes <- data.table::fread(file = file.path(system.file("testdata", package="ARCOS"), "4obj2events_res.csv"))

  locDTcalcRes <- ARCOS::trackCollEvents(locDTin,
                                         inEps = 0.6,
                                         inMinPts = 1L,
                                         inNprev = 1L,
                                         inCols = list(frame = "frame",
                                                       x = "x",
                                                       id = "id",
                                                       collid = "collId"),
                                         inDeb = F)

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("collid 1 obj", {

  locDTin <- data.table(
    time = 0,
    id = 0,
    x = 0
  )

  locDTtrueRes <- data.table(
    time = 0,
    id = 0,
    x = 0,
    collid = 1
  )

  locDTcalcRes <- ARCOS::createCollEvents(locDTin,
                                   inCols = list(
                                     x = "x",
                                     y = NULL,
                                     z = NULL,
                                     frame = "time",
                                     id = "id",
                                     collid = "collid"
                                   ),
                                   inEps = 1.01, inMinPts = 1,
                                   inClOffset = 0,
                                   inDeb = F
  )

  expect_equal(locDTcalcRes, locDTtrueRes)
})


testthat::test_that("collid 2 obj in neigh", {

  locDTin <- data.table(
    time = c(0, 0),
    id = c(0, 1),
    x = c(0, 1)
  )

  locDTtrueRes <- data.table(
    time = c(0, 0),
    id = c(0, 1),
    x = c(0, 1),
    collid = c(1, 1)
  )

  locDTcalcRes <- ARCOS::createCollEvents(locDTin,
                                   inCols = list(
                                     x = "x",
                                     y = NULL,
                                     z = NULL,
                                     frame = "time",
                                     id = "id",
                                     collid = "collid"
                                   ),
                                   inEps = 1.01, inMinPts = 1,
                                   inClOffset = 0,
                                   inDeb = F
  )

  expect_equal(locDTcalcRes, locDTtrueRes)
})

testthat::test_that("collid 3+2 obj in neigh", {

  locDTin <- data.table(
    time = rep(0, 5),
    id = 1:5,
    x = c(1:3, 5:6)
  )

  locDTtrueRes <- data.table(
    time = rep(0, 5),
    id = 1:5,
    x = c(1:3, 5:6),
    collid = c(rep(1, 3), rep(2, 2))
  )

  locDTcalcRes <- ARCOS::createCollEvents(locDTin,
                                   inCols = list(
                                     x = "x",
                                     y = NULL,
                                     z = NULL,
                                     frame = "time",
                                     id = "id",
                                     collid = "collid"
                                   ),
                                   inEps = 1.01, inMinPts = 1,
                                   inClOffset = 0,
                                   inDeb = F
  )

  expect_equal(locDTcalcRes, locDTtrueRes)
})

