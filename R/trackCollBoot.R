#' Track collective events with bootstrapping randomisation
#'
#' Identifies collective events using one of five randomisation methods.
#'
#' @title "Track collective events with bootstrapping randomisation"
#' @param obj an arcosTS object.
#' @param nboot an integer, number of bootstrap iterations; default 100.
#' @param ncores an integer, number of parallel cores; default 2.
#' @param method either of the five bootstrapping methods: shuffCoord', 'randShiftMeas', 'shuffMeasTrack', 'shuffMeasFrame', 'shuffBlockTrack'.
#' @param eps a numeric, sets the search radius for spatial clustering with dbscan; default 1.
#' @param minClSz an integer, minimum cluster size for dbscan; default 1L.
#' @param nPrev an integer, number of previous frames to link; default 1L.
#' @param epsPrev a float with the search radius for linking clusters between frames, default NULL means that epsPrev = eps.
#' @param colldurlim a two-element vector with limits for filtering cluster duration; default c(1, Inf).
#' @param colltotszlim a two-element vector with limits for filtering cluster size; default c(1, Inf).
#' @param deb boolean, additional debug output; default FALSE.
#'
#' @return a data.table with filtered stats of collective events at every bootstrapping iteration
#'
#' @import data.table
#' @importFrom parallel mclapply
#'
#' @rdname trackCollBoot
#' @export trackCollBoot
#'
#' @examples
#' cat("No example\n")
trackCollBoot <- function(obj,
                          nboot = 100L,
                          ncores = 2L,
                          method = c('shuffCoord', 'randShiftMeas', 'shuffMeasTrack', 'shuffMeasFrame', 'shuffBlockTrack'),
                          eps = 1., minClSz = 1L, nPrev = 1L, epsPrev = NULL,
                          colldurlim = c(1, Inf), colltotszlim = c(1, Inf),
                          deb = FALSE) {
  UseMethod("trackCollBoot")
}

trackCollBoot.default <- function(obj,
                                  nboot = 100L,
                                  ncores = 2L,
                                  method = c('shuffCoord', 'randShiftMeas', 'shuffMeasTrack', 'shuffMeasFrame', 'shuffBlockTrack'),
                                  eps = 1., minClSz = 1L, nPrev = 1L, epsPrev = NULL,
                                  colldurlim = c(1, Inf), colltotszlim = c(1, Inf),
                                  deb = FALSE) {
  cat("This is a generic function\n")
}

#' @rdname trackCollBoot
#' @export trackCollBoot.arcosTS
#' @export
trackCollBoot.arcosTS <- function(obj,
                                  nboot = 100L,
                                  ncores = 2L,
                                  method = c('shuffCoord', 'randShiftMeas', 'shuffMeasTrack', 'shuffMeasFrame', 'shuffBlockTrack'),
                                  eps = 1., minClSz = 1L, nPrev = 1L, epsPrev = NULL,
                                  colldurlim = c(1, Inf), colltotszlim = c(1, Inf),
                                  deb = FALSE) {

  method = match.arg(method)

  stopifnot(is.arcosTS(obj))

  # extract the column with binarised measurement
  locColSource <- attr(obj, 'colMeasBin')
  stmp = paste0(locColSource, '.shuff')

  # extract all column names
  locColAll <- attr(ts, 'names')

  lbootRes <- parallel::mclapply(seq_len(nboot), function(x) {

    if (deb) cat(sprintf("Bootstrap iteration %d\n", x))

    switch (method,
            shuffCoord      = {tsRand = ARCOS::shuffCoord(obj)},
            randShiftMeas   = {tsRand = ARCOS::randShiftMeas(obj)},
            shuffMeasTrack  = {tsRand = ARCOS::shuffMeasTrack(obj)},
            shuffMeasFrame  = {tsRand = ARCOS::shuffMeasFrame(obj)},
            shuffBlockTrack = {tsRand = ARCOS::shuffBlockTrack(obj)}
    )

    # TODO: deal with cases when trackColl does not identify any objects!
    tcollRand = ARCOS::trackColl(obj = tsRand[get(stmp) > 0],
                                 eps = eps,
                                 minClSz = minClSz,
                                 nPrev = nPrev,
                                 epsPrev = epsPrev)

    if (is.null(tcollRand)) {
      warning(sprintf("No collective events identified in bootstrap iteration %d\n", x))
      tcollselRand = NULL
    } else {
      tcollselRand = ARCOS::selColl(obj = tcollRand,
                                    colldur = colldurlim,
                                    colltotsz = colltotszlim)

      if (nrow(tcollselRand) == 0)
        warning(sprintf("No selected collective events in bootstrap iteration %d\n", x))
    }

    return(tcollselRand)
  }, mc.cores = ncores)

  locBootRes = rbindlist(lbootRes, fill = TRUE, idcol = 'bootiter')

  # Stretch the resulting table to include the result for all bootstrap iterations,
  # even if an iteration did not produce any results
  setkeyv(locBootRes, 'bootiter')
  dtmp = locBootRes[, .(seq(1, nboot, 1))]
  setkey(dtmp, V1)
  locBootRes = locBootRes[dtmp]

  # Inherit attributes from the input object
  locBootRes = new_arcosTS(dt = locBootRes,
                      colPos = attr(obj, "colPos"),
                      colFrame = attr(obj, "colFrame"),
                      colIDobj = attr(obj, "colIDobj"),
                      colIDcoll = "collid",
                      colMeas =  attr(obj, "colMeas"),
                      colMeasResc =  attr(obj, "colMeasResc"),
                      colMeasBin =  attr(obj, "colMeasBin"),
                      colBootIter = 'bootiter',
                      colRT = attr(obj, "colRT"),
                      interVal = attr(obj, "interVal"),
                      interType = attr(obj, "interType"),
                      fromBin = attr(obj, "fromBin"),
                      fromColl = TRUE,
                      fromBoot = TRUE)

  return(locBootRes)
}
