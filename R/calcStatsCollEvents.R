#' Calculate statistics of collective events
#'
#' Calculate statistics of collective events identified by the \code{trackCollEvents} function.
#' Calculates:
#' - event duration in time frames, \code{clDur},
#' - the total number of unique objects/cells that participate in an event, \code{totSz},
#' - the smallest number of objects/cells that comprise an event, \code{minSz},
#' - the largest number of objects/cells that comprise an event, \code{maxSz}.
#'
#' @param inDTcoll a data.table with collective events in the long format produced by the \code{trackCollEvents} function. Consists of 3 columns: integer frame number, object id, id's of collective events.
#' @param inCols a list with column names, \code{list(frame = , id = , collid = , bootiter =)}, that correspond to the integer frame number, object id, id of collective events, and bootstrapping iteration, respectively.
#' @param fromBoot logical, indicates whether input data comes from bootstrapping; default FALSE.
#'
#' @keywords internal
#' @return a data.table with aggregated stats of collective events.
#' @import data.table
#'
#' @examples
#' cat("no examples")
calcStatsCollEvents = function(inDTcoll,
                               inCols = list(frame = "frame",
                                             id = "trackID",
                                             collid = "clTrackID",
                                             bootiter = "bootiter"),
                               fromBoot = FALSE) {

  if (fromBoot) sByCols = c(inCols$bootiter, inCols$collid) else sByCols = inCols$collid

  # The size = the number of unique cells in a collective event
  # First aggregate per frame; used for min & max size per CE
  locAggrSzPerFrame = inDTcoll[,
                             .(totSz = length(unique(get(inCols$id)))),
                             by = c(sByCols, inCols$frame)]


  locAggrPerColl = locAggrSzPerFrame[,
                                     .(clDur = max(get(inCols$frame)) - min(get(inCols$frame)) + 1,
                                       minSz = min(totSz),
                                       maxSz = max(totSz)),
                                     by = c(sByCols)]

  # Aggregate per CE to calculate the total size
  locAggrSzPerColl = inDTcoll[,
                              .(totSz = length(unique(get(inCols$id)))),
                              by = c(sByCols)]

  locOut = merge(locAggrPerColl,
                 locAggrSzPerColl,
                 by = sByCols)

  # Bootstrap iterations that don't identify any collective events
  # produce a dt with all columns NAs.
  # The above aggregation will generate NAs in the collid column.
  # Here we set the stats to 0.
  locOut[is.na(get(inCols$collid)),
        `:=`(clDur = 0L,
             totSz = 0L,
             minSz = 0L,
             maxSz = 0L)]

  return(locOut)
}



#' Calculate statistics of collective events
#'
#' Wrapper for the \code{calcStatsCollEvents} function to calculate statistics of collective events identified by the \code{trackCollEvents} function.
#' Calculates:
#' - event duration in time frames, \code{clDur},
#' - the total number of unique objects/cells that participate in an event, \code{totSz},
#' - the smallest number of objects/cells that comprise an event, \code{minSz},
#' - the largest number of objects/cells that comprise an event, \code{maxSz}.
#'
#' @title "Stats of collective events"
#' @param obj an arcosTS object.
#'
#' @return an arcosTS object
#'
#' @rdname calcStatsColl
#' @export calcStatsColl
#'
#' @examples
#' cat("no examples")
calcStatsColl <- function(obj) {
  UseMethod("calcStatsColl")
}

calcStatsColl.default <- function(obj) {
  cat("This is a generic function\n")
}


#' @rdname calcStatsColl
#' @export calcStatsColl.arcosTS
#' @export
calcStatsColl.arcosTS <- function(obj) {

  stopifnot(is.arcosTS(obj))

  colFrame <- attr(obj, "colFrame")
  colIDobj <- attr(obj, "colIDobj")
  colIDcoll <- attr(obj, "colIDcoll")
  fromBoot <- attr(obj, "fromBoot")

  if (is.null(colFrame))
    stop("Frame number column not defined in the data.")

  if (is.null(colIDobj))
    stop("Object/track identifier column not defined in the data.")

  if (is.null(colIDcoll))
    stop("Collective event identifier column not defined in the data.")

  if (fromBoot) {
    colBootIter <- "bootiter"

    if (!(colBootIter %in% attr(obj, 'names')))
      stop('Object attributes indicate that it comes from bootstrapping, but the \"bootiter\" column is missing.')
  } else {
    colBootIter <- NULL
  }

  locDT = calcStatsCollEvents(inDTcoll = obj,
                              inCols = list(frame = colFrame,
                                            id = colIDobj,
                                            collid = colIDcoll,
                                            bootiter = colBootIter),
                              fromBoot = fromBoot)

  return(locDT)
}
