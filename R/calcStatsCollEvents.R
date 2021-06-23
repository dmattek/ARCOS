#' Wrapper for calculating stats of collective events
#'
#' Stats of collective events calculated by the \code{trackCollEvents} function by size and duration.
#'
#' @param inDTcoll a data.table with collective events in the long format produced by the \code{trackCollEvents} function. Consists of 3 columns: integer frame number, object id, id's of collective events.
#' @param inCols a list with column names, \code{list(frame = , id = , collid = )}, that correspond to the integer frame number, position, object id and id of collective events, respectively.
#'
#' @keywords internal
#' @return a data.table as the \code{inDTcoll} with aggregated stats of collective events.
#' @import data.table
#'
#' @examples
#' cat("no examples")
calcStatsCollEvents = function(inDTcoll,
                               inCols = list(frame = "frame",
                                             id = "trackID",
                                             collid = "clTrackID")) {

  locDT = inDTcoll[,
                    .(clDur = max(get(inCols$frame)) - min(get(inCols$frame)) + 1,
                      totSz = length(unique(get(inCols$id))),
                      minSz = min(.SD[,
                                      .N,
                                      by = c(inCols$frame)][["N"]]),
                      maxSz = max(.SD[,
                                      .N,
                                      by = c(inCols$frame)][["N"]])),
                    by = c(inCols$collid)]

  return(locDT)
}



#' Stats of collective events
#'
#' Wrapper for the \code{calcStatsCollEvents} function.
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

  if (is.null(colFrame))
    stop("Frame number column not defined in the data.")

  if (is.null(colIDobj))
    stop("Object/track identifier column not defined in the data.")

  if (is.null(colIDcoll))
    stop("Collective event identifier column not defined in the data.")

  locDT = calcStatsCollEvents(inDTcoll = obj,
                              inCols = list(frame = colFrame,
                                            id = colIDobj,
                                            collid = colIDcoll))

  return(locDT)
}
