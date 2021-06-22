#' Wrapper for filtering collective events
#'
#' Filter collective events calculated by the \code{trackCollEvents} function by size and duration.
#'
#' @param inDTcoll a data.table with collective events in the long format produced by the \code{trackCollEvents} function. Consists of 3 columns: integer frame number, object id, id's of collective events.
#' @param inCols a list with column names, \code{list(frame = , id = , collid = )}, that correspond to the integer frame number, position, object id and id of collective events, respectively.
#' @param inCollDur a vector with 2 integers that correspond to the minimum and maximum duration of clusters in frame numbers.
#' @param inCollTotSz a vector with 2 integers that correspond to the minimum and maximum total size of clusters in frame numbers. The total size is the number of unique objects involved in a collective event throughout its entire duration.
#'
#' @return a data.table as the \code{inDTcoll} with collective events within provided duration and size bounds.
#' @import data.table
#'
#' @examples
#' cat("no examples")
selCollEvents = function(inDTcoll,
                         inCols = list(frame = "frame",
                                       id = "trackID",
                                       collid = "clTrackID"),
                         inCollDur,
                         inCollTotSz) {

  # Chain:
  # 1. Calculate duration & size of collective events in frames
  # 2. Select only collective events above size & duration thresholds
  # 3. store collid's
  locVsel = inDTcoll[,
                     .(clDur = max(get(inCols$frame)) - min(get(inCols$frame)) + 1,
                       totSz = length(unique(get(inCols$id)))),
                     by = c(inCols$collid)][clDur %between% inCollDur &
                                              totSz %between% inCollTotSz][[inCols$collid]]

  return(inDTcoll[get(inCols$collid) %in% locVsel])
}


#' Select collective events
#'
#' Wrapper for the \code{selCollEvents} function.
#'
#' @param obj an arcosTS object.
#' @param colldur a two-element vector of integers with minimum and maximum duration of collective events; default c(1, Inf).
#' @param colltotsz a two-element vector of integers with minimum and maximum total size of collective events; default c(1, Inf).
#'
#' @return an arcosTS object
#'
#' @rdname selColl
#' @export selColl
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#'  ts = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
#'                 objid = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
#'                 x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))
#'
#' arcosTS(ts,
#'         colPos = "x",
#'         col = list(Frame = "frame",
#'                    IDobj = "objid",
#'                    RT = NULL,
#'                    IDcoll = NULL),
#'         interVal = 1.,
#'         interType = "fixed")
#'
#' tc = trackColl(ts)
#' tcsel = selColl(tc,
#'                 colldur = c(3,Inf),
#'                 colltotsz = c(3, Inf))
selColl <- function(obj, colldur = c(1, Inf), colltotsz = c(1, Inf)) {
  UseMethod("selColl")
}

selColl.default <- function(obj, colldur = c(1, Inf), coltlotsz = c(1, Inf)) {
  cat("This is a generic function\n")
}

#' @rdname selColl
#' @export selColl.arcosTS
#' @export
selColl.arcosTS <- function(obj, colldur = c(1, Inf), colltotsz = c(1, Inf)) {

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

  locDT = selCollEvents(inDTcoll = obj,
                        inCols = list(frame = colFrame,
                                      id = colIDobj,
                                      collid = colIDcoll),
                        inCollDur = colldur,
                        inCollTotSz = colltotsz)

  return(locDT)
}
