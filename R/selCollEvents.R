#' Wrapper for filtering collective events
#'
#' Filter collective events calculated by the \code{trackCollEvents} function by size and duration.
#'
#' @param inDTcoll a data.table with collective events in the long format produced by the \code{trackCollEvents} function. Consists of 3 columns: integer frame number, object id, id's of collective events.
#' @param inCols a list with column names, \code{list(frame = , id = , collid = )}, that correspond to the integer frame number, position, object id and id of collective events, respectively.
#' @param inCollDur a vector with 2 integers that correspond to the minimum and maximum duration of clusters in frame numbers.
#' @param inCollTotSz a vector with 2 integers that correspond to the minimum and maximum total size  of clusters in frame numbers. The total size is the number of unique objects involved in a collective event throughout its entire duration.
#'
#' @return a data.table as the \code{inDTcoll} with collective events within provided duration and size bounds.
#' @export
#' @import data.table
#'
#' @examples
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

  return(locDTcollSel[get(lCols$collid) %in% locVsel])
}
