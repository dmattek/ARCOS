#' Calculate the growth of collective events over time
#'
#' Wrapper for the \code{getMinBBox2D} function that implements the rotating callipers algorithm. Calculates the minimal directed bounding box for every event at every time frame.
#' The longer side is taken as the "diameter". Currently works only with 2D data!
#'
#' @title "Calculate collective events' growth"
#' @param obj an arcosTS object with collective events as obtained from the trackColl function.
#'
#' @return a data.table with the width and height of the minimal directed bounding box, and the number of points used for the calculation.
#'
#' @import data.table
#'
#' @rdname calcGrowthColl
#' @export calcGrowthColl
#'
#' @examples
#' cat("no examples")
calcGrowthColl <- function(obj) {
  UseMethod("calcGrowthColl")
}

calcGrowthColl.default <- function(obj) {
  cat("This is a generic function\n")
}

#' @rdname calcGrowthColl
#' @export calcGrowthColl.arcosTS
#' @export
calcGrowthColl.arcosTS <- function(obj) {

  stopifnot(is.arcosTS(obj))
  stopifnot(is.arcosTS.fromColl(obj))

  colPos <- attr(obj, "colPos")
  if (length(colPos) != 2)
    stop("Currently the function is limited only to 2D spatial data!")

  colFrame <- attr(obj, "colFrame")
  colIDcoll <- attr(obj, "colIDcoll")

  # Calculate the minimal bounding box
  tDiam = obj[,
              as.list(getMinBBox2D(as.matrix(cbind(get(colPos[1]),
                                                   get(colPos[2]))))),
              by = c(colFrame,
                     colIDcoll)]

  # Use the largest side as the 'radius'
  tDiam[,
        diam := pmax(w,h)]

  # Add time counter for every event
  tDiam[,
        tevent := seq_len(.N) - 1,
        by = c(colIDcoll)]

  return(tDiam)
}
