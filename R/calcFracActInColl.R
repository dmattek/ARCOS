#' Calculate the fraction of activity in collective events
#'
#' Calculate the fraction of total binarised activity involved in collective events.
#'
#' @title "Shuffle coordinates"
#' @param objFull an arcosTS object with full data, without collective events.
#' @param objColl an arcosTS object with collective events. Can be also an arcosTS object returned from bootstrapping trackCollBoot function.
#'
#' @return a data.table with the fraction. If the objColl parameter comes from bootstrapping, fractions are calculated per bootstrapping iteration.
#'
#' @rdname calcFracActInColl
#' @export calcFracActInColl
#'
#' @examples
#' cat("no examples")
#'
calcFracActInColl <- function(objFull, objColl) {
  UseMethod("calcFracActInColl")
}

calcFracActInColl.default <- function(objFull, objColl) {
  cat("This is a generic function\n")
}

#' @rdname shuffCoord
#' @export shuffCoord.arcosTS
#' @export
calcFracActInColl.default <- function(objFull, objColl) {
  stopifnot(is.arcosTS(objFull))
  stopifnot(is.arcosTS(objColl))

  # Initiate flags
  locFromBoot = FALSE

  # Check whether objColl comes from bootstrapping
  locFromBoot = attr(objColl, "fromBoot")

  # If objColl comes from bootstrapping, check for the bootiter column
  if (locFromBoot) {
    if ("bootiter" %in% attr(objColl, "names"))
      locColBy = "bootiter"
    else
      stop("Even though objColl comes from bootstrapping, it does not have the bootiter column.")
  } else {
    locColBy = ""
  }

  # Extract column name with binarised measurement
  locColMeasBin = attr(objFull, "colMeasBin")

  if (is.null(locColMeasBin)) stop("Missing column with binarised measurement. Run ARCOS::binMeas first!")
  if (!(locColMeasBin %in% attr(objFull, "names"))) stop("The column indicated as binarised measurement does not exist in the data. Check column name definitions when initialising ARCOS object!")

  # Total activity in the full data
  locTotActFull = sum(objFull[[locColMeasBin]])

  # Total activity fraction in collective events.
  # If objColl comes from bootstrapping, calculate the fraction per bootstrapping iteration.
  # Otherwise, calculate the fraction for all collective events in the data.
  locDTtotAcColl = objColl[, .(fracAct = .N / locTotActFull), by = c(locColBy)]

  return(locDTtotAcColl)
}
