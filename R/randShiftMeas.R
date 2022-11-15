#' Randomly shift the measurement
#'
#' Randomly shift/roll the measurement left or right, independently in every trajectory.
#' Spatial coordinates remain intact.
#'
#' @title "Randomly shift the measurement"
#' @param obj an arcosTS object without collective events.
#'
#' @return an arcosTS object
#'
#' @rdname randShiftMeas
#' @export randShiftMeas
#'
#' @examples
#' cat("no examples")
#'
randShiftMeas <- function(obj) {
  UseMethod("randShiftMeas")
}

randShiftMeas.default <- function(obj) {
  cat("This is a generic function\n")
}

#' @rdname randShiftMeas
#' @export randShiftMeas.arcosTS
#' @export
randShiftMeas.arcosTS <- function(obj) {
  stopifnot(is.arcosTS(obj))

  if (!is.null(attr(obj, "colIDcoll"))) {
    stop("The object already has collective events.")
  }

  if ("collid" %in% names(obj)) {
    stop("Check your object. Its colIDcoll attribute is NULL but it contains collid.frame and collid columns.")
  }

  # extract object ID (e.g., track_id) from the input object
  locColIDobj = attr(obj, "colIDobj")

  # extract the column with binarised measurement
  locColSource = attr(obj, 'colMeasBin')

  # set the column name with the resulting shifted binarised measurement
  locColShuff = paste0(locColSource, '.shuff')


  ## Add a column with binarised measurement shifted randomly left or right
  obj[,
      c(locColShuff) := ARCOS:::shifter(get(locColSource),
                                        n = round(runif(1, min = -1, max = +1) * nrow(.SD))),
      by = c(locColIDobj)]

  return(obj)
}
