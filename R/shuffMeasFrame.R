#' Shuffle the measurement per frame
#'
#' Shuffle the measurement independently in every frame.
#' Spatial coordinates remain intact.
#'
#' @title "Shuffle the measurement per frame"
#' @param obj an arcosTS object without collective events.
#'
#' @return an arcosTS object
#'
#' @rdname shuffMeasFrame
#' @export shuffMeasFrame
#'
#' @examples
#' cat("no examples")
#'
shuffMeasFrame <- function(obj) {
  UseMethod("shuffMeasFrame")
}

shuffMeasFrame.default <- function(obj) {
  cat("This is a generic function\n")
}

#' @rdname shuffMeasFrame
#' @export shuffMeasFrame.arcosTS
#' @export
shuffMeasFrame.arcosTS <- function(obj) {
  stopifnot(is.arcosTS(obj))

  if (!is.null(attr(obj, "colIDcoll"))) {
    stop("The object already has collective events.")
  }

  if ("collid" %in% names(obj)) {
    stop("Check your object. Its colIDcoll attribute is NULL but it contains collid.frame and collid columns.")
  }

  # extract frame column name
  locColFrame = attr(obj, "colFrame")

  # extract the column with binarised measurement
  locColSource = attr(obj, 'colMeasBin')

  # set the column name with the resulting shifted binarised measurement
  locColShuff = paste0(locColSource, '.shuff')


  ## Add a column with binarised measurement shuffled per frame
  obj[,
      c(locColShuff) := sample(get(locColSource)),
      by = c(locColFrame)]

  return(obj)
}
