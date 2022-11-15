#' Shuffle the measurement per track
#'
#' Shuffle time points in the measurement independently for every track.
#' Spatial coordinates remain intact.
#'
#' @title "Shuffle the measurement per track"
#' @param obj an arcosTS object without collective events.
#'
#' @return an arcosTS object
#'
#' @rdname shuffMeasTrack
#' @export shuffMeasTrack
#'
#' @examples
#' cat("no examples")
#'
shuffMeasTrack <- function(obj) {
  UseMethod("shuffMeasTrack")
}

shuffMeasTrack.default <- function(obj) {
  cat("This is a generic function\n")
}

#' @rdname shuffMeasTrack
#' @export shuffMeasTrack.arcosTS
#' @export
shuffMeasTrack.arcosTS <- function(obj) {
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


  ## Add a column with binarised measurement shuffled per track
  obj[,
      c(locColShuff) := sample(get(locColSource)),
      by = c(locColIDobj)]

  return(obj)
}
