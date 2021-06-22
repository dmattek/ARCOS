#' Title
#'
#' @param path a string with path to images.
#' @param ext a string with pattern for image search/
#'
#' @return an arcosTS object.
#' @export
#' @importFrom data.table fread
#'
#' @examples
#' cat("no examples")
loadDataFromFile <- function(fname,
                             colPos = "x",
                             colMeas = NULL,
                             col = list(Frame = "f",
                                        IDobj = "id",
                                        RT = NULL,
                                        IDcoll = NULL),
                             interVal = 1L,
                             interType = c("fixed"),
                             ...) {

  locDT = data.table::fread(fname, ...)

  locColNames = names(locDT)

  if (!(colPos %in% locColNames))
    stop("Position columns missing from the data file.")

  if (!(colMeas %in% locColNames))
    stop("Measurement column missing from the data file.")

  if (!(col$Frame %in% locColNames))
    stop("Frame column missing from the data file.")

  if (!(col$IDobj %in% locColNames))
    stop("Object ID column missing from the data file.")

  new_arcosTS(dt = locDT,
              colPos = colPos,
              colFrame = col$Frame,
              colIDobj = col$IDobj,
              colRT = col$RT,
              colIDcoll = col$IDcoll,
              colMeas = colMeas,
              interVal = interVal,
              interType = interType)
}
