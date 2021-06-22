#' Title
#'
#' @param fname a string with the file name path.
#' @param colPos a vector with names of positional columns; default "x".
#' @param colMeas a string with the column name of the measurement; default NULL.
#' @param col a list with names of other columns, i.e. list(Frame = , IDobj = , RT = , IDcoll = ), with names of the frame, object id, real time, and collective id columns.
#' @param interVal a numeric with the interval length.
#' @param interType a string to designate whether the time series has fixed or variable intervals; possible values fixed or var.
#' @param ... additional parameters passed to data.table::fread.
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
