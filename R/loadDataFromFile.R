#' Load data from file
#'
#' Loads data from a file and creates an arcosTS object based on provided parameters.
#' Data is loaded using data.table::fread function, thus all file formats recognised
#' by fread are admissible, including gzip and bz2 compressions.
#'
#' @param fname a string, file name path.
#' @param colPos a vector, names of positional columns; default "x".
#' @param colFrame a string, column name with frame numbers; default "f".
#' @param colIDobj a string, column name with object IDs; default "id".
#' @param colIDcoll a string, column name with IDs of collective events; default NULL.
#' @param colMeas a string, column name of the measurement; default NULL.
#' @param colMeasResc a string, column name of the rescaled measurement; default NULL.
#' @param colMeasBin a string, column name of the binarised measurement; default NULL.
#' @param colBootIter a string, column name with bootstrap iterations; default NULL.
#' @param colRT a string, column name with real time; default NULL (currently unused).
#' @param interVal a numeric with the interval length (currently unused).
#' @param interType a string to designate whether the time series has fixed or variable intervals; possible values fixed or var (currently unused).
#' @param fromBin logical, whether the output has been passed by binarisation, relevant for plotting of binarised activity; default FALSE.
#' @param fromColl logical, whether the output comes from collective event identification, relevant for plotting of collective events; default FALSE.
#' @param fromBoot logical, whether the output comes from bootstrapping, relevant for calculating stats of collective events; default FALSE.
#' @param ... additional parameters passed to data.table::fread.
#'
#' @return an arcosTS object.
#' @export
#' @importFrom data.table fread
#'
#' @examples
#' cat("no examples")
loadDataFromFile <- function(fname,
                             colPos = 'x',
                             colFrame = 'f',
                             colIDobj = 'id',
                             colIDcoll = NULL,
                             colMeas = NULL,
                             colMeasResc = NULL,
                             colMeasBin = NULL,
                             colBootIter = NULL,
                             colRT = NULL,
                             interVal = 1L,
                             interType = c("fixed"),
                             fromBin = FALSE,
                             fromColl = FALSE,
                             fromBoot = FALSE,
                             ...) {

  locDT = data.table::fread(fname, ...)

  # Get existing column names in the input data
  locColNames = names(locDT)

  # Check whether column names provided as parameters exist in the data
  # Obligatory columns
  checkColsInData(colPos, locColNames)
  checkColsInData(colFrame, locColNames)
  checkColsInData(colIDobj, locColNames)

  # Optional columns
  checkColsInData(colIDcoll, locColNames, fromColl)

  if (!is.null(colMeas))
    if(!(colMeas %in% locColNames))
      stop("Column with the measurement missing from the input data.")

  if (!is.null(colMeasResc))
    if(!(colMeasResc %in% locColNames))
      stop("Column with rescaled measurement missing from the input data.")

  checkColsInData(colMeasBin, locColNames, fromBin)
  checkColsInData(colBootIter, locColNames, fromBoot)

  if (!is.null(colRT))
    if(!(colRT %in% locColNames))
      stop("Real time column missing from the input data.")


  # Set order by time frame & object ID
  data.table::setorderv(locDT, c(colBootIter, colFrame, colIDobj))

  new_arcosTS(dt = locDT,
              colPos = colPos,
              colFrame = colFrame,
              colIDobj = colIDobj,
              colIDcoll = colIDcoll,
              colMeas = colMeas,
              colMeasResc = colMeasResc,
              colMeasBin = colMeasBin,
              colBootIter = colBootIter,
              colRT = colRT,
              interVal = interVal,
              interType = interType,
              fromBin = fromBin,
              fromColl = fromColl,
              fromBoot = fromBoot)
}
