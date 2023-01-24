#' Definitions and helper functions for the arcosTS class
#'
#' @title ARCOS
#' @description Definitions and helper functions for the arcosTS class
#' @details Definitions and helper functions for the arcosTS class
NULL

#' Test whether an object is the arcosTS object
#'
#' @title "Test whether an object is the arcosTS object"
#' @param x object
#'
#' @return logical
#' @rdname is.arcosTS
#' @export is.arcosTS
#' @export
#'
#' @examples
#' cat("no examples")
is.arcosTS = function(x) inherits(x, "arcosTS")

#' Test whether an arcosTS object has a column with the binarised measurement calculated by the `binMeas` function.
#'
#' @title "Test whether the measurement in the arcosTS object has been binarised"
#' @param x an object
#'
#' @return logical
#' @rdname is.arcosTS.fromBin
#' @export is.arcosTS.fromBin
#' @export
#'
#' @examples
#' cat("no examples")
is.arcosTS.fromBin = function(x) attr(x, 'fromBin')

#' Test whether an arcosTS object has collective events calculated by the `trackColl` function.
#'
#' @title "Test whether an arcosTS object has collective events"
#' @param x an object
#'
#' @return logical
#' @rdname is.arcosTS.fromColl
#' @export is.arcosTS.fromColl
#' @export
#'
#' @examples
#' cat("no examples")
is.arcosTS.fromColl = function(x) attr(x, 'fromColl')

#' Test whether an arcosTS object comes from bootstrapping calculated by the `trackCollBoot` function.
#'
#' @title "Test whether an arcosTS comes from bootstrapping"
#' @param x an object
#'
#' @return logical
#' @rdname is.arcosTS.fromBoot
#' @export is.arcosTS.fromBoot
#' @export
#'
#' @examples
#' cat("no examples")
is.arcosTS.fromBoot = function(x) attr(x, 'fromBoot')

# Constructor of the arcosTS class
new_arcosTS <- function(dt,
                        colPos,
                        colFrame,
                        colRT,
                        colIDobj,
                        colIDcoll,
                        colMeas,
                        colMeasResc,
                        colMeasBin,
                        colBootIter,
                        interVal,
                        interType,
                        fromBin,
                        fromColl,
                        fromBoot) {

  stopifnot(is.data.table(dt))

  # Check column names
  stopifnot(is.vector(colPos))
  stopifnot(is.character(colFrame))
  stopifnot(is.character(colRT) | is.null(colRT))
  stopifnot(is.character(colIDobj) | is.null(colIDobj))
  stopifnot(is.character(colIDcoll) | is.null(colIDcoll))
  stopifnot(is.character(colMeas) | is.null(colMeas))
  stopifnot(is.character(colMeasResc) | is.null(colMeasResc))
  stopifnot(is.character(colMeasBin) | is.null(colMeasBin))
  stopifnot(is.character(colBootIter) | is.null(colBootIter))

  # Check other params
  stopifnot(is.numeric(interVal))
  stopifnot(interType %in% c("fixed", "var"))
  stopifnot(is.logical(fromBin))
  stopifnot(is.logical(fromColl))
  stopifnot(is.logical(fromBoot))

  # Check for a column with unique object ID; create if missing.
  if (missing(colIDobj)) {
    message("The object ID column set to NULL. Creating a unique ID for all objects.")

    dt[,
       id := 1:.N]

    colIDobj = "id"
  }

  data.table::setattr(dt, "colPos",     colPos)
  data.table::setattr(dt, "colFrame",   colFrame)
  data.table::setattr(dt, "colRT",      colRT)
  data.table::setattr(dt, "colIDobj",   colIDobj)
  data.table::setattr(dt, "colIDcoll",  colIDcoll)
  data.table::setattr(dt, "colMeas",    colMeas)
  data.table::setattr(dt, "colMeasResc",colMeasResc)
  data.table::setattr(dt, "colMeasBin", colMeasBin)
  data.table::setattr(dt, "colBootIter",colBootIter)
  data.table::setattr(dt, "interVal",   interVal)
  data.table::setattr(dt, "interType",  interType)
  data.table::setattr(dt, "fromBin",    fromBin)
  data.table::setattr(dt, "fromColl",   fromColl)
  data.table::setattr(dt, "fromBoot",   fromBoot)

  data.table::setattr(dt, "class",
                      append(c("arcosTS"), class(dt)))

  invisible(dt)
}

# Validator of the arcosTS class
validate_arcosTS <- function(obj) {

  stopifnot(is.data.table(obj))

  colsObj = attr(obj, 'names')
  colPos = attr(obj, 'colPos')
  colFrame = attr(obj, 'colFrame')
  colIDobj = attr(obj, 'colIDobj')

  if (sum(colPos %in% colsObj) != length(colPos)) {
    stop("Input data.table does not have the required position column.",
         call. = FALSE)
  }

  if (!(colFrame %in% colsObj)) {
    stop("Input data.table does not have the required frame column.",
         call. = FALSE)
  }

  if (!(colIDobj %in% colsObj)) {
    stop("Input data.table does not have the required object ID column.",
         call. = FALSE)
  }

  invisible(obj)
}

#' Create an arcosTS class
#'
#' Creates an arcosTS object from time series data in long format stored in a `data.table`. Assigns relevant column names and data parameters.
#'
#' @param dt a data.table with time series in the long format.
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
#'
#' @return an arcosTS object.
#'
#' @rdname arcosTS
#' @export arcosTS
#' @export
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#' dts = arcosTS(dt = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
#'                               id = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
#'                               x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3)),
#'               colFrame = "frame",
#'               colIDobj = "id",
#'               colPos = "x")
arcosTS <- function(dt,
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
                    fromBoot = FALSE) {

  interType = match.arg(interType, c("fixed", "var"))

  validate_arcosTS(new_arcosTS(dt = dt,
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
                               fromBoot = fromBoot))

  invisible(dt)
}
