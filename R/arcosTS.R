#' Definitions and helper functions for the arcosTS class
#'
#' @title ARCOS
#' @description Definitions and helper functions for the arcosTS class
#' @details Definitions and helper functions for the arcosTS class
NULL

#' Test whether an object is an arcosTS object
#'
#' @title "Test whether an object is an arcosTS object"
#' @param x object
#'
#' @rdname is.arcosTS
#' @export is.arcosTS
#' @export
#'
#' @examples
#' cat("no examples")
is.arcosTS = function(x) inherits(x, "arcosTS")

# Constructor of the arcosTS class
new_arcosTS <- function(dt,
                        colPos,
                        colFrame,
                        colRT,
                        colIDobj,
                        colIDcoll,
                        colMeas,
                        interVal,
                        interType) {

  stopifnot(is.data.table(dt))
  stopifnot(is.vector(colPos))
  stopifnot(is.character(colFrame))
  stopifnot(is.character(colRT) | is.null(colRT))
  stopifnot(is.character(colIDobj) | is.null(colIDobj))
  stopifnot(is.character(colIDcoll) | is.null(colIDcoll))
  stopifnot(is.vector(colMeas) | is.null(colMeas))
  stopifnot(is.numeric(interVal))
  stopifnot(interType %in% c("fixed", "var"))

  if (missing(colIDobj)) {
    message("The object ID column set to NULL. Creating a unique ID for all objects.")

    dt[,
       IDobj := 1:.N]

    colIDobj = "IDobj"
  }

  data.table::setattr(dt, "colPos", colPos)
  data.table::setattr(dt, "colFrame", colFrame)
  data.table::setattr(dt, "colRT", colRT)
  data.table::setattr(dt, "colIDobj", colIDobj)
  data.table::setattr(dt, "colIDcoll", colIDcoll)
  data.table::setattr(dt, "colMeas", colMeas)
  data.table::setattr(dt, "interVal", interVal)
  data.table::setattr(dt, "interType", interType)

  data.table::setattr(dt, "class", union("arcosTS", class(dt)))

  invisible(dt)
}

# Validator of the arcosTS class
validate_arcosTS <- function(obj) {

  # TODO: expand checks

  stopifnot(is.data.table(obj))

  if (!(attr(obj, "colFrame") %in% attr(obj, "names"))) {
    stop("Input data.table does not have the required frame column.",
         call. = FALSE)
  }

  if (!(attr(obj, "colIDobj") %in% attr(obj, "names"))) {
    stop("Input data.table does not have the required object ID column.",
         call. = FALSE)
  }

  #return(obj)
  invisible(obj)
}

#' Create an arcosTS class
#'
#' Creates an arcosTS object from time series data in long format stored in a `data.table`. Assigns relevant column names and data parameters.
#'
#' @param dt a data.table with time series in the long format.
#' @param colPos a vector with names of positional columns; default "x".
#' @param colMeas a string with the column name of the measurement; default NULL.
#' @param col a list with names of other columns, i.e. list(Frame = , IDobj = , RT = , IDcoll = ), with names of the frame, object id, real time, and collective id columns.
#' @param interVal a numeric with the interval length.
#' @param interType a string to designate whether the time series has fixed or variable intervals; possible values fixed or var.
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
#'               colPos = "x",
#'               col = list(Frame = "frame",
#'                          IDobj = "id"),
#'               interVal = 1,
#'               interType = "fixed")
arcosTS <- function(dt,
                    colPos = "x",
                    colMeas = NULL,
                    col = list(Frame = "f",
                               IDobj = "id",
                               RT = NULL,
                               IDcoll = NULL),
                    interVal = NULL,
                    interType = c("fixed")) {

  interType = match.arg(interType, c("fixed", "var"))
  validate_arcosTS(new_arcosTS(dt = dt,
                               colPos = colPos,
                               colFrame = col$Frame,
                               colRT = col$RT,
                               colIDobj = col$IDobj,
                               colIDcoll = col$IDcoll,
                               colMeas = colMeas,
                               interVal = interVal,
                               interType = interType))

  invisible(dt)
}

