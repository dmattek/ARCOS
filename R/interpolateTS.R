#' Interpolate missing rows in time series
#'
#' @param inDT a data.table with time series in the long format.
#' @param inColID a string with the name of the column with unique time series IDs.
#' @param inColFN a string with the name of the column with integer frame numbers.
#' @param inColY a string or a vector of strings with column names of numerical columns to interpolate.
#' @param inFNfreq an integer with the interval between frames.
#' @param inDeb logical, whether to output debug information.
#'
#' @keywords internal
#' @return a data.table with interpolated missing time points.
#' @import data.table
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#'
#' dt = data.table(t = c(1,2,3,5,6,7),
#'                 y = c(1,2,3,5,6,7),
#'                 id = rep(1, 6))
#'
#' dtInt = ARCOS:::interpolateTS(inDT = dt,
#'                       inColID = "id",
#'                       inColFN = "t",
#'                       inColY = "y",
#'                       inFNfreq = 1)
#'
#'                       plot(dt$t, dt$y, type = "p")
#'                       points(dtInt$t, dtInt$y, col = "red", pch = 3)
#'
interpolateTS = function(inDT, inColID, inColFN, inColY, inFNfreq = 1L, inDeb = F) {

  ## Checks
  # Check whether inDT is a data.table
  if(!is.data.table(inDT))
    stop("Input data is not a data.table!")

  # Check whether inDT isn't NULL
  if (is.null(inDT)) {
    stop("Input data is NULL!")
  }

  # Check whether inDT has data
  if (nrow(inDT) < 1L) {
    warning("Input data has no records! Returning NULL")
    return(NULL)
  }

  # Check whether the indicated columns are present in the data.table
  if (sum( c(inColID, inColFN, inColY) %in% names(inDT) ) < 3 ) {
    stop("Indicated column names are not present in the input data!")
  }

  # Stretch time series by every time series' min/max time gaps filled with NA's
  setkeyv(inDT, c(inColID, inColFN))
  inDT = inDT[setkeyv(inDT[,
                           .(seq(min(get(inColFN), na.rm = T),
                                 max(get(inColFN), na.rm = T),
                                 inFNfreq)),
                           by = c(inColID)],
                      c(inColID, "V1"))]

  # check rows with NAs in columns selected for interpolation
  locNA = inDT[rowSums(is.na(inDT[,
                                  c(inColY),
                                  with = F])) > 0, ]

  if (nrow(locNA) > 0) {
    # x-check: print all rows with NA's
    if (inDeb) {
      cat(file = stdout(), "\ninterpolateTS: Rows with NAs to interpolate:\n")
      print(locNA)
    }

    # Apparently the loop is faster than lapply+SDcols
    for (col in inColY) {
      if (inDeb)
        cat(file = stdout(), sprintf("Interpolating NAs in column: %s\n",
                                     col))

      # Interpolated columns should be of type numeric (double).
      # This is to ensure that interpolated columns are of the proper type.
      data.table::set(inDT, j = col, value = as.numeric(inDT[[col]]))

      inDT[, `:=`((col), imputeTS::na_interpolation(get(col))), by = c(inColID)]
    }
  } else {
    if (inDeb) {
      cat(file = stdout(), "\ninterpolateTS: No rows with NAs to interpolate.\n")
    }
  }

  return(inDT)
}


#' Interpolate NAs and missing data
#'
#' Wrapper for the \code{interpolateTS} function.
#'
#' @param obj an arcosTS object.
#'
#' @return an arcosTS object.
#' @import data.table
#'
#' @rdname interpolMeas
#' @export interpolMeas
#'
#' @examples
#' cat("no examples")
interpolMeas <- function(obj) {
  UseMethod("interpolMeas")
}

interpolMeas.default <- function(obj) {
  cat("This is a generic function\n")
}

#' @rdname interpolMeas
#' @export interpolMeas.arcosTS
#' @export
interpolMeas.arcosTS <- function(obj) {

  stopifnot(is.arcosTS(obj))

  locDT = interpolateTS(inDT = obj,
                        inColID = attr(obj, "colIDobj"),
                        inColFN = attr(obj, "colFrame"),
                        inColY = c(attr(obj, "colPos"), attr(obj, "colMeas")),
                        inFNfreq = attr(obj, "interVal"),
                        inDeb = F)

  # Add attributes to the data.table
  new_arcosTS(dt = locDT,
              colPos = attr(obj, "colPos"),
              colFrame = attr(obj, "colFrame"),
              colIDobj = attr(obj, "colIDobj"),
              colIDcoll = attr(obj, "colIDcoll"),
              colMeas = attr(obj, "colMeas"),
              colMeasResc = attr(obj, "colMeasResc"),
              colMeasBin = attr(obj, "colMeasBin"),
              colBootIter = attr(obj, "colBootIter"),
              colRT = attr(obj, "colRT"),
              interVal = attr(obj, "interVal"),
              interType = attr(obj, "interType"),
              fromBin = attr(obj, "fromBin"),
              fromColl = attr(obj, "fromColl"),
              fromBoot = attr(obj, "fromBoot"))

  return(locDT)
}

