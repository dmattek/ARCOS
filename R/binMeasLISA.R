#' Calculate LISA in 2D
#'
#' @description
#' Calculate local indicators of spatial associations (LISAa) for a 2D point process.
#' Uses LISA implementations in the package elsa.
#'
#' @param x a numeric vector with x coordinates.
#' @param y a numeric vector with y coordinates.
#' @param m a numeric vector with values at x and y.
#' @param d1 numeric lower bound of local distance; default 0.
#' @param d2 numeric upper bound of local distance; default 1.
#' @param statistic a character string specifying the LISA statistic that should be calculated. This can be one of "localmoran", "localgeary", "localG" or "localG*".
#'
#' @return a numeric vector with LISA statistic.
#' @export
#' @importFrom elsa lisa
#' @importFrom sp SpatialPointsDataFrame
#'
#' @examples
#' library(ARCOS)
#' calcLisa2D(x = runif(1000),
#'            y = runif(1000),
#'            m = rnorm(1000))
calcLisa2D = function(x, y, m,
                      d1 = 0,
                      d2 = 1,
                      statistic = "localmoran") {

  locSP = sp::SpatialPointsDataFrame(coords = data.frame(x = x,
                                                         y = y),
                                     data = data.frame(m = m))

  locE = elsa::lisa(locSP,
                    d1 = d1,
                    d2 = d2,
                    statistic = statistic)

  return(locE@data[, 1])
}

#' Binarise the measurement using LISAs
#'
#' @description
#' Binarise the measurement using local indicators of spatial association (LISAs) implemented in the package elsa.
#'
#' @title "Binarise the measurement using LISAs"
#' @param obj an arcosTS object.
#' @param d1 numeric lower bound of local distance; default 0.
#' @param d2 numeric upper bound of local distance; default 1.
#' @param statistic a character string specifying the LISA statistic that should be calculated. This can be one of "localmoran", "localgeary", "localG" or "localG*".
#' @param binThr a double, threshold for signal binarisation, default 0.5.
#' @param binThrAsFrac logical; if FALSE, the binThr is used as a fixed threshold to binarisa LISA; if TRUE, the binThr is treated as a fraction of LISA values independently per frame; default FALSE.
#'
#' @return an arcosTS object.
#'
#' @import data.table
#' @rdname binMeasLISA
#' @export binMeasLISA
#'
#' @examples
#' cat("no examples")
binMeasLISA <- function(obj,
                        d1 = 1,
                        d2 = 10,
                        statistic = c("localmoran",
                                      "localgeary",
                                      "localG",
                                      "localG*"),
                        binThr = 0.5,
                        binThrAsFrac = F) {
  UseMethod("binMeasLISA")
}

binMeasLISA.default <- function(obj,
                                d1 = 1,
                                d2 = 10,
                                statistic = c("localmoran",
                                              "localgeary",
                                              "localG",
                                              "localG*"),
                                binThr = 0.5,
                                binThrAsFrac = F) {
  cat("This is a generic function\n")
}

#' @rdname binMeasLISA
#' @export binMeasLISA.arcosTS
#' @export
binMeasLISA.arcosTS <- function(obj,
                                d1 = 1,
                                d2 = 10,
                                statistic = c("localmoran",
                                              "localgeary",
                                              "localG",
                                              "localG*"),
                                binThr = 0.5,
                                binThrAsFrac = F) {

  stopifnot(is.arcosTS(obj))

  colFrame <- attr(obj, "colFrame")
  colMeas <- attr(obj, "colMeas")
  if (is.null(colMeas)) stop("Measurement column not defined in the data.")

  colPos <- attr(obj, "colPos")
  if ( length(colPos) == 1 ) stop("Only one position column defined in the data. Two required.")

  if ( length(colPos) > 2) warning("More than 2 position columns defined. LISA-based binarisation works only for 2D. Taking the first 2 dimensions only!")

  statistic = match.arg(statistic)

  # a column with lisa statistic added to existing data table
  obj[,
      e := calcLisa2D(x = get(colPos[1]),
                      y = get(colPos[2]),
                      m = get(colMeas),
                      d1 = d1,
                      d2 = d2,
                      statistic = statistic),
      by = c(colFrame)]

  if (binThrAsFrac) {
    # Binarise the measurement by thresholding LISA statistic at a percentage threshold independently in every frame.
    obj[,
        meas.bin := as.integer(e > (max(e, na.rm = T) - min(e, na.rm = T)) * binThr),
        by = c(colFrame)]
  } else {
    obj[,
        meas.bin := as.numeric(e > binThr)]
  }

  data.table::setattr(obj, "colMeasBin", "meas.bin")

  # Set the flag to signify that data was processed by this binarisation function
  data.table::setattr(obj, "fromBin", TRUE)

  invisible(obj)
}
