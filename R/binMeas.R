#' Smooth and de-trend time series
#'
#' First a short-term median filter with size \code{smoothK} is applied to remove fast noise from the time series.
#' The subsequent de-trending can be performed with a long-term median filter with the size \code{biasK} (\code{biasMet = "runmed"})
#' or by fitting a polynomial of degree \code{polyDeg} (\code{biasMet = "lm"}).
#'
#' @param x a numeric vector with the time series for smoothing.
#' @param smoothK an integer, size of the short-term median smoothing filter, default 3L.
#' @param biasK an integer, size of the long-term de-trending median filter, default 51L.
#' @param peakThr a threshold for rescaling of the de-trended signal, default 0.2.
#' @param polyDeg an integer, sets the degree of the polynomial for lm fitting; default 1.
#' @param biasMet a string with the de-trending method, default "runmed".
#'
#' @keywords internal
#' @return a numeric vector with a smoothed and/or de-trended and rescaled time series.
#' @importFrom stats lm predict.lm runmed
#'
#' @examples
#' library(ARCOS)
#' vT = seq(0, 1, 0.001) * 10 * pi
#' vY = sin(vT)+vT/10 + 1 + runif(length(vT))/2
#' vYs = ARCOS:::detrendTS(vY, smoothK = 21, biasMet = "lm")
#'
#' plot(vT/pi, vY, type = "l", ylim = c(0,6))
#' lines(vT/pi, vYs, col = "red")
#' legend(0, 6,
#'        legend=c("Original", "Smoothed &\nde-trended"),
#'        col=c("black", "red"),
#'               lty=1:2, cex=0.8,
#'                      box.lty=0)
detrendTS = function(x, smoothK = 3L, biasK = 51L, peakThr = 0.2, polyDeg = 1L, biasMet = c("runmed", "lm", "none")) {

  biasMet = match.arg(biasMet)

  if (length(x) > 0) {
    # smooth the signal locally
    locS = as.vector(stats::runmed(x, k = smoothK, endrule = "constant"))

    if (!(biasMet == "none")) {
      # de-trending
      if (biasMet == "runmed") {
        # de-trend using a running average with a long window
        locN = as.vector(stats::runmed(locS, k = biasK, endrule = "median"))

      } else if(biasMet == "lm") {
        # de-trend using linear regression
        locForm = paste0("y ~", paste0(sprintf("I(x^%s)", 1:polyDeg), collapse = "+"))

        locResLM = stats::lm(as.formula(locForm),
                             data.frame(x = seq_along(locS),
                                        y = locS))
        locN = stats::predict.lm(locResLM)
      }

      # de-trend the smoothed signal
      locS = locS - locN

      # remove negative values that might have appeared due to de-trending
      locS = pmax(locS, 0)

      # Check the global difference in the de-biased signal.
      if ((max(locS) - min(locS)) > peakThr) {
        # Rescale to [0, 1] if the
        # difference is larger than the threshold.
        # Peaks are rescaled with respect to the highest peak whose value becomes 1.

        locS = locS/max(locS)
      }

      # set possible NAs from dividing by max(locS) to 0
      locS[is.nan(locS)] = 0

    } # endif !(biasMet == "none")
  } else {
    locS = NULL
  } # endif length(x) > 0

  return(locS)
}




#' Smooth, de-trend, and binarise the measurement
#'
#' First a short-term median filter with size \code{smoothK} is applied to remove fast noise from the time series.
#' If the de-trending method is set to \code{"none"}, smoothing is applied on globally rescaled time series.
#' The subsequent de-trending can be performed with a long-term median filter with the size \code{biasK} (\code{biasMet = "runmed"})
#' or by fitting a polynomial of degree \code{polyDeg} (\code{biasMet = "lm"}).
#'
#' After de-trending, if the global difference between min/max is greater than the threshold \code{peakThr}
#' the signal is rescaled to the \code{[0,1]} range.
#'
#' The final signal is binarised using the \code{binThr} threshold.
#' .
#'
#' @title "Smooth, de-trend, and binarise the measurement"
#' @param obj an arcosTS object.
#' @param smoothK an integer, length of the short-term median filter, i.e. smoothing, default 3L.
#' @param biasK an integer, length of the long-term median filter, i.e. de-trending, default 51L.
#' @param peakThr a double, threshold for peak detection from signal rescaled to [0,1], default 0.2.
#' @param polyDeg an integer, sets the degree of the polynomial for lm fitting; default 1.
#' @param biasMet method for de-trending, choose from runmed (median filter), lm (linear regression), none, default runmed.
#' @param binThr a double, threshold for signal binarisation, default 0.5.
#'
#' @return an arcosTS object.
#'
#' @import data.table
#' @rdname binMeas
#' @export binMeas
#'
#' @examples
#' cat("no examples")
binMeas <- function(obj,
                    smoothK = 3L,
                    biasK = 51L,
                    peakThr = 0.2,
                    polyDeg = 1L,
                    biasMet = c("runmed", "lm", "none"),
                    binThr = 0.5) {
  UseMethod("binMeas")
}

binMeas.default <- function(obj,
                            smoothK = 3L,
                            biasK = 51L,
                            peakThr = 0.2,
                            polyDeg = 1L,
                            biasMet = c("runmed", "lm", "none"),
                            binThr = 0.5) {
  cat("This is a generic function\n")
}

#' @rdname binMeas
#' @export binMeas.arcosTS
#' @export
binMeas.arcosTS <- function(obj,
                            smoothK = 3L,
                            biasK = 51L,
                            peakThr = 0.2,
                            polyDeg = 1L,
                            biasMet = c("runmed", "lm", "none"),
                            binThr = 0.5) {

  stopifnot(is.arcosTS(obj))

  colFrame <- attr(obj, "colFrame")
  colIDobj <- attr(obj, "colIDobj")
  colMeas <- attr(obj, "colMeas")

  # Order by unique track IDs and by the frame number
  setorderv(obj,
            c(colIDobj,
              colFrame))

  biasMet = match.arg(biasMet)

  if (biasMet == "none") {
    # Add a column with globally rescaled meas to [0,1]
    locMin = min(obj[[colMeas]], na.rm = T)
    locMax = max(obj[[colMeas]], na.rm = T)
    obj[,
        meas.resc := (get(colMeas) - locMin) / (locMax - locMin)]

    # if biasMet = none, perform only short-term smoothing on globally rescaled measurement
    obj[,
        meas.resc := detrendTS(meas.resc,
                               smoothK = smoothK,
                               biasK = biasK,
                               peakThr = peakThr,
                               polyDeg = polyDeg,
                               biasMet = biasMet),
        by = c(colIDobj)]
  } else {
    # if biasMet in c(runmed, lm), smooth (and de-trend) per time series.
    # 1. Short-term smooth
    # 2. De-trend, either by subtracting the time series smoothed with a long-term
    #    median filter (biasMet = "runmed") or a polynomial fit (biasMet = "lm")
    # 3. Rescale de-trended signal to [0,1] per time series
    obj[,
        meas.resc := detrendTS(get(colMeas),
                               smoothK = smoothK,
                               biasK = biasK,
                               peakThr = peakThr,
                               polyDeg = polyDeg,
                               biasMet = biasMet),
        by = c(colIDobj)]

  }

  data.table::setattr(obj, "colMeasResc", "meas.resc")

  # 4. binMeas rescaled time series
  obj[,
      meas.bin := as.integer(meas.resc > binThr)]

  data.table::setattr(obj, "colMeasBin", "meas.bin")

  # Set the flag to signify that data was processed by detrending
  data.table::setattr(obj, "fromBin", TRUE)

  invisible(obj)
}
