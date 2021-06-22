#' Smooth and de-trend time series
#'
#' First, the short-term median filter with size \code{smoothK} is applied to smooth the time series.
#' Two options are available for the subsequent de-trending:
#' a long-term median filter with the size \code{biasK} and linear regression.
#' No further parameters are required for linear regression.
#'
#' After de-trending, the signal is rescaled to the \code{[0,1]} range,
#' if the global difference between min/max is greater than the threshold \code{peakThr}.
#'
#' @param x a numeric vector with the time series for smoothing.
#' @param smoothK an integer, size of the short-term median smoothing filter, default 3L.
#' @param biasK an integer, size of the long-term de-trending median filter, default 51L.
#' @param peakThr a threshold for rescaling of the de-trended signal, default 0.2.
#' @param biasMet a string with the de-trending method, default "runmed".
#'
#' @return a numeric vector with a smoothed time series.
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
detrendTS = function(x, smoothK = 3L, biasK = 51L, peakThr = 0.2, biasMet = c("runmed", "lm", "none")) {

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
        locResLM = stats::lm(y~x,
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
#' A wrapper for \code{detrendTS}.
#'
#' @title "Smooth, de-trend, and binarise the measurement"
#' @param obj an arcosTS object.
#' @param smoothK an integer, length of the short-term median filter, i.e. smoothing, default 3L.
#' @param biasK an integer, length of the long-term median filter, i.e. de-trending, default 51L.
#' @param peakThr a double, threshold for peak detection from signal rescaled to [0,1], default 0.2.
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
                    biasMet = c("runmed", "lm", "none"),
                    binThr = 0.5) {
  UseMethod("binMeas")
}

binMeas.default <- function(obj,
                            smoothK = 3L,
                            biasK = 51L,
                            peakThr = 0.2,
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

    # detrendTS with biasMet = none performs only short-term smoothing
    obj[,
        meas.resc := detrendTS(meas.resc,
                               smoothK = smoothK,
                               biasK = biasK,
                               peakThr = peakThr,
                               biasMet = biasMet),
        by = c(attr(obj, "colIDobj"))]
  } else {
    # Smooth (and de-trend) per time series
    # 1. Short smooth
    # 2. if biasMet in c(runmed, lm), de-trend
    # 3. rescale detrended signal to [0,1] pPER time series
    obj[,
        meas.resc := detrendTS(get(colMeas),
                               smoothK = smoothK,
                               biasK = biasK,
                               peakThr = peakThr,
                               biasMet = biasMet),
        by = c(attr(obj, "colIDobj"))]

  }

  data.table::setattr(obj, "colMeasResc", "meas.resc")

  # 4. binMeas rescaled time series
  obj[,
      meas.bin := as.integer(meas.resc > binThr)]

  data.table::setattr(obj, "colMeasBin", "meas.bin")

  invisible(obj)
}
