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
#' @export
#'
#' @examples
#' vT = seq(0, 1, 0.001) * 10 * pi
#' vY = sin(vT)+vT/10 + 1 + runif(length(vT))/2
#' vYs = detrendTS(vY, smoothK = 21, biasMet = "lm")
#'
#' plot(vT/pi, vY, type = "l", ylim = c(0,6))
#' lines(vT/pi, vYs, col = "red")
#' legend(0, 6,
#'        legend=c("Original", "Smoothed &\nde-trended"),
#'        col=c("black", "red"),
#'               lty=1:2, cex=0.8,
#'                      box.lty=0)
detrendTS = function(x, smoothK = 3L, biasK = 51L, peakThr = 0.2, biasMet = c("runmed", "lm")) {

  biasMet = match.arg(biasMet)

  if (length(x) > 0) {
    # smooth the signal locally
    locS = as.vector(runmed(x, k = smoothK, endrule = "constant"))

    # de-bias
    biasMet = match.arg(biasMet)
    if (biasMet == "runmed") {
      locN = as.vector(runmed(locS, k = biasK, endrule = "median"))
    } else if(biasMet == "lm") {
      locDF = data.frame(x = seq_along(locS),
                         y = locS)
      locResLM = lm(y~x, locDF)
      locN = predict.lm(locResLM)
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
  } else locS = NULL

  return(locS)
}

