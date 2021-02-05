#' Smooth and de-trend time series
#'
#' Run short- and long-term median filters to smooth and de-trend a time series.
#' After de-trending, rescale the signal to [0,1], if the global difference between
#' min/max is greater than the threshold.
#'
#' @param x a numeric vector.
#' @param smoothK an integer, size of the short-term median smoothing filter, default 3L.
#' @param biasK an integer, ize of the long-term detrending median filter, default 51L.
#' @param peakThr a threshold for rescaling of the de-trended signal, default 0.2
#'
#' @return
#' @export
#'
#' @examples
detrendTS = function(x, smoothK = 3L, biasK = 51L, peakThr = 0.2, biasMet = c("runmed", "lm")) {

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

