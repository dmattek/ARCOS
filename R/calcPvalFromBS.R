#' Calculate p-value from bootstrapping
#'
#' The calculation of the p-value that accounts for the finite sample size uses a formula from Davison and Hinkley (1997), Bootstrap Methods and their Application, chapter 4, p. 161.
#'
#' @param testStat a numeric, test statistic.
#' @param bsStat a numeric vector with statistic from bootstrapping.
#' @param corrected a logical; if TRUE, calculates a p-value that accounts for the finite sample size; default FALSE.
#' @param alternative either of "less" or "greater"; default "greater"
#'
#' @return a scalar with the p-value
#'
#' @rdname calcPvalFromBS
#' @export calcPvalFromBS
#'
#' @examples
#' library(ARCOS)
#' ARCOS::calcPvalFromBS(2, rnorm(1000))
calcPvalFromBS <- function(testStat, bsStat, corrected = FALSE, alternative = c("greater", "less")) {

  stopifnot(length(testStat) == 1)
  stopifnot(is.vector(bsStat))
  alternative = match.arg(alternative)

  locN = length(bsStat)
  if (locN <= 20) warning('The size of the bootstrap should be greater than 20 to calculate the significance level 0.05')

  if (corrected) {
    if (alternative == "less") {
      locPval = (1 + sum(bsStat <= testStat, na.rm = T)) / (locN + 1)
    } else {
      locPval = (1 + sum(bsStat >= testStat, na.rm = T)) / (locN + 1)
    }

  } else {
    if (alternative == "less") {
      locPval = mean(bsStat <= testStat, na.rm = T)
    } else {
      locPval = mean(bsStat >= testStat, na.rm = T)
    }
  }

  return(locPval)
}
