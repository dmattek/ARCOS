#' Calculate p-value from bootstrapping
#'
#' The corrected p-value calculate uses the formula from Davison and Hinkley (1997), Bootstrap Methods and their Application, p. 141.
#'
#' @param testStat a numeric, test statistic.
#' @param bsStat a numeric vector with statistic from bootstrapping.
#' @param corrected a logical; if TRUE, calculates a corrected p-value; default FALSE.
#' @param alternative either of "two.sided", "less", "greater".
#'
#' @return a scalar with the p-value
#'
#' @rdname calcPvalFromBS
#' @export calcPvalFromBS
#'
#' @examples
#' library(ARCOS)
#' ARCOS::calcPvalFromBS(2, rnorm(1000))
calcPvalFromBS <- function(testStat, bsStat, corrected = FALSE, alternative = c("two.sided", "less", "greater")) {

  stopifnot(length(testStat) == 1)
  stopifnot(is.vector(bsStat))
  alternative = match.arg(alternative)

  locN = length(bsStat)
  if (locN <= 20) warning('The size of the bootstrap should be greater than 20 to calculate the significance level 0.05')

  if (corrected) {
    if (alternative == "two.sided") {
      locPval = 2 * min((1 + sum(bsStat <= testStat)) / (locN + 1),
                        (1 + sum(bsStat >= testStat))  / (locN + 1))
    } else if (alternative == "less") {
      locPval = (1 + sum(bsStat <= testStat)) / (locN + 1)
    } else {
      locPval = (1 + sum(bsStat >= testStat)) / (locN + 1)
    }

  } else {
    if (alternative == "two.sided") {
      locPval = 2 * min(mean(bsStat <= testStat),
                        mean(bsStat >= testStat))
    } else if (alternative == "less") {
      locPval = mean(bsStat <= testStat)
    } else {
      locPval = mean(bsStat >= testStat)
    }
  }

  return(locPval)
}
