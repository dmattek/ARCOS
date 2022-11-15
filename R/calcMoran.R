#' Global Moran's I spatial correlation
#'
#' Uses function Moran.I from the ape package to calculate the global spatial correlation of a point process with weights.
#'
#' @param x a numeric matrix, with rows corresponding to objects' positions and columns as spatial coordinates.
#' @param scol a numeric scalar or vector that prescribes column numbers with spatial coordinates.
#' @param mcol a numeric scalar that prescribes a column number with the measurement.
#' @param ... additional parameters passed to ape::Moran.I
#'
#' @return a list with observed, expected, sd, and p-value
#' @importFrom ape Moran.I
#'
#' @rdname calcMoran
#' @export calcMoran
#'
#' @examples
#' library(ARCOS)
#' ARCOS::calcMoran(cbind(runif(100), runif(100), rnorm(100)), scol = c(1,2), mcol = 3)
calcMoran = function(x, scol, mcol, ...) {
  locDist = as.matrix(dist(x[, scol]))
  locDistInv = 1./locDist

  diag(locDistInv) = 0.

  locRes = ape::Moran.I(x = x[, mcol],
                        weight = locDistInv,
                        ...)

  return(locRes)
}
