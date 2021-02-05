#' Clip a vector
#'
#' @param x Numeric vector
#' @param a lower bound (double)
#' @param b upper bound (double)
#'
#' @return
#' @export
#'
#' @examples

Rcpp::cppFunction('NumericVector rcpp_clip( NumericVector x, double a, double b){
    return clamp( a, x, b ) ;
}')

#' Keep significant digits in double numerical columns of a data.table
#'
#' @param inDT
#' @param inDigits
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
keepSignifDig <- function(inDT, inDigits) {

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

  locDT = copy(inDT)

  locCols = vapply(locDT, is.double, FUN.VALUE = logical(1))
  locCols = names(locCols[locCols])

  locDT[, (locCols) := signif(.SD, inDigits), .SDcols = locCols]

  return(locDT)
}

