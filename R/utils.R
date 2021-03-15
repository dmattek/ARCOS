#' Keep significant digits in double numerical columns of a data.table
#'
#' @param inDT a data.table with time series in the long format.
#' @param inDigits an integer with the number of significant digits.
#'
#' @return a data.table with numeric columns trimmed to the provided number of significant digits.
#' @export
#' @importFrom Rcpp sourceCpp
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#'
#' dt = data.table(id = LETTERS[1:10],
#'                 x = runif(10))
#'
#' dtTrim = ARCOS::keepSignifDig(dt, 2)
#'
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

