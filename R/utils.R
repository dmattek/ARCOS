#' Keep significant digits in double numerical columns of a data.table
#'
#' @param inlocdt a data.table with time series in the long format.
#' @param inDigits an integer with the number of significant digits.
#'
#' @return a data.table with numeric columns trimmed to the provided number of significant digits.
#' @export
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#'
#' locdt = data.table(id = LETTERS[1:10],
#'                 x = runif(10))
#'
#' locdtTrim = ARCOS::keepSignifDig(locdt, 2)
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

  locdt = copy(inDT)

  locCols = vapply(locdt, is.double, FUN.VALUE = logical(1))
  locCols = names(locCols[locCols])

  locdt[, (locCols) := signif(.SD, inDigits), .SDcols = locCols]

  return(locdt)
}


#' Synthetic collective event in 2D
#'
#' 81 objects in 2D in 8 time frames. X/Y positions have a small added gaussian noise added.
#'
#' @param inSeed an integer with the seed for random number generator, default NULL.
#'
#' @return an arcosTS object
#' @export
#'
#' @examples
#' library(ARCOS)
#' dts = genSynth2D()
genSynth2D <- function(inSeed = NULL) {

  if (!is.null(inSeed)) set.seed((inSeed))

  lpar = list()
  lpar$ntp = 8
  lpar$nrowcell = 9
  lpar$ncolcell = 9

  # define empty frames
  locdt = data.table(t = rep(1:lpar$ntp, each = lpar$nrowcell * lpar$ncolcell),
                  x = rep((0:(lpar$nrowcell * lpar$ncolcell - 1)) %% lpar$ncolcell, lpar$ntp),
                  y = rep((0:(lpar$nrowcell * lpar$ncolcell - 1)) %/% lpar$nrowcell, lpar$ntp),
                  m = rep(0, lpar$ntp * lpar$nrowcell * lpar$ncolcell))

  # add object id
  locdt[,
     id := 1:.N,
     by = t]

  # add gaussian noise to X/Y
  locdt[,
     `:=`(x = x + rnorm(nrow(locdt), 0, .1),
          y = y + rnorm(nrow(locdt), 0, .1))]

  # define active objects that form collective events
  locdt[t == 2 & id == 41, m := 1]

  locdt[t == 3 & id %in% c(32,
                        40,41,42,
                        50, 51),
     m := 1]

  locdt[t == 4 & id %in% c(31, 32, 33,
                        40,42,
                        49,50, 51),
     m := 1]

  locdt[t == 5 & id %in% c(22,23,
                        30, 31, 32, 33, 34,
                        39, 40, 42, 43,
                        48, 49, 50, 51, 52,
                        58, 60), m := 1]

  locdt[t == 6 & id %in% c(22,23,24,
                        30, 31, 33,
                        38, 39, 43, 44,
                        48, 52,
                        57, 58, 60, 61),
     m := 1]

  locdt[t == 7 & id %in% c(22,24,
                        30, 34,
                        38, 44,
                        57, 61,
                        69),
     m := 1]

  locdt[t == 8 & id %in% c(21,
                        35,
                        69),
     m := 1]

  ARCOS::arcosTS(locdt,
                 colPos = c("x", "y"),
                 colMeas = "m",
                 col = list(Frame = "t",
                            IDobj = "id",
                            RT = NULL,
                            IDcoll = NULL),
                 interType = "fixed",
                 interVal = 1)

  return(locdt)
}
