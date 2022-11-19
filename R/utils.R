#' Keep significant digits
#'
#' @description
#' Keep significant digits in double numerical columns of a data.table.
#'
#' @param inDT a data.table with time series in the long format.
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


#' A single synthetic collective event in 2D
#'
#' @description
#' Create a single collective event comprising 81 objects in 2D in 8 time frames. X/Y positions have an additional small Gaussian noise.
#'
#' @param inSeed an integer with the seed for the random number generator, default NULL.
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

#' Random synthetic collective events in 2D
#'
#' @description
#' Create a sequence of collective events. A collective event is
#' a concentrically growing circle that increases its radius at every frame.
#' The location of the collective event is random on a `maxx`-by-`maxy` lattice,
#' and the duration is random between 1 and `maxdur` frames.
#' There are `nevents` that occur within `maxt` frames.
#'
#' @param nevents an integer, defines the number of events; default 10.
#' @param maxt an integer, defines the maximum number of frames; default 25.
#' @param maxx an integer, defines the maximum width of the grid; default 20.
#' @param maxy an integer, defines the maximum height of the grid; default 20.
#' @param maxdur, an integer, defines the maximum duration of events; default 5.
#' @param inSeed an integer with the seed for the random number generator, default NULL.
#'
#' @return an arcosTS object
#' @export
#'
#' @examples
#' library(ARCOS)
#' dts = genRandSynth2D()
genRandSynth2D <- function(nevents = 10L,
                           maxt = 25L,
                           maxx = 20L,
                           maxy = 20L,
                           maxdur = 5L,
                           inSeed = NULL) {

  if (!is.null(inSeed)) set.seed((inSeed))

  # random starting time points, positions and durations of CEs
  tpts = sort(sample(1:maxt, nevents))
  posx = sample(1:maxx, nevents)
  posy = sample(1:maxy, nevents)
  dur = sample(1:maxdur, nevents, replace = T)

  # xytAll matrix columns:
  # - event id
  # - time
  # - x/y

  # start with a dummy first row
  xytAll = c(0,0,0,0)

  # loop to create events
  for (iEv in seq_len(nevents)) {

    # create the first frame of the event
    xytEv = c(iEv,
              tpts[iEv],
              midPtCir(posx[iEv],
                       posy[iEv], 0))

    # create subsequent frames of the event
    for (iT in seq_len(dur[iEv])) {
      #cat(sprintf("iEv=%d iT=%d\n", iEv, iT))

      # xy positions
      xy = midPtCir(posx[iEv], posy[iEv], iT)

      # clip xy
      xy[xy[, 1] < 0, 1] <- 0
      xy[xy[, 1] > maxx, 1] <- maxx

      xy[xy[, 2] < 0, 2] <- 0
      xy[xy[, 2] > maxy, 2] <- maxy

      nrowxy = nrow(xy)

      # combine event id, time, and object id
      xyt = cbind(rep(iEv, nrowxy),
                  rep(iT + tpts[iEv], nrowxy),
                  xy)

      # combine frames
      xytEv = rbind(xytEv,
                    xyt)
    }

    xytAll = rbind(xytAll,
                   xytEv)
  }

  # remove dummy first row
  xytAll = xytAll[-1, ]

  # clip t
  xytAll <- xytAll[xytAll[, 2] <= maxt, ]

  # add object id based on xy;
  # object in the same xy location have the same object id
  id = xytAll[, 4] * maxx + xytAll[, 3]
  xytAll = cbind(xytAll,
                 id)

  # create arcosTS object
  ts = data.table(t = xytAll[, 2],
                  x = xytAll[, 3],
                  y = xytAll[, 4],
                  eventid = xytAll[, 1],
                  id = xytAll[, 5])

  # because of xy clipping and/or multiple events,
  # there can be duplicated rows, i.e. same xyt; remove.
  ts = ts[!duplicated(ts)]

  ARCOS::arcosTS(dt = ts,
                 colPos = c("x", "y"),
                 col = list(IDobj = "id",
                            Frame = "t"),
                 interVal = 1,
                 interType = "fixed")

  return(ts)
}

#' Mid-Point Circle Drawing Algorithm
#'
#' @description
#' A utility function to create X/Y positions on a circle.
#' The algorithm taken from <https://www.geeksforgeeks.org/mid-point-circle-drawing-algorithm/>
#'
#' @param x0 a numeric, defines the x coordinate of the circle's centre.
#' @param y0 a numeric, defines the y coordinate of the circle's centre.
#' @param r a numeric, defines the circle's radius.
#'
#' @return a numeric matrix.
#'
#' @examples
#' library(ARCOS)
#' mcir = ARCOS:::midPtCir(1.4, 4.3, 5)
midPtCir = function(x0, y0, r) {
  x = r
  y = 0

  c0 = matrix(c(x+x0, y+y0),
              ncol = 2,
              byrow = T)

  if (r > 0) {
    c0 = rbind(c0,
               matrix(c( x + x0, -y + y0,
                         y + x0,  x + y0,
                         -y + x0,  x + y0),
                      ncol = 2,
                      byrow = T))
  }

  # Initialising the value of P
  P = 1 - r
  while (x > y) {
    y = y + 1

    # Mid-point is inside or on the perimeter
    if (P <= 0) {
      P = P + 2*y + 1
    }
    else
    {
      #Mid-point is outside the perimeter
      x = x - 1
      P = P + 2*y - 2*x + 1
    }

    # All the perimeter points have already been printed
    if (x < y) break

    # Printing the generated point and its reflection
    # in the other octants after translation
    c0 = rbind(c0,
               matrix(c(x + x0, y + y0,
                        -x + x0, y + y0,
                        x + x0, -y + y0,
                        -x + x0, -y + y0),
                      ncol = 2,
                      byrow = T))

    # If the generated point is on the line x = y then
    # the perimeter points have already been printed
    if (x != y)
    {
      c0 = rbind(c0,
                 matrix(c(y + x0, x + y0,
                          -y + x0, x + y0,
                          y + x0, -x + y0,
                          -y + x0, -x + y0),
                        ncol = 2,
                        byrow = T))
    }
  }

  return(c0)
}

#' Vector shifter
#'
#' @description
#' Shift a vector left or right by a number of spaces.
#' From: https://stackoverflow.com/a/30542172/1898713
#' library(SOfun); shifter(a)#'
#'
#' @param x a numeric vector
#' @param n an integer, the number of indices to shift the input vector. Positive, shifts left; negative, shifts right.
#'
#' @return a numeric vector
#'
#' @examples
#' library(ARCOS)
#' ARCOS:::shifter(1:10, 2)
shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}


#' Block shuffle a binary vector
#'
#' @description
#' Randomly shuffle runs of 0s & 1s in a vector but maintain their alternating order,
#' i.e. there'll never be joint runs of 0s or 1s from the original sequence.
#' We assume that the vector consists of 0s & 1s.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#'
#' @examples
#' library(ARCOS)
#' set.seed(7)
#' x <- round(runif(20))
#' ARCOS:::shuffBlockVecAlt(x)
shuffBlockVecAlt <- function(x) {

  # Calculate lengths of 0s & 1s using run length encoding
  rleres <- rle(x)

  # Shuffle separately odd and even lengths;
  # 0s & 1s are shuffled separately to avoid placing adjacent blocks from the original vector

  # the number of runs in the original vector; both 0s & 1s
  nruns <- length(rleres$lengths)

  if (nruns > 1) {
    # create a sequence of odd numbers up to the number of runs in the original vector
    seqodd <- seq(1, nruns, 2)
    seqoddlen <- length(seqodd)

    # lengths of odd runs
    lenrunsodd <- rleres$lengths[seqodd]

    # if there is more than 1 odd run, shuffle
    if (seqoddlen > 1) {
      lenrunsodd  <- sample(lenrunsodd, seqoddlen)
    }

    # create a sequence of even numbers up to the number of runs in the original vector
    seqeven <- seq(2, nruns, 2)
    seqevenlen <- length(seqeven)

    # lengths of even runs
    lenrunseven <- rleres$lengths[seqeven]

    # if there is more than 1 even run, shuffle
    if (seqevenlen > 1) {
      lenrunseven  <- sample(lenrunseven, seqevenlen)
    }

    # Combine odd and even run lengths into a single alternating vector.
    # https://stackoverflow.com/a/43876294/1898713
    lenrunsrand <- c(lenrunsodd,
                     lenrunseven)[order(c(seq_along(lenrunsodd),
                                          seq_along(lenrunseven)))]

  } else {
    lenrunsrand <- rleres$lengths
  }

  # Recreate the alternating sequence using shuffled lengths
  return(rep(rleres$values, lenrunsrand))
}

#' Block shuffle a binary vector
#'
#' @description
#' Randomly shuffle runs of 0s & 1s in a vector. Do not maintain the alternating order of 0s & 1s.
#' We assume that the vector consists of 0s & 1s.
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#'
#' @examples
#' library(ARCOS)
#' set.seed(7)
#' x <- round(runif(20))
#' ARCOS:::shuffBlockVec(x)
shuffBlockVec <- function(x) {

  # Calculate lengths of 0s & 1s using run length encoding
  rleres <- rle(x)

  # draw a random vector for shuffling
  vrand <- sample(seq(1, length(rleres$lengths), 1))

  # Recreate the alternating sequence using shuffled lengths
  return(rep(rleres$values[vrand], rleres$lengths[vrand]))
}

#' Rotating callipers algorithm in 2D
#'
#' @description
#' Calculates the minimum oriented bounding box using the
#' rotating callipers algorithm in 2D.
#' Credits go to Daniel Wollschlaeger.
#' The function modified from <http://dwoll.de/rexrepos/posts/diagBounding.html>.
#'
#' @param xy a matrix of xy values from which to calculate the minimum oriented bounding box.
#' @param prec numerical, rounding precision; default 1e-08, lose to .Machine$double.eps^0.5.
#'
#' @return a list width numeric width and height of the bounding box
#' @export
#' @importFrom grDevices chull
#'
#' @examples
#' library(ARCOS)
#' getMinBBox(cbind(c(0,1,3,2,1), c(0,-1,1,3,2)))
getMinBBox2D <- function(xy, prec=1e-08) { # precision close to .Machine$double.eps^0.5
  if(!is.matrix(xy))  { stop("xy must be a matrix") }
  if(!is.numeric(xy)) { stop("xy must be numeric") }
  if(ncol(xy) != 2L)  { stop("xy must have two columns") }

  if(nrow(xy) < 2L)   { stop("xy must have at least two rows") }

  if (nrow(xy) > 1L) {
    ## rotating calipers algorithm using the convex hull
    H    <- chull(xy)                    # hull indices, vertices ordered clockwise
    hull <- xy[H, ]                      # hull vertices

    n <- length(H)                       # number of hull vertices

    ## unit basis vectors for all subspaces spanned by the hull edges
    hDir  <- diff(rbind(hull, hull[1,])) # account for circular hull vertices
    hLens <- sqrt(rowSums(hDir^2))       # length of basis vectors
    huDir <- diag(1/hLens) %*% hDir      # scaled to unit length

    ## unit basis vectors for the orthogonal subspaces
    ## rotation by 90 deg -> y' = x, x' = -y
    ouDir <- cbind(-huDir[ , 2], huDir[ , 1])

    ## project hull vertices on the subspaces spanned by the hull edges, and on
    ## the subspaces spanned by their orthogonal complements - in subspace coords
    projMat <- rbind(huDir, ouDir) %*% t(hull)

    ## range of projections and corresponding width/height of bounding rectangle
    rangeH  <- matrix(numeric(n*2), ncol=2)   # hull edge
    rangeO  <- matrix(numeric(n*2), ncol=2)   # orth subspace
    widths  <- numeric(n)
    heights <- numeric(n)
    for(i in seq(along=H)) {
      rangeH[i, ] <- range(projMat[  i, ])
      rangeO[i, ] <- range(projMat[n+i, ])  # orth subspace is in 2nd half
      widths[i]   <- abs(diff(rangeH[i, ]))
      heights[i]  <- abs(diff(rangeO[i, ]))
    }

    ## extreme projections for min-area rect in subspace coordinates
    areas <- round(widths*heights, digits=prec)
    eMin  <- which.min(areas)   # hull edge leading to minimum-area

    ## box size
    bbWidth  <- widths[eMin]
    bbHeight <- heights[eMin]
  } else {
    bbWidth <- 0
    bbWidth <- 0
  }

  return(list(w = bbWidth,
              h = bbHeight))
}
