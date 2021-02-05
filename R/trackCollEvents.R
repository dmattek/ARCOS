#' Track collective events
#'
#' @param inDT
#' @param inEps
#' @param inMinPts
#' @param inNprev
#' @param inCols
#' @param DEB
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#'
trackCollEvents <- function(inDT,
                            inEps = 1, inMinPts = 1L,
                            inNprev = 1L,
                            inCols = list(
                              frame = "time",
                              x = "x",
                              y = NULL,
                              z = NULL,
                              id = "trackID",
                              collid = "clTrackID"
                            ),
                            DEB = T) {
  ## Checks

  # Check whether inDT is a data.table
  if(!is.data.table(inDT))
    stop("Input data is not a data.table!")

  # Check if inDT isn't NULL and has data
  if (is.null(inDT)) {
    stop("Input data is NULL!")
  }

  if (nrow(inDT) < 1L) {
    warning("Input data has no records! Returning NULL")
    return(NULL)
  }

  # String vector with position colums defined as f-n arguments
  locPosColsDefined <- c(
    inCols$x,
    inCols$y,
    inCols$z
  )
  # Check if position columns are present in the input data
  if (length(setdiff(
    locPosColsDefined,
    names(inDT)
  )) > 0) {
    stop("Input data does not have the indicated position columns!")
  }

  # String vector with position colums present in the input data
  locPosColsInDT <- intersect(
    locPosColsDefined,
    names(inDT)
  )
  if (DEB) {
    cat("Names of position columns for distance calculation:\n")
    print(locPosColsInDT)
  }

  # Check if time column present in the input data
  if (sum(names(inDT) %in% inCols$frame) < 1L) {
    stop("Input data does not have the indicated frame column!")
  }

  # Check if time column present in the input data
  if (sum(names(inDT) %in% inCols$id) < 1L) {
    stop("Input data does not have the indicated id column!")
  }

  if (!(inEps > 0)) {
    stop("Parameter inEps has to be greater than 0!")
  }

  if (inMinPts < 1L | !is.integer(inMinPts)) {
    stop("Parameter inMinPts has to be an integer greater than 0!")
  }


  ## Algorithm
  # Initiate the list of collective events
  locCollEvents <- data.table()

  for (iFrame in sort(unique(inDT[[inCols$frame]]))) {
    if (DEB) {
      cat(sprintf("\nFrame: %d\n", iFrame))
    }

    # get all objects from a frame
    locDTtime <- inDT[get(inCols$frame) == iFrame]

    if (nrow(locDTtime) > 0) {
      if (DEB) {
        cat(sprintf("%d object(s) present\n", nrow(locDTtime)))
      }

      if (nrow(locCollEvents) > 0) {
        if (DEB) {
          cat(sprintf("  collective events already initiated\n"))
        }

        # Obtain objects that belong to collective events in the previous frame(s).
        # NEXT: more testing when looking at more than 1 frame back
        locCollPrev <- locCollEvents[get(inCols$frame) < iFrame &
          get(inCols$frame) > iFrame - 1 - inNprev,
        c(
          locPosColsInDT,
          inCols$collid
        ),
        with = F
        ]

        if (nrow(locCollPrev) > 0) {
          if (DEB) {
            cat(sprintf("    collective events present in the previous frame: link\n"))
          }

          # There are collective events in the previous frame: link to current cells

          # Calculate cartesian product of current objects and
          # objects that belong to collective events in previous frame(s).
          # This will create a table with every current object
          # having a column with all objects that belong to collective events
          # in previous frames. (Equivalently loop)

          # 1. add a dummy column with the current frame number
          # to have something to merge by
          locCollPrev[, (inCols$frame) := iFrame]

          # 2. add .prev suffix to the names of xyz columns
          # such that merge doesn't change column names
          setnames(locCollPrev,
            locPosColsInDT,
            paste0(locPosColsInDT, ".prev"),
            skip_absent = T
          )

          # cartesian product of current objects
          # and objects in previous collective events
          locTmp <- merge(locDTtime,
            locCollPrev,
            by = inCols$frame,
            allow.cartesian = T
          )

          ## Link new tracks to collective events within a distance

          # Prepare an expression to calculate the distance,
          # based on positions columns present in inDT
          locDistForm <- paste0(sprintf(
            "(%s - %s.prev)^2",
            locPosColsInDT, locPosColsInDT
          ),
          collapse = " + "
          )
          locDistForm <- sprintf("sqrt(%s)", locDistForm)

          # Subset rows of objects that are within inEps distance
          # to objects from collective events in the previous frame.
          locTmp[, d := eval(parse(text = locDistForm))]

          setkeyv(locTmp, inCols$id)
          locWithinEps <- locTmp[locTmp[, (d == min(d)) & (d < inEps), by = c(inCols$id)]$V1, ]

          # Sometimes the current object might be equidistant to
          # more than 1 object in a previous cluster. Choose the first match!
          locWithinEps <- locWithinEps[, .SD[1], by = c(inCols$id)]

          # Add objects from the current frame to previous collective events.
          if (nrow(locWithinEps) > 0) {
            locCollEvents <- rbind(
              locCollEvents,
              locWithinEps[,
                c(
                  inCols$frame,
                  inCols$id,
                  locPosColsInDT,
                  inCols$collid
                ),
                with = F
              ]
            )
          }

          ## Tracks outside a distance should form new collective events
          locOutsideEps <- unique(
            locTmp[!(get(inCols$id) %in% unique(locWithinEps[[inCols$id]]))][,
              c(
                inCols$frame,
                inCols$id,
                locPosColsInDT
              ),
              with = F
            ]
          )

          if (nrow(locOutsideEps) > 0) {
            locNewCollEvents <- ARCOS::createCollEvents(
              inDT = locOutsideEps,
              inEps = inEps,
              inMinPts = inMinPts,
              inCols = inCols,
              inClOffset = max(locCollEvents[[inCols$collid]]),
              DEB = DEB
            )

            # add new collective events
            locCollEvents <- rbind(
              locCollEvents,
              locNewCollEvents[,
                c(
                  inCols$frame,
                  inCols$id,
                  locPosColsInDT,
                  inCols$collid
                ),
                with = F
              ]
            )
          }
        } else {
          if (DEB) {
            cat(sprintf("    no collective events in the previous frame: create new\n"))
          }

          # No collective events in the previous frame: create new ones
          locNewCollEvents <- ARCOS::createCollEvents(
            inDT = locDTtime,
            inEps = inEps,
            inMinPts = inMinPts,
            inCols = inCols,
            inClOffset = max(locCollEvents[[inCols$collid]]),
            DEB = DEB
          )

          # add new collective events
          locCollEvents <- rbind(
            locCollEvents,
            locNewCollEvents[,
              c(
                inCols$frame,
                inCols$id,
                locPosColsInDT,
                inCols$collid
              ),
              with = F
            ]
          )
        }
      } else {
        if (DEB) {
          cat(sprintf("  initiating collective events\n"))
        }

        # identify collective events from objects in the current frame
        locNewCollEvents <- ARCOS::createCollEvents(
          inDT = locDTtime,
          inEps = inEps,
          inMinPts = inMinPts,
          inCols = inCols,
          inClOffset = 0,
          DEB = DEB
        )

        locCollEvents <- rbind(
          locCollEvents,
          locNewCollEvents[,
            c(
              inCols$frame,
              inCols$id,
              locPosColsInDT,
              inCols$collid
            ),
            with = F
          ]
        )
      }
    } else {
      if (DEB) {
        cat("no objects\n")
      }
    }
  }

  if (nrow(locCollEvents) > 0) {
    return(locCollEvents[,
      c(
        inCols$frame,
        inCols$id,
        inCols$collid
      ),
      with = F
    ])
  } else {
    return(locCollEvents)
  }
}


#' Identify collective events from objects in the current frame
#'
#' @param inDT
#' @param inCols
#' @param inEps
#' @param inMinPts
#' @param inClOffset
#' @param DEB
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
createCollEvents <- function(inDT,
                             inCols = list(
                               x = "x",
                               y = NULL,
                               z = NULL,
                               frame = "time",
                               id = "trackID",
                               collid = "clTrackID"
                             ),
                             inEps = 1,
                             inMinPts = 1L,
                             inClOffset = 0,
                             DEB = T) {

  locPosColsDefined <- c(
    inCols$x,
    inCols$y,
    inCols$z
  )

  # Check if position columns present in the input data
  if (length(setdiff(
    locPosColsDefined,
    names(inDT)
  )) > 0) {
    stop("Input data does not have the indicated position columns!")
  }

  # Identify position columns in the input data
  locPosColsInDT <- intersect(
    locPosColsDefined,
    names(inDT)
  )

  # Positional clustering of objects
  locDB <- dbscan::dbscan(as.matrix(inDT[,
    c(locPosColsInDT),
    with = F
  ]),
  eps = inEps,
  minPts = inMinPts
  )

  if (sum(locDB$cluster) > 0) {

    # Logical vector that describes whether an object was part of a cluster
    locVclTrue <- locDB$cluster > 0

    if (DEB) {
      cat(
        "    createCollEvents:",
        sum(locVclTrue),
        "object(s) form",
        length(unique(locDB$cluster[locVclTrue])),
        "cluster(s)\n"
      )
    }

    # Select only objects from the input data that belong to clusters
    locCl <- inDT[locVclTrue,
      c(
        inCols$frame,
        inCols$id,
        locPosColsInDT
      ),
      with = F
    ]

    # A a column with cluster numbers;
    # add an offset that corresponds to the max cluster number identified in previous frames
    locCl[, (inCols$collid) := locDB$cluster[locVclTrue] + inClOffset]
  } else {
    if (DEB) {
      cat("    createCollEvents: no clusters\n")
    }

    locCl <- NULL
  }

  return(locCl)
}
