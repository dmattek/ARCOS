#' Track collective events
#'
#' Time series data should be supplied in a long-format data.table with at least 3 columns:
#' frame number, position (1, 2, or 3 columns), and the object id.
#'
#' @param inDT a data.table with time series in the long format.
#' @param inEps a float with the search radius, default 1.
#' @param inMinPts an integer with the minimum size of the cluster, default 1L.
#' @param inNprev an integer with the number of previous frames to search for an event, default 1L.
#' @param inCols a list with column names, \code{list(frame = , x = , y = , z = , id = , collid = )}, that correspond to the frame number, position, track id's and id's of collective events, respectively.
#' @param inDeb logical, whether to output debug information.
#'
#' @return a data.table with cluster numbers and id's of the corresponding objects
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' require(ggplot2)
#'
#' dt = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
#'                 id = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
#'                 x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))
#'
#'dtColl = trackCollEvents(dt,
#'                         inEps = 0.6,
#'                         inMinPts = 1L,
#'                         inNprev = 1L,
#'                         inCols = list(frame = "frame",
#'                                       x = "x",
#'                                       id = "id",
#'                                       collid = "collId"),
#'                         inDeb = F)
#'
#'dt = merge(dt, dtColl, by = c("frame", "id"))
#'
#'ggplot(dt,
#'       aes(x=x,
#'           y = frame,
#'           color = factor(id),
#'           group = id)) +
#'  geom_path() +
#'  geom_point(aes(shape = as.factor(collId))) +
#'  scale_shape_discrete("Collective id:") +
#'  scale_color_discrete(name = "Object id:") +
#'  theme_bw()
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
                            inDeb = T) {
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

  # String vector with position columns present in the input data
  locPosColsInDT <- intersect(
    locPosColsDefined,
    names(inDT)
  )
  if (inDeb) {
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
    if (inDeb) {
      cat(sprintf("\nFrame: %d\n", iFrame))
    }

    # get all objects from a frame
    locDTtime <- inDT[get(inCols$frame) == iFrame]

    if (nrow(locDTtime) > 0) {
      if (inDeb) {
        cat(sprintf("%d object(s) present\n", nrow(locDTtime)))
      }

      if (nrow(locCollEvents) > 0) {
        if (inDeb) {
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
          if (inDeb) {
            cat(sprintf("    collective events present in the previous frame: link\n"))
          }

          # There are collective events in the previous frame: link to current cells

          # Calculate the Cartesian product of current objects and
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
              inDeb = inDeb
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
          if (inDeb) {
            cat(sprintf("    no collective events in the previous frame: create new\n"))
          }

          # No collective events in the previous frame: create new ones
          locNewCollEvents <- ARCOS::createCollEvents(
            inDT = locDTtime,
            inEps = inEps,
            inMinPts = inMinPts,
            inCols = inCols,
            inClOffset = max(locCollEvents[[inCols$collid]]),
            inDeb = inDeb
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
        if (inDeb) {
          cat(sprintf("  initiating collective events\n"))
        }

        # identify collective events from objects in the current frame
        locNewCollEvents <- ARCOS::createCollEvents(
          inDT = locDTtime,
          inEps = inEps,
          inMinPts = inMinPts,
          inCols = inCols,
          inClOffset = 0,
          inDeb = inDeb
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
      if (inDeb) {
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
#' A helper function for the trackCollEvents function.
#'
#' @param inDT a data.table with time series in the long format.
#' @param inEps a float with the search radius, default 1.
#' @param inMinPts an integer with the minimum size of the cluster, default 1L.
#' @param inClOffset
#' @param inCols a list with column names, \code{list(frame = , x = , y = , z = , id = , collid = )}, that correspond to the frame number, position, track id's and id's of collective events, respectively.
#' @param inDeb logical, whether to output debug information.
#'
#' @return a data.table with cluster numbers and id's of the corresponding objects.
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' require(ggplot2)
#'
#' dtIn <- data.table(
#'   time = rep(0, 5),
#'   id = 1:5,
#'   x = c(1:3, 5:6))
#'
#' dtCalc <- ARCOS::createCollEvents(dtIn,
#'                                   inCols = list(
#'                                     x = "x",
#'                                     y = NULL,
#'                                     z = NULL,
#'                                     frame = "time",
#'                                     id = "id",
#'                                     collid = "collid"
#'                                   ),
#'                                   inEps = 1.01, inMinPts = 1,
#'                                   inClOffset = 0,
#'                                   inDeb = F)
#'
#' ggplot(dtCalc,
#'        aes(x = x,
#'            y = time)) +
#'   geom_point(aes(color = as.factor(id),
#'                  shape = as.factor(collid)),
#'              size = 2) +
#'   scale_color_discrete("Object id:") +
#'   scale_shape_discrete("Collective id:") +
#'   theme_bw()
#'
createCollEvents <- function(inDT,
                             inEps = 1,
                             inMinPts = 1L,
                             inClOffset = 0,
                             inCols = list(
                               x = "x",
                               y = NULL,
                               z = NULL,
                               frame = "time",
                               id = "trackID",
                               collid = "clTrackID"
                             ),
                             inDeb = T) {

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

    if (inDeb) {
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
    if (inDeb) {
      cat("    createCollEvents: no clusters\n")
    }

    locCl <- NULL
  }

  return(locCl)
}
