#' Track collective events
#'
#' Time series data should be supplied in a long-format data.table with at least 3 columns:
#' frame number, position (1, 2, or 3 columns), and the object id.
#'
#' @param inDT a data.table with time series in the long format with at least 3 columns: integer frame number, object id, object position.
#' @param inEps a float with the search radius, default 1.
#' @param inMinPts an integer with the minimum size of the cluster, default 1L.
#' @param inNprev an integer with the number of previous frames to search for an event, default 1L.
#' @param inCols a list with column names, \code{list(frame = , id = , clid = )}, that correspond to the integer frame number, object id and id of collective events, respectively.
#' @param inPosCols a vector with names of position columns, default \code{c("x")}.
#' @param inDeb logical, whether to output debug information.
#'
#' @return a data.table with cluster numbers and id's of the corresponding objects
#' @export
#' @import
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#' library(ggplot2)
#'
#' dt = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
#'                 id = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
#'                 x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))
#'
#' dtColl = ARCOS::trackCollEvents(dt,
#'                         inEps = 0.6,
#'                         inMinPts = 1L,
#'                         inNprev = 1L,
#'                         inCols = list(frame = "frame",
#'                                       id = "id",
#'                                       clid = "collId"),
#'                         inPosCols = c("x"),
#'                         inDeb = FALSE)
#'
#' dt = merge(dt, dtColl, by = c("frame", "id"))
#'
#' ggplot(dt,
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
                               id = "trackID",
                               clid = "clTrackID"
                             ),
                             inPosCols = c("x"),
                             inDeb = FALSE) {
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

  # String vector with position columns defined as f-n arguments
  locPosColsDefined <- c(
    inCols$x,
    inCols$y,
    inCols$z
  )
  # Check if position columns are present in the input data
  if (length(setdiff(
    inPosCols,
    names(inDT)
  )) > 0) {
    stop("Input data does not have the indicated position columns!")
  }

  # String vector with position columns present in the input data
  locPosColsInDT <- intersect(
    inPosCols,
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

  if (inNprev < 1L | !is.integer(inNprev)) {
    stop("Parameter inNprev has to be an integer greater than 0!")
  }

  # Make a local copy of input data only with necessary frames
  locDT = inDT[,
               c(inCols$frame,
                 inCols$id,
                 locPosColsInDT),
               with = F]

  ## Step 1
  ## Identify spatial clusters in every frame using dbscan

  LOCmydbscan = function(x) {
    locRes = dbscan::dbscan(as.matrix(x),
                            eps = inEps,
                            minPts = inMinPts)
    return(as.integer(locRes$cluster))
  }

  locSclidFrame = paste0(inCols$clid, ".frame")
  locDT[,
        (locSclidFrame) := LOCmydbscan(
          eval(
            parse(text = sprintf("cbind(%s)",
                                 paste(locPosColsInDT,
                                       collapse = ",")
            )
            )
          )
        ),
        by = c(inCols$frame)]

  # Keep only objects with identified clusters, i.e. cl > 0
  locDT = locDT[get(locSclidFrame) > 0]

  # To every cluster number add the cumulated cluster number from previous frame(s).
  # This ensures that clusters identified by dbscan in individual frames
  # have unique cluster number in the entire sequence.
  locDTclAggr = locDT[,
                      .(cl.max = max(get(locSclidFrame))),
                      by = c(inCols$frame)]

  locDTclAggr[,
              cl.max.prev := shift(cumsum(cl.max))]

  locDT = merge(locDT,
                locDTclAggr[,
                            c(inCols$frame,
                              "cl.max.prev"),
                            with = F],
                by = c(inCols$frame))

  locDT[,
        (locSclidFrame) := get(locSclidFrame) +
          ifelse(is.na(cl.max.prev), 0L, cl.max.prev)]

  locDT[,
        cl.max.prev := NULL]

  rm(locDTclAggr)

  # Duplicate the column with cluster numbers per frame;
  # both will be returned in the final table
  locDT[,
        (inCols$clid) := get(locSclidFrame)]


  ## Step 2
  ## Link clusters between frames

  # Main loop over frames; start from the second frame
  for ( iFrame in (sort(unique(locDT[[inCols$frame]])))[-1] ) {

    if (inDeb) {
      cat(sprintf("Frame: %d\n", iFrame))
    }

    # Get positions of all cells in previous frame(s)
    locDTposPrev = locDT[get(inCols$frame) %between% c(iFrame - inNprev,
                                                       iFrame - 1),
                         c(locPosColsInDT),
                         with = F]

    # Proceed if objects found in previous frame(s)
    if (nrow(locDTposPrev) > 0) {

      # Get cluster ids of objects in the current & previous frame(s)
      locVclCurr = locDT[get(inCols$frame) == iFrame][[inCols$clid]]
      locVclPrev = locDT[get(inCols$frame) %between% c(iFrame - inNprev,
                                                       iFrame - 1)][[inCols$clid]]

      # Loop over all clusters in the current frame &
      # search for the closest neighbour in previous frame(s)
      for ( iCl in sort(unique(locVclCurr)) ) {

        if (inDeb) {
          cat(sprintf("   Cluster: %d\n", iCl))
        }
        # Get positions of all objects in a cluster in the current frame
        locDTposCurr = locDT[get(inCols$frame) == iFrame &
                               get(inCols$clid) == iCl,
                             c(locPosColsInDT),
                             with = F]

        # Calculate distances to the nearest neighbour
        # between objects in the current cluster and
        # all objects in previous frame(s)
        locResNN2 = RANN::nn2(locDTposPrev,
                              locDTposCurr,
                              k = 1)

        # Get cluster numbers of all neighbours in previous frame(s)
        locVclPrevNNall = locVclPrev[locResNN2$nn.idx]

        # Get cluster numbers of neighbours within eps in previous frame(s)
        locVclPrevNNeps = locVclPrev[locResNN2$nn.idx[locResNN2$nn.dists <= inEps]]

        # Proceed if there are neighbour clusters in previous frame(s)
        if (length(locVclPrevNNeps) > 0) {

          # Calculate unique cluster numbers
          # of neighbours within eps in previous frame(s)
          locVclPrevNNuni = unique(locVclPrevNNeps)

          # Reassign cluster numbers of the current frame
          # to cluster numbers of neighbours in previous frame(s)

          if (length(locVclPrevNNuni) > 1) {
            # Current cluster is close to more than 1 clusters in previous frame(s)
            locDT[get(inCols$frame) == iFrame &
                    get(inCols$clid) == iCl,
                  (inCols$clid) := locVclPrevNNall]

          } else {
            # Current cluster is close to only 1 cluster in previous frame(s)
            locDT[get(inCols$frame) == iFrame &
                    get(inCols$clid) == iCl,
                  (inCols$clid) := rep(locVclPrevNNuni, nrow(locDTposCurr))]
          }
        }
      }
    }
  }

  # After reassignment, cluster numbers ar not consecutive;
  # make them consecutive here.

  locDT[,
        (inCols$clid) := .GRP,
        by = c(inCols$clid)]

  setorderv(locDT,
            c(inCols$frame,
              inCols$id,
              inCols$clid))

  return(locDT[,
               c(inCols$frame,
                 inCols$id,
                 locSclidFrame,
                 inCols$clid),
               with = F])
}

