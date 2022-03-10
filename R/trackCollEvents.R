#' Track collective events
#'
#' Track spatial clusters of objects between frames of a time series. Positions of objects should be supplied
#' in a long-format data.table with at least 3 columns:
#' frame number, position (1, 2, or 3 columns), and the object id.
#' The frame number should be an integer, the object ID may be a string or an integer, and the object position has to be numeric (an integer or a float).
#'
#' Thanks to long format, every frame contains at least one object, however, frames do not need to be consecutive.
#' Objects are searched at least one frame back. Adjust the length of the search history with \code{nPrev}.
#'
#' The algorithm first spatially clusters objects independently in every frame,
#' then propagates cluster numbers to clusters in consecutive frames, if at least one objects is within a threshold distance.
#' Spatial clustering is performed using \code{dbscan::dbscan} and the nearest-neighbour (NN) distance is calculated with the \code{RANN::nn2} function.
#'
#' The algorithm proceeds as follows:
#'
#' spatially cluster objects independently in every frame; the resulting cluster IDs are unique for the entire time sequence
#' **for** every frame \code{i}
#'    search for objects in \code{nPrev} frames
#'    **for** every cluster in the current frame \code{k}
#'       calculate the NN distance between objects in cluster \code{k} and all objects from all clusters in \code{nPrev} frame(s)
#'       **if** the NN distance is below the threshold
#'          change the cluster ID of all objects in cluster \code{k} to that of the closest object in \code{nPrev} frame(s)
#'    **end for** every cluster
#' **end for** every frame
#'
#'
#' @param dt a data.table with time series in the long format with at least 3 columns: integer frame number, object id, object position.
#' @param eps a float with the search radius, default 1.
#' @param minClSz an integer with the minimum size of the cluster, default 1L.
#' @param nPrev an integer with the number of previous frames to search for an event, default 1L.
#' @param cols a list with column names, \code{list(frame = , id = , clid = )}, that correspond to the integer frame number, object id and id of collective events, respectively.
#' @param posCols a vector with names of position columns, default \code{c("x")}.
#' @param deb logical, whether to output debug information.
#'
#' @keywords internal
#' @return a data.table with cluster numbers and id's of the corresponding objects
#' @import data.table
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
#' dtColl = ARCOS:::trackCollEvents(dt,
#'                         eps = 0.6,
#'                         minClSz = 1L,
#'                         nPrev = 1L,
#'                         cols = list(frame = "frame",
#'                                       id = "id",
#'                                       clid = "collId"),
#'                         posCols = c("x"),
#'                         deb = FALSE)
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
trackCollEvents <- function(dt,
                            eps = 1, minClSz = 1L,
                            nPrev = 1L,
                            cols = list(
                              frame = "time",
                              id = "trackID",
                              clid = "clTrackID"),
                            posCols = c("x"),
                            deb = FALSE) {
  ## Checks

  # Check whether dt is a data.table
  if(!is.data.table(dt))
    stop("Input data is not a data.table!")

  # Check if dt isn't NULL and has data
  if (is.null(dt)) {
    stop("Input data is NULL!")
  }

  if (nrow(dt) < 1L) {
    warning("Input data has no records! Returning NULL")
    return(NULL)
  }

  # Check if position columns are present in the input data
  if (length(setdiff(
    posCols,
    names(dt)
  )) > 0) {
    stop("Input data does not have the indicated position columns!")
  }

  # String vector with position columns present in the input data
  locPosColsDT <- intersect(
    posCols,
    names(dt)
  )

  if (deb) {
    cat("Names of position columns for distance calculation:\n")
    print(locPosColsDT)
  }

  # Check if time column present in the input data
  if (sum(names(dt) %in% cols$frame) < 1L) {
    stop("Input data does not have the indicated frame column!")
  }

  # Check if time column present in the input data
  if (sum(names(dt) %in% cols$id) < 1L) {
    stop("Input data does not have the indicated id column!")
  }

  if (!(eps > 0)) {
    stop("Parameter eps has to be greater than 0!")
  }

  if (minClSz < 1L | !is.integer(minClSz)) {
    stop("Parameter minClSz has to be an integer greater than 0!")
  }

  if (nPrev < 1L | !is.integer(nPrev)) {
    stop("Parameter nPrev has to be an integer greater than 0!")
  }

  # Make a local copy of input data only with necessary frames
  locDT = dt[,
             c(cols$frame,
               cols$id,
               locPosColsDT),
             with = F]

  ## Step 1
  ## Identify spatial clusters in every frame using dbscan

  # A wrapper for dbscaen that only returns a vector with cluster numbers
  LOCmydbscan = function(x) {
    locRes = dbscan::dbscan(as.matrix(x),
                            eps = eps,
                            minPts = minClSz)
    return(as.integer(locRes$cluster))
  }

  # Create a new column named the same as the column with cluster IDs but with a ".frame" suffix.
  # This column stores cluster IDs from dbscan; they are unique only within a single frame.
  # Later, the algorithm will link those clusters and re-assign cluster IDs
  locSclidFrame = paste0(cols$clid, ".frame")
  locDT[,
        (locSclidFrame) := LOCmydbscan(
          eval(
            parse(text = sprintf("cbind(%s)",
                                 paste(locPosColsDT,
                                       collapse = ",")
            )
            )
          )
        ),
        by = c(cols$frame)]

  # Keep only objects that belong to clusters identified by dbscan,
  # i.e. column cl > 0
  locDT = locDT[get(locSclidFrame) > 0]

  # To every cluster number add the cumulated cluster number from previous frame(s).
  # This ensures that clusters identified by dbscan in individual frames
  # have unique cluster number across the entire sequence.
  locDTclAggr = locDT[,
                      .(cl.max = max(get(locSclidFrame))),
                      by = c(cols$frame)]

  locDTclAggr[,
              cl.max.prev := shift(cumsum(cl.max))]

  locDT = merge(locDT,
                locDTclAggr[,
                            c(cols$frame,
                              "cl.max.prev"),
                            with = F],
                by = c(cols$frame))

  locDT[,
        (locSclidFrame) := get(locSclidFrame) +
          ifelse(is.na(cl.max.prev), 0L, cl.max.prev)]

  locDT[,
        cl.max.prev := NULL]

  rm(locDTclAggr)

  # Duplicate the column with cluster numbers per frame;
  # will be returned in the final table together with
  # the tracked cluster number.
  locDT[,
        (cols$clid) := get(locSclidFrame)]


  ## Step 2
  ## Link clusters between frames

  # Main loop over frames; start from the second frame
  for ( iFrame in (sort(unique(locDT[[cols$frame]])))[-1] ) {

    if (deb) {
      cat(sprintf("Frame: %d\n", iFrame))
    }

    # Get positions of all cells in previous frame(s)
    # TODO: instead of searching the frame column,
    # search for positions based on the actual time, if such a column is present.
    locDTposPrev = locDT[get(cols$frame) %between% c(iFrame - nPrev,
                                                     iFrame - 1),
                         c(locPosColsDT),
                         with = F]

    # Proceed if objects found in previous frame(s)
    if (nrow(locDTposPrev) > 0) {

      # Get cluster ids of objects in the current & previous frame(s)
      locVclCurr = locDT[get(cols$frame) == iFrame][[cols$clid]]
      locVclPrev = locDT[get(cols$frame) %between% c(iFrame - nPrev,
                                                     iFrame - 1)][[cols$clid]]

      # Loop over all clusters in the current frame &
      # search for the closest neighbour in previous frame(s)
      for ( iCl in sort(unique(locVclCurr)) ) {

        if (deb) {
          cat(sprintf("   Cluster: %d\n", iCl))
        }
        # Get positions of all objects in a cluster in the current frame
        locDTposCurr = locDT[get(cols$frame) == iFrame &
                               get(cols$clid) == iCl,
                             c(locPosColsDT),
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
        locVclPrevNNeps = locVclPrev[locResNN2$nn.idx[locResNN2$nn.dists <= eps]]

        # Proceed if there are neighbour clusters in previous frame(s)
        if (length(locVclPrevNNeps) > 0) {
          # Reassign cluster numbers of the current frame
          # to cluster numbers of neighbours in previous frame(s)

          locDT[get(cols$frame) == iFrame &
                  get(cols$clid) == iCl,
                (cols$clid) := locVclPrevNNall]
        }
      } # end of loop over clusters in the current frame
    }
  } # end of loop over frames

  # After reassignment, cluster numbers ar not consecutive;
  # make them consecutive here.

  locDT[,
        (cols$clid) := .GRP,
        by = c(cols$clid)]

  setorderv(locDT,
            c(cols$frame,
              cols$id,
              cols$clid))

  return(locDT[,
               c(cols$frame,
                 cols$id,
                 locSclidFrame,
                 cols$clid),
               with = F])
}


#' Track collective events
#'
#' Wrapper for the \code{trackCollEvents} function.
#'
#' @title "Track collective events"
#' @param obj an arcosTS object.
#' @param eps a numeric, sets the search radius; default 1.
#' @param minClSz an integer, minimum cluster size; default 1L.
#' @param nPrev an integer, number of previous frames to link; default 1L.
#' @param deb boolean, additional debug output; default FALSE,
#'
#' @return an arcosTS object
#'
#' @import data.table
#'
#' @rdname trackColl
#' @export trackColl
#'
#' @examples
#' library(ARCOS)
#' library(data.table)
#' ts = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
#'                 objid = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
#'                 x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))
#'
#' arcosTS(ts,
#'         colPos = "x",
#'         col = list(Frame = "frame",
#'                    IDobj = "objid",
#'                    RT = NULL,
#'                    IDcoll = NULL),
#'         interVal = 1.,
#'         interType = "fixed")
#'
#' tc = trackColl(ts)
trackColl <- function(obj, eps = 1., minClSz = 1L, nPrev = 1L, deb = FALSE) {
  UseMethod("trackColl")
}

trackColl.default <- function(obj, eps = 1., minClSz = 1L, nPrev = 1L, deb = FALSE) {
  cat("This is a generic function\n")
}

#' @rdname trackColl
#' @export trackColl.arcosTS
#' @export
trackColl.arcosTS <- function(obj, eps = 1., minClSz = 1L, nPrev = 1L, deb = FALSE) {

  stopifnot(is.arcosTS(obj))

  if (!is.null(attr(obj, "colIDcoll"))) {
    stop("The object already has collective events.")
  }

  if ("collid" %in% names(obj)) {
    stop("Check your object. Its colIDcoll attribute is NULL but it contains collid.frame and collid columns.")
  }

  locDT = trackCollEvents(obj,
                          eps = eps,
                          minClSz = minClSz,
                          nPrev = nPrev,
                          cols = list(
                            frame = attr(obj, "colFrame"),
                            id = attr(obj, "colIDobj"),
                            clid = "collid"
                          ),
                          posCols = attr(obj, "colPos"),
                          deb = FALSE)

  if (deb)
    cat("Finished detecting and tracking collective events.\n")

  locDT = merge(locDT,
                obj,
                by = c(attr(obj, "colFrame"),
                       attr(obj, "colIDobj")))

  locDT = new_arcosTS(dt = locDT,
                      colPos = attr(obj, "colPos"),
                      colMeas =  attr(obj, "colMeas"),
                      colFrame = attr(obj, "colFrame"),
                      colRT = attr(obj, "colRT"),
                      colIDobj = attr(obj, "colIDobj"),
                      colIDcoll = "collid",
                      interVal = attr(obj, "interVal"),
                      interType = attr(obj, "interType"))

  locColMeasResc = attr(obj, "colMeasResc")
  if (!is.null(locColMeasResc)) data.table::setattr(locDT, "colMeasResc", locColMeasResc)

  locColMeasBin = attr(obj, "colMeasBin")
  if (!is.null(locColMeasBin)) data.table::setattr(locDT, "colMeasBin", locColMeasBin)

  return(locDT)
}
