#' Shuffle coordinates
#'
#' Shuffle spatial coordinates of all tracks in the dataset.
#' Time points and the measurement remain intact.
#'
#' First, initial positions of all tracks are shuffled between the objects.
#' Then, original displacements are used to calculate new coordinates with
#' respect to the new origin. This approach of coordinate shuffling is
#' useful for bootstrapping of data where objects don't change spatial
#' coordinates over time. Otherwise, the trajectories with
#' new initial spatial coordinates go outside of the FOV over time.
#'
#' @title "Shuffle coordinates"
#' @param obj an arcosTS object without collective events.
#'
#' @return an arcosTS object
#'
#' @rdname shuffCoord
#' @export shuffCoord
#'
#' @examples
#' cat("no examples")
#'
shuffCoord <- function(obj) {
  UseMethod("shuffCoord")
}

shuffCoord.default <- function(obj) {
  cat("This is a generic function\n")
}

#' @rdname shuffCoord
#' @export shuffCoord.arcosTS
#' @export
shuffCoord.arcosTS <- function(obj) {
  stopifnot(is.arcosTS(obj))

  if (!is.null(attr(obj, "colIDcoll"))) {
    stop("The object already has collective events.")
  }

  if ("collid" %in% names(obj)) {
    stop("Check your object. Its colIDcoll attribute is NULL but it contains collid.frame and collid columns.")
  }

  # extract object ID (e.g., track_id) from the input object
  locColIDobj = attr(obj, "colIDobj")

  # extract position columns from the input object
  locColPos = attr(obj, "colPos")

  # create column names for tracks' initial positions
  locColPos0 = paste0(locColPos, "_0")

  # create column names for tracks' displacements
  locColPosDiff = paste0(locColPos, "_diff")


  ## Create a new DT with shuffled start positions for every track

  # Get coordinates in the first frame for every track
  locFirstPos = obj[,
                    first(.SD),
                    by = c(locColIDobj),
                    .SDcols = locColPos]

  # Keep a vector of object IDs
  locObjID = locFirstPos[[locColIDobj]]

  # Shuffle the rows
  locFirstPos = locFirstPos[sample(seq_len(nrow(locFirstPos)))]

  # Re-assign object IDs to shuffled positions
  locFirstPos[,
              (locColIDobj) := locObjID]

  # Rename the columns; needed before the merge
  setnames(locFirstPos,
           c(locColIDobj, locColPos0))

  # Merge with the existing obj
  locObj = obj[locFirstPos, on = locColIDobj]

  # Re-set object attributes
  locObj = new_arcosTS(dt = locObj,
                       colPos = attr(obj, "colPos"),
                       colFrame = attr(obj, "colFrame"),
                       colIDobj = attr(obj, "colIDobj"),
                       colIDcoll = attr(obj, "colIDcoll"),
                       colMeas =  attr(obj, "colMeas"),
                       colMeasResc =  attr(obj, "colMeasResc"),
                       colMeasBin =  attr(obj, "colMeasBin"),
                       colBootIter = attr(obj, "colBootIter"),
                       colRT = attr(obj, "colRT"),
                       interVal = attr(obj, "interVal"),
                       interType = attr(obj, "interType"),
                       fromBin = attr(obj, "fromBin"),
                       fromColl = attr(obj, "fromColl"),
                       fromBoot = attr(obj, "fromBoot"))

  # Calculate the displacement of every object in consecutive frames;
  # the first element of the diff vector is the new random origin;
  # loop over every spatial coordinate (i.e., column) separately
  for (ii in seq_along(locColPos)) {
    locObj[,
           (locColPosDiff[ii]) := c(first(get(locColPos0[ii])),
                                    diff(get(locColPos[ii]))),
           by = c(locColIDobj)]
  }

  # Calculate new positions with respect to new coordinate origins;
  # use existing displacements
  locObj[,
         (locColPos) := lapply(.SD, cumsum),
         by = c(locColIDobj),
         .SDcols = locColPosDiff]

  # Names of columns that will be kept in the output
  locColKeep = setdiff(names(locObj),
                       c(locColPos0, locColPosDiff))

  locObj = locObj[,
                  c(locColKeep),
                  with = F]

  # Add meas.bin.shuff column for compatibility with other randomisation methods
  locObj[,
         (paste0(attr(obj, "colMeasBin"), '.shuff')) := get(attr(obj, "colMeasBin"))]

  return(locObj)
}
