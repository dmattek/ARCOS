#' Title
#'
#' @param path a string with path to images.
#' @param ext a string with pattern for image search.
#' @param thres a numeric with pixel threshold; default 0.
#'
#' @return an arcosTS object.
#' @export
#' @importFrom OpenImageR readImage
#' @import data.table
#'
#' @examples
#' cat("no examples")
loadDataFromImages <- function(path, ext, thres = 0) {

  # Prepare a list with image files
  vFiles = sort(list.files(path = path,
                           pattern = ext,
                           full.names = T))

  # Load images; store data in a long format in a data.table.
  lIn = lapply(seq_along(vFiles), function (ii) {

    # Image file name
    locFin = vFiles[ii]

    # Read a PNG image
    locDTwide = data.table::as.data.table(OpenImageR::readImage(locFin))

    # renaming columns
    data.table::setnames(locDTwide, as.character(1:ncol(locDTwide)))

    # Add an index column for later melting
    locDTwide[,
              var1 := .I]

    # melting; avoiding reshape2::melt that handles matrices directly but is deprecated
    locDTlong = data.table::melt(locDTwide,
                                 id.vars = "var1",
                                 value.name = "m")
    setnames(locDTlong,
             c("y", "x", "m"))

    # Add "time" and "cellID" columns
    # Keep only pixel values greater than the thres parameter
    locDTlong[,
              `:=`(x = as.numeric(x),
                   m = as.numeric(m),
                   IDobj = .I,
                   frame = ii)][m > thres]
  })

  new_arcosTS(dt = data.table::rbindlist(lIn),
              colPos = c("y", "x"),
              colFrame = "frame",
              colIDobj = "IDobj",
              colIDcoll = NULL,
              colMeas = "m",
              colMeasResc = NULL,
              colMeasBin = NULL,
              colBootIter = NULL,
              colRT = NULL,
              interVal = 1,
              interType = "fixed",
              fromBin = FALSE,
              fromColl = FALSE,
              fromBoot = FALSE)
}
