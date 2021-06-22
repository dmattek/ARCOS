#' Title
#'
#' @param path a string with path to images.
#' @param ext a string with pattern for image search.
#' @param thres a numeric with pixel threshold; default 0.
#'
#' @return an arcosTS object.
#' @export
#' @importFrom OpenImageR readImage
#' @importFrom reshape2 melt
#' @import data.table
#'
#' @examples
#' cat("no examples")
loadDataFromImages <- function(path, ext, thres = 0) {

  # Prepare a list with image files
  vFiles = list.files(path = path,
                      pattern = ext,
                      full.names = T)

  # Load images; store data in a long format in a data.table.
  lIn = lapply(seq_along(vFiles), function (ii) {

    # Image file name
    locFin = vFiles[ii]

    # Read a PNG image
    locM = OpenImageR::readImage(locFin)

    # Convert to long format
    locDT = data.table::as.data.table(reshape2::melt(locM,
                                                     value.name = "m"))
    setnames(locDT,
             c("Var1", "Var2"),
             c("y", "x"))

    # Add "time" and "cellID" columns
    # Keep only pixel values greater than the thres parameter
    locDT[,
          `:=`(m = as.numeric(m),
               IDobj = .I,
               frame = ii)][m > thres]
  })

  new_arcosTS(dt = data.table::rbindlist(lIn),
              colPos = c("y", "x"),
              colFrame = "frame",
              colIDobj = "IDobj",
              colRT = NULL,
              colIDcoll = NULL,
              colMeas = "m",
              interVal = 1,
              interType = "fixed")
}
