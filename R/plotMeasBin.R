#' Visualise measurement binarisation
#'
#' @title "Visualise measurement binarisation"
#' @param obj an arcosTS object.
#'
#' @return a ggplot2 object.
#' @import data.table
#'
#' @rdname plotBinMeas
#' @export plotBinMeas
#'
#' @examples
#' cat("no examples")

plotBinMeas <- function(obj, ntraj = 16, measfac = 1.) {
  UseMethod("plotBinMeas")
}

plotBinMeas.default <- function(obj, ntraj = 16, measfac = 1.) {
  cat("This is a generic function\n")
}

#' @rdname plotBinMeas
#' @export plotBinMeas.arcosTS
#' @export
plotBinMeas.arcosTS <- function(obj, ntraj = 16, measfac = 1.) {

  stopifnot(is.arcosTS(obj))

  colIDobj = attr(obj, "colIDobj")
  colFrame = attr(obj, "colFrame")
  colMeas = attr(obj, "colMeas")
  colMeasResc = attr(obj, "colMeasResc")
  colMeasBin = attr(obj, "colMeasBin")

  vCols = c(colIDobj,
            colFrame,
            colMeas,
            colMeasResc,
            colMeasBin)

  if( !(length(intersect(
    vCols,
    attr(obj, "names"))) == length(vCols))) {
    stop("Not enough columns in the data. Did you perform binarisation?")
  }

  # choose random track IDs
  vSelTrackID = sample(unique(obj[[colIDobj]]), ntraj)

  dtPlot = obj[get(colIDobj) %in% vSelTrackID,
               vCols,
               with = F]

  # rescale the original measurement column for better visualisation
  dtPlot[,
         (colMeas) := get(colMeas) * measfac]

  # convert to long format for easy plotting
  dtPlot.long = data.table::melt(dtPlot,
                                 id.vars = c(colIDobj,
                                             colFrame),
                                 variable.name = "meas.source",
                                 value.name = "meas.value")

  # remove 0's from binarised time points to draw segments only
  dtPlot.long = dtPlot.long[meas.source == colMeasBin,
                            meas.value := ifelse(meas.value > 0,
                                                 meas.value,
                                                 NA)]

  vScale = c(0.5, 0.5, 2.0)
  names(vScale) = c(colMeas, colMeasResc, colMeasBin)

  vLabels = c("Original", "Rescaled", "Binarised")
  names(vLabels) = c(colMeas, colMeasResc, colMeasBin)

  locP = ggplot2::ggplot(data = dtPlot.long,
                         ggplot2::aes(x = get(colFrame),
                                      y = meas.value,
                                      group = meas.source)) +
    ggplot2::geom_path(aes(colour = meas.source,
                           size = meas.source)) +
    ggplot2::scale_size_manual(name = "",
                               values = vScale,
                               guide = F) +
    ggplot2::facet_wrap(colIDobj) +
    ggplot2::scale_colour_manual(name = "",
                                 values = c("#4E79A7", "#F28E2B",
                                            "#E15759", "#76B7B2",
                                            "#59A14F", "#EDC948",
                                            "#B07AA1", "#FF9DA7",
                                            "#9C755F", "#BAB0AC"),
                                 labels = vLabels) +
    ggplot2::xlab("Frame") +
    ggplot2::ylab("") +
    ggplot2::theme_bw()

  return(locP)
}
