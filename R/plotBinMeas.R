#' Visualise binarisation
#'
#' Visualise de-trending and visualisation of the measurement column on a random selection of time series.
#'
#' @title "Visualise measurement de-trending and binarisation"
#' @param obj an arcosTS object.
#' @param ntraj an integer with the number of random trajectories to plot. Set to 0 to plot all trajectories; default 16L.
#' @param xfac a numeric with a rescaling factor for the x-axis; default 1.
#' @param measfac a numeric with a rescaling factor for the measurement for visualisation purposes; default 1.
#' @param plotResc logical, plot rescaled trajectory; default TRUE.
#' @param inSeed an integer with the seed for the random number generator, default NULL.
#'
#' @return a ggplot2 object.
#' @import data.table
#'
#' @rdname plotBinMeas
#' @export plotBinMeas
#'
#' @examples
#' cat("no examples")
plotBinMeas <- function(obj,
                        ntraj = 16L,
                        xfac = 1.,
                        measfac = 1.,
                        plotResc = TRUE,
                        inSeed = NULL) {
  UseMethod("plotBinMeas")
}

plotBinMeas.default <- function(obj,
                                ntraj = 16L,
                                xfac = 1.,
                                measfac = 1.,
                                plotResc = TRUE,
                                inSeed = NULL) {
  cat("This is a generic function\n")
}

#' @rdname plotBinMeas
#' @export plotBinMeas.arcosTS
#' @export
plotBinMeas.arcosTS <- function(obj,
                                ntraj = 16L,
                                xfac = 1.,
                                measfac = 1.,
                                plotResc = TRUE,
                                inSeed = NULL) {

  stopifnot(is.arcosTS(obj))

  colIDobj = attr(obj, "colIDobj")
  colFrame = attr(obj, "colFrame")
  colMeas = attr(obj, "colMeas")
  colMeasResc = attr(obj, "colMeasResc")
  colMeasBin = attr(obj, "colMeasBin")

  if (plotResc) {
    vCols = c(colIDobj,
              colFrame,
              colMeas,
              colMeasResc,
              colMeasBin)

    # line thickness
    vScale = c(0.5, 0.5, 2.0)
    names(vScale) = c(colMeas, colMeasResc, colMeasBin)

    # colour scales
    vLabels = c("Original", "Rescaled", "Binarised")
    names(vLabels) = c(colMeas, colMeasResc, colMeasBin)

    vColor = values = c("#4E79A7", "#F28E2B", "#E15759")
  } else {
    vCols = c(colIDobj,
              colFrame,
              colMeas,
              colMeasBin)

    # line thickness
    vScale = c(0.5, 2.0)
    names(vScale) = c(colMeas, colMeasBin)

    # colour scales
    vLabels = c("Original", "Binarised")
    names(vLabels) = c(colMeas, colMeasBin)

    vColor = values = c("#4E79A7", "#E15759")
  }

  if( !(length(intersect(
    vCols,
    attr(obj, "names"))) == length(vCols))) {
    stop("Not enough columns in the data. Did you perform binarisation?")
  }

  if (!is.null(inSeed)) set.seed((inSeed))

  # choose random track IDs
  if (ntraj < 1) {
    vSelTrackID = unique(obj[[colIDobj]])
  } else {
    vSelTrackID = sample(unique(obj[[colIDobj]]), ntraj)
  }

  # create a tmp table for plotting
  dtPlot = obj[get(colIDobj) %in% vSelTrackID,
               vCols,
               with = F]

  # rescale the original measurement column for better visualisation
  dtPlot[,
         (colMeas) := get(colMeas) * measfac]

  # convert meas.bin column from integer to numeric to avoid warning during melting
  dtPlot[,
         (colMeasBin) := as.numeric(get(colMeasBin))]

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

  locP = ggplot2::ggplot(data = dtPlot.long,
                         ggplot2::aes(x = get(colFrame) * xfac,
                                      y = meas.value,
                                      group = meas.source)) +
    ggplot2::geom_path(aes(colour = meas.source,
                           size = meas.source)) +
    ggplot2::scale_size_manual(name = "",
                               values = vScale,
                               guide = "none") +
    ggplot2::facet_wrap(colIDobj) +
    ggplot2::scale_colour_manual(name = "",
                                 values = vColor,
                                 labels = vLabels)

  return(locP)
}
