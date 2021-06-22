#' Plot an arcosTS object
#'
#' @param obj an arcosTS object.
#'
#' @return
#' @export
#'
#' @examples
#' cat("no examples")
plot.arcosTS <- function(obj) {

  MAX_PLOT_TRACKS = 20

  stopifnot(is.arcosTS(obj))

  locDim = length(attr(obj, "colPos"))
  if (locDim > 2) {
    cat(sprintf("The arcosTS object has %d spatial dimesions. Only the first 2 dimensions will be plotted.\n",
                locDim))
    cat("Use xxx function to plot 3D...\n")
  }

  locVtrackIDuni = unique(obj[[attr(obj, "colIDobj")]])
  locNtracks = length(locVtrackIDuni)

  if (locNtracks > MAX_PLOT_TRACKS) {
    cat(sprintf("%d tracks in the dataset. Plotting random %d only.\n",
                locNtracks,
                MAX_PLOT_TRACKS))

    locVtrackIDuni = sample(locVtrackIDuni,
                            MAX_PLOT_TRACKS)
  }

  if (locDim == 1) {
    locP = ggplot2::ggplot(obj[get(attr(obj, "colIDobj")) %in% locVtrackIDuni],
                           ggplot2::aes(x = get(attr(obj, "colFrame")),
                                        y = get(attr(obj, "colPos")[1]),
                                        color = as.factor(get(attr(obj, "colIDobj"))),
                                        group = as.factor(get(attr(obj, "colIDobj"))))) +
      ggplot2::xlab("Frame") +
      ggplot2::ylab("Position 1")
  } else {
    locP = ggplot2::ggplot(obj[get(attr(obj, "colIDobj")) %in% locVtrackIDuni],
                           ggplot2::aes(x = get(attr(obj, "colPos")[1]),
                                        y = get(attr(obj, "colPos")[2]),
                                        color = as.factor(get(attr(obj, "colIDobj"))),
                                        group = as.factor(get(attr(obj, "colIDobj"))))) +
      ggplot2::xlab("Position 1") +
      ggplot2::ylab("Position 2")
  }

  locP = locP +
    ggplot2::geom_path() +
    ggplot2::scale_color_discrete("Object ID") +
    ggplot2::theme_bw()

  if (is.null(attr(obj, "colIDcoll"))) {
    locP = locP +
      ggplot2::geom_point()
  } else {
    locP = locP +
      ggplot2::geom_point(ggplot2::aes(shape = as.factor(get(attr(obj, "colIDcoll")))),
                          size = 2) +
      ggplot2::scale_shape_discrete("Collective ID")
  }

  return(locP)
}


