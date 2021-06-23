#' Plot tracks from an arcosTS object
#'
#' Plot a random selection of tracks in 1 or 2D.
#'
#' @title "Plot tracks"
#' @param obj an arcosTS object.
#' @param nmax an integer, maximum number of tracks to sample; default 20.
#'
#' @return a ggplot2 object.
#'
#' @rdname plotTracks
#' @export plotTracks
#'
#' @examples
#' cat("no examples")
plotTracks <- function(obj, nmax = 20) {
  UseMethod("plotTracks")
}

plotTracks.default <- function(obj, nmax = 20) {
  cat("This is a generic function\n")
}

#' @rdname plotTracks
#' @export plotTracks.arcosTS
#' @export
plotTracks.arcosTS <- function(obj, nmax = 20) {

  stopifnot(is.arcosTS(obj))

  locDim = length(attr(obj, "colPos"))
  if (locDim > 2) {
    cat(sprintf("The arcosTS object has %d spatial dimesions. Only first 2 dimensions will be plotted.\n",
                locDim))
    cat("Use xxx function to plot 3D...\n")
  }

  locVtrackIDuni = unique(obj[[attr(obj, "colIDobj")]])
  locNtracks = length(locVtrackIDuni)

  if (locNtracks > nmax) {
    cat(sprintf("%d tracks in the dataset. Plotting random %d only.\n",
                locNtracks,
                nmax))

    locVtrackIDuni = sample(locVtrackIDuni,
                            nmax)
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


