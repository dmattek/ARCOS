#' Plot a histogram of track lengths
#' @title "Plot a histogram of track lengths
#' @param obj an arcosTS object.
#'
#' @return a ggplot2 object.
#'
#' @rdname histTrackLen
#' @export histTrackLen
#'
#' @examples
#' cat("no examples")
histTrackLen <- function(obj, binwidth = NULL) {
  UseMethod("histTrackLen")
}

histTrackLen.default <- function(obj, binwidth = NULL) {
  cat("This is a generic function\n")
}

#' @rdname histTrackLen
#' @export histTrackLen.arcosTS
#' @export
histTrackLen.arcosTS <- function(obj, binwidth = NULL) {

  stopifnot(is.arcosTS(obj))

  colIDobj = attr(obj, "colIDobj")
  colFrame = attr(obj, "colFrame")

  dtTrackLength =  obj[,
                       .(trackStart = min(get(colFrame), na.rm = T),
                         trackEnd = max(get(colFrame), na.rm = T)),
                       by = c(colIDobj)]
  dtTrackLength[, trackLen := trackEnd - trackStart + 1]

  locP = ggplot2::ggplot(dtTrackLength,
                         ggplot2::aes(x = trackLen,
                                      y = stat(density * width))) +
    ggplot2::geom_histogram(binwidth = binwidth,
                            boundary = 0,
                            closed = "left",
                            position = "identity",
                            alpha = 0.5,
                            color = "grey50") +
    ggplot2::scale_fill_discrete("Track type:") +
    ggplot2::ggtitle(label = "Histogram of track lengths") +
    ggplot2::xlab("Track length [frames]") +
    ggplot2::ylab("Probability")

  return(locP)
}


#' Select track lengths
#'
#' @param obj an arcosTS object.
#'
#' @return an arcosTS object.
#' @import data.table
#'
#' @rdname selTrackLen
#' @export selTrackLen
#'
#' @examples
#' cat("no examples")
selTrackLen <- function(obj, lenmin = NULL, lenmax = NULL) {
  UseMethod("selTrackLen")
}

selTrackLen.default <- function(obj, lenmin = NULL, lenmax = NULL) {
  cat("This is a generic function\n")
}

#' @rdname selTrackLen
#' @export selTrackLen.arcosTS
#' @export
selTrackLen.arcosTS <- function(obj, lenmin = NULL, lenmax = NULL) {

  stopifnot(is.arcosTS(obj))
  stopifnot( !(is.null(lenmin) & is.null(lenmax)) )

  colIDobj = attr(obj, "colIDobj")
  colFrame = attr(obj, "colFrame")

  dtTrackLength =  obj[,
                       .(trackStart = min(get(colFrame), na.rm = T),
                         trackEnd = max(get(colFrame), na.rm = T)),
                       by = c(colIDobj)]
  dtTrackLength[, trackLen := trackEnd - trackStart + 1]

  if (is.null(lenmin)) {
    obj = obj[get(colIDobj) %in% unique(dtTrackLength[trackLen <= lenmax][[colIDobj]])]
  } else if (is.null(lenmax)) {
    obj = obj[get(colIDobj) %in% unique(dtTrackLength[trackLen >= lenmin][[colIDobj]])]
  } else {
    obj = obj[get(colIDobj) %in% unique(dtTrackLength[trackLen %between% c(lenmin, lenmax)][[colIDobj]])]
  }

  return(obj)
}

