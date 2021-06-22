#' Plot a histogram of the measurement
#'
#' The histogram can optionally indicate regions defined by lower and upper clipping values or by quantiles.
#'
#' @title "Plot a histogram of the measurement"
#' @param obj an arcosTS object.
#' @param clip a two-element vector with lower and upper measurement values (if quant is FALSE) or quantiles (if quant is TRUE) to indicate clipping, default NULL.
#' @param quant a logical to indicate whether the clip parameter is treated as lower and upper bounds of the measurement or as quantiles, default FALSE.
#'
#' @return a ggplot2 object.
#'
#' @rdname histMeas
#' @export histMeas
#'
#' @examples
#' cat("no examples")
histMeas <- function(obj, clip = NULL, quant = FALSE) {
  UseMethod("histMeas")
}

histMeas.default <- function(obj, clip = NULL, quant = FALSE) {
  cat("This is a generic function\n")
}

#' @rdname histMeas
#' @export histMeas.arcosTS
#' @export
histMeas.arcosTS <- function(obj, clip = NULL, quant = FALSE) {

  stopifnot(is.arcosTS(obj))

  colFrame = attr(obj, "colFrame")
  colIDobj = attr(obj, "colIDobj")
  colMeas = attr(obj, "colMeas")

  if (is.null(colFrame))
    stop("Frame number column not defined in the data.")

  if (is.null(colIDobj))
    stop("Object/track identifier column not defined in the data.")

  if (is.null(colMeas))
    stop("Measurememnt column not defined in the data.")


  locDens = density(obj[[colMeas]])
  locDTdens = data.table(x = locDens$x,
                         y = locDens$y)

  locP = ggplot2::ggplot(locDTdens,
                         ggplot2::aes(x,y)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(label = "Distribution of the measurement") +
    ggplot2::xlab("Measurement") +
    ggplot2::ylab("Density")

  if (!is.null(clip)) {

    if (quant) {
      locQuantVal = clip

      # Calculate clipping values due to quantiles
      locClipVal = quantile(obj[[colMeas]], locQuantVal)

    } else {
      locClipVal = clip

      # Calculate quantiles due to clipping values
      locECDF = ecdf(obj[[colMeas]])
      locQuantVal = c(locECDF(locClipVal[1]),
                      locECDF(locClipVal[2]))
    }

    # Add a column with integers that correspond to regions due to quantiles
    locDTdens[, quant := factor(findInterval(x, locClipVal))]

    locP = locP +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = 0,
                                        ymax = y,
                                        fill = quant)) +
      ggplot2::geom_vline(xintercept = locClipVal, color = "grey10", linetype = "dashed") +
      ggplot2::scale_fill_brewer(name = "Quantiles:",
                                 labels = c(sprintf("[0, %.3f)", locQuantVal[1]),
                                            sprintf("[%.3f, %.3f]", locQuantVal[1], locQuantVal[2]),
                                            sprintf("(%.3f, 1]", locQuantVal[2])))
  }

  return(locP)
}


#' Clip the measurement
#'
#' @title "Clip the measurement"
#' @param obj an arcosTS object.
#' @param clip a two-element vector with lower and upper measurement values (if quant is FALSE) or quantiles (if quant is TRUE) for clipping.
#' @param quant a logical to indicate whether the clip parameter is treated as lower and upper bounds of the measurement or as quantiles, default FALSE.
#'
#' @return an arcosTS object with a modified measurement column.
#'
#' @rdname clipMeas
#' @export clipMeas
#'
#' @examples
#' cat("no examples")
clipMeas <- function(obj, clip, quant = FALSE) {
  UseMethod("clipMeas")
}

clipMeas.default <- function(obj, clip, quant = FALSE) {
  cat("This is a generic function\n")
}

#' @rdname clipMeas
#' @export clipMeas.arcosTS
#' @export
clipMeas.arcosTS <- function(obj, clip, quant = FALSE) {

  stopifnot(is.arcosTS(obj))
  stopifnot(length(clip) == 2)

  colMeas = attr(obj, "colMeas")

  if (is.null(colMeas))
    stop("Measurememnt column not defined in the data.")

  if (quant)
    locClipVal = quantile(obj[[colMeas]], clip)

  obj[,
      (colMeas) := rcpp_clip(get(colMeas),
                             a = locClipVal[1],
                             b = locClipVal[2])]

  invisible(obj)
}
