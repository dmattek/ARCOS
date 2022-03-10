#' Plot tracks from an arcosTS object
#'
#' Plot tracks in 2D.
#'
#' @title "Plot collective events"
#' @param objTS an arcosTS object.
#' @param xfac a numeric, multiplication factor for the x-axis; default 1.
#' @param yfac a numeric, multiplication factor for the y-axis; default 1.
#' @param tfreq a numeric, frequency of the temporal column; default 1.
#' @param pos an integer, index of the coordinate to plot in the position vector; default 1L.
#' @param colpal a vector with colours in hex format, default colours from the Tableau 20 palette.
#'
#' @return a ggplot2 object.
#'
#' @rdname plotNoodle2D
#' @export plotNoodle2D
#'
#' @examples
#' cat("no examples")
plotNoodle2D <- function(objTS,
                         xfac = 1, yfac = 1,
                         tfreq = 1,
                         pos = 1L,
                         colpal = c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
                                    "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
                                    "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
                                    "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6")) {
  UseMethod("plotNoodle2D")
}

plotNoodle2D.default <- function(objTS,
                                 xfac = 1, yfac = 1,
                                 tfreq = 1,
                                 pos = 1L,
                                 colpal = c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
                                            "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
                                            "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
                                            "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6")) {
  cat("This is a generic function\n")
}

#' @rdname plotNoodle2D
#' @export plotNoodle2D.arcosTS
#' @export
plotNoodle2D.arcosTS = function(objTS,
                                xfac = 1, yfac = 1,
                                tfreq = 1,
                                pos = 1L,
                                colpal = c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
                                           "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
                                           "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
                                           "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6")) {

  stopifnot(is.arcosTS(objTS))

  colFrame <- attr(objTS, "colFrame")
  colIDobj <- attr(objTS, "colIDobj")
  colIDcoll <- attr(objTS, "colIDcoll")
  colPos <- attr(objTS, "colPos")

  # add a grouping column;
  # grouping runs per collective event and per trackid
  # makes it possible to plot paths with interruptions,
  # when the same track id is active at different epochs of time
  objTS[,
        pathgrp := .GRP,
        by = c(colIDcoll,
               colIDobj)]

  setorderv(objTS,
            c(colIDcoll,
              colFrame,
              colIDobj))

  # add grouping per collective event for plotting events with different colours
  objTS[,
        collgrp := rleid(get(colIDcoll))]

  # pad tracks with NAs to allow for breaks if the same track is active
  # in the same event at different time epochs;
  # a track is one trackid in a particular collective
  tcollselMinMax = objTS[,
                         .(as.integer(seq(min(get(colFrame)),
                                          max(get(colFrame)),
                                          tfreq))),
                         by = pathgrp]

  setkeyv(objTS,
          c("pathgrp", colFrame))
  setkey(tcollselMinMax,
         pathgrp, V1)

  # final padded table
  dtPlot = objTS[tcollselMinMax]

  p1 = ggplot(dtPlot,
              aes(x = get(colFrame) * xfac,
                  y = get(colPos[pos]) * yfac,
                  group = pathgrp,
                  color = as.factor(collgrp))) +
    geom_path(size = 0.25, na.rm = T) +
    scale_color_manual(values = rep(rep(colpal,
                                        ceiling(max(objTS[["collgrp"]]) / length(colpal)))))

  return(p1)
}
