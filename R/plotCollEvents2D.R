#' Plot collective events in 2D
#'
#' Plot subsequent frames as PNG or PDF files with collective events overlaid as convex hull polygons
#' over the original data. Optionally, a binarised measurement can be plotted as black dots and
#' arbitrary annotations as asterisks.
#'
#' @param inDTobj a data.table with objects in the long format.
#' @param inDTcoll
#' @param inDTbin
#' @param inDTanno
#' @param inCols
#' @param inDirPath
#' @param inFileCore
#' @param inXlim
#' @param inYlim
#' @param inPlotWH
#' @param inRevY
#' @param inStretchF
#' @param inGGtheme
#' @param inPlotType
#'
#' @return
#' @export
#'
#' @examples
plotCollEvents2D = function(inDTobj,
                            inDTcoll,
                            inDTbin = NULL,
                            inDTanno = NULL,
                            inCols = list(frame = "frame",
                                          x = "x",
                                          y = "y",
                                          id = "trackID",
                                          collid = "clTrackID",
                                          m = "m",
                                          mb = "mb"),
                            inDirPath = ".",
                            inFileCore = "",
                            inXlim = c(0, 1024),
                            inYlim = c(0, 1024),
                            inPlotWH = c(3,3),
                            inRevY = T,
                            inStretchF = T,
                            inGGtheme = NULL,
                            inPlotType = c("png", "pdf")) {

  if (!dir.exists(inDirPath))
    dir.create(inDirPath, recursive = T)

  inPlotType = match.arg(inPlotType)

  # Check whether input is DT
  # Check whether specified column names exist in DTs

  # Stretch frames to include frames without objects.
  # Frames are stretched to a sequence with period 1
  if (inStretchF) {
    setkeyv(inDTobj,
            c(inCols$frame))

    inDTobj = inDTobj[setkeyv(inDTobj[,
                                      .(seq(min(inDTobj[[inCols$frame]], na.rm = T),
                                            max(inDTobj[[inCols$frame]], na.rm = T),
                                            1))],
                              c('V1'))]
  }

  ## Calculate convex hulls around collective events
  # Add selected cluster trackid's to the original table
  locDTcollObj = merge(inDTobj,
                       inDTcoll[,
                                c(inCols$frame,
                                  inCols$id,
                                  inCols$collid),
                                with = F],
                       by = c(inCols$frame,
                              inCols$id))



  locDTch = locDTcollObj[,
                         .SD[chull(get(inCols$x),
                                   get(inCols$y))],
                         by = c(inCols$frame,
                                inCols$collid)]

  # Calculate min/max for the colour scale
  # Thanks to this, the colour scale will stay the same through all frames
  locColourLim = c(min(inDTobj[[inCols$m]]),
                max(inDTobj[[inCols$m]]))

  for (kk in unique(inDTobj[[inCols$frame]])) {

    # DT with all objects in a frame
    locDTobjF = inDTobj[get(inCols$frame) == kk]

    # DT with active objects
    if (!is.null(inDTbin)) {
      locDTbinF = inDTbin[get(inCols$frame) == kk]
    }

    # DT with convex hulls of collective events in a frame
    locDTchF = locDTch[get(inCols$frame) == kk]

    # DT with manual annotations
    if (!is.null(inDTanno)) {
      locDTannoF = inDTanno[get(inCols$frame) == kk]
    }

    #ii = 1
    locP = ggplot()

    if (nrow(locDTobjF) > 0) {
      locP = locP +
        geom_point(data = locDTobjF,
                   aes(x = get(inCols$x),
                       y = get(inCols$y),
                       color = get(inCols$m)),
                   size = 0.6,
                   alpha = 0.8,
                   shape = 19) +
        scale_color_distiller(palette = "RdYlBu",
                              limits = locColourLim)
    }

    if (nrow(locDTbinF) > 0) {
      locP = locP +
        ggnewscale::new_scale_color() +
        geom_point(data = locDTbinF,
                   aes(x = get(inCols$x),
                       y = get(inCols$y),
                       color = as.factor(get(inCols$mb))),
                   size = 0.1,
                   alpha = 1,
                   shape = 20,
                   color = "#000000")
    }


    if (nrow(locDTchF) > 0) {
      locP = locP +
        ggnewscale::new_scale_color() +
        geom_polygon(data = locDTchF,
                     aes_string(x = inCols$x,
                                y = inCols$y,
                                group = inCols$collid),
                     fill = NA,
                     size = 0.25,
                     color = "#000000")
    }

    if (nrow(locDTannoF) > 0) {
      locP = locP +
        ggnewscale::new_scale_color() +
        geom_point(data = locDTannoF,
                   aes_string(x = inCols$x,
                              y = inCols$y),
                   size = 2,
                   stroke = 1,
                   alpha = 0.8,
                   color = "#404040",
                   shape = 4)
    }

    if (inRevY) {
      locP = locP +
        scale_y_reverse() +
        coord_fixed(ratio = 1,
                    xlim = inXlim,
                    ylim = rev(inYlim),
                    expand = F)
    } else {
      locP = locP +
        coord_fixed(ratio = 1,
                    xlim = inXlim,
                    ylim = inYlim,
                    expand = F)

    }

    locP = locP +
      xlab(NULL) +
      ylab(NULL) +
      inGGtheme

    ggsave(filename = sprintf("%s/%sF%04d.%s", inDirPath, inFileCore, kk, inPlotType),
           plot = locP,
           width = inPlotWH[1],
           height = inPlotWH[2])
  }
}
