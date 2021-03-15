#' Plot collective events in 2D
#'
#' Plot subsequent frames as PNG or PDF files with collective events overlaid as convex hull polygons
#' over the original data. Optionally, a binarised measurement can be plotted as black dots and
#' arbitrary annotations as asterisks.
#'
#' @param inDTobj a data.table with time series in long format; required.
#' @param inDTcoll a data.table with collective events in long format; e.g. created by trackCollEvents; default NULL.
#' @param inDTbin a data.table with a binarised measurement to plot as black dots; default NULL.
#' @param inDTanno a data.table with annotations to plot as asterisks.
#' @param inCols a list with column names, default list(frame = "frame", x = "x", y = "y", id = "trackID", clid = "clTrackID", m = "m", mb = "mb").
#' @param inDirPath a string with the path to save output images, default ".".
#' @param inFileCore a string with a prefix for output image files, default ".".
#' @param inXlim a vector with limits for the x-axis, default c(0, 1024).
#' @param inYlim a vector with limits for the y-axis, default c(0, 1024).
#' @param inPlotWH a vector with width and height of the output image in inches, default c(3,3).
#' @param inRevY logical whether to reverse the y-axis, default TRUE.
#' @param inStretchF logical whether to stretch the output sequence to include frames without objects, default TRUE.
#' @param inGGtheme additional ggplot2 theme definitions, default NULL.
#' @param inPlotType definition of the output image type, either "png" or "pdf".
#'
#' @return does not return any value; saves a sequence of images to the specified folder.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' cat("no example")
plotCollEvents2D = function(inDTobj,
                            inDTcoll,
                            inDTbin = NULL,
                            inDTanno = NULL,
                            inCols = list(frame = "frame",
                                          x = "x",
                                          y = "y",
                                          id = "trackID",
                                          clid = "clTrackID",
                                          m = "m",
                                          mb = "mb"),
                            inDirPath = ".",
                            inFileCore = "",
                            inXlim = c(0, 1024),
                            inYlim = c(0, 1024),
                            inPlotWH = c(3,3),
                            inRevY = TRUE,
                            inStretchF = TRUE,
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
                                  inCols$clid),
                                with = F],
                       by = c(inCols$frame,
                              inCols$id))



  locDTch = locDTcollObj[,
                         .SD[chull(get(inCols$x),
                                   get(inCols$y))],
                         by = c(inCols$frame,
                                inCols$clid)]

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
    locP = ggplot2::ggplot()

    if (nrow(locDTobjF) > 0) {
      locP = locP +
        ggplot2::geom_point(data = locDTobjF,
                            aes(x = .data[[inCols$x]],
                                y = .data[[inCols$y]],
                                color = .data[[inCols$m]]),
                            size = 0.6,
                            alpha = 0.8,
                            shape = 19) +
        ggplot2::scale_color_distiller(palette = "RdYlBu",
                                       limits = locColourLim)
    }

    if (nrow(locDTbinF) > 0) {
      locP = locP +
        ggnewscale::new_scale_color() +
        ggplot2::geom_point(data = locDTbinF,
                            aes(x = .data[[inCols$x]],
                                y = .data[[inCols$y]],
                                color = as.factor(get(inCols$mb))),
                            size = 0.1,
                            alpha = 1,
                            shape = 20,
                            color = "#000000")
    }


    if (nrow(locDTchF) > 0) {
      locP = locP +
        ggnewscale::new_scale_color() +
        ggplot2::geom_polygon(data = locDTchF,
                              aes_string(x = inCols$x,
                                         y = inCols$y,
                                         group = inCols$clid),
                              fill = NA,
                              size = 0.25,
                              color = "#000000")
    }

    if (nrow(locDTannoF) > 0) {
      locP = locP +
        ggnewscale::new_scale_color() +
        ggplot2::geom_point(data = locDTannoF,
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
        ggplot2::scale_y_reverse() +
        ggplot2::coord_fixed(ratio = 1,
                             xlim = inXlim,
                             ylim = rev(inYlim),
                             expand = F)
    } else {
      locP = locP +
        ggplot2::coord_fixed(ratio = 1,
                             xlim = inXlim,
                             ylim = inYlim,
                             expand = F)

    }

    locP = locP +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      inGGtheme

    ggplot2::ggsave(filename = sprintf("%s/%sF%04d.%s", inDirPath, inFileCore, kk, inPlotType),
                    plot = locP,
                    width = inPlotWH[1],
                    height = inPlotWH[2])
  }
}
