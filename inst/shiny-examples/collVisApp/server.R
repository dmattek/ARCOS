#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$intTSplot <- renderPlotly({

        colX = attr(inDataTS, "colPos")[1]
        colY = attr(inDataTS, "colPos")[2]
        colIDobj = attr(inDataTS, "colIDobj")
        colFrame = attr(inDataTS, "colFrame")
        colMeas = attr(inDataTS, "colMeas")
        colIDcoll = attr(inDataColl, "colIDcoll")

        # Stretch inDataColl to the same number of frames as inDataTS; otherwise plotly gets weird
        setkeyv(inDataColl, colFrame)
        inDataColl = inDataColl[setkeyv(inDataColl[,
                                                   .(unique(inDataTS[[colFrame]]))],
                                        c("V1"))]

        locP = plotly::plot_ly(
                x = ~get(colX),
                y = ~get(colY),
                ids = ~get(colIDobj),
                frame = ~get(colFrame),
                text = ~paste('</br> object ID: ', get(colIDobj)),
                hoverinfo = "text"
            ) %>%
            layout(plot_bgcolor='rgb(224, 224, 224)',
                   paper_bgcolor='rgb(224, 224, 224)',
                   xaxis = list(title = "Position X"),
                   yaxis = list(title = "Position Y")) %>%
            add_markers(data = inDataTS,
                        color = ~get(colMeas),
                        marker = list(size=input$slPtSz),
                        alpha = 0.8,
                        colors = rev(RColorBrewer::brewer.pal(10, "RdYlBu"))) %>%
            add_markers(data = inDataColl,
                        color = ~as.factor(get(colIDcoll)),
                        marker = list(size=5),
                        alpha = 1) %>%
            plotly::animation_opts(frame = 100,
                                   easing = "linear",
                                   redraw = FALSE
            )

        locP
    })

}
