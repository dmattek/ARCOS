#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ARCOS: collective events"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("slPtSz", "Point size",
                        min = 1,
                        max = 20,
                        value = 10)
        ),

        mainPanel(
            plotly::plotlyOutput("intTSplot")
        )
    )
)
