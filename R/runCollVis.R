#' Shiny app to visualise collective events
#'
#' @param objTS an arcosTS object with time series.
#' @param objColl an arcosTS object with collective events.
#' @param ... additional parameters passed to shiny::runApp.
#'
#' @export
#' @examples
#' cat("no examples")
runCollVis <- function(objTS, objColl, ...) {

  # based on:
  # https://stackoverflow.com/a/48831832/1898713
  # https://stackoverflow.com/a/49472133/1898713

  if(!is.arcosTS(objTS) | !is.arcosTS(objColl)) {
    stop("Both input argument have to be arcosTS objects.", call. = FALSE)
  }

  appDir <- system.file("shiny-examples", "collVisApp", package = "ARCOS")
  if (!nzchar(appDir)) {
    stop("Could not find example directory. Try re-installing `ARCOS`.", call. = FALSE)
  }

  # avoid NOTE about undefined globals
  ui <- server <- NULL

  source(file.path(appDir, "ui.R"), local = TRUE)
  source(file.path(appDir, "server.R"), local = TRUE)
  server_env <- environment(server)

  # variables that the shinyServer can find
  server_env$test_param <- "test"
  server_env$inDataTS <- objTS
  server_env$inDataColl <- objColl

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app, display.mode = "normal", ...)
}
