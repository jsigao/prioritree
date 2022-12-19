if (!requireNamespace("shiny", quietly = T)) {
  stop("prioritree requires shiny to be installed.")
}

#' Main function that launches the Shiny app (browser interface)
#' @import shiny
#' @export
launchPrioriTree <- function() {
  options(warn = -1)
  options(shiny.maxRequestSize = 16384*1024^3)
  
  # Create Shiny app
  appDir <- system.file("app", package = "PrioriTree")
  shiny::runApp(appDir, display.mode = "normal")
}