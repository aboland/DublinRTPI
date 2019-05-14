#' 
#' Function to run shiny app
#' 
#' 
#' @export
runShiny <- function() {
  appDir <- system.file("shiny-example", package = "dublinRTPI")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}