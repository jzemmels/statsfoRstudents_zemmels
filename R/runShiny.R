#' @name runShiny
#' @export
#' @author Joe Zemmels
#' @source https://deanattali.com/2015/04/21/r-package-shiny-app/
#'
#' @title Launch the statsfoRstudents shiny app
#'
#' @description Launches the statsfoRstudents shiny app in the active R session
#'
#' @examples
#' runShiny()
#'
#' @import shiny

runShiny <- function() {
  appDir <- system.file("shiny", package = "statsfoRstudents")
  if (appDir == "") {
    stop("Could not find the shiny directory. Try re-installing `statsfoRstudents`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
