#' @name runShinyApp
#' @author Joe Zemmels
#' @source https://deanattali.com/2015/04/21/r-package-shiny-app/
#' @title Launch a Shiny App locally
#'
#' @description Launch a Shiny App from the inst/shiny-examples folder in this package
#'
#' @param example name of shiny app to be launched. Current options are: "joe"
#'
#' @examples
#' \dontrun{
#' runShinyApp() #to see a list of launchable apps in the inst/shiny-examples folder
#' runShinyApp("joe") #to launch Joe's shiny app
#'}
#' @import shiny

runShinyApp <- function(example){
  validExamples <- list.files(system.file("shiny-examples",package = "finalProject"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  appDir <- system.file("shiny-examples",example,package = "finalProject")
  shiny::runApp(appDir,display.mode = "normal")
}
