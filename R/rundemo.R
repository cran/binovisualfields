#' Runs shiny applications included in the package
#'
#' \code{rundemo(demo)} runs a shiny application provided in the package
#' @param demo A shiny application name. There are two applications i.e., "app", "app2" included in the package
#' @return the called shiny application in a new browser
#' @examples
#' \dontrun{
#' rundemo("app2.R")
#' }
#' @export
rundemo <- function(demo) {
  # locate all the shiny app examples that exist
  valid_demos <- list.files(system.file("shinydemo", package = "binovisualfields"))
  valid_demo_msg <-
    paste0(
      "Valid demos are: '",
      paste(valid_demos, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(demo) || !nzchar(demo) ||
      !demo %in% valid_demos) {
    stop(
      'Please run `rundemo()` with a valid demo app as an argument.\n',
      valid_demo_msg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shinydemo", demo, package = "binovisualfields")
  shiny::runApp(appDir, display.mode = "normal")
}
