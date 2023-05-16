#' @export
#' @title run_example_app(example)
#' @param example a string title of the app you want to run in the inst/shiny-examples dir
#' @description run our example app; note you must set up your rprofile to run.
#'
#'
run_example_app <- function(example) {

  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "pdaprules"))
  print(validExamples)
  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")
  print(validExamplesMsg)

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "pdaprules")
  shiny::runApp(appDir, display.mode = "normal")
}
