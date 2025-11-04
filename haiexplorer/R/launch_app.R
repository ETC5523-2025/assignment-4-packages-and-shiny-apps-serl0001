#' Launching the Shiny app
#'
#' Opens the interactive explorer shipped with the package.
#' @export
launch_app <- function(){
  app_dir <- system.file("app", package = utils::packageName())
  if (app_dir == "") stop("App not found. Did you install the package?")
  shiny::runApp(app_dir, display.mode = "normal")
}


