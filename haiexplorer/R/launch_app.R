#' Launch the HAI Explorer app
#' @return Invisibly, the shiny app object

launch_app <- function() {
  appdir <- system.file("app", package = utils::packageName())
  shiny::shinyAppFile(file.path(appdir, "app.R"))
}
