#' Run the Shiny application
#'
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Runs the Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' yisasnxlooukup::run_app()
#' }
run_app <- function(...) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the application")
  }
  
  app_dir <- system.file("shinyapp", package = "yisasnxlooukup")
  
  if (app_dir == "" || !dir.exists(app_dir)) {
    pkg_root <- getwd()
    if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
      app_dir <- file.path(pkg_root, "inst", "shinyapp")
    } else {
      parent_dir <- dirname(pkg_root)
      if (file.exists(file.path(parent_dir, "DESCRIPTION"))) {
        app_dir <- file.path(parent_dir, "inst", "shinyapp")
      }
    }
  }
  
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Shiny app not found. Expected at: inst/shinyapp/")
  }
  
  shiny::runApp(app_dir, ...)
}
