#' Eviatlas Shiny App
#'
#' @param study_data a dataframe or tibble containing data to be used in an eviatlas shiny app
#' @param ... additional variables to feed into shiny app
#' @export

eviatlas <- function(study_data = NULL, ...) {
  shiny_env <- new.env()
  attr(shiny_env, "name") <- "eviatlas_shiny_env"

  if (!missing(study_data)) {
    assign("study_data", study_data, shiny_env)
  }
  environment(shiny_ui) <- shiny_env
  environment(shiny_server) <- shiny_env
  environment(shiny_config) <- shiny_env

  app <- shiny::shinyApp(
    ui = shiny_ui,
    server = shiny_server,
    onStart = shiny_config
  )

  environment(app) <- shiny_env
  shiny::runApp(app, ...)
}
