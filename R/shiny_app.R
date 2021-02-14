#' Eviatlas Shiny App
#' @export

eviatlas <- function(study_data, ...) {
  shiny_env <- new.env()
  attr(shiny_env, "name") <- "eviatlas_shiny_env"

  if (!missing(study_data)) {
    assign("study_data", study_data, shiny_env)
  }
  environment(shiny_ui) <- shiny_env
  environment(shiny_server) <- shiny_env
  app <- shiny::shinyApp(
    ui = shiny_ui,
    server = shiny_server,
    onStart = shiny_global
  )
  environment(app) <- shiny_env
  runApp(app, ...)
}
