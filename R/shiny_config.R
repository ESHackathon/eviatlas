#' EviAtlas ShinyApp Configuration file
#' @export

shiny_config <- function() {
  config <- list()
  # maximum upload size 100 MB-- could be increased if proves problematic for users
  config$max_file_size_mb <- 100

  options(shiny.maxRequestSize = config$max_file_size_mb * 1024^2)
}
