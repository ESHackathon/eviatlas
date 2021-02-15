#' EviAtlas ShinyApp Configuration file
#' @export
shiny_config <- list()
# maximum upload size 100 MB-- could be increased if proves problematic for users
shiny_config$max_file_size_mb <- 100

options(shiny.maxRequestSize = shiny_config$max_file_size_mb * 1024^2)
