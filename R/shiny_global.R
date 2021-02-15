#' The Shiny Global file
#' @export
shiny_global <- function() {
  # load help text
  start_text <- readr::read_file(system.file("html_shiny", "AboutEvi.html", package = "eviatlas"))
  about_sysmap_text <- readr::read_file(system.file("html_shiny", "AboutSysMap.html", package = "eviatlas"))
  how_cite_text <- readr::read_file(system.file("html_shiny", "HowCiteEvi.html", package = "eviatlas"))
  how_works_text <- readr::read_file(system.file("html_shiny", "HowEviWorks.html", package = "eviatlas"))

  # maximum upload size 100 MB-- could be increased if proves problematic for users and we have server space
  max_file_size_mb <- 100
  options(shiny.maxRequestSize = max_file_size_mb * 1024^2)
}
