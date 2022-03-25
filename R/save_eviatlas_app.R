#' Save a Shiny App to an app.R file.
#'
#' This function will create three files in the \code{out_dir}: \code{server.R},
#' \code{ui.R}, and \code{global.R}. The contents of \code{server.R} and
#' \code{ui.R} will be the source code of the \code{server_function} and
#' \code{ui_function}, respectively. The \code{global.R} file will only contain
#' \code{library} calls for \code{shiny} and any other packages specified in
#' the \code{pkgs} parameter.
#'
#' If \code{run_app = TRUE} the function will start the Shiny app once the
#' files are written.
#'
#' @importFrom utils capture.output
#' @param ui_function the function for the UI.
#' @param server_function the function for the server.
#' @param pkgs any packages that need to be loaded for the app to work. At
#'        minimum the package containing the shiny app should be included.
#' @param overwrite delete the existing out_dir directory if it already exists
#' @param out_dir the directory to save the shiny app files.
#' @param run_app whether to run the app once the files are saved
#' @export
save_eviatlas_app <-
  function(ui_function = eviatlas::shiny_ui,
           server_function = eviatlas::shiny_server,
           pkgs = c("eviatlas", "tidyverse"),
           out_dir = "shiny",
           overwrite = TRUE,
           run_app = FALSE) {
    # https://bryer.org/post/2021-02-12-shiny_apps_in_r_packages/

    server_txt <- capture.output(server_function)
    ui_txt <- capture.output(ui_function)

    # Remove the bytecode and environment info
    server_txt <- server_txt[1:(length(server_txt) - 2)]
    ui_txt <- ui_txt[3:(length(ui_txt) - 3)]

    # Add global.R file
    global_txt <- c("library('shiny')")
    if (length(pkgs) > 0) {
      global_txt <- c(global_txt, paste0("library('", pkgs, "')"))
    }

    if (dir.exists(out_dir)) {
      message(paste0("folder ", out_dir, "/ already exists, deleting and overwriting because parameter overwrite=TRUE"))
      unlink(out_dir, recursive = TRUE)
    }
    dir.create(out_dir)
    # Save the shiny app files
    cat(server_txt, sep = "\n", file = paste0(out_dir, "/server.R"))
    cat(ui_txt, sep = "\n", file = paste0(out_dir, "/ui.R"))
    cat(global_txt, sep = "\n", file = paste0(out_dir, "/global.R"))

    # Start the app
    if (run_app) {
      runApp(appDir = out_dir)
    }
  }
