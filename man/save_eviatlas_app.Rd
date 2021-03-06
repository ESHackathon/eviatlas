% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_eviatlas_app.R
\name{save_eviatlas_app}
\alias{save_eviatlas_app}
\title{Save a Shiny App to an app.R file.}
\usage{
save_eviatlas_app(
  ui_function = eviatlas::shiny_ui,
  server_function = eviatlas::shiny_server,
  pkgs = c("eviatlas", "tidyverse"),
  out_dir = "shiny",
  overwrite = TRUE,
  run_app = FALSE
)
}
\arguments{
\item{ui_function}{the function for the UI.}

\item{server_function}{the function for the server.}

\item{pkgs}{any packages that need to be loaded for the app to work. At
minimum the package containing the shiny app should be included.}

\item{out_dir}{the directory to save the shiny app files.}

\item{overwrite}{delete the existing out_dir directory if it already exists}

\item{run_app}{whether to run the app once the files are saved}
}
\description{
This function will create three files in the \code{out_dir}: \code{server.R},
\code{ui.R}, and \code{global.R}. The contents of \code{server.R} and
\code{ui.R} will be the source code of the \code{server_function} and
\code{ui_function}, respectively. The \code{global.R} file will only contain
\code{library} calls for \code{shiny} and any other packages specified in
the \code{pkgs} parameter.
}
\details{
If \code{run_app = TRUE} the function will start the Shiny app once the
files are written.
}
