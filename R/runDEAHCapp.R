# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to run the DE_AHC shiny app
#'
#' This function runs the app
#' @import shiny
#' @import tidyverse
#' @import egg
#' @import gridExtra
#' @import magrittr
#' @rawNamespace import(Hmisc, except = c(summarize, src))
#' @return Plot
#' @param ... Any argument that you can pass to shiny::runApp
#' @examples
#' # microRNA.TEvMP()
#' @return Runs the shiny app.
#' @export
runDEAHCapp <- function(...)
{
  appDir <- system.file("app", package = "DE.AHC")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing DE.AHC",
         call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = TRUE, ...)
}
