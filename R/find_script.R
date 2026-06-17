#'
#' @title Internal Function for pathing of files that will be provided
#' @description This is an internal function which will find the correct path for files.
#' @details This function adapts the code from the internal function find_template from the `usethis::use_template` function.
#' @param script_name specifies which script is called.
#' @param package is the name of the package where the files are included ("dsAnalysis").
#' @return the path of the script_name
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import fs
#' @import usethis
#'

find_script <- function(script_name, package = "dsAnalysis") {

  path <- tryCatch(
    fs::path_package(package = package, "templates", script_name),
    error = function(e) ""
  )

  if (identical(path, "")) {
    stop(paste0("Could not find the file: ", script_name,
                ". Please contact the developer team."),
         call. = FALSE)
  }

  path

}
