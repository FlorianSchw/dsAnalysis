#'
#' @title Internal Function for pathing of the building blocks for the template
#' @description This is an internal function which will find the correct path for the buildingblocks.
#' @details This function adapts the code from the internal function find_template from the `usethis::use_template` function.
#' @param script_name specifies which script is called.
#' @param package is the name of the package where the buildingblocks are included ("dsAnalysis").
#' @return the path of the script_name
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import fs
#' @import usethis
#'


find_script <- function(script_name, package = "dsAnalysis"){
  #usethis:::check_installed(package)
  path <- tryCatch(
    fs::path_package(package = package, "templates", script_name),
    error = function(e) ""
  )
  if (identical(path, "")){
    usethis::ui_stop(
      "Could not find buildingblock {ui_value(buildingblock_name)} \\
      in package {ui_value(package)}."
    )
  }
  path
}
