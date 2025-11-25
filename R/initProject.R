#'
#' @title Function to initiate a new project
#' @description The function will create a new DataSHIELD analysis project environment
#' @details This function adaptssome things.
#' @param location specifies the location where the project shall be installed
#' @param initiate_git XXXX
#' @return the path of the script_name
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import renv
#' @import usethis
#' @export
#'






initProject <- function(location = NULL,
                        initiate_git = FALSE){

  #### sets up normal R Project
  usethis::create_project(location,
                          open = FALSE)

  #### creates additional folder structure
  dir.create(paste0(location, "/results"))
  dir.create(paste0(location, "/results/tables"))
  dir.create(paste0(location, "/results/figures"))
  dir.create(paste0(location, "/utils"))
  dir.create(paste0(location, "/utils/mock_data"))
  dir.create(paste0(location, "/utils/data_dictionary"))
  dir.create(paste0(location, "/utils/setup"))

  #### copies over standardised R scripts for start
  file.copy(from = find_script("datashield/main.R"),
            to = paste0(location, "/R/main.R"))
  file.copy(from = find_script("datashield/01_DS_Login.R"),
            to = paste0(location, "/R/01_DS_Login.R"))
  file.copy(from = find_script("datashield/02_QualityCheck.R"),
            to = paste0(location, "/R/02_QualityCheck.R"))
  file.copy(from = find_script("datashield/03_DescriptiveStatistics.R"),
            to = paste0(location, "/R/03_DescriptiveStatistics.R"))

  #### copies over placeholder files to keep folder structure in place for GitHub
  #### for folders that should not be shared (e.g. results)
  file.copy(from = find_script("utils/placeholder.txt"),
            to = paste0(location, "/results/tables/placeholder.txt"))
  file.copy(from = find_script("utils/placeholder.txt"),
            to = paste0(location, "/results/figures/placeholder.txt"))

  #### copies over standardised R scripts for DSLite
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"),
            to = paste0(location, "/utils/setup/01_DSLite_Setup.R"))


  #### modify .gitignore file
  gitignore_lines <- c(".Renviron",
                       "results/plots/*",
                       "!results/plots/placeholder.txt",
                       "results/tables/*",
                       "!results/tables/placeholder.txt")

  usethis::write_union(path = paste0(location, "/.gitignore"),
                       lines = gitignore_lines)


  ## Opal_DataDictionary.xlsx
  ## mock_data
  #### mock data files



  #### .Renviron file with project scope setup


  renv::init(project = location)


}
