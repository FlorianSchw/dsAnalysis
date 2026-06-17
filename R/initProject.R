#'
#' @title Initiate a new project environment dedicated for DataSHIELD analysis
#' @description The function will create a new DataSHIELD analysis project environment
#' @details This function creates a new DataSHIELD analysis project environment that has multiple features:
#' renv is being used to control R versions for analysis, the .Renviron file in combination with the config.yml files are
#' used to switch between a realworld setting connecting to live DataSHIELD servers and a local DSLite environment where testing
#' can be done. Folder structure and some initial R scripts are provided as well to assist the start of DataSHIELD analysis.
#' @param path specifies the path where the project shall be initiated. Defaults to the home folder.
#' @param name name of the new project
#' @param switch_to_proj boolean. Indicates whether to open and switch to the newly created project.
#' @return R Project (not initiated)
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import renv
#' @import fs
#' @import usethis
#' @importFrom utils download.file
#' @export
#'

initProject <- function(path = "home",
                        name = NULL,
                        switch_to_proj = FALSE){

  if (is.null(name)) {
    stop("Please provide a path name for the project to be created.",
         call. = FALSE)
  }

  if (!(is.logical(switch_to_proj))) {
    stop("switch_to_proj has to be logical, i.e. either TRUE or FALSE.",
         call. = FALSE)
  }


  if (path == "home") {

    new_project_path <- paste0(fs::path_expand("~"), "/", name)

  } else {

    new_project_path <- paste0(path, "/", name)

  }

  if (fs::dir_exists(new_project_path)) {
    stop(paste0("The path and name you have provided would overwrite an existing
                directory (", new_project_path, "). Setup aborted."),
         call. = FALSE)
  }

  #### sets up normal R Project
  usethis::create_project(new_project_path,
                          open = FALSE,
                          rstudio = TRUE)

  #### creates additional folder structure
  dir.create(paste0(new_project_path, "/results"))
  dir.create(paste0(new_project_path, "/results/tables"))
  dir.create(paste0(new_project_path, "/results/figures"))
  dir.create(paste0(new_project_path, "/utils"))
  dir.create(paste0(new_project_path, "/utils/mock_data"))
  dir.create(paste0(new_project_path, "/utils/mock_data/demo_obiba"))
  dir.create(paste0(new_project_path, "/utils/data_dictionary"))
  dir.create(paste0(new_project_path, "/utils/setup"))
  dir.create(paste0(new_project_path, "/citations"))

  #### copies over standardised R scripts for start
  file.copy(from = find_script("datashield/main.R"),
            to = paste0(new_project_path, "/R/main.R"))
  file.copy(from = find_script("datashield/01_DS_Login.R"),
            to = paste0(new_project_path, "/R/01_DS_Login.R"))
  file.copy(from = find_script("datashield/02_QualityCheck.R"),
            to = paste0(new_project_path, "/R/02_QualityCheck.R"))
  file.copy(from = find_script("datashield/03_DescriptiveStatistics.R"),
            to = paste0(new_project_path, "/R/03_DescriptiveStatistics.R"))
  file.copy(from = find_script("datashield/99_DSLiteLearning.R"),
            to = paste0(new_project_path, "/R/99_DSLiteLearning.R"))

  #### copies over placeholder files to keep folder structure in place for GitHub
  #### for folders that should not be shared (e.g. results)
  file.copy(from = find_script("utils/placeholder.txt"),
            to = paste0(new_project_path, "/results/tables/placeholder.txt"))
  file.copy(from = find_script("utils/placeholder.txt"),
            to = paste0(new_project_path, "/results/figures/placeholder.txt"))

  #### copies over standardised R scripts for DSLite
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"),
            to = paste0(new_project_path, "/utils/setup/01_DSLite_Setup.R"))


  #### copies over initial config.yml file
  file.copy(from = find_script("utils/config.yml"),
            to = paste0(new_project_path, "/config.yml"))

  #### copies over dependencies file for renv
  file.copy(from = find_script("utils/dependencies.R"),
            to = paste0(new_project_path, "/dependencies.R"))


  #### modify .gitignore file
  gitignore_lines <- c("",
                       ".Renviron",
                       " ",
                       "results/figures/*",
                       "!results/figures/placeholder.txt",
                       "  ",
                       "results/tables/*",
                       "!results/tables/placeholder.txt")

  usethis::write_union(path = paste0(new_project_path, "/.gitignore"),
                       lines = gitignore_lines)


  #### initiate and fill standard .Renviron file

  r_environ_lines <- c("R_CONFIG_ACTIVE = 'production'",
                       "",
                       "OBIBA1_URL = 'https://opal-demo.obiba.org/'",
                       "OBIBA1_USER = 'dsuser'",
                       "OBIBA1_PWD = 'P@ssw0rd'",
                       "OBIBA1_TABLE = 'CNSIM.CNSIM1'",
                       " ",
                       "OBIBA2_URL = 'https://opal-demo.obiba.org/'",
                       "OBIBA2_USER = 'dsuser'",
                       "OBIBA2_PWD = 'P@ssw0rd'",
                       "OBIBA2_TABLE = 'CNSIM.CNSIM2'",
                       "  ",
                       "OBIBA3_URL = 'https://opal-demo.obiba.org/'",
                       "OBIBA3_USER = 'dsuser'",
                       "OBIBA3_PWD = 'P@ssw0rd'",
                       "OBIBA3_TABLE = 'CNSIM.CNSIM3'")

  usethis::write_over(path = paste0(new_project_path, "/.Renviron"),
                      lines = r_environ_lines)


  download.file(url = "https://github.com/datashield/dsBaseClient/raw/master/tests/testthat/data_files/CNSIM/CNSIM1.rda",
                destfile = paste0(new_project_path, "/utils/mock_data/demo_obiba/CNSIM1.rda"))
  download.file(url = "https://github.com/datashield/dsBaseClient/raw/master/tests/testthat/data_files/CNSIM/CNSIM2.rda",
                destfile = paste0(new_project_path, "/utils/mock_data/demo_obiba/CNSIM2.rda"))
  download.file(url = "https://github.com/datashield/dsBaseClient/raw/master/tests/testthat/data_files/CNSIM/CNSIM3.rda",
                destfile = paste0(new_project_path, "/utils/mock_data/demo_obiba/CNSIM3.rda"))

  renv::install("datashield/dsBaseClient")
  renv::install("sofiasiamp/dsSupportClient")
  renv::install("FlorianSchw/dsAnalysis")


  renv::init(project = new_project_path,
             load = switch_to_proj,
             restart = switch_to_proj)


}
