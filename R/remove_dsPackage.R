#'
#' @title Removes DataSHIELD packages from the DSLite setup and dependencies.R
#' @param dsPackage name or names of the DataSHIELD server-side packages to remove
#' @param client name or names of the matching client-side packages
#' @param uninstall also uninstall the packages from the project library
#' @export
#'

remove_dsPackage <- function(dsPackage = NULL, client = NULL, uninstall = FALSE){

  if(is.null(dsPackage)){
    stop("No package name has been given.", call. = FALSE)
  }

  if("dsBase" %in% dsPackage){
    stop("dsBase can't be removed: every DataSHIELD setup needs it.", call. = FALSE)
  }

  if(!is.null(client) && !(length(client) == length(dsPackage))){
    stop("Please provide one client package per DataSHIELD package.", call. = FALSE)
  }

  #### the client package: as given, else as recorded in dependencies.R, else <server>Client
  if(is.null(client)){

    client <- paste0(dsPackage, "Client")
    dependencies_file <- here::here("dependencies.R")

    if(file.exists(dependencies_file)){
      recorded <- dependencies_dsPackages(readLines(con = dependencies_file))
      known <- dsPackage %in% recorded$server
      client[known] <- recorded$client[match(dsPackage[known], recorded$server)]
    }

  }

  setup_file <- here::here("utils/setup", "01_DSLite_Setup.R")
  dslite_setup_codelines <- readLines(con = setup_file)
  included <- dslite_included_packages(dslite_setup_codelines)

  for (p in setdiff(dsPackage, included)){
    message(paste0("The DataSHIELD package ", p, " is not included in the DSLite Setup."))
  }

  dslite_setup_codelines <- dslite_write_included_packages(dslite_setup_codelines,
                                                           setdiff(included, dsPackage))
  dslite_setup_codelines <- dslite_remove_library_calls(dslite_setup_codelines,
                                                        client[!is.na(client)])

  writeLines(text = dslite_setup_codelines, con = setup_file)

  dependencies_set_dsPackages(remove = dsPackage)

  #### renv: uninstall if asked, and record that the project no longer uses them
  project <- here::here()

  if(uninstall){
    renv::remove(c(dsPackage, client[!is.na(client)]), project = project)
  }

  renv_record(project)

  invisible(dsPackage)

}
