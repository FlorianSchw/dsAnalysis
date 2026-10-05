#'
#' @title Function to add dsPackages to the 01_DSLite_Setup.R file ABCDDE
#' @description XXX
#' @details XXXXXXsssddasdsdfsdasdfsaasdsadfsdfsdf
#' @return adjusted R Script
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @param dsPackage name or names of the DataSHIELD server-side packages to add to DSLite instance
#' @param client name or names of the matching client-side packages
#' @import stringr
#' @export
#'


add_dsPackage <- function(dsPackage = NULL, client = NULL){

  if(is.null(dsPackage)){
    stop("No package name has been given.",call.=FALSE)
  }

  if(is.null(client)){
    client <- paste0(dsPackage, "Client")
  }

  if(!(length(client) == length(dsPackage))){
    stop("Please provide one client package per DataSHIELD package.", call. = FALSE)
  }

  setup_file <- here::here("utils/setup", "01_DSLite_Setup.R")
  dslite_setup_codelines <- readLines(con = setup_file)

  included <- internal_dslite_included_packages(dslite_setup_codelines)

  dupl_int <- dsPackage %in% included
  for (p in dsPackage[dupl_int]){
    message(paste0("The DataSHIELD package ", p, " is already included in the DSLite Setup."))
  }

  #### nothing to add when all packages are already included
  if(all(dupl_int)){
    return(invisible(NULL))
  }

  dsPackage_unique <- dsPackage[!dupl_int]
  client_unique <- client[!dupl_int]

  #### step 1: library calls of the client packages
  dslite_setup_codelines <- internal_dslite_set_library_calls(dslite_setup_codelines,
                                                              add = client_unique[!is.na(client_unique)])

  #### step 4: server packages in the DSLite configuration
  dslite_setup_codelines <- internal_dslite_write_included_packages(dslite_setup_codelines,
                                                                    c(included, dsPackage_unique))

  writeLines(text = dslite_setup_codelines, con = setup_file)

  #### dependencies.R: library calls so that renv tracks server and client packages
  internal_dependencies_set_dsPackages(add = dsPackage_unique, client = client_unique)

  invisible(dsPackage_unique)

}
