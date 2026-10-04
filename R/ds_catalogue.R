#### Where DataSHIELD packages are installed from: the DataSHIELD package
#### catalogue (https://packages.datashield.org, built from
#### FederatedMethods/packages) gives each package's CRAN or GitHub location.

#### the catalogue, read once per session
dsAnalysis_cache <- new.env(parent = emptyenv())

#### the catalogue as a named list (one entry per package); NULL if it can't be read
ds_catalogue <- function(refresh = FALSE){

  url <- getOption("dsAnalysis.catalogue", "https://packages.datashield.org/packages.json")

  if(!refresh && !is.null(dsAnalysis_cache$catalogue)){
    return(dsAnalysis_cache$catalogue)
  }

  catalogue <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
                        error = function(e) NULL)

  if(is.null(catalogue)){
    message("The DataSHIELD package catalogue (", url, ") could not be read. ",
            "Give the package's GitHub repository with source = \"owner/repo\".")
    return(NULL)
  }

  dsAnalysis_cache$catalogue <- catalogue
  catalogue
}
