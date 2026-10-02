#'
#' @title Lists the DataSHIELD packages in the DataSHIELD package catalogue
#' @param search text to look for in the package names and descriptions
#' @param status statuses to keep, e.g. "production" or c("production", "development")
#' @param refresh read the catalogue again instead of using the copy from this session
#' @export
#'

list_dsPackages <- function(search = NULL, status = NULL, refresh = FALSE){

  catalogue <- ds_catalogue(refresh = refresh)

  if(is.null(catalogue)){
    return(invisible(tibble::tibble()))
  }

  #### one row per package that isn't the client of another one
  clients <- vapply(names(catalogue), catalogue_client, "", catalogue = catalogue)
  packages <- setdiff(names(catalogue), stats::na.omit(clients))

  field <- function(entry, part, name){
    value <- entry[[part]][[name]]
    if (is.null(value)) "" else as.character(value)
  }

  packages_info <- tibble::tibble(
    package = packages,
    client = unname(ifelse(is.na(clients[packages]), "", clients[packages])),
    status = vapply(catalogue[packages], field, "", part = "input", name = "status"),
    github_version = vapply(catalogue[packages], field, "", part = "repo", name = "Version"),
    source = vapply(catalogue[packages], function(entry){
      src <- catalogue_source(entry)
      if (src$cran) "CRAN" else if (!is.na(src$repo)) paste0("GitHub: ", src$repo) else ""
    }, ""),
    description = vapply(catalogue[packages], field, "", part = "input", name = "description")
  )

  packages_info$status[!nzchar(packages_info$status)] <- "unknown"

  if(!is.null(status)){
    packages_info <- packages_info[tolower(packages_info$status) %in% tolower(status), ]
  }

  if(!is.null(search)){
    text <- tolower(paste(packages_info$package, packages_info$client, packages_info$description))
    packages_info <- packages_info[grepl(tolower(search), text, fixed = TRUE), ]
  }

  packages_info <- packages_info[order(packages_info$package), ]

  if(nrow(packages_info) == 0){
    message("No DataSHIELD packages found. Try another search or status.")
  } else {
    message(nrow(packages_info), " DataSHIELD package(s) found. Install one with install_dsPackage(\"",
            packages_info$package[1], "\").")
  }

  packages_info

}
