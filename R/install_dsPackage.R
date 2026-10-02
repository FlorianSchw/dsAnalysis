#'
#' @title Installs DataSHIELD packages and adds them to the project
#' @param dsPackage name or names of the DataSHIELD server-side packages
#' @param version version of the server package
#' @param ref GitHub branch, tag or commit of the server package
#' @param source GitHub repository of the server package ("owner/repo")
#' @param client name of the client package
#' @param client_version version of the client package
#' @param client_source GitHub repository of the client package ("owner/repo")
#' @export
#'

install_dsPackage <- function(dsPackage = NULL, version = NULL, ref = NULL, source = NULL,
                              client = NULL, client_version = NULL, client_source = NULL){

  if(is.null(dsPackage)){
    stop("No package name has been given.", call. = FALSE)
  }

  options_given <- !all(vapply(list(version, ref, source, client, client_version, client_source), is.null, TRUE))

  if(length(dsPackage) > 1){

    if(options_given){
      stop("Versions, refs, sources and clients can only be given for one package at a time.", call. = FALSE)
    }

    for (p in dsPackage){
      install_dsPackage(p)
    }

    return(invisible(dsPackage))
  }

  packages <- resolve_dsPackage(dsPackage, version = version, ref = ref, source = source,
                                client = client, client_version = client_version,
                                client_source = client_source)

  project <- here::here()
  specs <- c(packages$server$spec, packages$client$spec)

  message("Installing ", paste(specs, collapse = " and "), " ...")
  renv::install(specs, project = project, prompt = FALSE)

  client_name <- if (is.null(packages$client)) NA_character_ else packages$client$package
  add_dsPackage(packages$server$package, client = client_name)

  renv_record(project)

  message("Done: ", paste(c(packages$server$package, stats::na.omit(client_name)), collapse = " and "),
          " installed, added to the DSLite setup and to dependencies.R.")

  invisible(packages)

}
