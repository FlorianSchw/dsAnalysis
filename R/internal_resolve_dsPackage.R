#### what to install for a DataSHIELD package: the server and the client package,
#### each with its name and renv::install() specification (client is NULL if there is none)
internal_resolve_dsPackage <- function(dsPackage, version = NULL, ref = NULL, source = NULL,
                              client = NULL, client_version = NULL, client_source = NULL){

  catalogue <- internal_ds_catalogue()

  #### server package
  name <- if (is.null(catalogue)) NA_character_ else internal_catalogue_name(dsPackage, catalogue)

  if(!is.null(source)){
    server_src <- list(cran = FALSE, repo = source)
  } else if(!is.na(name)){
    server_src <- internal_catalogue_source(catalogue[[name]])
  } else {
    stop(dsPackage, " is not in the DataSHIELD package catalogue. ",
         "If it is on GitHub, give its repository with source = \"owner/repo\".", call. = FALSE)
  }

  if(is.na(name)){
    name <- dsPackage
  }

  server <- list(package = name, spec = internal_install_spec(name, server_src, version, ref))

  #### client package: as given, else from the catalogue
  if(is.null(client) && !is.null(catalogue)){
    client <- internal_catalogue_client(name, catalogue)
  }

  if(is.null(client) || is.na(client)){
    message("No client package found for ", name, "; only the server package is installed. ",
            "Give client = \"...\" if it has one.")
    return(list(server = server, client = NULL))
  }

  client_name <- if (is.null(catalogue)) NA_character_ else internal_catalogue_name(client, catalogue)

  if(!is.null(client_source)){
    client_src <- list(cran = FALSE, repo = client_source)
  } else if(!is.na(client_name)){
    client_src <- internal_catalogue_source(catalogue[[client_name]])
  } else if(!is.na(server_src$repo)){
    #### same owner as the server package
    client_src <- list(cran = FALSE, repo = paste0(sub("/.*$", "", server_src$repo), "/", client))
  } else {
    stop("No CRAN or GitHub location is known for ", client, ". Give client_source = \"owner/repo\".", call. = FALSE)
  }

  if(is.na(client_name)){
    client_name <- client
  }

  #### the client's version: as given, else the server's version if the client has it, else the latest
  if(is.null(client_version) && !is.null(version)){
    client_spec <- tryCatch(internal_install_spec(client_name, client_src, version),
                            error = function(e) NULL)
    if(is.null(client_spec)){
      message("Version ", version, " was not found for ", client_name, "; its latest version is installed. ",
              "Give client_version = \"...\" to choose one.")
      client_spec <- internal_install_spec(client_name, client_src)
    }
  } else {
    client_spec <- internal_install_spec(client_name, client_src, client_version)
  }

  list(server = server, client = list(package = client_name, spec = client_spec))
}
