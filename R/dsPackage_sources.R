#### Where DataSHIELD packages are installed from: the DataSHIELD package
#### catalogue (https://packages.datashield.org, built from
#### FederatedMethods/packages) gives each package's CRAN or GitHub location.

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

#### the catalogue's name of a package, ignoring case; NA if it isn't listed
catalogue_name <- function(package, catalogue){

  hit <- match(tolower(package), tolower(names(catalogue)))
  if(is.na(hit)) NA_character_ else names(catalogue)[hit]
}

#### where a catalogue entry is installed from: list(cran = TRUE/FALSE, repo = "owner/repo" or NA)
catalogue_source <- function(entry){

  cran_link <- entry[["input"]][["cran_link"]]
  github_link <- entry[["input"]][["github_link"]]

  repo <- NA_character_
  if(!is.null(github_link) && nzchar(github_link)){
    repo <- sub("^https?://github\\.com/", "", github_link)
    repo <- sub("(\\.git)?/*$", "", repo)
  }

  list(cran = !is.null(cran_link) && nzchar(cran_link), repo = repo)
}

#### the client package of a server package in the catalogue; NA if there is none
#### (<server>Client, or <server without Base>Client as in dsMTLBase -> dsMTLClient,
#### or <server without Server> as in dsQueryLibraryServer -> dsQueryLibrary)
catalogue_client <- function(package, catalogue){

  candidates <- unique(c(paste0(package, "Client"),
                         paste0(sub("Base$", "", package), "Client"),
                         sub("Server$", "", package)))
  candidates <- setdiff(candidates, package)

  hits <- vapply(candidates, catalogue_name, "", catalogue = catalogue)
  hits <- hits[!is.na(hits)]

  if(length(hits) == 0) NA_character_ else hits[[1]]
}

#### the tags of a GitHub repository (first 100)
github_tags <- function(repo){

  headers <- c(Accept = "application/vnd.github+json")
  if(nzchar(Sys.getenv("GITHUB_PAT"))){
    headers <- c(headers, Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
  }

  tags <- tryCatch({
    con <- url(paste0("https://api.github.com/repos/", repo, "/tags?per_page=100"), headers = headers)
    on.exit(close(con))
    jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = ""))$name
  }, error = function(e) NULL)

  if(is.null(tags)){
    stop("Could not read the versions (tags) of https://github.com/", repo,
         ". Check the repository name, or give a ref instead of a version.", call. = FALSE)
  }

  tags
}

#### the tag of a version: "1.2.3" matches the tag 1.2.3 or v1.2.3; NA if there is none
version_tag <- function(version, tags){

  hit <- tags[tags %in% c(version, paste0("v", version))]
  if(length(hit) == 0) NA_character_ else hit[[1]]
}

#### the renv::install() specification of one package
#### src: list(cran, repo) from catalogue_source() or a user's "owner/repo"
install_spec <- function(package, src, version = NULL, ref = NULL){

  if(!is.null(ref)){
    if(is.na(src$repo)){
      stop("A ref needs the GitHub repository of ", package, ": give source = \"owner/repo\".", call. = FALSE)
    }
    return(paste0(src$repo, "@", ref))
  }

  if(src$cran){
    return(if (is.null(version)) package else paste0(package, "@", version))
  }

  if(is.na(src$repo)){
    stop("No CRAN or GitHub location is known for ", package, ". Give source = \"owner/repo\".", call. = FALSE)
  }

  #### no version: the latest release (tag), else the default branch
  if(is.null(version)){

    tags <- tryCatch(github_tags(src$repo), error = function(e) NULL)
    released <- released_tags(tags)

    if(length(released) == 0){
      message("No released version of ", package, " found on GitHub; installing its default branch.")
      return(src$repo)
    }

    return(paste0(src$repo, "@", released[[1]]))
  }

  tags <- github_tags(src$repo)
  tag <- version_tag(version, tags)

  if(is.na(tag)){
    stop("Version ", version, " of ", package, " was not found on GitHub (", src$repo, "). ",
         "Available versions: ", paste(sub("^v", "", released_tags(tags)), collapse = ", "), call. = FALSE)
  }

  paste0(src$repo, "@", tag)
}

#### the release tags (1.2.3 or v1.2.3, no release candidates), newest first
released_tags <- function(tags){

  released <- tags[grepl("^v?[0-9]+(\\.[0-9]+)+$", tags)]

  if(length(released) == 0){
    return(character(0))
  }

  released[order(numeric_version(sub("^v", "", released)), decreasing = TRUE)]
}

#### what to install for a DataSHIELD package: the server and the client package,
#### each with its name and renv::install() specification (client is NULL if there is none)
resolve_dsPackage <- function(dsPackage, version = NULL, ref = NULL, source = NULL,
                              client = NULL, client_version = NULL, client_source = NULL){

  catalogue <- ds_catalogue()

  #### server package
  name <- if (is.null(catalogue)) NA_character_ else catalogue_name(dsPackage, catalogue)

  if(!is.null(source)){
    server_src <- list(cran = FALSE, repo = source)
  } else if(!is.na(name)){
    server_src <- catalogue_source(catalogue[[name]])
  } else {
    stop(dsPackage, " is not in the DataSHIELD package catalogue. ",
         "If it is on GitHub, give its repository with source = \"owner/repo\".", call. = FALSE)
  }

  if(is.na(name)){
    name <- dsPackage
  }

  server <- list(package = name, spec = install_spec(name, server_src, version, ref))

  #### client package: as given, else from the catalogue
  if(is.null(client) && !is.null(catalogue)){
    client <- catalogue_client(name, catalogue)
  }

  if(is.null(client) || is.na(client)){
    message("No client package found for ", name, "; only the server package is installed. ",
            "Give client = \"...\" if it has one.")
    return(list(server = server, client = NULL))
  }

  client_name <- if (is.null(catalogue)) NA_character_ else catalogue_name(client, catalogue)

  if(!is.null(client_source)){
    client_src <- list(cran = FALSE, repo = client_source)
  } else if(!is.na(client_name)){
    client_src <- catalogue_source(catalogue[[client_name]])
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
    client_spec <- tryCatch(install_spec(client_name, client_src, version),
                            error = function(e) NULL)
    if(is.null(client_spec)){
      message("Version ", version, " was not found for ", client_name, "; its latest version is installed. ",
              "Give client_version = \"...\" to choose one.")
      client_spec <- install_spec(client_name, client_src)
    }
  } else {
    client_spec <- install_spec(client_name, client_src, client_version)
  }

  list(server = server, client = list(package = client_name, spec = client_spec))
}
