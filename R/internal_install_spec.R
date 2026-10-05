#### the renv::install() specification of one package
#### src: list(cran, repo) from internal_catalogue_source() or a user's "owner/repo"
internal_install_spec <- function(package, src, version = NULL, ref = NULL){

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

  #### the GitHub tags; without a version, a failed lookup falls back to the default branch
  tags <- if (is.null(version)) tryCatch(internal_github_tags(src$repo), error = function(e) character(0)) else internal_github_tags(src$repo)

  #### the release tags (1.2.3 or v1.2.3, no release candidates), newest first
  released <- tags[grepl("^v?[0-9]+(\\.[0-9]+)+$", tags)]
  released <- released[order(numeric_version(sub("^v", "", released)), decreasing = TRUE)]

  #### no version: the latest release, else the default branch
  if(is.null(version)){
    if(length(released) == 0){
      message("No released version of ", package, " found on GitHub; installing its default branch.")
      return(src$repo)
    }
    return(paste0(src$repo, "@", released[[1]]))
  }

  #### a version: the tag 1.2.3 or v1.2.3
  tag <- tags[tags %in% c(version, paste0("v", version))][1]

  if(is.na(tag)){
    stop("Version ", version, " of ", package, " was not found on GitHub (", src$repo, "). ",
         "Available versions: ", paste(sub("^v", "", released), collapse = ", "), call. = FALSE)
  }

  paste0(src$repo, "@", tag)
}
