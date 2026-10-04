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
