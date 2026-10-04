#### where a catalogue entry is installed from: list(cran = TRUE/FALSE, repo = "owner/repo" or NA)
internal_catalogue_source <- function(entry){

  cran_link <- entry[["input"]][["cran_link"]]
  github_link <- entry[["input"]][["github_link"]]

  repo <- NA_character_
  if(!is.null(github_link) && nzchar(github_link)){
    repo <- sub("^https?://github\\.com/", "", github_link)
    repo <- sub("(\\.git)?/*$", "", repo)
  }

  list(cran = !is.null(cran_link) && nzchar(cran_link), repo = repo)
}
