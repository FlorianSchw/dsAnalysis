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
