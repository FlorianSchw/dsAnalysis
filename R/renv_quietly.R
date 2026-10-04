#### runs a renv call without its printout (the functions here report in their own words)
renv_quietly <- function(expr){
  result <- NULL
  utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
  invisible(result)
}
