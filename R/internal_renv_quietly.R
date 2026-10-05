#### runs a renv call without its printout (the functions here report in their own words)
internal_renv_quietly <- function(expr){
  result <- NULL
  utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
  invisible(result)
}
