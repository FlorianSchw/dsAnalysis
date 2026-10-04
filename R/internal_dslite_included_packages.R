#### the server packages in include=c(...) of step 4
internal_dslite_included_packages <- function(codelines){

  config_code <- paste(codelines[internal_dslite_config_lines(codelines)], collapse = "")
  include_code <- stringr::str_match(config_code, "include\\s*=\\s*c\\(([^)]*)\\)")[1, 2]

  if(is.na(include_code)){
    return(character(0))
  }

  stringr::str_match_all(include_code, "\"([^\"]+)\"")[[1]][, 2]
}
