#### the catalogue's name of a package, ignoring case; NA if it isn't listed
catalogue_name <- function(package, catalogue){

  hit <- match(tolower(package), tolower(names(catalogue)))
  if(is.na(hit)) NA_character_ else names(catalogue)[hit]
}
