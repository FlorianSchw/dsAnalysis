#### the tag of a version: "1.2.3" matches the tag 1.2.3 or v1.2.3; NA if there is none
version_tag <- function(version, tags){

  hit <- tags[tags %in% c(version, paste0("v", version))]
  if(length(hit) == 0) NA_character_ else hit[[1]]
}
