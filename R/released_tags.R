#### the release tags (1.2.3 or v1.2.3, no release candidates), newest first
released_tags <- function(tags){

  released <- tags[grepl("^v?[0-9]+(\\.[0-9]+)+$", tags)]

  if(length(released) == 0){
    return(character(0))
  }

  released[order(numeric_version(sub("^v", "", released)), decreasing = TRUE)]
}
