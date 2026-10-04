#### the packages in the dependencies.R block, one line per server package:
#### library(<server>); library(<client>)
dependencies_dsPackages <- function(codelines){

  block <- dependencies_block_lines(codelines)

  if(length(block) == 0){
    return(data.frame(server = character(0), client = character(0)))
  }

  calls <- stringr::str_match_all(codelines[block], "library\\(([^)]+)\\)")
  packages <- data.frame(server = vapply(calls, function(x) if (nrow(x) > 0) x[1, 2] else NA_character_, ""),
                         client = vapply(calls, function(x) if (nrow(x) > 1) x[2, 2] else NA_character_, ""))

  packages[!is.na(packages$server), , drop = FALSE]
}
