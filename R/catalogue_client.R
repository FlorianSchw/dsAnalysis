#### the client package of a server package in the catalogue; NA if there is none
#### (<server>Client, or <server without Base>Client as in dsMTLBase -> dsMTLClient,
#### or <server without Server> as in dsQueryLibraryServer -> dsQueryLibrary)
catalogue_client <- function(package, catalogue){

  candidates <- unique(c(paste0(package, "Client"),
                         paste0(sub("Base$", "", package), "Client"),
                         sub("Server$", "", package)))
  candidates <- setdiff(candidates, package)

  hits <- vapply(candidates, catalogue_name, "", catalogue = catalogue)
  hits <- hits[!is.na(hits)]

  if(length(hits) == 0) NA_character_ else hits[[1]]
}
