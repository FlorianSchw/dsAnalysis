#### the line numbers between the block's markers (empty if there is no block)
dependencies_block_lines <- function(codelines){

  start <- which(codelines == dependencies_markers()[["start"]])
  end <- which(codelines == dependencies_markers()[["end"]])

  if(length(start) == 0 || length(end) == 0 || end[1] <= start[1] + 1){
    return(integer(0))
  }

  (start[1] + 1):(end[1] - 1)
}
