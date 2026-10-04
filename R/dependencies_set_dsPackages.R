#### adds and removes packages in the dependencies.R block (creates the block if needed)
dependencies_set_dsPackages <- function(add = character(0), client = character(0), remove = character(0)){

  #### marker line of the block the datashield-analysis-suggest workflow maintains
  bot_marker <- "#### bot-suggest: packages (updated by datashield-analysis-suggest)"

  dependencies_file <- here::here("dependencies.R")

  if(!file.exists(dependencies_file)){
    message("No dependencies.R found in the project, so it was not updated.")
    return(invisible(NULL))
  }

  codelines <- readLines(con = dependencies_file)
  packages <- dependencies_dsPackages(codelines)

  packages <- packages[!(packages$server %in% c(remove, add)), , drop = FALSE]
  packages <- rbind(packages, data.frame(server = add, client = client))

  block_new <- c(dependencies_markers()[["start"]],
                 ifelse(is.na(packages$client) | !nzchar(packages$client),
                        paste0("library(", packages$server, ")"),
                        paste0("library(", packages$server, "); library(", packages$client, ")")),
                 dependencies_markers()[["end"]])

  start <- which(codelines == dependencies_markers()[["start"]])
  end <- which(codelines == dependencies_markers()[["end"]])

  if(length(start) > 0 && length(end) > 0){

    codelines <- c(codelines[seq_len(start[1] - 1)],
                   block_new,
                   codelines[-seq_len(end[1])])

  } else {

    #### a new block goes before the bot's block, or at the end
    bot_line <- which(codelines == bot_marker)

    if(length(bot_line) > 0){
      codelines <- append(codelines, c(block_new, ""), after = bot_line[1] - 1)
    } else {
      codelines <- c(codelines, "", block_new)
    }

  }

  writeLines(text = codelines, con = dependencies_file)
  invisible(packages)
}
