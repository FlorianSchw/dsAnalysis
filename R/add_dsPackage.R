#'
#' @title Function to add dsPackages to the 01_DSLite_Setup.R file ABCDDE
#' @description XXX
#' @details XXXXXXsssddasdsdfsdasdfsaasdsadfsdfsdf
#' @return adjusted R Script
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @param dsPackage name or names of the DataSHIELD server-side packages to add to DSLite instance
#' @param client name or names of the matching client-side packages
#' @import stringr
#' @export
#'


add_dsPackage <- function(dsPackage = NULL, client = NULL){

  if(is.null(dsPackage)){
    stop("No package name has been given.",call.=FALSE)
  }

  if(is.null(client)){
    client <- paste0(dsPackage, "Client")
  }

  if(!(length(client) == length(dsPackage))){
    stop("Please provide one client package per DataSHIELD package.", call. = FALSE)
  }

  setup_file <- here::here("utils/setup", "01_DSLite_Setup.R")
  dslite_setup_codelines <- readLines(con = setup_file)

  included <- dslite_included_packages(dslite_setup_codelines)

  dupl_int <- dsPackage %in% included
  for (p in dsPackage[dupl_int]){
    message(paste0("The DataSHIELD package ", p, " is already included in the DSLite Setup."))
  }

  #### nothing to add when all packages are already included
  if(all(dupl_int)){
    return(invisible(NULL))
  }

  dsPackage_unique <- dsPackage[!dupl_int]
  client_unique <- client[!dupl_int]

  #### step 1: library calls of the client packages
  dslite_setup_codelines <- dslite_add_library_calls(dslite_setup_codelines,
                                                     client_unique[!is.na(client_unique)])

  #### step 4: server packages in the DSLite configuration
  dslite_setup_codelines <- dslite_write_included_packages(dslite_setup_codelines,
                                                           c(included, dsPackage_unique))

  writeLines(text = dslite_setup_codelines, con = setup_file)

  #### dependencies.R: library calls so that renv tracks server and client packages
  dependencies_set_dsPackages(add = dsPackage_unique, client = client_unique)

  invisible(dsPackage_unique)

}


#### The helpers below stay in this file: the datashield-analysis-suggest
#### workflow (package-workflows) sources add_dsPackage.R on its own.

#### marker lines of the steps in 01_DSLite_Setup.R
dslite_step_markers <- c("#### Step 1: Loading necessary libraries",
                         "#### Step 2: Import of mock data files",
                         "#### Step 3: Defining the server-side data in a new dslite server",
                         "#### Step 4: Defining the server-side settings",
                         "#### Step 5: Building the logindata object",
                         "#### Step 6: Login to the different DSLite Servers",
                         "#### Step 7: Cleaning the environment")

#### line number of a step marker; stops if the file doesn't have it
dslite_step_line <- function(codelines, step){

  step_line <- which(codelines == dslite_step_markers[step])

  if(!(length(step_line) == 1)){
    stop(paste0("Could not find the line '", dslite_step_markers[step],
                "' in 01_DSLite_Setup.R. Please don't edit the step markers."),
         call. = FALSE)
  }

  step_line
}

#### lines of the dslite.server$config(...) call in step 4
dslite_config_lines <- function(codelines){

  step4_line <- dslite_step_line(codelines, 4)
  step5_line <- dslite_step_line(codelines, 5)
  step4_range <- step4_line:(step5_line - 1)

  config_start <- step4_range[startsWith(codelines[step4_range], "dslite.server$config(")]
  config_end <- step4_range[startsWith(codelines[step4_range], "dslite.server$profile()")]

  if(!(length(config_start) == 1) || !(length(config_end) == 1)){
    stop("Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
         call. = FALSE)
  }

  config_start:(config_end - 1)
}

#### the server packages in include=c(...) of step 4
dslite_included_packages <- function(codelines){

  config_code <- paste(codelines[dslite_config_lines(codelines)], collapse = "")
  include_code <- stringr::str_match(config_code, "include\\s*=\\s*c\\(([^)]*)\\)")[1, 2]

  if(is.na(include_code)){
    return(character(0))
  }

  stringr::str_match_all(include_code, "\"([^\"]+)\"")[[1]][, 2]
}

#### rewrites the dslite.server$config(...) call in step 4 with these server packages
dslite_write_included_packages <- function(codelines, packages){

  config_lines <- dslite_config_lines(codelines)
  config_start <- "dslite.server$config(DSLite::defaultDSConfiguration(include=c("
  indent <- strrep(" ", nchar(config_start))

  quoted <- paste0("\"", packages, "\"")
  config_new <- paste0(c(config_start, rep(indent, length(quoted) - 1)),
                       quoted,
                       c(rep(",", length(quoted) - 1), ")))"))

  c(codelines[seq_len(config_lines[1] - 1)],
    config_new,
    codelines[(config_lines[length(config_lines)] + 1):length(codelines)])
}

#### adds library() calls to step 1, after the existing ones
dslite_add_library_calls <- function(codelines, packages){

  step1_line <- dslite_step_line(codelines, 1)
  step2_line <- dslite_step_line(codelines, 2)

  library_lines <- paste0("library(", packages, ")")
  library_lines <- setdiff(library_lines, codelines[step1_line:(step2_line - 1)])

  #### after the last non-empty line of step 1
  step1_range <- step1_line:(step2_line - 1)
  last_line <- max(step1_range[nzchar(trimws(codelines[step1_range]))])

  append(codelines, library_lines, after = last_line)
}

#### removes library() calls from step 1
dslite_remove_library_calls <- function(codelines, packages){

  step1_line <- dslite_step_line(codelines, 1)
  step2_line <- dslite_step_line(codelines, 2)
  step1_range <- step1_line:(step2_line - 1)

  drop <- step1_range[trimws(codelines[step1_range]) %in% paste0("library(", packages, ")")]

  if(length(drop) == 0){
    return(codelines)
  }

  codelines[-drop]
}

#### marker lines of the block in dependencies.R that add_dsPackage() and remove_dsPackage() maintain
dependencies_markers <- c(start = "#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)",
                          end = "#### DataSHIELD packages end")

#### marker line of the block the datashield-analysis-suggest workflow maintains
dependencies_bot_marker <- "#### bot-suggest: packages (updated by datashield-analysis-suggest)"

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

#### the line numbers between the block's markers (empty if there is no block)
dependencies_block_lines <- function(codelines){

  start <- which(codelines == dependencies_markers[["start"]])
  end <- which(codelines == dependencies_markers[["end"]])

  if(length(start) == 0 || length(end) == 0 || end[1] <= start[1] + 1){
    return(integer(0))
  }

  (start[1] + 1):(end[1] - 1)
}

#### adds and removes packages in the dependencies.R block (creates the block if needed)
dependencies_set_dsPackages <- function(add = character(0), client = character(0), remove = character(0)){

  dependencies_file <- here::here("dependencies.R")

  if(!file.exists(dependencies_file)){
    message("No dependencies.R found in the project, so it was not updated.")
    return(invisible(NULL))
  }

  codelines <- readLines(con = dependencies_file)
  packages <- dependencies_dsPackages(codelines)

  packages <- packages[!(packages$server %in% c(remove, add)), , drop = FALSE]
  packages <- rbind(packages, data.frame(server = add, client = client))

  block_new <- c(dependencies_markers[["start"]],
                 ifelse(is.na(packages$client) | !nzchar(packages$client),
                        paste0("library(", packages$server, ")"),
                        paste0("library(", packages$server, "); library(", packages$client, ")")),
                 dependencies_markers[["end"]])

  start <- which(codelines == dependencies_markers[["start"]])
  end <- which(codelines == dependencies_markers[["end"]])

  if(length(start) > 0 && length(end) > 0){

    codelines <- c(codelines[seq_len(start[1] - 1)],
                   block_new,
                   codelines[-seq_len(end[1])])

  } else {

    #### a new block goes before the bot's block, or at the end
    bot_line <- which(codelines == dependencies_bot_marker)

    if(length(bot_line) > 0){
      codelines <- append(codelines, c(block_new, ""), after = bot_line[1] - 1)
    } else {
      codelines <- c(codelines, "", block_new)
    }

  }

  writeLines(text = codelines, con = dependencies_file)
  invisible(packages)
}
