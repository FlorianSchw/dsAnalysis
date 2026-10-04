test_that("add_dsPackage leaves the DSLite setup unchanged when all packages are already included", {

  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)

  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")

  setup_lines_before <- readLines(setup_file)

  testthat::expect_message(dsAnalysis::add_dsPackage(dsPackage = "dsBase"),
                           "The DataSHIELD package dsBase is already included in the DSLite Setup.")
  testthat::expect_identical(readLines(setup_file), setup_lines_before)

  #### a new package is still added
  dsAnalysis::add_dsPackage(dsPackage = "dsSurvival")
  setup_lines_after <- readLines(setup_file)
  testthat::expect_true("library(dsSurvivalClient)" %in% setup_lines_after)
  testthat::expect_true(any(stringr::str_detect(setup_lines_after, "\"dsSurvival\"\\)\\)\\)")))

})

#
#
# test_that("project setup structure", {
#
#   #### Testing that all top-level folders and files are created as expected
#   test_name <- "testproj-123"
#   tmp_path <- fs::path_temp()
#   tmp_path_to_proj <- mepr::initialize_project(path = tmp_path, name = test_name)
#
#   file_structure_top <- fs::dir_ls(path = tmp_path_to_proj,
#                                    all = TRUE)
#
#   expect_elements <- c(".gitignore",
#                        #".Renviron",
#                        ".Rprofile",
#                        #"config.yml",
#                        "citations",
#                        "data",
#                        "data-raw",
#                        "R",
#                        "README.md",
#                        "renv",
#                        "renv.lock",
#                        "results",
#                        paste0(test_name, ".Rproj"))
#
#   expected_paths <- paste0(tmp_path_to_proj, "/", expect_elements)
#
#   testthat::expect_setequal(file_structure_top,
#                             expected_paths)
#
#
#   #### Testing whether important elements exist in the .gitignore file
#   gitignore_lines_expected <- c(".Renviron",
#                                 "data/*",
#                                 "!data/placeholder.txt",
#                                 "data-raw/*",
#                                 "!data-raw/placeholder.txt",
#                                 "results/plots/*",
#                                 "!results/plots/placeholder.txt",
#                                 "results/tables/*",
#                                 "!results/tables/placeholder.txt")
#
#   gitignore_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/", ".gitignore"))
#
#   testthat::expect_true(all(gitignore_lines_expected %in% gitignore_lines_created))
#
#
#   #### testing whether important packages have been written to the renv.lock file
#
#   renv_lock_file <- paste0(tmp_path_to_proj, "/", "renv.lock")
#   renv_lock_data <- rjson::fromJSON(paste(readLines(renv_lock_file), collapse = ""))[[2]]
#   installed_package_names <- names(renv_lock_data)
#
#   renv_lock_expected <- c("survival",
#                           "tidyverse",
#                           "here",
#                           "pak")
#
#   testthat::expect_true(all(renv_lock_expected %in% installed_package_names))
#
#
#   #### Testing whether the function stops when no name has been provided
#   error_message <- testthat::expect_error(mepr::initialize_project(path = tmp_path))
#
#   #### Testing that the error message is consistent
#   testthat::expect_equal(error_message$message,
#                          paste0("Please provide a path name for the project to be created."))
#
#
#   #### Testing whether the function stops when it would overwrite a folder directory
#   error_message2 <- testthat::expect_error(mepr::initialize_project(path = tmp_path, name = test_name))
#   error_message2_message <- stringr::str_replace_all(string = error_message2$message,
#                                                      pattern = "\\n",
#                                                      replacement = "")
#   error_message2_message <- stringr::str_squish(error_message2_message)
#
#   if(cfg_dir_overwrite){
#
#     #### Testing that the error message is consistent
#     testthat::expect_equal(error_message2_message,
#                            paste0("The path and name you have provided would overwrite an existing directory (", tmp_path_to_proj , "). Setup aborted."))
#
#   }
#
# })
#
#
#
#
#
#

test_that("add_dsPackage errors when no package name is given", {
  testthat::expect_error(dsAnalysis::add_dsPackage(), "No package name has been given.", fixed = TRUE)
})

test_that("add_dsPackage errors when the number of client packages does not match the number of server packages", {
  testthat::expect_error(dsAnalysis::add_dsPackage(dsPackage = c("dsSurvival", "dsOmics"), client = "dsSurvivalClient"),
                         "Please provide one client package per DataSHIELD package.", fixed = TRUE)
})

test_that("add_dsPackage returns the added package invisibly and writes library call and include entry for a single new package", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  res <- testthat::expect_invisible(dsAnalysis::add_dsPackage(dsPackage = "dsSurvival"))
  testthat::expect_identical(res, "dsSurvival")
  
  setup_lines <- readLines(setup_file)
  testthat::expect_true("library(dsSurvivalClient)" %in% setup_lines)
  testthat::expect_equal(sum(setup_lines == "library(dsSurvivalClient)"), 1)
  testthat::expect_true(any(stringr::str_detect(setup_lines, "include=c\\(\"dsBase\",")))
  testthat::expect_true(any(stringr::str_detect(setup_lines, "\"dsSurvival\"\\)\\)\\)")))
})

test_that("add_dsPackage adds several packages at once and keeps the previously included ones in the include list", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  res <- dsAnalysis::add_dsPackage(dsPackage = c("dsSurvival", "dsOmics"))
  testthat::expect_identical(res, c("dsSurvival", "dsOmics"))
  
  setup_lines <- readLines(setup_file)
  testthat::expect_true(all(c("library(dsSurvivalClient)", "library(dsOmicsClient)") %in% setup_lines))
  
  config_code <- paste(setup_lines, collapse = "")
  include_code <- stringr::str_match(config_code, "include\\s*=\\s*c\\(([^)]*)\\)")[1, 2]
  included <- stringr::str_match_all(include_code, "\"([^\"]+)\"")[[1]][, 2]
  testthat::expect_identical(included, c("dsBase", "dsSurvival", "dsOmics"))
})

test_that("add_dsPackage adds only the new package and messages about the duplicate when a mix of known and new packages is given", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::expect_message(res <- dsAnalysis::add_dsPackage(dsPackage = c("dsBase", "dsSurvival")),
                           "The DataSHIELD package dsBase is already included in the DSLite Setup.", fixed = TRUE)
  testthat::expect_identical(res, "dsSurvival")
  
  setup_lines <- readLines(setup_file)
  config_code <- paste(setup_lines, collapse = "")
  include_code <- stringr::str_match(config_code, "include\\s*=\\s*c\\(([^)]*)\\)")[1, 2]
  included <- stringr::str_match_all(include_code, "\"([^\"]+)\"")[[1]][, 2]
  testthat::expect_identical(included, c("dsBase", "dsSurvival"))
  testthat::expect_equal(sum(setup_lines == "library(dsSurvivalClient)"), 1)
})

test_that("add_dsPackage uses the given client package name instead of the default <dsPackage>Client", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  dsAnalysis::add_dsPackage(dsPackage = "dsSurvival", client = "myOwnClient")
  
  setup_lines <- readLines(setup_file)
  testthat::expect_true("library(myOwnClient)" %in% setup_lines)
  testthat::expect_false("library(dsSurvivalClient)" %in% setup_lines)
})

test_that("add_dsPackage writes the server and client library calls into the dependencies.R block", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  dependencies_file <- file.path(tmp_proj, "dependencies.R")
  writeLines(c("library(here)", "library(tidyverse)"), dependencies_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  dsAnalysis::add_dsPackage(dsPackage = "dsSurvival")
  
  dep_lines <- readLines(dependencies_file)
  testthat::expect_true("#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)" %in% dep_lines)
  testthat::expect_true("#### DataSHIELD packages end" %in% dep_lines)
  testthat::expect_true("library(dsSurvival); library(dsSurvivalClient)" %in% dep_lines)
  testthat::expect_true(all(c("library(here)", "library(tidyverse)") %in% dep_lines))
})

test_that("add_dsPackage messages that dependencies.R was not updated when the project has no dependencies.R", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::expect_message(dsAnalysis::add_dsPackage(dsPackage = "dsSurvival"),
                           "No dependencies.R found in the project, so it was not updated.", fixed = TRUE)
  testthat::expect_false(file.exists(file.path(tmp_proj, "dependencies.R")))
  testthat::expect_true("library(dsSurvivalClient)" %in% readLines(setup_file))
})

test_that("add_dsPackage errors when a step marker is missing from 01_DSLite_Setup.R", {
  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  broken <- readLines(setup_file)
  broken[broken == "#### Step 4: Defining the server-side settings"] <- "#### Step 4: my own heading"
  writeLines(broken, setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::expect_error(dsAnalysis::add_dsPackage(dsPackage = "dsSurvival"),
                         "Please don't edit the step markers.", fixed = TRUE)
})
