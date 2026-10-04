test_that("add_dsPackage leaves the DSLite setup unchanged when all packages are already included", {

  tmp_proj <- tempfile("dslite-setup-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = setup_file)

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
