

test_that("project setup structure", {

  testthat::expect_error(dsAnalysis::initProject(), regexp = "Please provide a path name for the project to be created.")
  testthat::expect_error(dsAnalysis::initProject(name = "ZZZZ", switch_to_proj = "abc"),
                           regexp = "switch_to_proj has to be logical, i.e. either TRUE or FALSE.")

  #### Testing that all top-level folders and files are created as expected
  test_name <- "zzz-testproj-123"
  tmp_path <- fs::path_temp()
  tmp_path_to_proj <- dsAnalysis::initProject(path = tmp_path, name = test_name)

  file_structure_top <- fs::dir_ls(path = tmp_path_to_proj,
                                   all = TRUE)

  expect_elements <- c(".gitignore",
                       ".Renviron",
                       ".Rprofile",
                       "config.yml",
                       "citations",
                       "dependencies.R",
                       "R",
                       "renv",
                       "renv.lock",
                       "results",
                       "utils",
                       paste0(test_name, ".Rproj"))

  expected_paths <- paste0(tmp_path_to_proj, "/", expect_elements)

  testthat::expect_setequal(file_structure_top,
                            expected_paths)


  #### Testing whether important elements exist in the .gitignore file
  gitignore_lines_expected <- c(".Renviron",
                                "results/figures/*",
                                "!results/figures/placeholder.txt",
                                "results/tables/*",
                                "!results/tables/placeholder.txt")


  gitignore_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/", ".gitignore"))

  testthat::expect_true(all(gitignore_lines_expected %in% gitignore_lines_created))

  #### Testing whether important elements exist in the .Renviron file
  renviron_lines_expected <- c("R_CONFIG_ACTIVE = 'production'",
                               "OBIBA1_URL = 'https://opal-demo.obiba.org/'",
                               "OBIBA1_USER = 'dsuser'",
                               "OBIBA1_PWD = 'P@ssw0rd'",
                               "OBIBA1_TABLE = 'CNSIM.CNSIM1'")


  renviron_lines_created <- readLines(con = paste0(tmp_path_to_proj, "/", ".Renviron"))

  testthat::expect_true(all(renviron_lines_expected %in% renviron_lines_created))


  #### testing whether important packages have been written to the renv.lock file

  renv_lock_file <- paste0(tmp_path_to_proj, "/", "renv.lock")
  renv_lock_data <- rjson::fromJSON(paste(readLines(renv_lock_file), collapse = ""))[[2]]
  installed_package_names <- names(renv_lock_data)

  renv_lock_expected <- c("dsBaseClient",
                          "dsBase")

  testthat::expect_true(all(renv_lock_expected %in% installed_package_names))

  #### Testing whether the function stops when no name has been provided
  error_message <- testthat::expect_error(dsAnalysis::initProject(path = tmp_path))

  #### Testing that the error message is consistent
  testthat::expect_equal(error_message$message,
                         paste0("Please provide a path name for the project to be created."))


  #### Testing whether the function stops when it would overwrite a folder directory
  error_message2 <- testthat::expect_error(dsAnalysis::initialize_project(path = tmp_path, name = test_name))
  error_message2_message <- stringr::str_replace_all(string = error_message2$message,
                                                     pattern = "\\n",
                                                     replacement = "")
  error_message2_message <- stringr::str_squish(error_message2_message)

  if(cfg_dir_overwrite){

    #### Testing that the error message is consistent
    testthat::expect_equal(error_message2_message,
                           paste0("The path and name you have provided would overwrite an existing directory (", tmp_path_to_proj , "). Setup aborted."))

  }

})






