test_that("find script", {

  package <- "dsAnalysis"
  test_name <- "testproj-456"
  tmp_path <- fs::path_temp()
  tmp_path_to_proj <- dsAnalysis::initProject(path = tmp_path, name = test_name)

  script_name1 <- "datashield/01_DS_Login.R"
  script_name2 <- "datashield/main.R"
  script_name3 <- "datashield/99_DSLiteLearning.R"
  script_name4 <- "dslite/01_DSLite_Setup.R"
  script_name5 <- "utils/config.yml"
  script_name6 <- "utils/dependencies.R"

  testthat::expect_no_error(dsAnalysis:::find_script(script_name = script_name1))
  testthat::expect_no_error(dsAnalysis:::find_script(script_name = script_name2))
  testthat::expect_no_error(dsAnalysis:::find_script(script_name = script_name3))
  testthat::expect_no_error(dsAnalysis:::find_script(script_name = script_name4))
  testthat::expect_no_error(dsAnalysis:::find_script(script_name = script_name5))
  testthat::expect_no_error(dsAnalysis:::find_script(script_name = script_name6))

  #### Testing when pathing goes wrong to the scripts / files
  script_name_error <- "01_DS_Login.R"
  error_message <- testthat::expect_error(dsAnalysis:::find_script(script_name = script_name_error))

  #### Testing that the error message is consistent
  testthat::expect_equal(error_message$message,
                         paste0("Could not find the file: ", script_name_error, ". Please contact the developer team."))


})
