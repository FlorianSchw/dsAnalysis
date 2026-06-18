

test_that("project setup structure", {


  testthat::expect_error(dsAnalysis::initMockdata(datasources = "abc"),
                         regexp = "The 'datasources' were expected to be a list of DSConnection-class objects")


  #### Testing that all top-level folders and files are created as expected
  test_name <- "testproj-789"
  tmp_path <- fs::path_temp()
  tmp_path_to_proj <- dsAnalysis::initProject(path = tmp_path,
                                              name = test_name,
                                              switch_to_proj = TRUE)

  library(DSI)
  library(DSOpal)
  library(dsBaseClient)

  builder <- DSI::newDSLoginBuilder(.silent = FALSE)
  builder$append(server = "DEMO_OBIBA_1",
                 url = 'https://opal-demo.obiba.org/',
                 user = 'dsuser',
                 password = 'P@ssw0rd',
                 table = "CNSIM.CNSIM1",
                 driver = "OpalDriver")
  builder$append(server = "DEMO_OBIBA_2",
                 url = 'https://opal-demo.obiba.org/',
                 user = 'dsuser',
                 password = 'P@ssw0rd',
                 table = "CNSIM.CNSIM2",
                 driver = "OpalDriver")
  builder$append(server = "DEMO_OBIBA_3",
                 url = 'https://opal-demo.obiba.org/',
                 user = 'dsuser',
                 password = 'P@ssw0rd',
                 table = "CNSIM.CNSIM3",
                 driver = "OpalDriver")

  logindata <- builder$build()
  conns <- DSI::datashield.login(logins = logindata,
                                 assign = TRUE,
                                 symbol = "D")

  dsAnalysis::initMockdata(folder_name = "test-mock-data",
                           df = "D",
                           datasources = conns)


  file_structure_top <- fs::dir_ls(path = paste0(tmp_path_to_proj, "/utils/mock_data/test-mock-data"),
                                   all = TRUE)

  expect_elements <- c("DEMO_OBIBA_1.rda",
                       "DEMO_OBIBA_2.rda",
                       "DEMO_OBIBA_3.rda")

  expected_paths <- paste0(tmp_path_to_proj, "/utils/mock_data/test-mock-data", expect_elements)

  testthat::expect_setequal(file_structure_top,
                            expected_paths)



  #### Testing that the error message is consistent
  testthat::expect_equal(error_message$message,
                         paste0("Please provide a path name for the project to be created."))


  #### Testing whether the function stops when it would overwrite a folder directory
  error_message2 <- testthat::expect_error(dsAnalysis::initMockdata(folder_name = "test-mock-data",
                                                                    df = "D",
                                                                    datasources = conns))
  error_message2_message <- stringr::str_replace_all(string = error_message2$message,
                                                     pattern = "\\n",
                                                     replacement = "")
  error_message2_message <- stringr::str_squish(error_message2_message)

  if(cfg_dir_overwrite){

    #### Testing that the error message is consistent
    testthat::expect_equal(error_message2_message,
                           paste0("The folder name you have provided would overwrite an existing directory (", tmp_path_to_proj , "). Setup aborted."))

  }

})






