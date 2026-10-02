test_that("initMockdata errors without contacting servers when the target folder already exists", {
  tmp_proj <- tempfile("mockdata-exists-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_Existing"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata(folder_name = "MockData_Existing"))
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_equal(msg,
                         paste0("The folder name you have provided would overwrite an existing directory (",
                                file.path(tmp_proj, "utils/mock_data", "MockData_Existing"),
                                "). Setup aborted."))
  testthat::expect_null(err$call)
})

test_that("initMockdata uses MockData_New as the default folder name in the existing-directory error", {
  tmp_proj <- tempfile("mockdata-default-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_New"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata())
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_equal(msg,
                         paste0("The folder name you have provided would overwrite an existing directory (",
                                file.path(tmp_proj, "utils/mock_data", "MockData_New"),
                                "). Setup aborted."))
})

test_that("initMockdata errors when datasources is not a list of DSConnection objects and creates no folder", {
  tmp_proj <- tempfile("mockdata-badconn-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata(folder_name = "MockData_Bad",
                                                        datasources = list(server1 = "not_a_connection")))
  testthat::expect_equal(err$message,
                         "The 'datasources' were expected to be a list of DSConnection-class objects")
  testthat::expect_null(err$call)
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils/mock_data", "MockData_Bad")))
})

test_that("initMockdata rejects a single non-list datasources argument", {
  tmp_proj <- tempfile("mockdata-nonlist-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata(folder_name = "MockData_NonList",
                                                        datasources = 42))
  testthat::expect_equal(err$message,
                         "The 'datasources' were expected to be a list of DSConnection-class objects")
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils/mock_data", "MockData_NonList")))
})

test_that("initMockdata writes one mock data file per server, shaped like the server data", {

  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")

  testthat::expect_error(dsAnalysis::initMockdata(datasources = "abc"),
                         regexp = "The 'datasources' were expected to be a list of DSConnection-class objects")

  #### a project folder for the mock data
  tmp_proj <- tempfile("mockdata-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")

  #### three DSLite servers with the CNSIM data
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)

  #### the DSLite driver finds the server object by its name in the global environment
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)

  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim,
                                 assign = TRUE,
                                 symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)

  mock_path <- dsAnalysis::initMockdata(folder_name = "test-mock-data",
                                        df = "D",
                                        datasources = conns)

  #### one .rda per server, each holding an object named after its server
  testthat::expect_setequal(basename(fs::dir_ls(mock_path)),
                            c("sim1.rda", "sim2.rda", "sim3.rda"))

  real <- list(sim1 = cnsim$CNSIM1, sim2 = cnsim$CNSIM2, sim3 = cnsim$CNSIM3)

  for (server in names(real)){

    mock <- new.env()
    testthat::expect_identical(load(file.path(mock_path, paste0(server, ".rda")), envir = mock), server)
    mock_data <- mock[[server]]

    #### same columns and rows as the server's data
    testthat::expect_setequal(colnames(mock_data), colnames(real[[server]]))
    testthat::expect_equal(nrow(mock_data), nrow(real[[server]]))

    #### categorical variables are factors with the server's levels; no categories missing values by mistake
    for (variable in colnames(real[[server]])[vapply(real[[server]], is.factor, TRUE)]){
      testthat::expect_true(is.factor(mock_data[[variable]]))
      testthat::expect_true(all(levels(mock_data[[variable]]) %in% levels(real[[server]][[variable]])))
      testthat::expect_equal(sum(is.na(mock_data[[variable]])), sum(is.na(real[[server]][[variable]])))
    }

  }

  #### a second run would overwrite the folder: it stops
  testthat::expect_error(dsAnalysis::initMockdata(folder_name = "test-mock-data",
                                                  df = "D",
                                                  datasources = conns),
                         regexp = "would overwrite an existing")

})
