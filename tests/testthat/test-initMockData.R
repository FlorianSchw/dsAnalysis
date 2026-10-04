test_that("initMockData errors without contacting servers when the target folder already exists", {
  tmp_proj <- tempfile("mockdata-exists-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_Existing"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockData(folder_name = "MockData_Existing"))
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_equal(msg,
                         paste0("The folder name you have provided would overwrite an existing directory (",
                                file.path(tmp_proj, "utils/mock_data", "MockData_Existing"),
                                "). Setup aborted."))
  testthat::expect_null(err$call)
})

test_that("initMockData uses MockData_New as the default folder name in the existing-directory error", {
  tmp_proj <- tempfile("mockdata-default-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_New"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockData())
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_equal(msg,
                         paste0("The folder name you have provided would overwrite an existing directory (",
                                file.path(tmp_proj, "utils/mock_data", "MockData_New"),
                                "). Setup aborted."))
})

test_that("initMockData errors when datasources is not a list of DSConnection objects and creates no folder", {
  tmp_proj <- tempfile("mockdata-badconn-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockData(folder_name = "MockData_Bad",
                                                        datasources = list(server1 = "not_a_connection")))
  testthat::expect_equal(err$message,
                         "The 'datasources' were expected to be a list of DSConnection-class objects")
  testthat::expect_null(err$call)
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils/mock_data", "MockData_Bad")))
})

test_that("initMockData rejects a single non-list datasources argument", {
  tmp_proj <- tempfile("mockdata-nonlist-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockData(folder_name = "MockData_NonList",
                                                        datasources = 42))
  testthat::expect_equal(err$message,
                         "The 'datasources' were expected to be a list of DSConnection-class objects")
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils/mock_data", "MockData_NonList")))
})

test_that("initMockData writes one mock data file per server, shaped like the server data", {

  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")

  testthat::expect_error(dsAnalysis::initMockData(datasources = "abc"),
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

  mock_path <- dsAnalysis::initMockData(folder_name = "test-mock-data",
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
  testthat::expect_error(dsAnalysis::initMockData(folder_name = "test-mock-data",
                                                  df = "D",
                                                  datasources = conns),
                         regexp = "would overwrite an existing")

})

test_that("initMockData returns the created folder path invisibly and creates it under utils/mock_data", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-path-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  testthat::expect_invisible(mock_path <- dsAnalysis::initMockData(folder_name = "mock-invisible", df = "D", datasources = conns))
  testthat::expect_equal(mock_path, file.path(tmp_proj, "utils/mock_data", "mock-invisible"))
  testthat::expect_true(fs::dir_exists(mock_path))
  testthat::expect_equal(length(fs::dir_ls(mock_path)), 3)
})

test_that("initMockData reproduces column order, numeric types and per-variable NA counts of each server's data", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-shape-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  mock_path <- dsAnalysis::initMockData(folder_name = "mock-shape", df = "D", datasources = conns)
  
  real <- list(sim1 = cnsim$CNSIM1, sim2 = cnsim$CNSIM2, sim3 = cnsim$CNSIM3)
  
  for (server in names(real)){
    mock <- new.env()
    load(file.path(mock_path, paste0(server, ".rda")), envir = mock)
    mock_data <- mock[[server]]
  
    testthat::expect_s3_class(mock_data, "data.frame")
    testthat::expect_identical(colnames(mock_data), colnames(real[[server]]))
    testthat::expect_equal(nrow(mock_data), nrow(real[[server]]))
  
    numeric_vars <- colnames(real[[server]])[vapply(real[[server]], is.numeric, TRUE)]
    testthat::expect_true(all(vapply(mock_data[numeric_vars], is.numeric, TRUE)))
  
    testthat::expect_identical(vapply(mock_data, function(x) sum(is.na(x)), 1L)[colnames(real[[server]])],
                               vapply(real[[server]], function(x) sum(is.na(x)), 1L))
  }
})

test_that("initMockData keeps strictly non-negative server variables non-negative in the mock data", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-neg-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  mock_path <- dsAnalysis::initMockData(folder_name = "mock-neg", df = "D", datasources = conns)
  
  real <- list(sim1 = cnsim$CNSIM1, sim2 = cnsim$CNSIM2, sim3 = cnsim$CNSIM3)
  
  for (server in names(real)){
    mock <- new.env()
    load(file.path(mock_path, paste0(server, ".rda")), envir = mock)
    mock_data <- mock[[server]]
  
    numeric_vars <- colnames(real[[server]])[vapply(real[[server]], is.numeric, TRUE)]
    nonneg_vars <- numeric_vars[vapply(numeric_vars,
                                       function(v) min(real[[server]][[v]], na.rm = TRUE) >= 0,
                                       TRUE)]
    testthat::expect_true(length(nonneg_vars) > 0)
    for (v in nonneg_vars){
      testthat::expect_true(all(mock_data[[v]] >= 0, na.rm = TRUE))
    }
  }
})

test_that("initMockData writes files only for the servers in the supplied datasources subset", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-subset-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  mock_path <- dsAnalysis::initMockData(folder_name = "mock-subset", df = "D", datasources = conns[1])
  
  testthat::expect_equal(basename(fs::dir_ls(mock_path)), "sim1.rda")
  
  mock <- new.env()
  testthat::expect_identical(load(file.path(mock_path, "sim1.rda"), envir = mock), "sim1")
  testthat::expect_equal(nrow(mock$sim1), nrow(cnsim$CNSIM1))
  testthat::expect_identical(colnames(mock$sim1), colnames(cnsim$CNSIM1))
})

test_that("initMockData accepts a user-assigned data frame symbol other than D", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-symbol-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "MYDF")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  mock_path <- dsAnalysis::initMockData(folder_name = "mock-symbol", df = "MYDF", datasources = conns)
  
  testthat::expect_setequal(basename(fs::dir_ls(mock_path)), c("sim1.rda", "sim2.rda", "sim3.rda"))
  
  mock <- new.env()
  load(file.path(mock_path, "sim2.rda"), envir = mock)
  testthat::expect_identical(colnames(mock$sim2), colnames(cnsim$CNSIM2))
  testthat::expect_equal(nrow(mock$sim2), nrow(cnsim$CNSIM2))
})

test_that("initMockData creates missing parent directories of utils/mock_data recursively", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-recursive-")
  dir.create(tmp_proj, recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils", "mock_data")))
  mock_path <- dsAnalysis::initMockData(folder_name = "mock-recursive", df = "D", datasources = conns)
  testthat::expect_true(fs::dir_exists(file.path(tmp_proj, "utils", "mock_data", "mock-recursive")))
  testthat::expect_equal(mock_path, file.path(tmp_proj, "utils/mock_data", "mock-recursive"))
  testthat::expect_setequal(basename(fs::dir_ls(mock_path)), c("sim1.rda", "sim2.rda", "sim3.rda"))
})

test_that("initMockData creates no folder when the servers cannot answer because the data frame symbol does not exist", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-nosymbol-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  testthat::expect_error(dsAnalysis::initMockData(folder_name = "mock-nosymbol",
                                                  df = "NOT_THERE",
                                                  datasources = conns))
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils/mock_data", "mock-nosymbol")))
})

test_that("initMockData writes mock data whose means of complete numeric variables are close to the server's means", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  
  tmp_proj <- tempfile("mockdata-mean-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  
  cnsim <- new.env()
  utils::data("CNSIM1", "CNSIM2", "CNSIM3", "logindata.dslite.cnsim", package = "DSLite", envir = cnsim)
  assign("dslite.server",
         DSLite::newDSLiteServer(tables = list(CNSIM1 = cnsim$CNSIM1,
                                               CNSIM2 = cnsim$CNSIM2,
                                               CNSIM3 = cnsim$CNSIM3),
                                 config = DSLite::defaultDSConfiguration(include = "dsBase")),
         envir = globalenv())
  on.exit(rm("dslite.server", envir = globalenv()), add = TRUE)
  conns <- DSI::datashield.login(logins = cnsim$logindata.dslite.cnsim, assign = TRUE, symbol = "D")
  on.exit(DSI::datashield.logout(conns), add = TRUE)
  mock_path <- dsAnalysis::initMockData(folder_name = "mock-mean", df = "D", datasources = conns)
  mock <- new.env()
  load(file.path(mock_path, "sim1.rda"), envir = mock)
  mock_data <- mock$sim1
  real_data <- cnsim$CNSIM1
  numeric_vars <- colnames(real_data)[vapply(real_data, is.numeric, TRUE)]
  numeric_vars <- setdiff(numeric_vars, "ID")
  testthat::expect_true(length(numeric_vars) > 0)
  for (v in numeric_vars){
    real_mean <- mean(real_data[[v]], na.rm = TRUE)
    real_sd <- stats::sd(real_data[[v]], na.rm = TRUE)
    mock_mean <- mean(mock_data[[v]], na.rm = TRUE)
    testthat::expect_lt(abs(mock_mean - real_mean), 0.5 * real_sd)
  }
})
