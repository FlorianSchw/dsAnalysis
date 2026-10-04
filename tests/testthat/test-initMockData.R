test_that("initMockData stops before contacting any server when the target folder exists", {
  tmp_proj <- tempfile("mockdata-exists-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_Existing"), recursive = TRUE)
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_New"))
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")

  overwrite_error <- function(folder){
    paste0("The folder name you have provided would overwrite an existing directory (",
           file.path(tmp_proj, "utils/mock_data", folder), "). Setup aborted.")
  }
  squish <- function(err) stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))

  err <- testthat::expect_error(dsAnalysis::initMockData(folder_name = "MockData_Existing"))
  testthat::expect_equal(squish(err), overwrite_error("MockData_Existing"))
  testthat::expect_null(err$call)

  #### without a folder name: MockData_New
  err <- testthat::expect_error(dsAnalysis::initMockData())
  testthat::expect_equal(squish(err), overwrite_error("MockData_New"))
})

test_that("initMockData rejects datasources that aren't a list of DSConnection objects and creates no folder", {
  tmp_proj <- tempfile("mockdata-badconn-")
  dir.create(file.path(tmp_proj, "utils", "mock_data"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")

  for (bad in list(list(server1 = "not_a_connection"), 42)){
    err <- testthat::expect_error(dsAnalysis::initMockData(folder_name = "MockData_Bad", datasources = bad))
    testthat::expect_equal(err$message, "The 'datasources' were expected to be a list of DSConnection-class objects")
    testthat::expect_null(err$call)
    testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils/mock_data", "MockData_Bad")))
  }
})

test_that("initMockData writes one mock data file per server, shaped like the server data", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()

  testthat::expect_invisible(mock_path <- dsAnalysis::initMockData(folder_name = "test-mock-data", df = "D",
                                                                   datasources = cnsim$conns))

  #### the folder under utils/mock_data, one .rda per server, each holding an object named after its server
  testthat::expect_equal(mock_path, file.path(cnsim$project, "utils/mock_data", "test-mock-data"))
  testthat::expect_setequal(basename(fs::dir_ls(mock_path)), c("sim1.rda", "sim2.rda", "sim3.rda"))

  for (server in names(cnsim$real)){

    real <- cnsim$real[[server]]
    mock <- new.env()
    testthat::expect_identical(load(file.path(mock_path, paste0(server, ".rda")), envir = mock), server)
    mock_data <- mock[[server]]

    #### same columns in the same order, same rows, same missing values per variable
    testthat::expect_s3_class(mock_data, "data.frame")
    testthat::expect_identical(colnames(mock_data), colnames(real))
    testthat::expect_equal(nrow(mock_data), nrow(real))
    testthat::expect_identical(vapply(mock_data, function(x) sum(is.na(x)), 1L), vapply(real, function(x) sum(is.na(x)), 1L))

    #### numeric variables stay numeric; categorical ones are factors with the server's levels
    numeric_vars <- colnames(real)[vapply(real, is.numeric, TRUE)]
    testthat::expect_true(all(vapply(mock_data[numeric_vars], is.numeric, TRUE)))
    for (variable in colnames(real)[vapply(real, is.factor, TRUE)]){
      testthat::expect_true(is.factor(mock_data[[variable]]))
      testthat::expect_true(all(levels(mock_data[[variable]]) %in% levels(real[[variable]])))
    }
  }

  #### a second run would overwrite the folder: it stops
  testthat::expect_error(dsAnalysis::initMockData(folder_name = "test-mock-data", df = "D", datasources = cnsim$conns),
                         regexp = "would overwrite an existing")
})

test_that("initMockData keeps strictly non-negative server variables non-negative in the mock data", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()

  mock_path <- dsAnalysis::initMockData(folder_name = "mock-neg", df = "D", datasources = cnsim$conns)

  for (server in names(cnsim$real)){
    real <- cnsim$real[[server]]
    mock <- new.env()
    load(file.path(mock_path, paste0(server, ".rda")), envir = mock)

    numeric_vars <- colnames(real)[vapply(real, is.numeric, TRUE)]
    nonneg_vars <- numeric_vars[vapply(numeric_vars, function(v) min(real[[v]], na.rm = TRUE) >= 0, TRUE)]
    testthat::expect_true(length(nonneg_vars) > 0)
    for (v in nonneg_vars){
      testthat::expect_true(all(mock[[server]][[v]] >= 0, na.rm = TRUE))
    }
  }
})

test_that("initMockData writes files only for the servers in the supplied datasources subset", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project()

  mock_path <- dsAnalysis::initMockData(folder_name = "mock-subset", df = "D", datasources = cnsim$conns[1])

  testthat::expect_equal(basename(fs::dir_ls(mock_path)), "sim1.rda")
  mock <- new.env()
  testthat::expect_identical(load(file.path(mock_path, "sim1.rda"), envir = mock), "sim1")
  testthat::expect_equal(nrow(mock$sim1), nrow(cnsim$real$sim1))
  testthat::expect_identical(colnames(mock$sim1), colnames(cnsim$real$sim1))
})

test_that("initMockData accepts a user-assigned data frame symbol other than D", {
  testthat::skip_if_not_installed("DSLite")
  testthat::skip_if_not_installed("dsBase")
  cnsim <- local_cnsim_project(symbol = "MYDF")

  mock_path <- dsAnalysis::initMockData(folder_name = "mock-symbol", df = "MYDF", datasources = cnsim$conns)

  testthat::expect_setequal(basename(fs::dir_ls(mock_path)), c("sim1.rda", "sim2.rda", "sim3.rda"))
  mock <- new.env()
  load(file.path(mock_path, "sim2.rda"), envir = mock)
  testthat::expect_identical(colnames(mock$sim2), colnames(cnsim$real$sim2))
  testthat::expect_equal(nrow(mock$sim2), nrow(cnsim$real$sim2))
})
