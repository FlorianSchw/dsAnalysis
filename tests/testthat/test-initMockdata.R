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
