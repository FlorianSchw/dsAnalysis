test_that("initMockdata aborts with the overwrite error and creates no files when the target folder already exists", {
  tmp_proj <- tempfile("mockdata-proj-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "ExistingFolder"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata(folder_name = "ExistingFolder"))
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_equal(msg,
                         paste0("The folder name you have provided would overwrite an existing directory (",
                                file.path(tmp_proj, "utils", "mock_data", "ExistingFolder"),
                                "). Setup aborted."))
  testthat::expect_equal(length(fs::dir_ls(file.path(tmp_proj, "utils", "mock_data", "ExistingFolder"), all = TRUE)), 0L)
})

test_that("initMockdata rejects a non-DSConnection datasources argument and creates no mock data folder", {
  tmp_proj <- tempfile("mockdata-proj-")
  dir.create(tmp_proj, recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata(folder_name = "BadConnections",
                                                        datasources = list("not_a_connection")))
  testthat::expect_equal(err$message,
                         "The 'datasources' were expected to be a list of DSConnection-class objects")
  testthat::expect_false(fs::dir_exists(file.path(tmp_proj, "utils", "mock_data", "BadConnections")))
})

test_that("initMockdata uses the default folder name MockData_New when folder_name is NULL", {
  tmp_proj <- tempfile("mockdata-proj-")
  dir.create(file.path(tmp_proj, "utils", "mock_data", "MockData_New"), recursive = TRUE)
  on.exit(unlink(tmp_proj, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  err <- testthat::expect_error(dsAnalysis::initMockdata())
  msg <- stringr::str_squish(stringr::str_replace_all(err$message, "\\n", ""))
  testthat::expect_true(stringr::str_detect(msg, stringr::fixed(file.path(tmp_proj, "utils", "mock_data", "MockData_New"))))
  testthat::expect_false(stringr::str_detect(msg, stringr::fixed("MockData_New/MockData_New")))
})
