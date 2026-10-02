test_that("sync_dsPackages errors with a login hint and no call when no DataSHIELD connections are found", {
  testthat::local_mocked_bindings(datashield.connections_find = function(...) list(), .package = "DSI")
  err <- testthat::expect_error(dsAnalysis::sync_dsPackages())
  testthat::expect_equal(err$message,
                         "No DataSHIELD connections found. Log in first (R/01_DS_Login.R with R_CONFIG_ACTIVE = 'production').")
  testthat::expect_null(err$call)
})

test_that("sync_dsPackages errors when the conns argument given is an empty list", {
  err <- testthat::expect_error(dsAnalysis::sync_dsPackages(conns = list(), install = FALSE))
  testthat::expect_equal(err$message,
                         "No DataSHIELD connections found. Log in first (R/01_DS_Login.R with R_CONFIG_ACTIVE = 'production').")
  testthat::expect_null(err$call)
})
