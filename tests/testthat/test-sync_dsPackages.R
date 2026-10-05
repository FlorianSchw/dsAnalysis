test_that("sync_dsPackages errors with a login hint and no call when there are no DataSHIELD connections", {
  hint <- "No DataSHIELD connections found. Log in first (R/01_DS_Login.R with R_CONFIG_ACTIVE = 'production')."

  #### none found in the session
  testthat::local_mocked_bindings(datashield.connections_find = function(...) list(), .package = "DSI")
  err <- testthat::expect_error(dsAnalysis::sync_dsPackages())
  testthat::expect_equal(err$message, hint)
  testthat::expect_null(err$call)

  #### an empty list given
  err <- testthat::expect_error(dsAnalysis::sync_dsPackages(conns = list(), install = FALSE))
  testthat::expect_equal(err$message, hint)
})
