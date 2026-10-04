test_that("list_dsPackages returns an empty tibble invisibly and no message when the catalogue is NULL", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function(refresh = FALSE) NULL)
  res <- withVisible(dsAnalysis::list_dsPackages())
  testthat::expect_false(res$visible)
  testthat::expect_s3_class(res$value, "tbl_df")
  testthat::expect_equal(nrow(res$value), 0L)
  testthat::expect_equal(ncol(res$value), 0L)
})
