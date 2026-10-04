test_that("list_dsPackages returns an empty tibble invisibly and no message when the catalogue is NULL", {
  testthat::local_mocked_bindings(ds_catalogue = function(refresh = FALSE) NULL)
  res <- withVisible(dsAnalysis::list_dsPackages())
  testthat::expect_false(res$visible)
  testthat::expect_s3_class(res$value, "tbl_df")
  testthat::expect_equal(nrow(res$value), 0L)
  testthat::expect_equal(ncol(res$value), 0L)
})

test_that("list_dsPackages passes refresh through to ds_catalogue", {
  seen <- NULL
  fake_catalogue <- list(
    dsBase = list(input = list(status = "production", description = "Base functions"), repo = list(Version = "6.3.0"))
  )
  testthat::local_mocked_bindings(
    ds_catalogue = function(refresh = FALSE) { seen <<- refresh; fake_catalogue },
    catalogue_client = function(package, catalogue) NA_character_,
    catalogue_source = function(entry) list(cran = TRUE, repo = NA_character_)
  )
  testthat::expect_message(dsAnalysis::list_dsPackages(refresh = TRUE), "1 DataSHIELD package\\(s\\) found")
  testthat::expect_true(seen)
  testthat::expect_message(dsAnalysis::list_dsPackages(), "1 DataSHIELD package\\(s\\) found")
  testthat::expect_false(seen)
})
