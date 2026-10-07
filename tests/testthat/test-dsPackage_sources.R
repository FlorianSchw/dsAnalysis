test_that("ds_catalogue reads the catalogue from the dsAnalysis.catalogue option as a named list", {
  tmp_dir <- withr::local_tempdir("ds-catalogue-read-")
  json_file <- file.path(tmp_dir, "packages.json")
  writeLines('{"dsBase":{"input":{"cran_link":"https://cran.r-project.org/package=dsBase","github_link":"https://github.com/datashield/dsBase"}},"dsBaseClient":{"input":{"cran_link":"","github_link":"https://github.com/datashield/dsBaseClient"}}}', json_file)
  withr::local_options(list(dsAnalysis.catalogue = json_file))
  assign("catalogue", NULL, envir = dsAnalysis:::dsAnalysis_cache)
  cat_out <- dsAnalysis:::ds_catalogue(refresh = TRUE)
  
  testthat::expect_type(cat_out, "list")
  testthat::expect_equal(names(cat_out), c("dsBase", "dsBaseClient"))
  testthat::expect_equal(length(cat_out), 2L)
  testthat::expect_equal(cat_out[["dsBase"]][["input"]][["github_link"]], "https://github.com/datashield/dsBase")
  testthat::expect_equal(cat_out[["dsBaseClient"]][["input"]][["cran_link"]], "")
})

test_that("ds_catalogue stores the catalogue in the package cache after a successful read", {
  tmp_dir <- withr::local_tempdir("ds-catalogue-cache-")
  json_file <- file.path(tmp_dir, "packages.json")
  writeLines('{"dsBase":{"input":{"cran_link":"","github_link":"https://github.com/datashield/dsBase"}}}', json_file)
  withr::local_options(list(dsAnalysis.catalogue = json_file))
  assign("catalogue", NULL, envir = dsAnalysis:::dsAnalysis_cache)
  testthat::expect_null(dsAnalysis:::dsAnalysis_cache$catalogue)
  
  cat_out <- dsAnalysis:::ds_catalogue(refresh = TRUE)
  
  testthat::expect_equal(names(dsAnalysis:::dsAnalysis_cache$catalogue), "dsBase")
  testthat::expect_identical(dsAnalysis:::dsAnalysis_cache$catalogue, cat_out)
})

test_that("ds_catalogue returns the cached catalogue without re-reading the source when refresh is FALSE", {
  tmp_dir <- withr::local_tempdir("ds-catalogue-cached-")
  json_file <- file.path(tmp_dir, "packages.json")
  writeLines('{"dsBase":{"input":{"cran_link":"","github_link":"https://github.com/datashield/dsBase"}}}', json_file)
  withr::local_options(list(dsAnalysis.catalogue = json_file))
  assign("catalogue", NULL, envir = dsAnalysis:::dsAnalysis_cache)
  first <- dsAnalysis:::ds_catalogue(refresh = TRUE)
  #### the file is replaced; a cached call must not see the new content
  writeLines('{"dsSurvival":{"input":{"cran_link":"","github_link":"https://github.com/neelsoumya/dsSurvival"}}}', json_file)
  
  cached <- dsAnalysis:::ds_catalogue()
  testthat::expect_equal(names(cached), "dsBase")
  testthat::expect_identical(cached, first)
  
  #### refresh = TRUE re-reads the file
  refreshed <- dsAnalysis:::ds_catalogue(refresh = TRUE)
  testthat::expect_equal(names(refreshed), "dsSurvival")
})

test_that("ds_catalogue returns NULL with a message naming the url when the catalogue cannot be read", {
  tmp_dir <- withr::local_tempdir("ds-catalogue-fail-")
  missing_file <- file.path(tmp_dir, "does-not-exist.json")
  withr::local_options(list(dsAnalysis.catalogue = missing_file))
  assign("catalogue", NULL, envir = dsAnalysis:::dsAnalysis_cache)
  msgs <- testthat::capture_messages(res <- dsAnalysis:::ds_catalogue(refresh = TRUE))
  
  testthat::expect_null(res)
  testthat::expect_equal(length(msgs), 1L)
  testthat::expect_true(grepl(missing_file, msgs[[1]], fixed = TRUE))
  testthat::expect_true(grepl("could not be read", msgs[[1]], fixed = TRUE))
  testthat::expect_true(grepl("source = \"owner/repo\"", msgs[[1]], fixed = TRUE))
  
  #### a failed read must not fill the cache
  testthat::expect_null(dsAnalysis:::dsAnalysis_cache$catalogue)
})

test_that("ds_catalogue keeps the previously cached catalogue when a refresh fails", {
  tmp_dir <- withr::local_tempdir("ds-catalogue-keep-")
  json_file <- file.path(tmp_dir, "packages.json")
  writeLines('{"dsBase":{"input":{"cran_link":"","github_link":"https://github.com/datashield/dsBase"}}}', json_file)
  withr::local_options(list(dsAnalysis.catalogue = json_file))
  assign("catalogue", NULL, envir = dsAnalysis:::dsAnalysis_cache)
  good <- dsAnalysis:::ds_catalogue(refresh = TRUE)
  testthat::expect_equal(names(good), "dsBase")
  
  unlink(json_file)
  testthat::expect_message(res <- dsAnalysis:::ds_catalogue(refresh = TRUE), "could not be read")
  testthat::expect_null(res)
  
  testthat::expect_equal(names(dsAnalysis:::dsAnalysis_cache$catalogue), "dsBase")
  testthat::expect_identical(dsAnalysis:::dsAnalysis_cache$catalogue, good)
})

test_that("ds_catalogue does not simplify json arrays, keeping entries as nested lists", {
  tmp_dir <- withr::local_tempdir("ds-catalogue-nosimplify-")
  json_file <- file.path(tmp_dir, "packages.json")
  writeLines('{"dsBase":{"input":{"cran_link":"","github_link":"https://github.com/datashield/dsBase","keywords":["a","b","c"]}}}', json_file)
  withr::local_options(list(dsAnalysis.catalogue = json_file))
  assign("catalogue", NULL, envir = dsAnalysis:::dsAnalysis_cache)
  cat_out <- dsAnalysis:::ds_catalogue(refresh = TRUE)
  
  kw <- cat_out[["dsBase"]][["input"]][["keywords"]]
  testthat::expect_type(kw, "list")
  testthat::expect_equal(length(kw), 3L)
  testthat::expect_equal(kw[[1]], "a")
  testthat::expect_equal(kw[[3]], "c")
})
