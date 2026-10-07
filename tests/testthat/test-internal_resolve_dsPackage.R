test_that("internal_resolve_dsPackage errors with a catalogue hint when the package is not in the catalogue and no source is given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient")))
  testthat::expect_error(dsAnalysis:::internal_resolve_dsPackage("dsUnknown"),
                         "dsUnknown is not in the DataSHIELD package catalogue. If it is on GitHub, give its repository with source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_resolve_dsPackage derives the client repository from the server's owner when the client is unknown to the catalogue", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "@", src$repo))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsMine", source = "myorg/dsMine", client = "dsMineClient")
  testthat::expect_identical(res$server$spec, "dsMine@myorg/dsMine")
  testthat::expect_identical(res$client$package, "dsMineClient")
  testthat::expect_identical(res$client$spec, "dsMineClient@myorg/dsMineClient")
})

test_that("internal_resolve_dsPackage errors when the client is unknown and the server package comes from CRAN without a repo", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(cran = TRUE, github = NA_character_)),
                                  internal_catalogue_source = function(entry) list(cran = TRUE, repo = NA_character_),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) name)
  testthat::expect_error(dsAnalysis:::internal_resolve_dsPackage("dsBase", client = "dsBaseClient"),
                         "No CRAN or GitHub location is known for dsBaseClient. Give client_source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_resolve_dsPackage passes the server version on to the client when client_version is not given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "-", if (is.null(version)) "latest" else version))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", version = "6.3.0")
  testthat::expect_identical(res$server$spec, "dsBase-6.3.0")
  testthat::expect_identical(res$client$spec, "dsBaseClient-6.3.0")
})

test_that("internal_resolve_dsPackage uses client_version instead of the server version when both are given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "-", if (is.null(version)) "latest" else version))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", version = "6.3.0", client_version = "6.2.0")
  testthat::expect_identical(res$server$spec, "dsBase-6.3.0")
  testthat::expect_identical(res$client$spec, "dsBaseClient-6.2.0")
})

test_that("internal_resolve_dsPackage falls back to the client's latest version with a message when the server version is unavailable for the client", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) {
                                    if (identical(name, "dsBaseClient") && !is.null(version)) stop("no such version")
                                    paste0(name, "-", if (is.null(version)) "latest" else version)
                                  })
  testthat::expect_message(res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", version = "6.3.0"),
                           "Version 6.3.0 was not found for dsBaseClient; its latest version is installed. Give client_version = \"...\" to choose one.",
                           fixed = TRUE)
  testthat::expect_identical(res$server$spec, "dsBase-6.3.0")
  testthat::expect_identical(res$client$spec, "dsBaseClient-latest")
})

test_that("internal_resolve_dsPackage uses client_source for the client and source for the server, overriding the catalogue", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_catalogue_source = function(entry) list(cran = TRUE, repo = NA_character_),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "|", if (src$cran) "cran" else src$repo))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", source = "fork/dsBase", client_source = "fork/dsBaseClient")
  testthat::expect_identical(res$server$spec, "dsBase|fork/dsBase")
  testthat::expect_identical(res$client$package, "dsBaseClient")
  testthat::expect_identical(res$client$spec, "dsBaseClient|fork/dsBaseClient")
})

test_that("internal_resolve_dsPackage passes ref on to the server spec only and leaves the client spec without it", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() list(dsBase = list(github = "datashield/dsBase", client = "dsBaseClient"),
                                                                          dsBaseClient = list(github = "datashield/dsBaseClient")),
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "@", if (is.null(ref)) "none" else ref))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsBase", ref = "my-branch")
  testthat::expect_identical(res$server$spec, "dsBase@my-branch")
  testthat::expect_identical(res$client$spec, "dsBaseClient@none")
})

test_that("internal_resolve_dsPackage uses the given source and client when the catalogue is NULL", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() NULL,
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "@", src$repo))
  res <- dsAnalysis:::internal_resolve_dsPackage("dsMine", source = "myorg/dsMine", client = "dsMineClient")
  testthat::expect_identical(res$server$package, "dsMine")
  testthat::expect_identical(res$server$spec, "dsMine@myorg/dsMine")
  testthat::expect_identical(res$client$package, "dsMineClient")
  testthat::expect_identical(res$client$spec, "dsMineClient@myorg/dsMineClient")
})

test_that("internal_resolve_dsPackage errors with the catalogue hint when the catalogue is NULL and no source is given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() NULL)
  testthat::expect_error(dsAnalysis:::internal_resolve_dsPackage("dsBase"),
                         "dsBase is not in the DataSHIELD package catalogue. If it is on GitHub, give its repository with source = \"owner/repo\".",
                         fixed = TRUE)
})

test_that("internal_resolve_dsPackage messages and returns a NULL client when the catalogue is NULL and no client is given", {
  testthat::local_mocked_bindings(internal_ds_catalogue = function() NULL,
                                  internal_install_spec = function(name, src, version = NULL, ref = NULL) paste0(name, "@", src$repo))
  testthat::expect_message(res <- dsAnalysis:::internal_resolve_dsPackage("dsMine", source = "myorg/dsMine"),
                           "No client package found for dsMine; only the server package is installed.",
                           fixed = TRUE)
  testthat::expect_null(res$client)
  testthat::expect_identical(res$server$spec, "dsMine@myorg/dsMine")
})
