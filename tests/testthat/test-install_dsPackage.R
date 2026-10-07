test_that("install_dsPackage errors when no package name is given", {
  err <- testthat::expect_error(dsAnalysis::install_dsPackage())
  testthat::expect_equal(err$message, "No package name has been given.")
  
  err_null <- testthat::expect_error(dsAnalysis::install_dsPackage(dsPackage = NULL))
  testthat::expect_equal(err_null$message, "No package name has been given.")
})

test_that("install_dsPackage errors when options are given together with several packages", {
  err <- testthat::expect_error(dsAnalysis::install_dsPackage(dsPackage = c("dsBase", "dsSurvival"), version = "1.0.0"))
  testthat::expect_equal(err$message, "Versions, refs, sources and clients can only be given for one package at a time.")
  
  err2 <- testthat::expect_error(dsAnalysis::install_dsPackage(dsPackage = c("dsBase", "dsSurvival"), client = "dsBaseClient"))
  testthat::expect_equal(err2$message, "Versions, refs, sources and clients can only be given for one package at a time.")
})

test_that("install_dsPackage reports the installed packages in a final Done message", {
  tmp_proj <- withr::local_tempdir("dslite-install-done-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) invisible(packages), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_record = function(project) invisible(project))
  msgs <- testthat::capture_messages(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"))
  
  testthat::expect_true(any(stringr::str_detect(msgs, "^Installing ")))
  testthat::expect_true(any(stringr::str_detect(msgs, "Done: dsSurvival and dsSurvivalClient installed, added to the DSLite setup and to dependencies\\.R\\.")))
})

test_that("install_dsPackage returns the resolved server and client package list invisibly for a single package", {
  tmp_proj <- withr::local_tempdir("dslite-install-return-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) invisible(packages), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_record = function(project) invisible(project))
  res <- withr::with_options(list(warn = -1), {
    suppressMessages(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"))
  })
  testthat::expect_equal(res$server$package, "dsSurvival")
  testthat::expect_equal(res$client$package, "dsSurvivalClient")
  testthat::expect_true(is.list(res))
})

test_that("install_dsPackage installs several packages and returns the given package names invisibly", {
  tmp_proj <- withr::local_tempdir("dslite-install-multi-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  installed <- new.env()
  installed$specs <- character(0)
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) { installed$specs <- c(installed$specs, packages); invisible(packages) }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_record = function(project) invisible(project))
  res <- suppressMessages(dsAnalysis::install_dsPackage(dsPackage = c("dsBase", "dsSurvival")))
  testthat::expect_equal(res, c("dsBase", "dsSurvival"))
  testthat::expect_true(any(grepl("dsBase", installed$specs)))
  testthat::expect_true(any(grepl("dsSurvival", installed$specs)))
})

test_that("install_dsPackage passes the resolved specs of server and client to renv::install", {
  tmp_proj <- withr::local_tempdir("dslite-install-specs-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  seen <- new.env()
  seen$specs <- NULL
  seen$project <- NULL
  seen$prompt <- NULL
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) { seen$specs <- packages; seen$project <- project; seen$prompt <- prompt; invisible(packages) }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_record = function(project) invisible(project))
  res <- suppressMessages(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"))
  testthat::expect_equal(seen$specs, c(res$server$spec, res$client$spec))
  testthat::expect_equal(length(seen$specs), 2L)
  testthat::expect_false(seen$prompt)
  testthat::expect_equal(normalizePath(seen$project, mustWork = FALSE), normalizePath(tmp_proj, mustWork = FALSE))
})

test_that("install_dsPackage calls internal_renv_record with the project path", {
  tmp_proj <- withr::local_tempdir("dslite-install-record-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) invisible(packages), .package = "renv")
  recorded <- new.env()
  recorded$project <- NULL
  recorded$n <- 0L
  testthat::local_mocked_bindings(internal_renv_record = function(project) { recorded$project <- project; recorded$n <- recorded$n + 1L; invisible(project) })
  suppressMessages(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"))
  testthat::expect_equal(recorded$n, 1L)
  testthat::expect_equal(normalizePath(recorded$project, mustWork = FALSE), normalizePath(tmp_proj, mustWork = FALSE))
})

test_that("install_dsPackage adds the installed server and client package to the DSLite setup script", {
  tmp_proj <- withr::local_tempdir("dslite-install-setupfile-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) invisible(packages), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_record = function(project) invisible(project))
  before <- readLines(setup_file)
  suppressMessages(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"))
  after <- readLines(setup_file)
  testthat::expect_true(any(grepl("dsSurvival", after)))
  testthat::expect_false(any(grepl("dsSurvival", before)))
  testthat::expect_true(file.exists(setup_file))
})

test_that("install_dsPackage installs the given version of a single package without a client", {
  tmp_proj <- withr::local_tempdir("dslite-install-version-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  file.copy(from = internal_find_script("dslite/01_DSLite_Setup.R"), to = file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  seen <- new.env()
  seen$specs <- NULL
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) { seen$specs <- packages; invisible(packages) }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_record = function(project) invisible(project))
  res <- suppressMessages(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival", version = "1.0.0"))
  testthat::expect_equal(res$server$package, "dsSurvival")
  testthat::expect_true(any(grepl("1.0.0", seen$specs, fixed = TRUE)))
})
