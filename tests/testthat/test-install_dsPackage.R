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

test_that("install_dsPackage adds the server and client package to the DSLite setup file", {
  tmp_proj <- withr::local_tempdir("dslite-install-setupfile-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  before <- readLines(setup_file)
  
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) invisible(packages), .package = "renv")
  testthat::local_mocked_bindings(renv_record = function(project) invisible(project))
  testthat::expect_message(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"), "Done: ")
  
  after <- readLines(setup_file)
  testthat::expect_true(any(stringr::str_detect(after, "dsSurvival")))
  testthat::expect_false(identical(before, after))
})

test_that("install_dsPackage passes the project from here::here() to renv::install and renv_record", {
  tmp_proj <- withr::local_tempdir("dslite-install-project-")
  dir.create(file.path(tmp_proj, "utils", "setup"), recursive = TRUE)
  setup_file <- file.path(tmp_proj, "utils", "setup", "01_DSLite_Setup.R")
  file.copy(from = find_script("dslite/01_DSLite_Setup.R"), to = setup_file)
  
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  install_project <- NULL
  install_prompt <- NULL
  record_project <- NULL
  testthat::local_mocked_bindings(install = function(packages, project = NULL, prompt = TRUE, ...) {
    install_project <<- project
    install_prompt <<- prompt
    invisible(packages)
  }, .package = "renv")
  testthat::local_mocked_bindings(renv_record = function(project) {
    record_project <<- project
    invisible(project)
  })
  testthat::expect_message(dsAnalysis::install_dsPackage(dsPackage = "dsSurvival"), "Done: ")
  
  testthat::expect_equal(install_project, file.path(tmp_proj))
  testthat::expect_false(install_prompt)
  testthat::expect_equal(record_project, file.path(tmp_proj))
})
