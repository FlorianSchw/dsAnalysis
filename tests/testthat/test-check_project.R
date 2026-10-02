test_that("check_project messages that there is nothing to check and returns NULL when the project has no renv.lock", {
  tmp_proj <- withr::local_tempdir("no-renv-")
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  testthat::expect_false(file.exists(file.path(tmp_proj, "renv.lock")))
  testthat::expect_message(res <- dsAnalysis::check_project(),
                           "This project doesn't use renv (no renv.lock), so there is nothing to check.",
                           fixed = TRUE)
  testthat::expect_null(res)
})

test_that("check_project reports a synchronized project and returns the comparison invisibly", {
  tmp_proj <- withr::local_tempdir("renv-sync-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_ok <- list(synchronized = TRUE,
                  recorded = character(0),
                  used_not_installed = character(0),
                  recorded_not_installed = character(0),
                  used_not_recorded = character(0),
                  other_version = character(0),
                  unexplained = FALSE)
  testthat::local_mocked_bindings(renv_in_use = function(project) TRUE,
                                  renv_compare = function(project) sync_ok)
  testthat::expect_message(res <- dsAnalysis::check_project(),
                           "All good: the installed packages match renv.lock.",
                           fixed = TRUE)
  testthat::expect_identical(res, sync_ok)
  testthat::expect_true(withVisible(dsAnalysis::check_project())$visible == FALSE) |> suppressMessages()
})

test_that("check_project lists used-but-not-installed packages, splitting recorded ones (restore) from unrecorded ones (install)", {
  tmp_proj <- withr::local_tempdir("renv-missing-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = c("dplyr"),
                   used_not_installed = c("dplyr", "dsBaseClient"),
                   recorded_not_installed = character(0),
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(renv_in_use = function(project) TRUE,
                                  renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_true(any(grepl("Used by the project but not installed: dplyr, dsBaseClient.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("renv::restore() installs dplyr in the versions recorded in renv.lock.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("dsBaseClient must be installed first: install_dsPackage() for DataSHIELD packages, renv::install() for others.", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project reports recorded-not-installed, used-not-recorded, other-version and unexplained differences in one run", {
  tmp_proj <- withr::local_tempdir("renv-diff-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  sync_bad <- list(synchronized = FALSE,
                   recorded = c("jsonlite", "glue"),
                   used_not_installed = character(0),
                   recorded_not_installed = c("jsonlite"),
                   used_not_recorded = c("stringr"),
                   other_version = c("glue"),
                   unexplained = TRUE)
  testthat::local_mocked_bindings(renv_in_use = function(project) TRUE,
                                  renv_compare = function(project) sync_bad)
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_true(any(grepl("Recorded in renv.lock but not installed: jsonlite.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("Used by the project but not recorded in renv.lock: stringr.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("Installed in another version than recorded in renv.lock: glue.", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("renv reports other differences", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("All good", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"restore\") calls renv::restore on the project and reports that everything matches afterwards", {
  tmp_proj <- withr::local_tempdir("renv-restore-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  restore_calls <- new.env(parent = emptyenv())
  restore_calls$n <- 0L
  restore_calls$project <- NULL
  restore_calls$prompt <- NULL
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  sync_bad <- list(synchronized = FALSE,
                   recorded = "jsonlite",
                   used_not_installed = character(0),
                   recorded_not_installed = "jsonlite",
                   used_not_recorded = character(0),
                   other_version = character(0),
                   unexplained = FALSE)
  sync_ok <- list(synchronized = TRUE,
                  recorded = "jsonlite",
                  used_not_installed = character(0),
                  recorded_not_installed = character(0),
                  used_not_recorded = character(0),
                  other_version = character(0),
                  unexplained = FALSE)
  testthat::local_mocked_bindings(renv_in_use = function(project) TRUE,
                                  renv_compare = function(project) {
                                    calls$n <- calls$n + 1L
                                    if (calls$n == 1L) sync_bad else sync_ok
                                  })
  testthat::local_mocked_bindings(restore = function(project, prompt, ...) {
                                    restore_calls$n <- restore_calls$n + 1L
                                    restore_calls$project <- project
                                    restore_calls$prompt <- prompt
                                    invisible(TRUE)
                                  }, .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "restore"))
  testthat::expect_equal(restore_calls$n, 1L)
  testthat::expect_equal(restore_calls$project, tmp_proj)
  testthat::expect_false(restore_calls$prompt)
  testthat::expect_true(any(grepl("All good now: the installed packages match renv.lock.", msgs, fixed = TRUE)))
  testthat::expect_equal(calls$n, 2L)
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"snapshot\") calls renv::snapshot and warns that the project is still not in sync when it stays unsynchronized", {
  tmp_proj <- withr::local_tempdir("renv-snapshot-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  snap <- new.env(parent = emptyenv())
  snap$n <- 0L
  snap$project <- NULL
  sync_bad <- list(synchronized = FALSE,
                   recorded = character(0),
                   used_not_installed = character(0),
                   recorded_not_installed = character(0),
                   used_not_recorded = "stringr",
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(renv_in_use = function(project) TRUE,
                                  renv_compare = function(project) sync_bad)
  testthat::local_mocked_bindings(snapshot = function(project, prompt, ...) {
                                    snap$n <- snap$n + 1L
                                    snap$project <- project
                                    invisible(TRUE)
                                  }, .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "snapshot"))
  testthat::expect_equal(snap$n, 1L)
  testthat::expect_equal(snap$project, tmp_proj)
  testthat::expect_true(any(grepl("Still not in sync. Run check_project() again to see what is left.", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("All good now", msgs, fixed = TRUE)))
  testthat::expect_identical(res, sync_bad)
})

test_that("check_project(fix = \"none\") neither restores nor snapshots and prints no follow-up sync message", {
  tmp_proj <- withr::local_tempdir("renv-none-")
  writeLines("{}", file.path(tmp_proj, "renv.lock"))
  lock_before <- readLines(file.path(tmp_proj, "renv.lock"))
  testthat::local_mocked_bindings(here = function(...) file.path(tmp_proj, ...), .package = "here")
  counters <- new.env(parent = emptyenv())
  counters$compare <- 0L
  counters$restore <- 0L
  counters$snapshot <- 0L
  sync_bad <- list(synchronized = FALSE,
                   recorded = character(0),
                   used_not_installed = character(0),
                   recorded_not_installed = character(0),
                   used_not_recorded = "stringr",
                   other_version = character(0),
                   unexplained = FALSE)
  testthat::local_mocked_bindings(renv_in_use = function(project) TRUE,
                                  renv_compare = function(project) {
                                    counters$compare <- counters$compare + 1L
                                    sync_bad
                                  })
  testthat::local_mocked_bindings(restore = function(...) counters$restore <- counters$restore + 1L,
                                  snapshot = function(...) counters$snapshot <- counters$snapshot + 1L,
                                  .package = "renv")
  msgs <- testthat::capture_messages(res <- dsAnalysis::check_project(fix = "none"))
  testthat::expect_equal(counters$restore, 0L)
  testthat::expect_equal(counters$snapshot, 0L)
  testthat::expect_equal(counters$compare, 1L)
  testthat::expect_false(any(grepl("All good now", msgs, fixed = TRUE)))
  testthat::expect_false(any(grepl("Still not in sync", msgs, fixed = TRUE)))
  testthat::expect_identical(readLines(file.path(tmp_proj, "renv.lock")), lock_before)
  testthat::expect_identical(res, sync_bad)
})
