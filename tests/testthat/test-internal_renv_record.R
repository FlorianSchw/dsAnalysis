test_that("internal_renv_record messages that the project doesn't use renv and returns invisible FALSE when renv.lock is missing", {
  tmp <- withr::local_tempdir()
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "This project doesn't use renv (no renv.lock), so nothing was recorded.",
                           fixed = TRUE)
  testthat::expect_false(res)
  testthat::expect_false(file.exists(file.path(tmp, "renv.lock")))
})

test_that("internal_renv_record calls renv::snapshot with the project and prompt = FALSE and reports being recorded when the comparison is synchronized", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    seen$project <- project
    seen$prompt <- prompt
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "Recorded in renv.lock.", fixed = TRUE)
  testthat::expect_true(res)
  testthat::expect_identical(seen$project, tmp)
  testthat::expect_false(seen$prompt)
})

test_that("internal_renv_record messages that something is still out of sync and returns invisible FALSE when the comparison is not synchronized", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) invisible(NULL), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = FALSE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "renv.lock was updated, but something is still out of sync: run check_project() to see what.",
                           fixed = TRUE)
  testthat::expect_false(res)
})

test_that("internal_renv_record returns its value invisibly in both the no-lockfile and the synchronized case", {
  tmp_nolock <- withr::local_tempdir()
  tmp_lock <- withr::local_tempdir()
  writeLines("{}", file.path(tmp_lock, "renv.lock"))
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) invisible(NULL), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  out_nolock <- testthat::capture_output(testthat::expect_message(dsAnalysis:::internal_renv_record(tmp_nolock)))
  testthat::expect_identical(out_nolock, "")
  out_lock <- testthat::capture_output(testthat::expect_message(dsAnalysis:::internal_renv_record(tmp_lock)))
  testthat::expect_identical(out_lock, "")
})

test_that("internal_renv_record does not call renv::snapshot or internal_renv_compare when renv.lock is missing", {
  tmp <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$snapshot <- 0L
  calls$compare <- 0L
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    calls$snapshot <- calls$snapshot + 1L
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    calls$compare <- calls$compare + 1L
    list(synchronized = TRUE)
  })
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp),
                           "nothing was recorded", fixed = TRUE)
  testthat::expect_false(res)
  testthat::expect_identical(calls$snapshot, 0L)
  testthat::expect_identical(calls$compare, 0L)
})

test_that("internal_renv_record passes the same project path to internal_renv_compare that it passes to renv::snapshot", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    seen$snapshot_project <- project
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    seen$compare_project <- project
    list(synchronized = TRUE)
  })
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp), "Recorded in renv.lock.", fixed = TRUE)
  testthat::expect_true(res)
  testthat::expect_identical(seen$compare_project, tmp)
  testthat::expect_identical(seen$compare_project, seen$snapshot_project)
})

test_that("internal_renv_record suppresses output printed by renv::snapshot via internal_renv_quietly and still returns TRUE", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    cat("noisy snapshot output\n")
    print("also printed")
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  out <- testthat::capture_output(testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp), "Recorded in renv.lock.", fixed = TRUE))
  testthat::expect_identical(out, "")
  testthat::expect_true(res)
})

test_that("internal_renv_record keeps the existing renv.lock file on disk unchanged when the mocked snapshot writes nothing", {
  tmp <- withr::local_tempdir()
  lock <- file.path(tmp, "renv.lock")
  writeLines(c("{", "  \"R\": {}", "}"), lock)
  before <- readLines(lock)
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) invisible(NULL), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = FALSE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp), "still out of sync", fixed = TRUE)
  testthat::expect_false(res)
  testthat::expect_true(file.exists(lock))
  testthat::expect_identical(readLines(lock), before)
})

test_that("internal_renv_record emits exactly one message in the synchronized case and in the missing-lockfile case", {
  tmp_lock <- withr::local_tempdir()
  writeLines("{}", file.path(tmp_lock, "renv.lock"))
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) invisible(NULL), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  msgs_lock <- testthat::capture_messages(dsAnalysis:::internal_renv_record(tmp_lock))
  testthat::expect_length(msgs_lock, 1L)
  testthat::expect_identical(msgs_lock, "Recorded in renv.lock.\n")
  tmp_nolock <- withr::local_tempdir()
  msgs_nolock <- testthat::capture_messages(dsAnalysis:::internal_renv_record(tmp_nolock))
  testthat::expect_length(msgs_nolock, 1L)
  testthat::expect_identical(msgs_nolock, "This project doesn't use renv (no renv.lock), so nothing was recorded.\n")
})

test_that("internal_renv_record detects renv.lock in a nested project directory given as a relative path and records it", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  dir.create(file.path("nested", "proj"), recursive = TRUE)
  writeLines("{}", file.path("nested", "proj", "renv.lock"))
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    seen$project <- project
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(file.path("nested", "proj")),
                           "Recorded in renv.lock.", fixed = TRUE)
  testthat::expect_true(res)
  testthat::expect_identical(seen$project, file.path("nested", "proj"))
})

test_that("internal_renv_record propagates an error raised by renv::snapshot and never reaches the comparison", {
  tmp <- withr::local_tempdir()
  writeLines("{}", file.path(tmp, "renv.lock"))
  calls <- new.env(parent = emptyenv())
  calls$compare <- 0L
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) stop("snapshot failed badly"), .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) {
    calls$compare <- calls$compare + 1L
    list(synchronized = TRUE)
  })
  testthat::expect_error(dsAnalysis:::internal_renv_record(tmp), "snapshot failed badly", fixed = TRUE)
  testthat::expect_identical(calls$compare, 0L)
})

test_that("internal_renv_record treats a directory named renv.lock as present and proceeds to snapshot", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "renv.lock"))
  calls <- new.env(parent = emptyenv())
  calls$snapshot <- 0L
  testthat::local_mocked_bindings(snapshot = function(project, prompt = TRUE, ...) {
    calls$snapshot <- calls$snapshot + 1L
    invisible(NULL)
  }, .package = "renv")
  testthat::local_mocked_bindings(internal_renv_compare = function(project) list(synchronized = TRUE))
  testthat::expect_message(res <- dsAnalysis:::internal_renv_record(tmp), "Recorded in renv.lock.", fixed = TRUE)
  testthat::expect_true(res)
  testthat::expect_identical(calls$snapshot, 1L)
})
