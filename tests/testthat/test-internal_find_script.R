test_that("internal_find_script errors with the developer-team message when the script does not exist in the package templates", {
  testthat::expect_error(
    dsAnalysis:::internal_find_script("definitely_not_a_template_file_12345.R"),
    "Could not find the file: definitely_not_a_template_file_12345\\.R\\. Please contact the developer team\\.",
    fixed = FALSE
  )
  err <- testthat::expect_error(dsAnalysis:::internal_find_script("definitely_not_a_template_file_12345.R"))
  testthat::expect_identical(err$message,
                             "Could not find the file: definitely_not_a_template_file_12345.R. Please contact the developer team.")
  testthat::expect_null(conditionCall(err))
})

test_that("internal_find_script errors when the package has no templates directory at all", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgWithoutTemplates")
  dir.create(file.path(pkg, "Meta"), recursive = TRUE)
  writeLines(c("Package: pkgWithoutTemplates", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  withr::local_libpaths(lib, action = "prefix")
  testthat::expect_error(
    dsAnalysis:::internal_find_script("anything.R", package = "pkgWithoutTemplates"),
    "Could not find the file: anything.R. Please contact the developer team.",
    fixed = TRUE
  )
})

test_that("internal_find_script returns the existing path of a template file inside an installed package", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgWithTemplates")
  dir.create(file.path(pkg, "templates"), recursive = TRUE)
  writeLines(c("Package: pkgWithTemplates", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  writeLines("#### template content", file.path(pkg, "templates", "my_script.R"))
  withr::local_libpaths(lib, action = "prefix")
  res <- dsAnalysis:::internal_find_script("my_script.R", package = "pkgWithTemplates")
  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_true(file.exists(res))
  testthat::expect_identical(basename(res), "my_script.R")
  testthat::expect_identical(basename(dirname(res)), "templates")
  testthat::expect_identical(readLines(res), "#### template content")
})

test_that("internal_find_script errors for a package that is not installed", {
  testthat::expect_error(
    dsAnalysis:::internal_find_script("some_script.R", package = "thisPackageDoesNotExist12345"),
    "Could not find the file: some_script.R. Please contact the developer team.",
    fixed = TRUE
  )
})

test_that("internal_find_script finds a template in the real dsAnalysis package and returns a path inside its templates folder", {
  tmpl_dir <- fs::path_package(package = "dsAnalysis", "templates")
  tmpl_files <- list.files(tmpl_dir)
  testthat::expect_gt(length(tmpl_files), 0L)
  res <- dsAnalysis:::internal_find_script(tmpl_files[[1]])
  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_true(file.exists(res))
  testthat::expect_identical(basename(res), tmpl_files[[1]])
  testthat::expect_identical(basename(dirname(res)), "templates")
})

test_that("internal_find_script uses dsAnalysis as the default package so a template of another package is not found by default", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgDefaultCheck")
  dir.create(file.path(pkg, "templates"), recursive = TRUE)
  writeLines(c("Package: pkgDefaultCheck", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  writeLines("#### only in the other package", file.path(pkg, "templates", "only_other_pkg_script.R"))
  withr::local_libpaths(lib, action = "prefix")
  found <- dsAnalysis:::internal_find_script("only_other_pkg_script.R", package = "pkgDefaultCheck")
  testthat::expect_identical(basename(found), "only_other_pkg_script.R")
  testthat::expect_error(
    dsAnalysis:::internal_find_script("only_other_pkg_script.R"),
    "Could not find the file: only_other_pkg_script.R. Please contact the developer team.",
    fixed = TRUE
  )
})

test_that("internal_find_script returns the path of a template in a nested sub-folder of templates", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgNestedTemplates")
  dir.create(file.path(pkg, "templates", "sub"), recursive = TRUE)
  writeLines(c("Package: pkgNestedTemplates", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  writeLines("#### nested template", file.path(pkg, "templates", "sub", "nested_script.R"))
  withr::local_libpaths(lib, action = "prefix")
  res <- dsAnalysis:::internal_find_script(file.path("sub", "nested_script.R"), package = "pkgNestedTemplates")
  testthat::expect_length(res, 1L)
  testthat::expect_true(file.exists(res))
  testthat::expect_identical(basename(res), "nested_script.R")
  testthat::expect_identical(basename(dirname(res)), "sub")
  testthat::expect_identical(readLines(res), "#### nested template")
})

test_that("internal_find_script errors when the templates folder exists but is empty", {
  lib <- withr::local_tempdir()
  pkg <- file.path(lib, "pkgEmptyTemplates")
  dir.create(file.path(pkg, "templates"), recursive = TRUE)
  writeLines(c("Package: pkgEmptyTemplates", "Version: 0.0.1", "Type: Package",
               "Title: Test", "Description: Test.", "License: MIT", "Author: t",
               "Maintainer: t <t@t.org>", "Built: R 4.0.0; ; now; unix"),
             file.path(pkg, "DESCRIPTION"))
  withr::local_libpaths(lib, action = "prefix")
  testthat::expect_length(list.files(file.path(pkg, "templates")), 0L)
  err <- testthat::expect_error(
    dsAnalysis:::internal_find_script("missing_here.R", package = "pkgEmptyTemplates")
  )
  testthat::expect_identical(err$message,
                             "Could not find the file: missing_here.R. Please contact the developer team.")
  testthat::expect_null(conditionCall(err))
})
