test_that("internal_dslite_config_lines returns a single index when the config call is directly followed by dslite.server$profile()", {
  codelines <- c(
    "#### Step 4: configure",
    "dslite.server$config(DSLite::defaultDSConfiguration(include = 'dsBase'))",
    "dslite.server$profile()",
    "#### Step 5: login"
  )
  testthat::local_mocked_bindings(internal_dslite_step_line = function(codelines, step) if (step == 4) 1L else 4L, .package = "dsAnalysis")
  res <- dsAnalysis:::internal_dslite_config_lines(codelines)
  testthat::expect_identical(res, 2L:2L)
  testthat::expect_length(res, 1L)
})

test_that("internal_dslite_config_lines errors when no dslite.server$config( line exists in step 4", {
  codelines <- c(
    "#### Step 4: configure",
    "something_else <- 1",
    "dslite.server$profile()",
    "#### Step 5: login"
  )
  testthat::local_mocked_bindings(internal_dslite_step_line = function(codelines, step) if (step == 4) 1L else 4L, .package = "dsAnalysis")
  testthat::expect_error(dsAnalysis:::internal_dslite_config_lines(codelines),
                         "Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
                         fixed = TRUE)
})

test_that("internal_dslite_config_lines errors when the dslite.server$profile() line is missing from step 4", {
  codelines <- c(
    "#### Step 4: configure",
    "dslite.server$config(DSLite::defaultDSConfiguration(include = 'dsBase'))",
    "#### Step 5: login"
  )
  testthat::local_mocked_bindings(internal_dslite_step_line = function(codelines, step) if (step == 4) 1L else 3L, .package = "dsAnalysis")
  testthat::expect_error(dsAnalysis:::internal_dslite_config_lines(codelines),
                         "Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
                         fixed = TRUE)
})

test_that("internal_dslite_config_lines errors when step 4 contains two dslite.server$config( lines", {
  codelines <- c(
    "#### Step 4: configure",
    "dslite.server$config(a)",
    "dslite.server$config(b)",
    "dslite.server$profile()",
    "#### Step 5: login"
  )
  testthat::local_mocked_bindings(internal_dslite_step_line = function(codelines, step) if (step == 4) 1L else 5L, .package = "dsAnalysis")
  testthat::expect_error(dsAnalysis:::internal_dslite_config_lines(codelines),
                         "Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
                         fixed = TRUE)
})

test_that("internal_dslite_config_lines ignores config and profile lines that lie outside the step 4 range", {
  codelines <- c(
    "dslite.server$config(early)",
    "dslite.server$profile()",
    "#### Step 4: configure",
    "dslite.server$config(real)",
    "  include = 'dsBase'",
    "dslite.server$profile()",
    "#### Step 5: login",
    "dslite.server$config(late)",
    "dslite.server$profile()"
  )
  testthat::local_mocked_bindings(internal_dslite_step_line = function(codelines, step) if (step == 4) 3L else 7L, .package = "dsAnalysis")
  res <- dsAnalysis:::internal_dslite_config_lines(codelines)
  testthat::expect_identical(res, 4:5)
  testthat::expect_identical(codelines[res], c("dslite.server$config(real)", "  include = 'dsBase'"))
})

test_that("internal_dslite_config_lines does not match an indented dslite.server$config( line and errors", {
  codelines <- c(
    "#### Step 4: configure",
    "  dslite.server$config(DSLite::defaultDSConfiguration())",
    "dslite.server$profile()",
    "#### Step 5: login"
  )
  testthat::local_mocked_bindings(internal_dslite_step_line = function(codelines, step) if (step == 4) 1L else 4L, .package = "dsAnalysis")
  testthat::expect_error(dsAnalysis:::internal_dslite_config_lines(codelines),
                         "Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
                         fixed = TRUE)
})
