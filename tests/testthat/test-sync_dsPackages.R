test_that("sync_dsPackages errors with a login hint and no call when there are no DataSHIELD connections", {
  hint <- "No DataSHIELD connections found. Log in first (R/01_DS_Login.R with R_CONFIG_ACTIVE = 'production')."

  #### none found in the session
  testthat::local_mocked_bindings(datashield.connections_find = function(...) list(), .package = "DSI")
  err <- testthat::expect_error(dsAnalysis::sync_dsPackages())
  testthat::expect_equal(err$message, hint)
  testthat::expect_null(err$call)

  #### an empty list given
  err <- testthat::expect_error(dsAnalysis::sync_dsPackages(conns = list(), install = FALSE))
  testthat::expect_equal(err$message, hint)
})

test_that("sync_dsPackages with install = FALSE returns invisibly a data frame with the lowest server version per package and the installed version", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(c(TRUE, TRUE, TRUE,
                              TRUE, TRUE, TRUE),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("dsBase", "dsOther"), c("sim1", "sim2", "sim3"))),
    version_status = matrix(c("6.3.0", "6.2.0", "6.3.0",
                              "1.0.0", "1.0.0", "1.0.0"),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("dsBase", "dsOther"), c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = c("default", "default", "default"),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  res <- NULL
  testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(res$package, c("dsBase", "dsOther"))
  testthat::expect_equal(res$servers, c("sim1, sim2, sim3", "sim1, sim2, sim3"))
  testthat::expect_equal(res$version, c("6.2.0", "1.0.0"))
  testthat::expect_equal(res$versions, c("6.3.0, 6.2.0", "1.0.0"))
  testthat::expect_true(is.na(res$installed[res$package == "dsOther"]))
})

test_that("sync_dsPackages with install = FALSE messages which packages have different versions on the servers", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(c(TRUE, TRUE, TRUE),
                            nrow = 1, byrow = TRUE,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3"))),
    version_status = matrix(c("6.3.0", "6.2.0", "6.3.0"),
                            nrow = 1, byrow = TRUE,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  testthat::expect_message(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE),
                           "dsBase has different versions on the servers \\(6\\.3\\.0, 6\\.2\\.0\\); version 6\\.2\\.0 is used here",
                           fixed = FALSE)
})

test_that("sync_dsPackages with install = FALSE messages the packages that are only on some servers", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(c(TRUE, TRUE, TRUE,
                              TRUE, FALSE, TRUE),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("dsBase", "dsOther"), c("sim1", "sim2", "sim3"))),
    version_status = matrix(c("6.3.0", "6.3.0", "6.3.0",
                              "1.0.0", NA, "1.0.0"),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("dsBase", "dsOther"), c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  testthat::expect_message(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE),
                           "dsOther is only on sim1, sim3.", fixed = TRUE)
  res <- NULL
  suppressMessages(testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE)))
  testthat::expect_equal(res$servers, c("sim1, sim2, sim3", "sim1, sim3"))
  testthat::expect_equal(res$version, c("6.3.0", "1.0.0"))
})

test_that("sync_dsPackages with install = FALSE lists all packages that would be installed in a 'To install' message", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(c(TRUE, TRUE, TRUE,
                              TRUE, TRUE, TRUE),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("dsNotInstalledA", "dsNotInstalledB"), c("sim1", "sim2", "sim3"))),
    version_status = matrix(c("1.0.0", "1.0.0", "1.0.0",
                              "2.0.0", "2.0.0", "2.0.0"),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c("dsNotInstalledA", "dsNotInstalledB"), c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  testthat::expect_message(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE),
                           "To install: dsNotInstalledA, dsNotInstalledB. Run sync_dsPackages() to install them.",
                           fixed = TRUE)
  res <- NULL
  suppressMessages(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_equal(res$package, c("dsNotInstalledA", "dsNotInstalledB"))
  testthat::expect_true(all(is.na(res$installed)))
})

test_that("sync_dsPackages with install = FALSE reports that all packages are installed when the installed version matches the server version", {
  setup <- local_cnsim_project()
  
  installed_version <- as.character(utils::packageVersion("DSI"))
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep(installed_version, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  testthat::expect_message(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE),
                           "All packages of the servers are installed in the same versions.", fixed = TRUE)
  res <- NULL
  suppressMessages(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_equal(res$package, "DSI")
  testthat::expect_equal(res$installed, installed_version)
  testthat::expect_equal(res$version, installed_version)
})

test_that("sync_dsPackages messages the non-default profiles of the servers and how to log in with them", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("6.3.0", 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = c("survival", "default", "default"),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  testthat::expect_message(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE),
                           "The servers use these DataSHIELD profiles: sim1 = survival, sim2 = default, sim3 = default.",
                           fixed = TRUE)
})

test_that("sync_dsPackages with install = FALSE does not message profiles when all servers use the default profile", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("6.3.0", 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  msgs <- testthat::capture_messages(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_false(any(grepl("DataSHIELD profiles", msgs, fixed = TRUE)))
  testthat::expect_length(msgs, 1)
})

test_that("sync_dsPackages still reports the packages when datashield.profiles errors", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("6.3.0", 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) stop("no profiles here"),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  res <- NULL
  suppressMessages(testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE)))
  testthat::expect_equal(res$package, "dsBase")
  testthat::expect_equal(res$version, "6.3.0")
  testthat::expect_equal(nrow(res), 1L)
})

test_that("sync_dsPackages uses the connections from datashield.connections_find when conns is NULL", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("6.3.0", 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3")))
  )
  
  seen <- NULL
  
  testthat::local_mocked_bindings(
    datashield.connections_find = function(...) setup$conns,
    datashield.profiles = function(conns, ...){
      seen <<- names(conns)
      list(current = data.frame(profile = rep("default", 3), row.names = c("sim1", "sim2", "sim3")))
    },
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  res <- NULL
  suppressMessages(res <- dsAnalysis::sync_dsPackages(install = FALSE))
  testthat::expect_equal(seen, c("sim1", "sim2", "sim3"))
  testthat::expect_equal(res$servers, "sim1, sim2, sim3")
})

test_that("sync_dsPackages with install = TRUE falls back to the latest version when the server version can't be installed", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsDev", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("6.3.6.9000", 3), nrow = 1,
                            dimnames = list("dsDev", c("sim1", "sim2", "sim3")))
  )
  
  calls <- list()
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    install_dsPackage = function(package, version = NULL, ...){
      calls[[length(calls) + 1]] <<- list(package = package, version = version)
      if(!is.null(version)) stop("no such release")
      invisible(TRUE)
    }
  )
  testthat::expect_message(dsAnalysis::sync_dsPackages(conns = setup$conns, install = TRUE),
                           "dsDev: installed its latest version instead.", fixed = TRUE)
  testthat::expect_length(calls, 2)
  testthat::expect_equal(calls[[1]]$package, "dsDev")
  testthat::expect_equal(calls[[1]]$version, "6.3.6.9000")
  testthat::expect_null(calls[[2]]$version)
})

test_that("sync_dsPackages with install = TRUE installs only the packages whose installed version differs from the server version", {
  setup <- local_cnsim_project()
  
  installed_version <- as.character(utils::packageVersion("DSI"))
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 6), nrow = 2, byrow = TRUE,
                            dimnames = list(c("DSI", "dsMissing"), c("sim1", "sim2", "sim3"))),
    version_status = matrix(c(rep(installed_version, 3), rep("1.2.3", 3)), nrow = 2, byrow = TRUE,
                            dimnames = list(c("DSI", "dsMissing"), c("sim1", "sim2", "sim3")))
  )
  
  calls <- list()
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    install_dsPackage = function(package, version = NULL, ...){
      calls[[length(calls) + 1]] <<- list(package = package, version = version)
      invisible(TRUE)
    }
  )
  res <- NULL
  testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = TRUE))
  testthat::expect_length(calls, 1)
  testthat::expect_equal(calls[[1]]$package, "dsMissing")
  testthat::expect_equal(calls[[1]]$version, "1.2.3")
  testthat::expect_equal(res$package, c("DSI", "dsMissing"))
  testthat::expect_equal(res$installed, c(installed_version, NA_character_))
})

test_that("sync_dsPackages with install = TRUE messages that a package was not installed when both the version and the latest install fail", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsBroken", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("9.9.9", 3), nrow = 1,
                            dimnames = list("dsBroken", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    install_dsPackage = function(package, version = NULL, ...) stop("repository down")
  )
  msgs <- testthat::capture_messages(dsAnalysis::sync_dsPackages(conns = setup$conns, install = TRUE))
  testthat::expect_true(any(grepl("Version 9.9.9 of dsBroken can't be installed: repository down", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("dsBroken was not installed: repository down", msgs, fixed = TRUE)))
})

test_that("sync_dsPackages with install = TRUE adds installed packages that are missing from the DSLite setup file via add_dsPackage", {
  setup <- local_cnsim_project()
  
  installed_version <- as.character(utils::packageVersion("DSI"))
  
  dir.create(file.path(setup$project, "utils", "setup"), recursive = TRUE, showWarnings = FALSE)
  writeLines("# DSLite setup file", file.path(setup$project, "utils", "setup", "01_DSLite_Setup.R"))
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep(installed_version, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3")))
  )
  
  added <- NULL
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    internal_dslite_included_packages = function(...) character(0),
    internal_ds_catalogue = function(...) NULL,
    add_dsPackage = function(package, client = NULL, ...){
      added <<- list(package = package, client = client)
      invisible(TRUE)
    }
  )
  res <- NULL
  testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = TRUE))
  testthat::expect_equal(added$package, "DSI")
  testthat::expect_equal(added$client, "DSIClient")
  testthat::expect_equal(res$package, "DSI")
})

test_that("sync_dsPackages with install = TRUE does not call add_dsPackage when the package is already in the DSLite setup file", {
  setup <- local_cnsim_project()
  
  installed_version <- as.character(utils::packageVersion("DSI"))
  
  dir.create(file.path(setup$project, "utils", "setup"), recursive = TRUE, showWarnings = FALSE)
  writeLines("# DSLite setup file", file.path(setup$project, "utils", "setup", "01_DSLite_Setup.R"))
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep(installed_version, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3")))
  )
  
  add_calls <- 0L
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    internal_dslite_included_packages = function(...) c("dsBase", "DSI"),
    add_dsPackage = function(package, client = NULL, ...){
      add_calls <<- add_calls + 1L
      invisible(TRUE)
    }
  )
  res <- NULL
  testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = TRUE))
  testthat::expect_equal(add_calls, 0L)
  testthat::expect_equal(res$package, "DSI")
  testthat::expect_equal(res$installed, installed_version)
})

test_that("sync_dsPackages with install = TRUE uses the catalogue client names when a catalogue is available", {
  setup <- local_cnsim_project()
  
  installed_version <- as.character(utils::packageVersion("DSI"))
  
  dir.create(file.path(setup$project, "utils", "setup"), recursive = TRUE, showWarnings = FALSE)
  writeLines("# DSLite setup file", file.path(setup$project, "utils", "setup", "01_DSLite_Setup.R"))
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep(installed_version, 3), nrow = 1,
                            dimnames = list("DSI", c("sim1", "sim2", "sim3")))
  )
  
  added <- NULL
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    internal_dslite_included_packages = function(...) character(0),
    internal_ds_catalogue = function(...) data.frame(server = "DSI", client = "DSIFancyClient"),
    internal_catalogue_client = function(package, catalogue, ...) "DSIFancyClient",
    add_dsPackage = function(package, client = NULL, ...){
      added <<- list(package = package, client = client)
      invisible(TRUE)
    }
  )
  res <- NULL
  testthat::expect_invisible(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = TRUE))
  testthat::expect_equal(added$package, "DSI")
  testthat::expect_equal(added$client, "DSIFancyClient")
  testthat::expect_equal(nrow(res), 1L)
})

test_that("sync_dsPackages picks the lowest version by numeric order, not alphabetically, when server versions differ in digit length", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3"))),
    version_status = matrix(c("6.10.0", "6.9.0", "6.10.0"), nrow = 1,
                            dimnames = list("dsBase", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  res <- NULL
  suppressMessages(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_equal(res$version, "6.9.0")
  testthat::expect_equal(res$versions, "6.10.0, 6.9.0")
  testthat::expect_equal(res$servers, "sim1, sim2, sim3")
})

test_that("sync_dsPackages with install = FALSE does not install anything even when packages are missing", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(rep(TRUE, 3), nrow = 1,
                            dimnames = list("dsNotInstalled", c("sim1", "sim2", "sim3"))),
    version_status = matrix(rep("1.0.0", 3), nrow = 1,
                            dimnames = list("dsNotInstalled", c("sim1", "sim2", "sim3")))
  )
  
  install_calls <- 0L
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  
  testthat::local_mocked_bindings(
    install_dsPackage = function(package, version = NULL, ...){
      install_calls <<- install_calls + 1L
      invisible(TRUE)
    }
  )
  res <- NULL
  suppressMessages(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_equal(install_calls, 0L)
  testthat::expect_true(is.na(res$installed))
  testthat::expect_equal(res$package, "dsNotInstalled")
})

test_that("sync_dsPackages reports a package that is on only one server with that server's version", {
  setup <- local_cnsim_project()
  
  testhat_status <- list(
    package_status = matrix(c(FALSE, TRUE, FALSE), nrow = 1,
                            dimnames = list("dsOnlyOne", c("sim1", "sim2", "sim3"))),
    version_status = matrix(c(NA, "2.5.0", NA), nrow = 1,
                            dimnames = list("dsOnlyOne", c("sim1", "sim2", "sim3")))
  )
  
  testthat::local_mocked_bindings(
    datashield.profiles = function(...) list(current = data.frame(profile = rep("default", 3),
                                                                 row.names = c("sim1", "sim2", "sim3"))),
    datashield.pkg_status = function(...) testhat_status,
    .package = "DSI"
  )
  msgs <- testthat::capture_messages(dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_true(any(grepl("dsOnlyOne is only on sim2.", msgs, fixed = TRUE)))
  res <- NULL
  suppressMessages(res <- dsAnalysis::sync_dsPackages(conns = setup$conns, install = FALSE))
  testthat::expect_equal(res$servers, "sim2")
  testthat::expect_equal(res$version, "2.5.0")
  testthat::expect_equal(res$versions, "2.5.0")
})
