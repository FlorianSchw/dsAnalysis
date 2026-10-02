#'
#' @title Checks that the installed packages match the project's renv.lock
#' @param fix "restore", "snapshot", or NULL to ask
#' @export
#'

check_project <- function(fix = NULL){

  project <- here::here()

  if(!renv_in_use(project)){
    message("This project doesn't use renv (no renv.lock), so there is nothing to check.")
    return(invisible(NULL))
  }

  sync <- renv_compare(project)

  if(sync$synchronized){
    message("All good: the installed packages match renv.lock.")
    return(invisible(sync))
  }

  if(length(sync$used_not_installed) > 0){
    recorded <- intersect(sync$used_not_installed, sync$recorded)
    other <- setdiff(sync$used_not_installed, sync$recorded)
    message("Used by the project but not installed: ", paste(sync$used_not_installed, collapse = ", "), ".")
    if(length(recorded) > 0){
      message("  -> renv::restore() installs ", paste(recorded, collapse = ", "), " in the versions recorded in renv.lock.")
    }
    if(length(other) > 0){
      message("  -> ", paste(other, collapse = ", "), " must be installed first: install_dsPackage() for DataSHIELD packages, ",
              "renv::install() for others.")
    }
  }

  if(length(sync$recorded_not_installed) > 0){
    message("Recorded in renv.lock but not installed: ", paste(sync$recorded_not_installed, collapse = ", "), ".\n",
            "  -> renv::restore() installs them in the recorded versions.")
  }

  if(length(sync$used_not_recorded) > 0){
    message("Used by the project but not recorded in renv.lock: ", paste(sync$used_not_recorded, collapse = ", "), ".\n",
            "  -> renv::snapshot() records them.")
  }

  if(length(sync$other_version) > 0){
    message("Installed in another version than recorded in renv.lock: ", paste(sync$other_version, collapse = ", "), ".\n",
            "  -> renv::restore() goes back to the recorded versions, renv::snapshot() records the installed ones.")
  }

  if(sync$unexplained){
    message("renv reports other differences (e.g. in the packages' own dependencies); renv::snapshot() usually fixes them. ",
            "renv::status() shows the details.")
  }

  if(is.null(fix) && interactive()){
    choice <- utils::menu(c("Install what renv.lock records (renv::restore())",
                            "Record what is installed in renv.lock (renv::snapshot())",
                            "Nothing for now"),
                          title = "What should be done?")
    fix <- c("restore", "snapshot", "none")[choice]
  }

  if(identical(fix, "restore")){
    renv::restore(project = project, prompt = FALSE)
  } else if(identical(fix, "snapshot")){
    renv_quietly(renv::snapshot(project = project, prompt = FALSE))
  }

  if(identical(fix, "restore") || identical(fix, "snapshot")){
    if(renv_compare(project)$synchronized){
      message("All good now: the installed packages match renv.lock.")
    } else {
      message("Still not in sync. Run check_project() again to see what is left.")
    }
  }

  invisible(sync)

}

#### TRUE if the project uses renv
renv_in_use <- function(project){
  file.exists(file.path(project, "renv.lock"))
}

#### runs a renv call without its printout (the functions here report in their own words)
renv_quietly <- function(expr){
  result <- NULL
  utils::capture.output(result <- suppressMessages(suppressWarnings(expr)))
  invisible(result)
}

#### compares what the project uses, what is installed and what renv.lock records
renv_compare <- function(project){

  status <- renv_quietly(renv::status(project = project))
  used <- renv_quietly(renv::dependencies(project, progress = FALSE))$Package

  base_packages <- rownames(utils::installed.packages(priority = "base"))
  used <- setdiff(unique(used), c(base_packages, "renv"))

  recorded_packages <- status$lockfile$Packages
  installed_packages <- status$library$Packages
  recorded <- names(recorded_packages)
  installed <- names(installed_packages)

  versions <- function(x) vapply(x, function(p) if (is.null(p$Version)) NA_character_ else p$Version, "")
  common <- intersect(recorded, installed)

  sync <- list(synchronized = isTRUE(status$synchronized),
               recorded = recorded,
               used_not_installed = sort(setdiff(used, installed)),
               recorded_not_installed = sort(setdiff(setdiff(recorded, installed), used)),
               used_not_recorded = sort(setdiff(intersect(used, installed), recorded)),
               other_version = sort(common[versions(recorded_packages[common]) != versions(installed_packages[common])]))

  sync$unexplained <- !sync$synchronized &&
    all(lengths(sync[c("used_not_installed", "recorded_not_installed", "used_not_recorded", "other_version")]) == 0)

  sync
}

#### records the project's packages in renv.lock and says in plain words whether all is in sync
renv_record <- function(project){

  if(!renv_in_use(project)){
    message("This project doesn't use renv (no renv.lock), so nothing was recorded.")
    return(invisible(FALSE))
  }

  renv_quietly(renv::snapshot(project = project, prompt = FALSE))

  if(renv_compare(project)$synchronized){
    message("Recorded in renv.lock.")
    return(invisible(TRUE))
  }

  message("renv.lock was updated, but something is still out of sync: run check_project() to see what.")
  invisible(FALSE)
}
