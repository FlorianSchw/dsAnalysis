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
