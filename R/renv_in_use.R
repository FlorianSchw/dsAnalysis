#### TRUE if the project uses renv
renv_in_use <- function(project){
  file.exists(file.path(project, "renv.lock"))
}
