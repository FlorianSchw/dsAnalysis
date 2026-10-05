#### records the project's packages in renv.lock and says in plain words whether all is in sync
internal_renv_record <- function(project){

  if(!file.exists(file.path(project, "renv.lock"))){
    message("This project doesn't use renv (no renv.lock), so nothing was recorded.")
    return(invisible(FALSE))
  }

  internal_renv_quietly(renv::snapshot(project = project, prompt = FALSE))

  if(internal_renv_compare(project)$synchronized){
    message("Recorded in renv.lock.")
    return(invisible(TRUE))
  }

  message("renv.lock was updated, but something is still out of sync: run check_project() to see what.")
  invisible(FALSE)
}
