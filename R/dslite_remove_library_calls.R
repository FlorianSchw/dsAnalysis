#### removes library() calls from step 1
dslite_remove_library_calls <- function(codelines, packages){

  step1_line <- dslite_step_line(codelines, 1)
  step2_line <- dslite_step_line(codelines, 2)
  step1_range <- step1_line:(step2_line - 1)

  drop <- step1_range[trimws(codelines[step1_range]) %in% paste0("library(", packages, ")")]

  if(length(drop) == 0){
    return(codelines)
  }

  codelines[-drop]
}
