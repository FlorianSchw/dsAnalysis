#### adds and removes library() calls in step 1 of 01_DSLite_Setup.R; added ones go after
#### the last non-empty line of step 1, a call that is already there isn't added again
internal_dslite_set_library_calls <- function(codelines, add = character(0), remove = character(0)){

  step1_line <- internal_dslite_step_line(codelines, 1)
  step2_line <- internal_dslite_step_line(codelines, 2)
  step1_range <- step1_line:(step2_line - 1)

  drop <- step1_range[trimws(codelines[step1_range]) %in% paste0("library(", remove, ")")]
  if(length(drop) > 0){
    codelines <- codelines[-drop]
    step2_line <- step2_line - length(drop)
    step1_range <- step1_line:(step2_line - 1)
  }

  library_lines <- setdiff(paste0("library(", add, ")"), codelines[step1_range])
  if(length(library_lines) == 0){
    return(codelines)
  }

  last_line <- max(step1_range[nzchar(trimws(codelines[step1_range]))])
  append(codelines, library_lines, after = last_line)
}
