#### adds library() calls to step 1, after the existing ones
dslite_add_library_calls <- function(codelines, packages){

  step1_line <- dslite_step_line(codelines, 1)
  step2_line <- dslite_step_line(codelines, 2)

  library_lines <- paste0("library(", packages, ")")
  library_lines <- setdiff(library_lines, codelines[step1_line:(step2_line - 1)])

  #### after the last non-empty line of step 1
  step1_range <- step1_line:(step2_line - 1)
  last_line <- max(step1_range[nzchar(trimws(codelines[step1_range]))])

  append(codelines, library_lines, after = last_line)
}
