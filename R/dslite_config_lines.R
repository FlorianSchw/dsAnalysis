#### lines of the dslite.server$config(...) call in step 4
dslite_config_lines <- function(codelines){

  step4_line <- dslite_step_line(codelines, 4)
  step5_line <- dslite_step_line(codelines, 5)
  step4_range <- step4_line:(step5_line - 1)

  config_start <- step4_range[startsWith(codelines[step4_range], "dslite.server$config(")]
  config_end <- step4_range[startsWith(codelines[step4_range], "dslite.server$profile()")]

  if(!(length(config_start) == 1) || !(length(config_end) == 1)){
    stop("Could not find the dslite.server$config(...) call in step 4 of 01_DSLite_Setup.R.",
         call. = FALSE)
  }

  config_start:(config_end - 1)
}
