#### rewrites the dslite.server$config(...) call in step 4 with these server packages
internal_dslite_write_included_packages <- function(codelines, packages){

  config_lines <- internal_dslite_config_lines(codelines)
  config_start <- "dslite.server$config(DSLite::defaultDSConfiguration(include=c("
  indent <- strrep(" ", nchar(config_start))

  quoted <- paste0("\"", packages, "\"")
  config_new <- paste0(c(config_start, rep(indent, length(quoted) - 1)),
                       quoted,
                       c(rep(",", length(quoted) - 1), ")))"))

  c(codelines[seq_len(config_lines[1] - 1)],
    config_new,
    codelines[(config_lines[length(config_lines)] + 1):length(codelines)])
}
