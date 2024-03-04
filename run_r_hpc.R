run_R_hpc <- function(comargs = commandArgs()) {
  # First, we source in the directory management parameters
  source('directorySet.R')


  rlang::inform(c("command args are ", glue::glue("{comargs}")))

  # Then, we build the R from qmd if needed.
  infile <- comargs[6]
  rfile <- stringr::str_replace(infile, '.qmd', '.R')

  knitr::purl(input = infile, output = rfile)

  source(rfile)
}
