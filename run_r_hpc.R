
# I want this to be a function, but the hpc nesting makes it easier if it's a script.

# run_R_hpc <- function(comargs = commandArgs()) {
  # First, we source in the directory management parameters
  source('directorySet.R') # This typically gets called in the script itself.


  rlang::inform(c("command args are ", glue::glue("{commandArgs()}")), .file = stdout())


  # Then, we build the R from qmd if needed.
  infile <- commandArgs()[6]
  rfile <- stringr::str_replace(infile, '.qmd', '.R')

  knitr::purl(input = infile, output = rfile)

  source(rfile)
# }
