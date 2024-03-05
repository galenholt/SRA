# Set up directory locations
# directorySet <- function() {
#   # this function
# }
# print(paste0('opened directoryset, time is ', Sys.time(), ', run is ', dataWhere))
# print(Sys.info())

# This gets sent to stderr when non-interactive (ie. hpc)
rlang::inform(c("Sys.info is ", glue::glue("{Sys.info()}")))


# Rather than a function, I want this to set a few things, so building as a script instead
# Set directory structure for pearcey (or petrichor)
# Pearcey seems to have all the nodes named 'c###'
# trying to avoid needing to say sysname == Linux, because would be nice to run on local linux
if (grepl('^petrichor', Sys.info()["nodename"]) |
    grepl('^pearcey', Sys.info()["nodename"]) |
    grepl('^c', Sys.info()["nodename"])) {

  if (dataWhere == 'SCRATCH') {
    # Let's assume I cp from flush/scratch/whatever into JOBDIR at the start of the job.
    datDir <-  file.path(Sys.getenv('SCRATCH1DIR'), 'dataBase')
    datOut <- file.path(Sys.getenv('SCRATCH1DIR'), 'datOut')
  } else if (dataWhere == 'JOB') {
    # Or, if we read to jobdir first
    # This needs a slightly different .sh, because it needs to read into/out of jobdir
    datDir <-  file.path(Sys.getenv('JOBDIR'), 'dataBase')
    datOut <- file.path(Sys.getenv('JOBDIR'), 'datOut')
  } else if (dataWhere == 'MER') {
    # Or, if we work straight out of Bowen
    datDir <-  file.path('/datasets/work/lw-mer/work/galen_holt/dataBase')
    datOut <- file.path('/datasets/work/lw-mer/work/galen_holt/datOut')
  }

  print(paste0('datOut is ', datOut))
  source(".Rprofile")
  print('libPaths in directorySet are now')
  print(.libPaths())

  # sort out libraries for HPC
  source('renvHPC.R')

  # set the future plan to actually use the CPUS
  parSet <- 'hpc'

} else if (grepl('^Windows', Sys.info()["sysname"])) {
  # myhome <- stringr::str_remove(path.expand("~"), "/Documents")
  myhome <- paste0('C:/Users/', Sys.getenv("USERNAME"))
  datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase")

  datOut <- "datOut"

  # Set up a multisession plan by default
  # The default workers argument works locally, so no need to specify
  parSet = 'local'

} else if (grepl('^gandalf', Sys.info()["nodename"])) {
  datDir <- file.path('..', 'dataBase')
  datOut <- 'datOut'

  parset <- 'gandalf'

  renvpaths <- .libPaths()
  .libPaths(new = c(renvpaths,'/ceph-g/opt/R/4.3/lib/R/library' ))

}

# Make the out directory, in case it doesn't exist
if (!dir.exists(datOut)) {dir.create(datOut, recursive = TRUE)}
# The in directory has to exist, or there won't be anything to use

# # source everything in the functions folder. This really is turning into a package
# funfiles <- list.files('R', pattern = '.R$')
# # funfiles
# for (s in 1:length(funfiles)) {
#   # print(s)
#   source(file.path('R', funfiles[s]))
# }
# This is now better, or just build the package and library it.
# path_to_efloweval <- '../eFlowEval'
# devtools::load_all(path_to_efloweval)
# progressr::handlers(global = TRUE)
