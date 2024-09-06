source("renv/activate.R")

if (grepl('^pearcey', Sys.info()["nodename"])) {
  oldpaths <- .libPaths() # Gets the renv libs
  .libPaths(new = c(oldpaths[1],'/apps/R/4.0.2/lib64/R/library' ))
} else if (grepl('^petrichor', Sys.info()["nodename"]) | grepl('^c', Sys.info()["nodename"])) {
  oldpaths <- .libPaths() # Gets the renv libs
  .libPaths(new = c(oldpaths[1],'/apps/R/4.0.5/lib64/R/library' ))
} else if (grepl('^gandalf', Sys.info()["nodename"])) {
  renvpaths <- .libPaths()
  .libPaths(new = c(renvpaths,'/ceph-g/opt/R/4.3/lib/R/library' ))
} else {
  # This is docker (rocker/geospatial) running on windows. I'm not sure how
  # stable these paths are or how to test for them more generally
  renvpaths <- .libPaths()
  .libPaths(new = c(renvpaths, "/usr/local/lib/R/site-library", "/usr/local/lib/R/library"))
}
