# I don't think the renv install is necessary-it should auto-install since there's a project skeleton

# This bit is needed for gandalf, or it stuffs up the install because of sf

renvpaths <- .libPaths()
.libPaths(new = c(renvpaths,'/ceph-g/opt/R/4.3/lib/R/library' ))
Sys.setenv('R_LIBS' = '/ceph-g/opt/R/4.3/lib/R/library')
# TESTING DELETE
library(foreach)
library(doFuture)

plan(list(tweak(future.batchtools::batchtools_slurm,
                template = "batchtools.slurm.tmpl",
                resources = list(time = 10,
                                 ntasks.per.node = 12,
                                 mem = "70GB",
                                 job.name = 'area_inundated')),
          multicore))

mi <- list(rver = getRversion(), libs = .libPaths())
wi %<-% list(rver = getRversion(), libs = .libPaths())

mi
wi
# END TESTING

renv::install('git@github.com:galenholt/eFlowEval.git@dev', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)
# renv::install('git@github.com:galenholt/eFlowEval.git', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)

deps <- unique(renv::dependencies()$Package)
pkgavail <- dimnames(installed.packages())[[1]]

not_installed <- deps[!deps %in% pkgavail]

if (length(not_installed) > 0) {
  message(paste0("These packages are not installed: ", not_installed,
          '.\nThey are in the object `not_installed`, so first thing to try is `renv::install(not_installed)`\n',
          'sf may need admin help due to C libraries\n',
          'eFlowEval and other github packages seem to need to be handled manually'))
}

# NOTE:
# renv might think it has installed sf, but fail, and then need to actually remove it and just send through the .libPaths.
