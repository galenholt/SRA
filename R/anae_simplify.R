anae_simplify <- function(out_dir,
                          catchment,
                          keep = 0.05,
                          keep_shapes = TRUE,
                          explode = TRUE,
                          thischunk,
                          nchunks = 1,
                          whichcrs = 3577) {
  starttime <- Sys.time()

  # Read in the anaes
  anaes <- readRDS(file.path(out_dir, 'ANAEcatchment',
                             paste0(catchment, '_ANAE.rds')))

  # simplify them
  anaes_simple <- rmapshaper::ms_simplify(anaes,
                                          keep = keep, keep_shapes = keep_shapes, explode = explode)

  return(anaes_simple)
}
