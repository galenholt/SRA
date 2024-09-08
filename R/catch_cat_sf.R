#' Helper to read in and concatenate sfs for each catchment from the standard output structures
#'
#' @param which_strict the name of the stricture level (e.g. 'germ_area')
#' @param outerdir the directory (e.g. 'black_box')
#' @param catchments The set of catchments
#'
#' @return
#' @export
#'
#' @examples
catch_cat_sf <- function(which_strict, outerdir, catchments = 'all') {
  if ('all' %in% catchments) {
    cleancatch <- eFlowEval::ltimNoNorth |>
      mutate(name_clean = stringr::str_remove_all(ValleyName, ' '))

    catchments <- cleancatch$name_clean
  }

  foreach(i = catchments,
          .combine = dplyr::bind_rows,
          .multicombine = TRUE) %do% {
            thiscatch <- readRDS(file.path(datOut, outerdir, which_strict,
                                           paste0(i, '_', which_strict, '.rds')))
          }
}
