anae_diversity <- function(out_dir,
                           strictname,
                           dataname,
                           summaryFun,
                           catchment,
                           thischunk,
                           nchunks = 1,
                           whichcrs = 3577,
                           saveout = TRUE,
                           returnForR = FALSE) {
  start_time <- Sys.time()

  if (nchunks == 1) {
    scriptOut <- file.path(out_dir, strictname)
    # chunk names, don't support subchunks
    unique_chunkname <- paste0(catchment, '_', strictname)
    unique_indexname <- paste0(catchment, '_', strictname, '_index')
  } else if (nchunks > 1) {
    scriptOut <- file.path(out_dir, strictname, 'chunked',
                           chunkpath)
    # chunk names, don't support subchunks
    unique_chunkname <- paste0(catchment, '_', strictname, '_', thischunk)
    unique_indexname <- paste0(catchment, '_', strictname, '_index', '_', thischunk)
  }

  # Make the out directory, in case it doesn't exist
  if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}

  ## DATA IN
  # Read in the data
  anae_inun <- readRDS(file.path(out_dir, dataname, summaryFun, paste0(catchment, '_', summaryFun, '.rds'))) |>
    setNames(c('aggdata', 'indices'))

  # Read in the anaes
  anaes <- readRDS(file.path(out_dir, 'ANAEcatchment', paste0(catchment, '_ANAE.rds')))

  # Clean up rounding errors with area
  anae_inun$aggdata <- clean_area(anae_inun$aggdata, anaes)

  # we need to extract from ltimNoNorth sometimes, but needs to match how 'catchment' is handled
  catches <- ltimNoNorth |>
    mutate(name_clean = stringr::str_remove_all(ValleyName, ' '))
  ## 'Responses'
  # These aren't really 'responses', per se, but things we want to calculate.

  ## Number 1: let's do everything on a yearly timestep, so aggregate everything to the water year.

  availdates <- st_get_dimension_values(anae_inun$aggdata, which = 'time')
  startyear <- lubridate::year(min(availdates))-1
  endyear <- lubridate::year(max(availdates)) + 1
  # we want to cut at June 30, and so need to make sure the 07-01 go into the next step.
  datebreaks <- startyear:endyear
  datebreaks <- paste0(as.character(datebreaks), '0701') |> lubridate::ymd() |> as.POSIXct()
  # check
  # cutfac <- tibble(availdates, cut(availdates, datebreaks))

  diversity_year_inun <- tempaggregate(starObj = anae_inun$aggdata, by = datebreaks, FUN = max, na.rm = TRUE) |>
    aperm(c('geometry', 'time'))

  ## Number 2: total area. Not sure this is actually very interesting.
  diversity_catch_area <- aggregate(diversity_year_inun, by = catches[catches$name_clean == catchment, ], FUN = sum, na.rm = TRUE)

  ## Number 3: number of anaes inundated
  diversity_catch_number <- diversity_year_inun
  diversity_catch_number[[1]] <- diversity_catch_number[[1]] > 0 # just ask if yes/no, so now each with inundation has a 1, and we can sum
  diversity_catch_number <- aggregate(diversity_catch_number, by = catches[catches$name_clean == catchment, ], FUN = sum, na.rm = TRUE)

  ## DIVERSITY- we're going to have to go to sf for this, I think. And there are a few things we could do, as with any diversity metric
  diversity_yearly_sf <- sf::st_as_sf(diversity_year_inun)
  class(diversity_yearly_sf) <- class(anaes)
  # check the indices, this is making me nervous
  inter3 <- diag(sf::st_intersects(sf::st_geometry(anae_inun$indices),
                                   sf::st_geometry(diversity_yearly_sf), sparse = FALSE))
  if (all(inter3)) {
    # rlang::inform("all good")
  } else if (!all(inter3)) {
    rlang::abort("polygons not ordered correctly")
  }

  # Be careful with the joins to the ANAE sheet- going via indices is safest, since we just double checked that.
  diversity_yearly_sf <- diversity_yearly_sf |>
    dplyr::bind_cols(sf::st_drop_geometry(anae_inun$indices)) |>
    dplyr::left_join(sf::st_drop_geometry(anaes), by = 'UID')

  # Now, stack it by year
  diversity_yearly_sf <- diversity_yearly_sf |>
    tidyr::pivot_longer(contains('01'), names_to = 'time', values_to = names(diversity_year_inun)) |>
    dplyr::mutate(time = lubridate::ymd(time))
  # we could just return this and do all the calcs later. But we'd never be able to work on multiple catchments at once. So do that, but give ourselves the option to do other things too.
  # That will be the only way to get basin-scale diversity though.

  ## Now we can get at diversity metrics
    # And these will be done at the catchment scale, so add that polygon on, typically.

  # diversity_richness- get it for both type and unique wetland
  diversity_richness <- diversity_yearly_sf |>
    st_drop_geometry() |>
    mutate(n_anaes = dplyr::n_distinct(ANAE_DESC),
           n_UIDs = dplyr::n_distinct(UID), .by = c(ValleyName, time)) |>
    filter(areaInun05cm_from_depth > 0) |>
    summarise(anae_diversity_richness = dplyr::n_distinct(ANAE_DESC),
              diversity_richness_proportion = anae_diversity_richness/unique(n_anaes),
              UID_diversity_richness = dplyr::n_distinct(UID),
              UID_proportion = UID_diversity_richness/unique(n_UIDs),
              .by = c(ValleyName, time)) |>
    arrange(time, ValleyName) |>
    rename(name_clean = ValleyName) |>
    left_join(catches, by = 'name_clean')

  # Shannon
    # Not doing this for UID, it doesn't seem that interesting, but maybe I'm just not thinking about something.

  # a function to get it from a vector of values, each of which corresponds to some measure of an anae type (number or area)
  calc_shan <- function(x) {
    px <- x/sum(x)
    shan <- -sum(px*log(px))
    return(shan)
  }

  # Shannon (based on number)
  diversity_shannon_number <- diversity_yearly_sf |>
    st_drop_geometry() |>
    mutate(area_binary = areaInun05cm_from_depth > 0) |> # just whether it's area or not
    summarise(n_anaes_type = sum(area_binary), .by = c(ANAE_DESC, time, ValleyName)) |> # the 'abundance', e.g. number of wetlands of each type
    filter(n_anaes_type > 0) |> # otherwise the log dies, and they don't count anyway.
    summarise(shannon_binary = calc_shan(n_anaes_type), .by = c(time, ValleyName)) |>
    arrange(time, ValleyName) |>
    rename(name_clean = ValleyName) |>
    left_join(catches, by = 'name_clean')

  # Shannon (based on area)
  diversity_shannon_area <- diversity_yearly_sf |>
    st_drop_geometry() |>
    summarise(total_area = sum(areaInun05cm_from_depth), .by = c(ANAE_DESC, time, ValleyName)) |> # the 'abundance', e.g. number of wetlands of each type
    filter(total_area > 0) |> # otherwise the log dies, and they don't count anyway.
    summarise(diversity_shannon_area = calc_shan(total_area), .by = c(time, ValleyName)) |>
    arrange(time, ValleyName) |>
    rename(name_clean = ValleyName) |>
    left_join(catches, by = 'name_clean')

  # Beta- if this means turnover/dissimilarity between catchments, we can't do
  # that here, will need to go glue all the diversity_yearly_sfs together. That probably
  # would need its own function/notebook.

  # Temporal-
    # Some measure of turnover?
    # Or do we want to go Bray-Curtis/Jaccard etc? Maybe if this becomes a paper, but probably not for SRA?
    # are we getting the same ANAE types each year? The same UIDs even? This isn't quite the same question as above.

  # just get simple Beta (gamma/alpha) for now

  # First, get the diversity_richness of the whole sequence
  total_binary_diversity_richness <- diversity_yearly_sf |>
    st_drop_geometry() |>
    mutate(area_binary = areaInun05cm_from_depth > 0) |>
    filter(area_binary > 0) |>
    summarise(gamma_diversity_anae = dplyr::n_distinct(ANAE_DESC),
              gamma_diversity_UID = dplyr::n_distinct(UID))

  diversity_temporal_abgamma <- diversity_richness |>
    bind_cols(total_binary_diversity_richness) |>
    group_by(ValleyName) |>
    summarise(alpha_diversity_anae = mean(anae_diversity_richness),
              gamma_diversity_anae = mean(gamma_diversity_anae),
              beta_diversity_anae = gamma_diversity_anae/alpha_diversity_anae,
              alpha_diversity_UID = mean(UID_diversity_richness),
              gamma_diversity_UID = mean(gamma_diversity_UID),
              beta_diversity_UID = gamma_diversity_UID/alpha_diversity_UID) |>
    ungroup()



  # list them up to save, I guess. Though I'm leaning away from this.
  diversity_responses <- tibble::lst(diversity_year_inun, # yearly inundation aggregation (stars)
                                     diversity_catch_area, # aggregated area to catchment (stars)
                                     diversity_catch_number, # aggregated number ANAEs to catchment (stars)
                                     diversity_yearly_sf, # an sf with all the anaes. Huge but powerful
                                     diversity_richness, # number of distinct types of ANAE (sf)
                                     diversity_shannon_number, # shannon diversity of number of wetted ANAES of each type (sf)
                                     diversity_shannon_area, # shannon diversity of area of wetted ANAEs of each type (sf)
                                     diversity_temporal_abgamma # diversity of anaes and UIDs for the catchment on average (alpha), in total (gamma), and beta (gamma/alpha).
                                     )

  if (saveout) {
    saveRDS(diversity_responses, file = file.path(scriptOut, paste0(unique_chunkname, '.rds')))

    # save each list-item separately. Just a different way to skin the cat, not sure which I'll prefer.
    for (i in names(diversity_responses)) {
      if (!dir.exists(file.path(scriptOut, i))) {dir.create(file.path(scriptOut, i), recursive = TRUE)}
      saveRDS(diversity_responses[[i]], file = file.path(scriptOut, i, paste0(catchment, '_', i, '.rds')))
    }
  }


  # Either return the list or a tibble of timings
  if (returnForR) {
    return(diversity_responses)
  } else {
    end_time <- Sys.time()
    elapsed <- end_time-start_time

    sumtab <- tibble::tibble(catchment,
                             npolys = nrow(anaes),
                             elapsed)
    return(sumtab)
  }

}
