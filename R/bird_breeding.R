bird_breeding <- function(out_dir,
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

  # I'll need to sort out lists of dataname and summaryFun if we have multiple
  # strictures based on different sets of stars polygons.
  # But trying to keep syntax consistent across the data functions.

  # This is really close to process_data, can we make it a function they each call?
  # If not chunking, don't need the inner chunked dir.
  # Note that this is largely a stub; I don't actually have a way to chunk stars full of polygons
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
  bird_polytime <- readRDS(file.path(out_dir, dataname, summaryFun, paste0(catchment, '_', summaryFun, '.rds'))) |>
    setNames(c('aggdata', 'indices'))

  # Read in the anaes
  anaes <- readRDS(file.path(out_dir, 'ANAEbirdbreed', paste0(catchment, '_ANAE.rds')))

  # Clean up rounding errors with area
  bird_polytime$aggdata <- clean_area(bird_polytime$aggdata, anaes)

  # We check the indices match when we make the data now, so don't check again I don't think.
  # # simple
  # all(anaes$UID == bird_polytime$indices$UID)
  # # complex
  # test <- matchStarsIndex(index1 = anaes, stars1 = NULL,
  #                 index2 = bird_polytime$indices, stars2 = bird_polytime$aggdata,
  #                 indexcol = c(1, 1),
  #                 testfinal = TRUE, as_test = TRUE)



  ## Stricture 1: Breeding season
  # Ideally, we'd look at Oct-Nov, but since Nov 1 is the cutoff, we want the Sept-Oct chunk, and according to the metadata, these dates are the start of the period, so we want Sept 1.

  times <- st_get_dimension_values(bird_polytime$aggdata, 'time')
  nobreedtimes <- which(lubridate::month(times) != 9)
  breedtimes <- which(lubridate::month(times) == 9)


  # make a new stars that only leaves data in Sept-Oct, ie zeros out other times
  bird_season <- bird_polytime$aggdata
  bird_season[[1]][ , nobreedtimes] <- 0

  # Actually, just drop the other time periods.
  # Don't tempaggregate since we only have one period per year. Just drop all the unused months. Otherwise it references to Jan 1.
  # bird_season_year <- tempaggregate(bird_season, by_t = 'year', FUN = maxna, dates_end_interval = FALSE) |>
  #   aperm(c('geometry', 'time'))

  bird_season_year <- bird_season[,,breedtimes]


  ## Stricture 2: Habitat type
  # These are from Kate and Gilad, paper in review
  breedtypes <- c('Freshwater meadow', 'Temporary tall emergent marsh',
                  'River cooba woodland riparian zone or floodplain', 'Temporary stream')

  typecheck <- anaes$ANAE_DESC %in% breedtypes

  #
  bird_season_anae <- bird_season_year[,typecheck,]

  # test/check- do we have the requisite types in the data? And each logical doesn't hit other stuff
  # all the data
  # anaes <- readRDS(file.path(out_dir, 'ANAEbirdbreed', 'ANAEbirdbreed.rds'))
  # anaetypes <- unique(anaes$ANAE_DESC) |> sort()
  # for (i in 1:length(breedtypes)) {
  #   print(anaetypes[grepl(breedtypes[i], anaetypes)])
  # }
  # anaetypes[grepl('Freshwater meadow', anaetypes)]
  # anaetypes[grepl('Temporary tall emergent', anaetypes)]
  # anaetypes[grepl('River cooba', anaetypes)]
  # anaetypes[grepl('Temporary stream', anaetypes)]



  ## Stricture 3 is a minimum area per anae type within the Wetland complex. It's not clear how to do that within the stars framework, I don't think.
  # Perhaps I can st_union the anaes? and then do a spatial aggregate? No, the union isn't maintained.
  # test <- anaes |>
  #   dplyr::filter(ANAE_DESC %in% breedtypes) |>
  #   dplyr::group_by(ANAE_DESC) |>
  #   sf::st_union()

  # I think what I'll do here is turn this into an sf, do the aggregation with groupings, and then re-stars. Or not.

  # Don't do the ANAE type filter at first, go back to season_year- it's easier to check things if we have everything.
  bird_sf <- st_as_sf(bird_season_year)
  class(bird_sf) <- class(anaes) # make it a tibble

  # check the indices, this is making me nervous
  inter3 <- diag(sf::st_intersects(sf::st_geometry(bird_polytime$indices),
                                   sf::st_geometry(bird_sf), sparse = FALSE))
  if (all(inter3)) {
    # rlang::inform("all good")
  } else if (!all(inter3)) {
    rlang::abort("polygons not ordered correctly")
  }


  # Be careful withe the joins to the ANAE sheet- going via indices is safest
  bird_sf <- bird_sf |>
    dplyr::bind_cols(sf::st_drop_geometry(bird_polytime$indices)) |>
    dplyr::left_join(sf::st_drop_geometry(anaes), by = 'UID')

  # Now, stack it by year
  bird_sf <- bird_sf |>
    tidyr::pivot_longer(contains('01'), names_to = 'time', values_to = names(bird_season_anae)) |>
    dplyr::mutate(time = lubridate::ymd(time))


  # At this point, the goal is to summarise breeding area over the whole wetland.
  bird_sf <- bird_sf |>
    sf::st_drop_geometry() |>
    dplyr::summarise(total_area = sum(areaBirdBreed_from_depth), .by = c(ANAE_DESC, time),
                     Wetland = unique(Wetland), State = unique(State), SWWRPAName = unique(SWWRPAName))

  # add the wetland polygon on
  # need to read them in.
  breeding_polys <- readRDS(file = file.path(out_dir, 'waterbird_breeding', 'breeding_polys.rds'))

  # Throw an inform on here due to overlap in the data- we want it to be treated as a single wetland, but it belongs about half and half in each state.
  rlang::inform("some bird strictures assessed at the level of wetland. Gunbower-Koondrook-Perricoota is kept together as a single wetland. If it is later desired to split between Victorian Murray and NSW Murray, should do a spatial intersection with WRPA.")

  # So, these are now each ANAE type in the larger wetland
  bird_asset <- bird_sf |>
    dplyr::left_join(breeding_polys, by = c('Wetland', 'State', 'SWWRPAName')) |>
    sf::st_as_sf()

  # drop the anaes again as a stricturef
  bird_asset_anae <- bird_asset |>
    dplyr::filter(ANAE_DESC %in% breedtypes)

  # and the minimal amount stricture
  areathreshold <- tibble::tibble(ANAE_DESC = breedtypes,
                                  area_min_hectares = c(10.67, 0.98, 5.57, 0.04),
                                  area_min_m2 = area_min_hectares*10000)

  bird_asset_anae_amount <- bird_asset_anae |>
    dplyr::left_join(areathreshold) |>
    dplyr::mutate(area_given_threshold = ifelse(total_area > area_min_m2, total_area, 0))

  # Now, just get the total breeding area meeting all requirements in the wetland
  # I think this likely makes more sense once the wetlands are joined, but this will do it too.
  bird_breed_area <- bird_asset_anae_amount |>
    dplyr::group_by(time) |>
    dplyr::summarise(breeding_area = sumna(area_given_threshold),
                     Wetland = unique(Wetland), State = unique(State), SWWRPAName = unique(SWWRPAName)) |>
    dplyr::ungroup()

  # return stuff

  # Do we want to return all of this? I'm not sure.
  bird_responses <- tibble::lst(bird_season_year,
                                bird_season_anae,
                                bird_asset,
                                bird_asset_anae,
                                bird_asset_anae_amount,
                                bird_breed_area)

  if (saveout) {
    saveRDS(bird_responses, file = file.path(scriptOut, paste0(unique_chunkname, '.rds')))

    # save each list-item separately. Just a different way to skin the cat, not sure which I'll prefer.
    for (i in names(bird_responses)) {
      if (!dir.exists(file.path(scriptOut, i))) {dir.create(file.path(scriptOut, i), recursive = TRUE)}
      saveRDS(bird_responses[[i]], file = file.path(scriptOut, i, paste0(catchment, '_', i, '.rds')))
    }
  }


  # Either return the list or a tibble of timings
  if (returnForR) {
    return(bird_responses)
  } else {
    end_time <- Sys.time()
    elapsed <- end_time-start_time

    sumtab <- tibble::tibble(catchment,
                             npolys = nrow(anaes),
                             elapsed)
    return(sumtab)
  }

}

sumna <- function(x) {
  sum(x, na.rm = TRUE)
}
