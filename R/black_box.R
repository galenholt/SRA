black_box <- function(out_dir,
                          catchment,
                          thischunk,
                          nchunks = 1,
                          whichcrs = 3577) {


  # I'll need to sort out lists of dataname and summaryFun if we have multiple
  # strictures based on different sets of stars polygons.
  # But trying to keep syntax consistent across the data functions.

    # Hard to make the data arguments, could do it with a list, but still not very general


  ## DATA IN
  # Need soil moisture and inundation and anaes
  soilmoist_polys <- readRDS(file.path(out_dir, 'soilmoisture',
                                       'areamoist_10_30', 'no_roll',
                                       paste0(catchment, '_areamoist_10_30', '.rds'))) |>
    setNames(c('aggdata', 'indices'))

  # Read in the data
  anae_inun <- readRDS(file.path(out_dir, 'inundation', 'areaInun05cm',
                                 paste0(catchment, '_', 'areaInun05cm', '.rds'))) |>
    setNames(c('aggdata', 'indices'))

  # Read in the anaes
  anaes <- readRDS(file.path(out_dir, 'ANAEcatchment',
                             paste0(catchment, '_ANAE.rds')))

  # are they lined up? This was checked on creation, so not doing here, but leaving commented for later confirmation/debug
  # testsm <- matchStarsIndex(index1 = anaes, stars1 = NULL, index2 = soilmoist_polys$indices, stars2 = soilmoist_polys$aggdata, indexcol = c(1,1), testfinal = TRUE)
  # testinun <- matchStarsIndex(index1 = anaes, stars1 = NULL, index2 = anae_inun$indices, stars2 = anae_inun$aggdata, indexcol = c(1,1), testfinal = TRUE)

  # Clean up rounding errors with area
  soilmoist_polys$aggdata <- clean_area(soilmoist_polys$aggdata, anaes)
  anae_inun$aggdata <- clean_area(anae_inun$aggdata, anaes)

  # ANAE type stricture

  # Two ways of doing this- one from the name of the anae, and one from where they're found in ALA (see veg1_anae_mapping).
  # In both cases, we develop the stricture at the end, but we read in the ALA side here.
  anae_types <- readRDS(file.path(out_dir, 'vegmapping', 'black_box_anae_type.rds'))


  # Stricture 1: Germination
  # North: May to Oct, South Nov to March
  # inundation immediately preceding,
  # then 10 days of moisture 10-30%

  # Define the N basin according to https://www.mdba.gov.au/water-management/northern-basin (n of B-D and Macquarie, inclusive)
  n_catches <- c('Barwon Darling', "Macquarie", "Castlereagh", 'Paroo', 'Warrego',
                 'Condamine Balonne', 'Border Rivers', "Gwydir", 'Namoi', 'Castlereagh') |>
    stringr::str_remove_all(' ')

  all_catches <- ltimNoNorth$ValleyName |> stringr::str_remove_all(' ')
  s_catches <- all_catches[!all_catches %in% n_catches]

  # So, since inundation timestep is at start of the data,
  # In north, we want inundation in May-June, July-Aug, and Sept-Oct, months 5, 7, 9 in inundation
  # In south, we want inundation in Nov-Dec, Jan-Feb, Mar-April (extend a bit), months 11, 1, 3 in inundation
  # We can include the even months here to hit the daily data, and they just get ignored in the inundation
  if (catchment %in% n_catches) {
    i_month <- c(5:10)
  } else if (catchment %in% s_catches) {
    i_month <- c(1:4, 11:12)
  }

  times <- st_get_dimension_values(anae_inun$aggdata, 'time')
  # Easier to set 0 than 1
  nogermtimes <- which(!lubridate::month(times) %in% i_month)

  # Only the area of inundation if in the right season
    # We may not actually need this at all- if we do the soil moisture it'll pick this up, with the time shifts around month starts, which is better anyway
  germ_inun_season <- anae_inun$aggdata
  germ_inun_season[[1]][ , nogermtimes] <- 0

  # soil moisture- the data is the daily area between 10-30, so we want the minimum of that over the last 10 days.
  soilmoist_10day <- timeRoll(soilmoist_polys$aggdata,
                                   FUN = RcppRoll::roll_min,
                                   rolln = 10,
                                   align = 'right',
                                   na.rm = TRUE)

  # and then we want to know if there was inundation 10 days ago. this is going
  # to be a bit of a fudge due to the bimonthly inundation- we won't be able to
  # get daily 10-day lookbacks. It's too strict to just look at the end of the
  # two-month periods, so for each day, if it's within 10 days of a bimonth
  # break, we'll ask about the preceding, otherwise, we'll ask about the
  # current. This will overestimate (since this will also capture periods of
  # inundation), but that seems better than the alternative. IE this isn't going
  # to be about 10 days post-inundation, it's just whether there was maybe
  # inundation nearby and soil moisture of > 10% for 10 days It seems like that
  # will be too strict (e.g. 11 days will fail), but because inundation is
  # bimonthly, there's almost no situation where that's actually an issue- the
  # 11th day almost always has the same inundation as the 10th.

  # Strangely, the easiest way to do this is probably to expand the inundation to daily
  # We can do that by abusing unevenTimeMult by filling with 1, since it multiplies a coarse and fine stars by each other and returns the fine

  # This returns NA for all times before inundatin (as it should)
  # use germ_inun_season here, since we care about the germ itself here.
  daily_inun <- unevenTimeMult(fineStars = soilmoist_polys$aggdata*0+1,
                               coarseStars = germ_inun_season,
                               lag = 0)

  # So, what's the assessment here? for each day, the soil moisture is the min
  # of the preceding 10 days. And then we need to ask if there was inundation 10
  # days ago. And get the min of those two numbers- only areas that were
  # inundated, and that retained 10%.

  # There's almost certainly a better way to do this with `lag`, but that really wants a timeseries
  shift_inun <- daily_inun[[1]][, -1:-11] # shift the time-cols over
  shift_inun <- cbind(shift_inun, daily_inun[[1]][, 1:11]*NA) # put the same number of NA cols at the end so we can multiply the matrices

  germ_area <- soilmoist_10day
  germ_area[[1]] <- pmin(shift_inun, soilmoist_10day[[1]])

  # Stage 2: Seedling survival
  # Moisture must be maintained above 10% (and less than 30?) for two years. Nothing ever passes for 2. Try 1? Or 1/2. The ref is pretty dodgy and at one point says 'at least through the summer'
  # No more than 70 days inundation
  # and it needs to happen following germination

  # soil moisture- the data is the daily area between 10-30, so we want the minimum of that over the last 2 years.
  soilmoist_2yr <- timeRoll(soilmoist_polys$aggdata,
                              FUN = RcppRoll::roll_min,
                            rolln = 183,
                              # rolln = 730,
                              align = 'right',
                              na.rm = TRUE)

  # No inundation > 70 days. Let's say 1 bimonth OK, 2 is a fail.
  # First, get the *un*inundated area
  # back to just anae_inun here, because the seasonality comes in with the did germ happen check.
  area_not_inundated <- anae_inun$aggdata
  area_not_inundated[[1]] <- (anae_inun$indices |>
                           st_area() |>
                           as.numeric()) -
    anae_inun$aggdata[[1]]

  # Then the max of that over two bimonths- this is the area that wasn't inundated for too long
  area_not_4mo <- timeRoll(area_not_inundated,
                           FUN = RcppRoll::roll_max,
                           rolln = 2,
                           align = 'right',
                           na.rm = TRUE)

  # Then the min of that over two years (12 bimonths) gives us the area that never got inundated too much in two years
  area_not_2yr <- timeRoll(area_not_4mo,
                       FUN = RcppRoll::roll_min,
                       rolln = 3, # Cut to 6 mo now.
                       # rolln = 12,
                       align = 'right',
                       na.rm = TRUE)

  # now we want the area that had enough moisture and not too much inundation over the two years.
  # again, abuse unevenTimeMult
  # This returns NA for all times before inundatin (as it should)
  daily_not_area <- unevenTimeMult(fineStars = soilmoist_2yr*0+1,
                               coarseStars = area_not_2yr,
                               lag = 0)

  # Then survival is
  seedling_area <- soilmoist_2yr
  seedling_area[[1]] <- pmin(soilmoist_2yr[[1]], daily_not_area[[1]])

  # I don't think we want to be precious about exactly two years ago for germ-
  # this asks if conditions have been right for two years of seedling survival,
  # but we can ask if germ happened within the last 18mo-2years.
  # How to do that? Get 6-month germ lookback max. Then shift 18mo forward, and compare with seedling area.
  germ_6m <- timeRoll(germ_area,
                           FUN = RcppRoll::roll_max,
                           rolln = 183,
                           align = 'right',
                           na.rm = TRUE)


  germ_shift18 <- cbind(germ_6m[[1]][, 1:365]*NA, germ_6m[[1]])
  # germ_shift18 <- cbind(germ_6m[[1]][, 1:548]*NA, germ_6m[[1]])
  # germ_shift18 <- germ_shift18[,-(ncol(germ_shift18)-547):-ncol(germ_shift18)]
  germ_shift18 <- germ_shift18[,-(ncol(germ_shift18)-364):-ncol(germ_shift18)]


  # now the area that germinated AND then survived an 18-24mo seedling stage is the minimum
  germ_and_seed <- seedling_area
  germ_and_seed[[1]] <- pmin(seedling_area[[1]], germ_shift18)

  # Stage 3: Adults
  # At least one flood in 8 years
  # Duration 2-6 months
  # Do we want to couple to seedlings? ie needs to have been seedling survival x
  # years in the past? I think no- presumably many of these trees are older than
  # the data we have. We can just report on condition for regeneration (germ and
  # seedlings) and conditions for persistence.

  # The min inundation over 6 mo (3 bimonths) is the amount that fails that test
  inun_6m <- timeRoll(anae_inun$aggdata,
                        FUN = RcppRoll::roll_min,
                        rolln = 3,
                        align = 'right',
                        na.rm = TRUE)

  # Then we need the MAX of that over 8 years, as this is the amount that doesn't count in the 8-year check because it was too wet
  inun_6m8y <- timeRoll(inun_6m,
                     FUN = RcppRoll::roll_max,
                     rolln = 48,
                     align = 'right',
                     na.rm = TRUE)

  # The max inun over 8 years is the amount that passes the 8-year requirement
  inun_8y <- timeRoll(anae_inun$aggdata,
                      FUN = RcppRoll::roll_max,
                      rolln = 48,
                      align = 'right',
                      na.rm = TRUE)

  # and the area that passes both is the difference
  adult_condition <- inun_8y-inun_6m8y

  # To return, let's aggregate up to water year
  availdates <- st_get_dimension_values(soilmoist_polys$aggdata, which = 'time')
  startyear <- lubridate::year(min(availdates))-1
  endyear <- lubridate::year(max(availdates)) + 1
  # we want to cut at June 30, and so need to make sure the 07-01 go into the next step.
  datebreaks <- startyear:endyear
  datebreaks <- paste0(as.character(datebreaks), '0701') |> lubridate::ymd() |> as.POSIXct()

  # I'm going to use the mean, even though these are only pseudo-daily. That captures a time-dependence that the max would miss (e.g. one possible string of 10 days, vs every day).
  # Germination
  germ_area_year <- tempaggregate(starObj = germ_area, by = datebreaks,
                                  FUN = mean, na.rm = TRUE) |>
    aperm(c('geometry', 'time'))

  # Seedlings
  seedling_area_year <- tempaggregate(starObj = seedling_area, by = datebreaks,
                                  FUN = mean, na.rm = TRUE) |>
    aperm(c('geometry', 'time'))

  # All recruitment
  germ_and_seed_year <- tempaggregate(starObj = germ_and_seed, by = datebreaks,
                                    FUN = mean, na.rm = TRUE) |>
    aperm(c('geometry', 'time'))

  # Adults
  adult_year <- tempaggregate(starObj = adult_condition, by = datebreaks,
                                    FUN = mean, na.rm = TRUE) |>
    aperm(c('geometry', 'time'))

  # Just inundation- useful for if we don't actually have strictures. This is
  # also done in diversity analyses to this point, but the idea is to do the
  # anae cut below, and so is moving towards a different comparison.
    # And I'm using the mean here to match the other stuff above.
  anae_year <- tempaggregate(starObj = anae_inun$aggdata, by = datebreaks,
                              FUN = mean, na.rm = TRUE) |>
    aperm(c('geometry', 'time'))

  ## ANAE Types

  # Option 1: it has the name 'black box'- can just do this in a mutate

  # Option 2: it's an anae type with records from ALA
  # Let's say it needs to have at least 0.5% of the records to be appreciable.
  boxala <- anae_types |>
    filter(n_records > 0.005*sum(anae_types$n_records)) |>
    select(ANAE_DESC) |>
    pull() |>
    unique() # Should be, but ensure

  # add those as columns to anaes
  anaes_bb <- anaes |>
    # option 1- by name
    dplyr::mutate(name_anae = grepl('black|box', ANAE_DESC, ignore.case = TRUE),
                  # option 2: by ala record
                  ala_anae = ANAE_DESC %in% boxala)

  # Then, cut the tempaggregates above.
  # This is getting factorial fast.
  germ_anae_name <- germ_area_year * anaes_bb$name_anae
  germ_anae_ala <- germ_area_year * anaes_bb$ala_anae

  seedling_anae_name <- seedling_area_year * anaes_bb$name_anae
  seedling_anae_ala <- seedling_area_year * anaes_bb$ala_anae

  germ_and_seed_anae_name <- germ_and_seed_year * anaes_bb$name_anae
  germ_and_seed_anae_ala <- germ_and_seed_year * anaes_bb$ala_anae

  adult_anae_name <- adult_year * anaes_bb$name_anae
  adult_anae_ala <- adult_year * anaes_bb$ala_anae

  # BUT ALSO, one that JUST cuts ANAE_inun by type- what if there's no info
  # about strictures and all we have is ANAE or records?
  inun_anae_name <- anae_year * anaes_bb$name_anae
  inun_anae_ala <- anae_year * anaes_bb$ala_anae


  ## Catchment-scale so read-in is OK? still save the upper stuff for posterity,
  #but mainly use the agged. We could use catchAggW, but I kind of like the sf
  #approach I used for anae_diversity better. Especially because we know we're
  #in `catchment` and so can avoid spatial joins that might have a bit of
  #spillover etc We could generalise that, especially since here we don't need
  #to retain the UIDs for each anae.

  catchpoly <- ltimNoNorth |>
    dplyr::mutate(ValleyName = stringr::str_remove_all(ValleyName, ' ')) |>
    dplyr::filter(ValleyName == catchment) |>
    dplyr::select(ValleyName, geometry)

  # we need to return NA if all values are NA, else use na.rm = TRUE. sum alone returns 0. The sumna function does that.
  # not anae-clipped
  germ_catch <- sf_and_aggforce(germ_area_year, catchpoly, newname = 'area', funlist = sumna)
  seed_catch <- sf_and_aggforce(seedling_area_year, catchpoly, newname = 'area', funlist = sumna)
  germ_and_seed_catch <- sf_and_aggforce(germ_and_seed_year, catchpoly, newname = 'area', funlist = sumna)
  adult_catch <- sf_and_aggforce(adult_year, catchpoly, newname = 'area', funlist = sumna)
  inun_anae_catch <- sf_and_aggforce(anae_year, catchpoly, newname = 'area', funlist = sumna)

  # ANAE clipped
  germ_anae_name_catch <- sf_and_aggforce(germ_anae_name, catchpoly, newname = 'area', funlist = sumna)
  seed_anae_name_catch <- sf_and_aggforce(seedling_anae_name, catchpoly, newname = 'area', funlist = sumna)
  germ_and_seed_anae_name_catch <- sf_and_aggforce(germ_and_seed_anae_name, catchpoly, newname = 'area', funlist = sumna)
  adult_anae_name_catch <- sf_and_aggforce(adult_anae_name, catchpoly, newname = 'area', funlist = sumna)
  inun_anae_name_catch <- sf_and_aggforce(inun_anae_name, catchpoly, newname = 'area', funlist = sumna)

  germ_anae_ala_catch <- sf_and_aggforce(germ_anae_ala, catchpoly, newname = 'area', funlist = sumna)
  seed_anae_ala_catch <- sf_and_aggforce(seedling_anae_ala, catchpoly, newname = 'area', funlist = sumna)
  germ_and_seed_anae_ala_catch <- sf_and_aggforce(germ_and_seed_anae_ala, catchpoly, newname = 'area', funlist = sumna)
  adult_anae_ala_catch <- sf_and_aggforce(adult_anae_ala, catchpoly, newname = 'area', funlist = sumna)
  inun_anae_ala_catch <- sf_and_aggforce(inun_anae_ala, catchpoly, newname = 'area', funlist = sumna)


  # list it up
  black_box_responses <- tibble::lst(germ_catch, # Area of germination met
                                     seed_catch, # Area of seedling survival met
                                     germ_and_seed_catch, # Area of germination followed by seed survival
                                     adult_catch, # area of acceptable adult condition
                                     inun_anae_catch, # area of inundation in *all* ANAEs
                                     germ_anae_name_catch, # area of germ met in anaes with 'black box'
                                     germ_anae_ala_catch, # Area of germ met in anaes with ala records
                                     seed_anae_name_catch, # As above
                                     seed_anae_ala_catch,
                                     germ_and_seed_anae_name_catch,
                                     germ_and_seed_anae_ala_catch,
                                     adult_anae_name_catch,
                                     adult_anae_ala_catch,
                                     inun_anae_name_catch, # area of inundation in anaes with `black box`
                                     inun_anae_ala_catch # area of inundation in anaes with ala records.
  )



}
#
# ggplot() +
#   geom_line(data = inun_anae_ala_catch, mapping = aes(x = date, y = area), color = 'black') +
#   geom_line(data = germ_anae_ala_catch, mapping = aes(x = date, y = area), color = 'green') +
#   geom_line(data = seed_anae_ala_catch, mapping = aes(x = date, y = area), color = 'purple') +
#   geom_line(data = germ_and_seed_anae_ala_catch, mapping = aes(x = date, y = area), color = 'red') +
#   geom_line(data = adult_anae_ala_catch, mapping = aes(x = date, y = area), color = 'blue')
#
# # How are adults > inun? The above is clipped and inun is the mean, but the adults are a rolling lookback at the 8-year max
# anae_year_max <- tempaggregate(starObj = anae_inun$aggdata, by = datebreaks,
#                            FUN = max, na.rm = TRUE) |>
#   aperm(c('geometry', 'time'))
# inun_max_catch <- sf_and_aggforce(anae_year_max, catchpoly, newname = 'area', funlist = sumna)
#
# ggplot() +
#   geom_line(data = inun_max_catch, mapping = aes(x = date, y = area), color = 'black') +
#   geom_line(data = adult_anae_ala_catch, mapping = aes(x = date, y = area), color = 'blue')
#
# #OK, I believe that.
# #
# #
# ggplot() +
#   geom_line(data = inun_anae_catch, mapping = aes(x = date, y = area), color = 'black') +
#   geom_line(data = germ_catch, mapping = aes(x = date, y = area), color = 'green') +
#   geom_line(data = seed_catch, mapping = aes(x = date, y = area), color = 'purple') +
#   geom_line(data = germ_and_seed_catch, mapping = aes(x = date, y = area), color = 'red', linetype = 'dashed') +
#   geom_line(data = adult_catch, mapping = aes(x = date, y = area), color = 'blue')
