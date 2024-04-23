red_gum <- function(out_dir,
                    catchment,
                    thischunk,
                    nchunks = 1,
                    whichcrs = 3577) {

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
  anae_types <- readRDS(file.path(out_dir, 'vegmapping', 'red_gum_anae_type.rds'))

  ## Strictures
  # Very similar to black box, but some numbers are different


# Germination -------------------------------------------------------------


  # Stricture 1: Germination
  # "spring to early summer", let's say Sept-Dec
  # inundation immediately preceding, then 5 days of soil moisture (David's sheet) or 14 (Casanova). Use 14?
  days_germmoist <- 14

  i_month <- c(9:12)

  times <- st_get_dimension_values(anae_inun$aggdata, 'time')
  # Easier to set 0 than 1
  nogermtimes <- which(!lubridate::month(times) %in% i_month)

  # Only the area of inundation if in the right season
  # We may not actually need this at all- if we do the soil moisture it'll pick this up, with the time shifts around month starts, which is better anyway
  germ_inun_season <- anae_inun$aggdata
  germ_inun_season[[1]][ , nogermtimes] <- 0

  # soil moisture- the data is the daily area between 10-30, so we want the minimum of that over the last 14 days.
  soilmoist_germdays <- timeRoll(soilmoist_polys$aggdata,
                              FUN = RcppRoll::roll_min,
                              rolln = days_germmoist,
                              align = 'right',
                              na.rm = TRUE)

  # that soil moisture has to come off the back of inundation, as in black_box
  # make daily inun
  # This returns NA for all times before inundatin (as it should)
  # use germ_inun_season here, since we care about the germ itself here.
  daily_inun <- unevenTimeMult(fineStars = soilmoist_polys$aggdata*0+1,
                               coarseStars = germ_inun_season,
                               lag = 0)

  # There's almost certainly a better way to do this with `lag`, but that really wants a timeseries
  shift_inun <- daily_inun[[1]][, -1:-(days_germmoist + 1)] # shift the time-cols over
  shift_inun <- cbind(shift_inun, daily_inun[[1]][, 1:(days_germmoist + 1)]*NA) # put the same number of NA cols at the end so we can multiply the matrices

  germ_area <- soilmoist_germdays
  germ_area[[1]] <- pmin(shift_inun, soilmoist_germdays[[1]])


# Seedling ----------------------------------------------------------------


  # Stage 2: Seedling survival
  # Moisture must be maintained above 10% (and less than 30?) for the summer. How long is 'summer'? we used 6mo for black box, I guess use that here too?
  # Immersion of young seedlings <25cm for 'several months' is fatal. Assume that's in the 6mo period.
  # and it needs to happen following germination

  # How long is the seedling period? say 6mo
  seedling_period <- 183

  # soil moisture- the data is the daily area between 10-30, so we want the minimum of that over the last seedling period.
  soilmoist_seedling <- timeRoll(soilmoist_polys$aggdata,
                            FUN = RcppRoll::roll_min,
                            rolln = seedling_period,
                            # rolln = 730,
                            align = 'right',
                            na.rm = TRUE)

  # No inundation 'several months'. Let's say 1 bimonth OK, 2 is a fail.
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

  # Then the min of that over 6mo (3 bimonths) gives us the area that never got inundated too much in 6mo
  notflood_seedling <- timeRoll(area_not_4mo,
                           FUN = RcppRoll::roll_min,
                           rolln = 3, # Cut to 6 mo now.
                           # rolln = 12,
                           align = 'right',
                           na.rm = TRUE)

  # now we want the area that had enough moisture and not too much inundation over the two years.
  # again, abuse unevenTimeMult
  # This returns NA for all times before inundatin (as it should)
  daily_not_area <- unevenTimeMult(fineStars = soilmoist_seedling*0+1,
                                   coarseStars = notflood_seedling,
                                   lag = 0)

  # Then survival is
  seedling_area <- soilmoist_seedling
  seedling_area[[1]] <- pmin(soilmoist_seedling[[1]], daily_not_area[[1]])

  # I don't think we want to be precious about exactly how long ago germ needs
  # to have happened. The key is whether soil moisture has persisted.

  # I don't think the way I was doing this made much sense. Or maybe just wasn't explained very well in my comment.

  # Let's say we want some germ window x days (e.g. 2 months) long. This has to
  # fit inside seedling_period, along with some minimum seedling period. IE we
  # assess soil moisture for a full seedling_period, but if germ occurs in the
  # first bit, it counts. So the length of seedling period = germ_window +
  # minimum seedling period. To get that for each day, we can roll_max for x
  # days to get whether there was germ sometime in that 2mo window
  germ_window <- 60

  germ_span <- timeRoll(germ_area,
                      FUN = RcppRoll::roll_max,
                      rolln = germ_window,
                      align = 'right',
                      na.rm = TRUE)


  # But, the question is whether that germination happened at least some minimum
  # seedling period ago.
  # The full possible germ period needs to fit in the seedling_period, because that's the check that soil stayed moist.
  # So if we're checking soil moisture for 6mo, we can't look beyond that for
  # germination. So let's say the min seedling period is 4 mo. Then we'd ask
  # whether germ in the preceding 2mo germ_window 4 months ago would yield
  # passing for a 6-mo soil moisture period

  min_seedling_period <- seedling_period - germ_window

  # Now, we want to shift the germ window over by that minimum period, so we can
  # see if there was any germination in teh germ_window preceding
  # min_seedling_period
  germ_span_shift <- cbind(germ_span[[1]][, 1:min_seedling_period]*NA, germ_span[[1]])
  germ_span_shift <- germ_span_shift[,-(ncol(germ_span_shift)-(min_seedling_period-1)):-ncol(germ_span_shift)]


  # now the area that germinated AND then survived a seedling stage ranging from
  # min_seedling_period to seedling_period is the minimum
  germ_and_seed <- seedling_area
  germ_and_seed[[1]] <- pmin(seedling_area[[1]], germ_span_shift)


# Adults ------------------------------------------------------------------


  # Stage 3: Adults
  # At least one flood in 4 years (max inter-flood dry period)
  # Duration 2-24 months
  # timing: Winter-early summer

  adult_maxflood <- 24/2 # /2 because bimonth, 24 is the limit, but use it anyway (instead of 26), since ideal is all the way down at 8
  adult_floodinterval <- 4*6 # *6 because bimonth year
  a_month <- c(7:12) # let's say winter-early summer is July-Dec

  # How do we calculate this? We want the maximum area flooded in floodinterval
  # years, minus the area that flooded too much
  # We get the area that flooded too much by getting the minimum area flooded
  # for maxflood (+2) months, which is too long. Then we want to subtract that off the
  # total area during floodinterval years. And that piece we subtract off should
  # be the max over floodinterval of the too-flooded areas in maxflood periods

  # The min inundation over maxflood is the amount that fails that test
  inun_adult_long <- timeRoll(anae_inun$aggdata,
                      FUN = RcppRoll::roll_min,
                      rolln = adult_maxflood,
                      align = 'right',
                      na.rm = TRUE)

  # Then we need the MAX of that over floodinterval years, as this is the amount that doesn't count in the floodinterval-year check because it was too wet
  inun_adult_inter <- timeRoll(inun_adult_long,
                        FUN = RcppRoll::roll_max,
                        rolln = adult_floodinterval,
                        align = 'right',
                        na.rm = TRUE)

  # The max inun over floodinterval years is the amount that passes the floodinterval-year requirement

  # but only if it occurs during the right period. We already have 'times' from above
  noadulttimes <- which(!lubridate::month(times) %in% a_month)
  # Only the area of inundation if in the right season
  # We may not actually need this at all- if we do the soil moisture it'll pick this up, with the time shifts around month starts, which is better anyway
  adult_inun_season <- anae_inun$aggdata
  adult_inun_season[[1]][ , noadulttimes] <- 0

  # i.e we want the max inundation over the floodinterval, but only those events  that occurred in a_months
  inun_adult_all <- timeRoll(adult_inun_season,
                      FUN = RcppRoll::roll_max,
                      rolln = adult_floodinterval,
                      align = 'right',
                      na.rm = TRUE)

  # and the area that passes both is the difference. Note we don't put the seasonality on the adult_inter, since those durations aren't seasonal.
  adult_condition <- inun_adult_all-inun_adult_inter


}
