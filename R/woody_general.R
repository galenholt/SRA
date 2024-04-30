#' A general woody veg stricture function
#'
#' The idea is a particular species/genus would have its own values for the
#' arguments here. This is a work in progress, but some things seem to be
#' becoming standard. It's likely more arguments will be added as needed.
#'
#' @param out_dir outer directory as in most other functions
#' @param catchment catchment name *without spaces*
#' @param veg_name name of the veg type, e.g. 'black_box'
#' @param north_g_month germination months in the northern basin
#' @param south_g_month germination months in the southern basin
#' @param days_germmoist days of soil moisture that need to persist following
#'   inundation for successful germination
#' @param seedling_period_month how long is the seedling period (in months)-
#'   i.e. how long does moisture need to persist?
#' @param germ_window number of days. How long is the window in the seedling
#'   period where germ can happen? The germ window is a period within
#'   seed_period_days during which germ can occur. This has to fit inside
#'   seedling_period_days, along with some minimum seedling period. IE we assess
#'   soil moisture for a full seedling_period_days, but if germ occurs in the
#'   first bit of length germ_window, it counts. So the length of seedling
#'   period = germ_window + minimum seedling period.
#' @param too_long_inun How long (in months) is too much inundation for seedlings to survive? This ends up being used as ceiling(too_long_inun/2) to get the bimonth, so if you want a round-down, make sure it's an even number.
#' @param adult_maxflood How long (in months) is too much inundation for adults to survive? This ends up being used as ceiling(adult_maxflood/2) to get the bimonth, so if you want a round-down, make sure it's an even number.
#' @param adult_floodinterval the maximum (in months) interval between floods before adults die/lose condition
#' @param a_month months in which inundation 'counts' for adults
#'
#' @return
#' @export
#'
woody_general <- function(out_dir, catchment,
                          veg_name,
                          north_g_month,
                          south_g_month,
                          days_germmoist,
                          seedling_period_month,
                          germ_window,
                          too_long_inun,
                          adult_maxflood,
                          adult_floodinterval,
                          a_month) {

  # some tweaks to handle conditionals on the inputs, e.g. grep formats, catchment info

  seedling_period_days <- seedling_period_month*30
  seedling_period_bimonth <- ceiling(seedling_period_month/2) ## because bimonth data

  too_long_inun <- ceiling(too_long_inun/2) # because bimonth data
  adult_maxflood <- ceiling(adult_maxflood/2) # because bimonth data

  adult_floodinterval <- ceiling(adult_floodinterval/2) # because bimonth data


  veg_grep <- dplyr::case_when(veg_name == 'black_box' ~ 'black box',
                               veg_name == 'red_gum' ~ 'red gum',
                               veg_name == 'coolabah' ~ 'cool',
                               veg_name == 'lignum' ~ 'lignum')

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
    g_month <- north_g_month
  } else if (catchment %in% s_catches) {
    g_month <- south_g_month
  }




  # Data read-in ------------------------------------------------------------


  ## DATA IN
  # Need soil moisture and inundation and anaes
  soilmoist_polys <- readRDS(file.path(out_dir, 'soilmoisture',
                                       'areamoist_10_30',
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
  # We could just do this with the $indices, but using the incoming anaes prevents drift by resetting to the originals
  soilmoist_polys$aggdata <- clean_area(soilmoist_polys$aggdata, anaes)
  soilmoist_polys$indices <- clean_area(soilmoist_polys$indices, anaes)
  anae_inun$aggdata <- clean_area(anae_inun$aggdata, anaes)
  anae_inun$indices <- clean_area(anae_inun$indices, anaes)


  # ANAE type stricture

  # Two ways of doing this- one from the name of the anae, and one from where they're found in ALA (see veg1_anae_mapping).
  # In both cases, we develop the stricture at the end, but we read in the ALA side here.
  anae_types <- readRDS(file.path(out_dir, 'vegmapping', paste0(veg_name, '_anae_type.rds')))


  # Stricture 1: Germination

  times <- stars::st_get_dimension_values(anae_inun$aggdata, 'time')
  # Easier to set 0 than 1
  nogermtimes <- which(!lubridate::month(times) %in% g_month)

  # Only the area of inundation if in the right season
  # We may not actually need this at all- if we do the soil moisture it'll pick this up, with the time shifts around month starts, which is better anyway
  germ_inun_season <- anae_inun$aggdata
  germ_inun_season[[1]][ , nogermtimes] <- 0

  # soil moisture- the data is the daily area between 10-30, so we want the minimum of that over the last 10 days.
  soilmoist_germdays <- timeRoll(soilmoist_polys$aggdata,
                                 FUN = RcppRoll::roll_min,
                                 rolln = days_germmoist,
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
  shift_inun <- daily_inun[[1]][, -1:-(days_germmoist + 1)] # shift the time-cols over
  shift_inun <- cbind(shift_inun, daily_inun[[1]][, 1:(days_germmoist + 1)]*NA) # put the same number of NA cols at the end so we can multiply the matrices

  germ_area <- soilmoist_germdays
  germ_area[[1]] <- pmin(shift_inun, soilmoist_germdays[[1]])

  # Stage 2: Seedling survival

  # soil moisture- the data is the daily area between 10-30, so we want the minimum of that over the last seedling period.
  soilmoist_seedling <- timeRoll(soilmoist_polys$aggdata,
                                 FUN = RcppRoll::roll_min,
                                 rolln = seedling_period_days,
                                 align = 'right',
                                 na.rm = TRUE)


  # Deal with too much inundation (drowning)

  # If the there isn't a stricture here, setting the too_long to longer than the data just returns NA for all the checks below

  if (length(too_long_inun) == 0 | is.null(too_long_inun)) {
    too_long_inun <- stars::st_dimensions(anae_inun$aggdata)$time$to + 1
  }

  # First, get the *un*inundated area
  # back to just anae_inun here, because the seasonality comes in with the did germ happen check.
  area_not_inundated <- anae_inun$aggdata
  area_not_inundated[[1]] <- (anae_inun$indices |>
                                sf::st_area() |>
                                as.numeric()) -
    anae_inun$aggdata[[1]]

  # Then the max of that over too_long_inun bimonths- this is the area that *wasn't* inundated for too long
  area_not_too_long <- timeRoll(area_not_inundated,
                                FUN = RcppRoll::roll_max,
                                rolln = too_long_inun,
                                align = 'right',
                                na.rm = TRUE)

  # Then the min of that over the seedling period gives us the area that never
  # got inundated too much while seedling establishing
  notflood_seedling <- timeRoll(area_not_too_long,
                                FUN = RcppRoll::roll_min,
                                rolln = seedling_period_bimonth,
                                align = 'right',
                                na.rm = TRUE)

  # now we want the area that had enough moisture and not too much inundation over the seedling period.
  # again, abuse unevenTimeMult
  # This returns NA for all times before inundatin (as it should)
  daily_not_area <- unevenTimeMult(fineStars = soilmoist_seedling*0+1,
                                   coarseStars = notflood_seedling,
                                   lag = 0)

  # Then survival is
  seedling_area <- soilmoist_seedling
  seedling_area[[1]] <- pmin(soilmoist_seedling[[1]], daily_not_area[[1]], na.rm = TRUE)

  # I don't think we want to be precious about exactly how long ago germ needs
  # to have happened. The key is whether soil moisture has persisted.

  # I don't think the way I was doing this made much sense. Or maybe just wasn't explained very well in my comment.

  # We have some germ window x days (e.g. 2 months) long. This has to
  # fit inside seedling_period_days, along with some minimum seedling period. IE we
  # assess soil moisture for a full seedling_period_days, but if germ occurs in the
  # first bit, it counts. So the length of seedling period = germ_window +
  # minimum seedling period. To get that for each day, we can roll_max for x
  # days to get whether there was germ sometime in that 2mo window

  germ_span <- timeRoll(germ_area,
                        FUN = RcppRoll::roll_max,
                        rolln = germ_window,
                        align = 'right',
                        na.rm = TRUE)


  # But, the question is whether that germination happened at least some minimum
  # seedling period ago.
  # The full possible germ period needs to fit in the seedling_period_days, because that's the check that soil stayed moist.
  # So if we're checking soil moisture for 6mo, we can't look beyond that for
  # germination. So let's say the min seedling period is 4 mo. Then we'd ask
  # whether germ in the preceding 2mo germ_window 4 months ago would yield
  # passing for a 6-mo soil moisture period

  min_seedling_period_days <- seedling_period_days - germ_window

  # Now, we want to shift the germ window over by that minimum period, so we can
  # see if there was any germination in teh germ_window preceding
  # min_seedling_period_days
  germ_span_shift <- cbind(germ_span[[1]][, 1:min_seedling_period_days]*NA, germ_span[[1]])
  germ_span_shift <- germ_span_shift[,-(ncol(germ_span_shift)-(min_seedling_period_days-1)):-ncol(germ_span_shift)]


  # now the area that germinated AND then survived a seedling stage ranging from
  # min_seedling_period_days to seedling_period_days is the minimum
  germ_and_seed <- seedling_area
  germ_and_seed[[1]] <- pmin(seedling_area[[1]], germ_span_shift)


  # Stage 3: Adults

  # How do we calculate this? We want the maximum area flooded in floodinterval
  # years, minus the area that flooded too much
  # We get the area that flooded too much by getting the minimum area flooded
  # for maxflood (+2) months, which is too long. Then we want to subtract that off the
  # total area during floodinterval years. And that piece we subtract off should
  # be the max over floodinterval of the too-flooded areas in maxflood periods

  # unlike above, where rolling and producing NA was fine, here I need 0s.

  if (length(adult_maxflood) == 0 | is.null(adult_maxflood)) {
    inun_adult_inter <- anae_inun$aggdata * 0
  } else {
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
  }

  # we don't have season for black box, but to make things consistent
  # We already have 'times' from above
  noadulttimes <- which(!lubridate::month(times) %in% a_month)
  # Only the area of inundation if in the right season
  # We may not actually need this at all- if we do the soil moisture it'll pick this up, with the time shifts around month starts, which is better anyway
  adult_inun_season <- anae_inun$aggdata
  adult_inun_season[[1]][ , noadulttimes] <- 0

  # The max inun over floodinterval years is the amount that passes the floodinterval-year requirement
  # i.e we want the max inundation over the floodinterval, but only those events  that occurred in a_months
  inun_adult_all <- timeRoll(adult_inun_season,
                             FUN = RcppRoll::roll_max,
                             rolln = adult_floodinterval,
                             align = 'right',
                             na.rm = TRUE)

  # and the area that passes both is the difference. Note we don't put the seasonality on the adult_inter, since those durations aren't seasonal.
  adult_area <- inun_adult_all-inun_adult_inter


  # Common post-processing --------------------------------------------------
  # We now have a set of outputs that we want to do some common post-processing on:
  # aggregate to year, clip to ANAE (by name and ala records)
  # I think the best way to do this is to make them a list, and then use purrr. Otherwise it's a TON of copy-paste

  # the anae_inun stuff is done elsewhere, but good to not have to go hunting
  bare_stricts <- tibble::lst(germ_area, seedling_area, germ_and_seed, adult_area, anae_inun = anae_inun$aggdata)

  # Aggregate to year -------------------------------------------------------

  # To return, let's aggregate up to water year
  availdates <- stars::st_get_dimension_values(soilmoist_polys$aggdata, which = 'time')
  startyear <- lubridate::year(min(availdates))-1
  endyear <- lubridate::year(max(availdates)) + 1
  # we want to cut at June 30, and so need to make sure the 07-01 go into the next step.
  datebreaks <- startyear:endyear
  datebreaks <- paste0(as.character(datebreaks), '0701') |> lubridate::ymd() |> as.POSIXct()

  # Aggregate to year with meaneven though these are only pseudo-daily. That
  # captures a time-dependence that the max would miss (e.g. one possible string
  # of 10 days, vs every day).
  yrstricts <- purrr::map(bare_stricts, \(x)
                          tempaggregate(starObj = x, by = datebreaks,
                                        FUN = mean, na.rm = TRUE) |>
                            aperm(c('geometry', 'time')))


  # ANAE types --------------------------------------------------------------

  # Option 1: it has the name 'red gum'- can just do this in a mutate


  # Option 2: it's an anae type with records from ALA
  # Let's say it needs to have at least 0.5% of the records to be appreciable.
  ala_types <- anae_types |>
    filter(n_records > 0.005*sum(anae_types$n_records)) |>
    select(ANAE_DESC) |>
    pull() |>
    unique() # Should be, but ensure

  # Have each method in separate cols of a df because we need logical vectors that match the full anae df (and stars)
  anaestricts <- anaes |>
    # option 1- by name
    dplyr::mutate(name_anae = grepl(veg_grep, ANAE_DESC, ignore.case = TRUE),
                  # option 2: by ala record
                  ala_anae = ANAE_DESC %in% ala_types)

  # clip the strictures
  anae_name_stricts <- purrr::map(yrstricts, \(x) x * anaestricts$name_anae) |>
    setNames(paste0(names(yrstricts), '_anae_name'))
  anae_ala_stricts <- purrr::map(yrstricts, \(x) x * anaestricts$ala_anae) |>
    setNames(paste0(names(yrstricts), '_anae_ala'))


  # make catchment scale ----------------------------------------------------

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

  # use sum to get total are in the catchment
  # we're doing this to everything above, so we can c() the three lists together and then operate on all of them
  response_list <- c(yrstricts, anae_name_stricts, anae_ala_stricts) |>
    purrr::map(\(x) sf_and_aggforce(x, catchpoly, newname = 'area', funlist = sumna))

  # check that nothing is all na
  naareas <- purrr::map_lgl(response_list, \(x) all(is.na(x$area)))
  if(any(naareas)) {
    rlang::warn(glue::glue("Response list for {catchment} contains all-NA area columns in {paste0(names(which(naareas)), collapse = ', ')}"))
  }

  # No area should be negative. Warn if so. Not erroring because I don't want to kill big runs
  # use minna or it throws a bunch of warnings with all NA
  negareas <- purrr::map_lgl(response_list, \(x) minna(x$area) < 0)
  if(any(negareas)) {
    rlang::warn(glue::glue("Response list for {catchment} contains negative areas in {paste0(names(which(negareas)), collapse = ', ')}"))
  }


  print('finished woody_general, about to return')
  return(response_list)

}


# # testing -----------------------------------------------------------------
# # This should move if this goes in the package, obviously
# source('directorySet.R')
# library(CC2)
# library(ggplot2)
# # source('R/woody_general.R')
# catchment <- 'Avoca'
# out_dir <- datOut
# thischunk <- 1
#
# #
# #
# ggplot() +
#   geom_line(data = response_list$anae_inun_anae_ala, mapping = aes(x = date, y = area), color = 'black') +
#   geom_line(data = response_list$germ_area_anae_ala, mapping = aes(x = date, y = area), color = 'green') +
#   geom_line(data = response_list$seedling_area_anae_ala, mapping = aes(x = date, y = area), color = 'purple') +
#   geom_line(data = response_list$germ_and_seed_anae_ala, mapping = aes(x = date, y = area), color = 'red') +
#   geom_line(data = response_list$adult_area_anae_ala, mapping = aes(x = date, y = area), color = 'blue')
# #
# #
# ggplot() +
#   geom_line(data = response_list$anae_inun, mapping = aes(x = date, y = area), color = 'black') +
#   geom_line(data = response_list$germ_area, mapping = aes(x = date, y = area), color = 'green') +
#   geom_line(data = response_list$seedling_area, mapping = aes(x = date, y = area), color = 'purple') +
#   geom_line(data = response_list$germ_and_seed, mapping = aes(x = date, y = area), color = 'red', linetype = 'dashed') +
#   geom_line(data = response_list$adult_area, mapping = aes(x = date, y = area), color = 'blue')
