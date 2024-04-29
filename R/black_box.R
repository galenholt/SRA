black_box <- function(out_dir,
                          catchment,
                          thischunk,
                          nchunks = 1,
                          whichcrs = 3577) {

# arguments ---------------------------------------------------------------
  source('R/woody_general.R')
  veg_name <- 'black_box'

  # Stricture 1: Germination
  # North: May to Oct, South Nov to March
  # inundation immediately preceding,
  # then 10 days of moisture 10-30% (days_germmoist)
  north_g_month <- c(5:10)
  south_g_month <- c(1:4, 11:12)

  days_germmoist <- 10

  # Stage 2: Seedling survival
  # Moisture must be maintained above 10% (and less than 30?) for two years.
  # Nothing ever passes for 2, and there wasn't much real info. Try 1? Or 1/2. The ref is pretty dodgy and at one point says 'at least through the summer'
  # No more than 70 days inundation
  # and it needs to happen following germination

  # How long is the seedling period? say 6mo
  seedling_period_month <- 6

  # The germ window is a period within seed_period_days during which germ can occur. This has to
  # fit inside seedling_period_days, along with some minimum seedling period. IE we
  # assess soil moisture for a full seedling_period_days, but if germ occurs in the
  # first bit of length germ_window, it counts. So the length of seedling period = germ_window +
  # minimum seedling period.
  germ_window <- 60

  # No inundation >70 days. Let's say 1 bimonth OK, 2 is a fail.This is
  # in months, so I guess say 3, it gets ceilinged in the fun
  too_long_inun <- 3

  # Stage 3: Adults
  # At least one flood in 8 years
  # Duration 2-4 months (max 5)
  # Do we want to couple to seedlings? ie needs to have been seedling survival x
  # years in the past? I think no- presumably many of these trees are older than
  # the data we have. We can just report on condition for regeneration (germ and
  # seedlings) and conditions for persistence.

  adult_maxflood <- 5
  adult_floodinterval <- 8*12
  a_month <- 1:12 # no seasonality restriction



  response_list <- CC2::woody_general(out_dir = out_dir,
                                 catchment = catchment,
                                 veg_name = veg_name,
                                 north_g_month = north_g_month,
                                 south_g_month = south_g_month,
                                 days_germmoist = days_germmoist,
                                 seedling_period_month = seedling_period_month,
                                 germ_window = germ_window,
                                 too_long_inun = too_long_inun,
                                 adult_maxflood = adult_maxflood,
                                 adult_floodinterval = adult_floodinterval,
                                 a_month = a_month)

  return(response_list)


}

