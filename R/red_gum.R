red_gum <- function(out_dir,
                    catchment,
                    thischunk,
                    nchunks = 1,
                    whichcrs = 3577) {

  # arguments ---------------------------------------------------------------
  source('R/woody_general.R')
  veg_name <- 'red_gum'

  # Stricture 1: Germination
  # "spring to early summer", let's say Sept-Dec
  # inundation immediately preceding, then 5 days of soil moisture (David's sheet) or 14 (Casanova). Use 14?
  days_germmoist <- 14

  north_g_month <- c(9:12)
  south_g_month <- c(9:12)

  # Stage 2: Seedling survival
  # Moisture must be maintained above 10% (and less than 30?) for the summer. How long is 'summer'? we used 6mo for black box, I guess use that here too?
  # Immersion of young seedlings <25cm for 'several months' is fatal. Assume that's in the 6mo period.
  # and it needs to happen following germination

  # How long is the seedling period? say 6mo
  seedling_period_month <- 6

  # No inundation 'several months'. Let's say 1 bimonth OK, 2 is a fail. This is
  # in months, so I guess say 3, it gets ceilinged in the fun
  too_long_inun <- 3

  # The germ window is a period within seed_period_days during which germ can occur. This has to
  # fit inside seedling_period_days, along with some minimum seedling period. IE we
  # assess soil moisture for a full seedling_period_days, but if germ occurs in the
  # first bit of length germ_window, it counts. So the length of seedling period = germ_window +
  # minimum seedling period.
  germ_window <- 60

  # Stage 3: Adults
  # At least one flood in 4 years (max inter-flood dry period)
  # Duration 2-24 months
  # timing: Winter-early summer

  adult_maxflood <- 24
  adult_floodinterval <- 48
  a_month <- c(7:12) # let's say winter-early summer is July-Dec

  # Run the response
  response_list <- woody_general(out_dir = out_dir,
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

  print('woody_general returned to red_gum')
  return(response_list)

}
