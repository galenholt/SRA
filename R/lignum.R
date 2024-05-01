lignum <- function(out_dir,
                      catchment,
                      thischunk,
                      nchunks = 1,
                      whichcrs = 3577) {

  # arguments ---------------------------------------------------------------
  source('R/woody_general.R')
  veg_name <- 'lignum'

  # Stricture 1: Germination
  # "spring to autumn", let's say Sept-April
  # inundation immediately preceding, then 14 days (Casanova via EWKR)
  north_g_month <- c(9:12, 1:4)
  south_g_month <- north_g_month

  days_germmoist <- 14

  # Stage 2: Seedling survival
  # Moisture must be maintained above 10% (and less than 30?) for a year. Holloway via EWKR says one year, but we know everything will fail.
  # Immersion of young seedlings <25cm for 1 month is fatal (cites in EWKR).
  # and it needs to happen following germination

  # How long is the seedling period? say 6mo, despite it being a year in Holloway
  seedling_period_month <- 6

  # No inundation longer than 1 month. Let's say any subsequent inundation is a fail. There are lots of references to not liking being waterlogged.
  too_long_inun <- 1

  # The germ window is a period within seed_period_days during which germ can occur. This has to
  # fit inside seedling_period_days, along with some minimum seedling period. IE we
  # assess soil moisture for a full seedling_period_days, but if germ occurs in the
  # first bit of length germ_window, it counts. So the length of seedling period = germ_window +
  # minimum seedling period. I suppose stick with two months here.
  germ_window <- 60

  # Stage 3: Adults
  # At least one flood in 10 years (max inter-flood dry period) Capon in Roberts and Marston 2011
  # Duration dead after 12 months (David's table)
  # timing: spring-early summer (david's table)

  adult_maxflood <- 12 #
  adult_floodinterval <- 10*12
  a_month <- c(9:12) # let's say winter-early summer is July-Dec

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

  return(response_list)

}
