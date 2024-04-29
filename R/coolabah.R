coolabah <- function(out_dir,
                   catchment,
                   thischunk,
                   nchunks = 1,
                   whichcrs = 3577) {

  # arguments ---------------------------------------------------------------
  source('R/woody_general.R')
  veg_name <- 'coolabah'

  # Stricture 1: Germination
  # "late summer", but because it likes high temps (see david notes). I think that means we need Feb in there, so we'll have to start in Jan to get the right bimonth
  # inundation immediately preceding, then 14 days (Casanova via EWKR)
  north_g_month <- c(1:4)
  south_g_month <- north_g_month

  days_germmoist <- 14

  # Stage 2: Seedling survival
  # Moisture must be maintained above 10% (and less than 30?) for ??? No one very clear, usual wishy-washy statements about 'through the summer or following summer'
  # Fatal immersion period unknown, do not include.
  # and it needs to happen following germination

  # How long is the seedling period? say 6mo, despite no clear statements
  seedling_period_month <- 6

  # There's no info here, so NULL skips the stricture.
  too_long_inun <- NULL

  # The germ window is a period within seed_period_days during which germ can occur. This has to
  # fit inside seedling_period_days, along with some minimum seedling period. IE we
  # assess soil moisture for a full seedling_period_days, but if germ occurs in the
  # first bit of length germ_window, it counts. So the length of seedling period = germ_window +
  # minimum seedling period. I suppose stick with two months here.
  germ_window <- 60

  # Stage 3: Adults
  # At least one flood in 20 years (max inter-flood dry period) David's table and in Roberts and Marston 2011
  # Duration: waterlogging detrimental, but no data
  # timing: no data, so just say all year

  adult_maxflood <- NULL #
  adult_floodinterval <- 20*12
  a_month <- c(1:12)

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
