# coolabah check

source('directorySet.R')
library(eFlowEval)
library(foreach)
library(ggplot2)

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

# Condamine should have some
response_list <- woody_general(out_dir = datOut,
                               catchment = "CondamineBalonne",
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

# Some testing
st_get_dimension_values(germ_area, which = 'time')
gat <- colSums(germ_area[[1]], na.rm = TRUE)

# To return, let's aggregate up to water year
# availdates <- stars::st_get_dimension_values(soilmoist_polys$aggdata, which = 'time')
startyear <- lubridate::year(min(availdates))-1
endyear <- lubridate::year(max(availdates)) + 1
# we want to cut at June 30, and so need to make sure the 07-01 go into the next step.
datebreaks <- startyear:endyear
datebreaks <- paste0(as.character(datebreaks), '0701') |> lubridate::ymd() |> as.POSIXct()

# Aggregate to year with meaneven though these are only pseudo-daily. That
# captures a time-dependence that the max would miss (e.g. one possible string
# of 10 days, vs every day). This uses as much time as the rest of the script put together. Should I furrr::future_map()?
yrgerm <- tempaggregate(starObj = germ_area, by = datebreaks,
                                      FUN = mean, na.rm = TRUE) |>
                          aperm(c('geometry', 'time'))


st_get_dimension_values(yrgerm, which = 'time')
gat2 <- colSums(yrgerm[[1]], na.rm = TRUE)
gat2

# So *why* do those drop to 0 for the last two years?
# q1- are there really no data there?
lastyrs <- which(st_get_dimension_values(germ_area, which = 'time') >= st_get_dimension_values(yrgerm, which = 'time')[44])
gat[lastyrs] |> plot()

# but there are some in the preceding few years.
lastyrs4 <- which(st_get_dimension_values(germ_area, which = 'time') >= st_get_dimension_values(yrgerm, which = 'time')[40])
gat[lastyrs4] |> plot()

# Is it coming from soilmoist germdays?
# Some testing
st_get_dimension_values(soilmoist_germdays, which = 'time')
smt <- colSums(soilmoist_germdays[[1]], na.rm = TRUE)
plot(smt)
# Aggresmte to year with meaneven though these are only pseudo-daily. That
# captures a time-dependence that the max would miss (e.g. one possible string
# of 10 days, vs every day). This uses as much time as the rest of the script put together. Should I furrr::future_map()?
yrsmgd <- tempaggregate(starObj = soilmoist_germdays, by = datebreaks,
                        FUN = mean, na.rm = TRUE) |>
  aperm(c('geometry', 'time'))


st_get_dimension_values(yrsmgd, which = 'time')
smt2 <- colSums(yrsmgd[[1]], na.rm = TRUE)
smt2

# These don't drop to 0.
smt[lastyrs] |> plot()

# Actually seem to do better than in most preceding years.
smt[lastyrs4] |> plot()

# So it's got to be coming from the pmin(shift_unun, ... )
# Some testing
# Shift_inun isn't a stars
shiftstars <- soilmoist_germdays
shiftstars[[1]] <- shift_inun
st_get_dimension_values(shiftstars, which = 'time')
sit <- colSums(shift_inun, na.rm = TRUE)
plot(sit)
# Aggregate to year with mean even though these are only pseudo-daily. That
# captures a time-dependence that the max would miss (e.g. one possible string
# of 10 days, vs every day). This uses as much time as the rest of the script put together. Should I furrr::future_map()?

yrsshifti <- tempaggregate(starObj = shiftstars, by = datebreaks,
                        FUN = mean, na.rm = TRUE) |>
  aperm(c('geometry', 'time'))


st_get_dimension_values(yrsshifti, which = 'time')
sit2 <- colSums(yrsshifti[[1]], na.rm = TRUE)
sit2

# How are those last two years 0?
sit[lastyrs] |> plot()

# all other years have nonzero values
sit[lastyrs4] |> plot()
plot(sit2)


# daily_inun is next
st_get_dimension_values(daily_inun, which = 'time')
dit <- colSums(daily_inun[[1]], na.rm = TRUE)
plot(dit)
# Aggregate to year with mean even though these are only pseudo-daily. That
# captures a time-dependence that the max would miss (e.g. one possible string
# of 10 days, vs every day). This uses as much time as the rest of the script put together. Should I furrr::future_map()?

yrsdaili <- tempaggregate(starObj = daily_inun, by = datebreaks,
                           FUN = mean, na.rm = TRUE) |>
  aperm(c('geometry', 'time'))


st_get_dimension_values(yrsdaili, which = 'time')
dit2 <- colSums(yrsdaili[[1]], na.rm = TRUE)
dit2

# How are those last two years 0?
dit[lastyrs] |> plot()

# all other years have nonzero values
dit[lastyrs4] |> plot()
plot(dit2)

# so, must be coming from germ_inun_season?
st_get_dimension_values(germ_inun_season, which = 'time')
gis <- colSums(germ_inun_season[[1]], na.rm = TRUE)
plot(gis)
# Aggregate to year with mean even though these are only pseudo-daily. That
# captures a time-dependence that the max would miss (e.g. one possible string
# of 10 days, vs every day). This uses as much time as the rest of the script put together. Should I furrr::future_map()?

yrsgisi <- tempaggregate(starObj = germ_inun_season, by = datebreaks,
                          FUN = mean, na.rm = TRUE) |>
  aperm(c('geometry', 'time'))


st_get_dimension_values(yrsgisi, which = 'time')
gis2 <- colSums(yrsgisi[[1]], na.rm = TRUE)
gis2


lastyrs_bim <- which(st_get_dimension_values(germ_inun_season, which = 'time') >= st_get_dimension_values(yrsgisi, which = 'time')[44])
# but there are some in the preceding few years.
lastyrs4_bim <- which(st_get_dimension_values(germ_inun_season, which = 'time') >= st_get_dimension_values(yrsgisi, which = 'time')[40])
# How are those last two years 0?
gis[lastyrs_bim] |> plot()

# all other years have nonzero values
gis[lastyrs4_bim] |> plot()
plot(gis2)
gis2

# So, is the issue that the last year doesn't get anything in the right season? But why two?
st_get_dimension_values(germ_inun_season, which = 'time')
st_get_dimension_values(yrsgisi, which = 'time')

gis[202:210]
st_get_dimension_values(germ_inun_season, which = 'time')[202:210]

cut(st_get_dimension_values(germ_inun_season, which = 'time'), breaks = datebreaks)
cut(st_get_dimension_values(germ_inun_season, which = 'time'), breaks = datebreaks)[202:210]

# Daily_inun has a bunch of NA on the end where it's been aggregated past the end. But it also has zeros where the data goes into the year. The catch is, it doesn't go into where the germ period is.

# I think we don't actually want to fix it in the germ- that's returning daily values, and they *should* be 0. The kicker is that we want to NOT return the last year's value if it's not a full year when we aggregate in time.ESPECIALLY if we haven't even sampled the right seasons.



# fixing ------------------------------------------------------------------

# so, I think we let germarea get all the way through- this is really a question of how we do the temporal aggregation.
# Do we just let it NA if there are any NA instead of only if all na? That'll also lose intermediate NAs. And for some species, a mean is OK to take over 6 mo, probably.
yrgermna <- tempaggregate(starObj = germ_area, by = datebreaks,
                        FUN = mean, na.rm = FALSE) |>
  aperm(c('geometry', 'time'))


st_get_dimension_values(yrgermna, which = 'time')
gatna <- colSums(yrgermna[[1]])
gatna

g2 <- colSums(germ_area[[1]])

# So *why* do those drop to 0 for the last two years?
# q1- are there really no data there?
gatna |> plot()
