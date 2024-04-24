library(stars)

download.file("https://raw.githubusercontent.com/galenholt/example/main/MYD11A1.061_1km_aid0001.nc",
              destfile = 'MYD11A1.061_1km_aid0001.nc',
              method = 'curl')

# Works without proxy
t_np <- read_stars("MYD11A1.061_1km_aid0001.nc", sub = "LST_Day_1km", proxy = FALSE)

# Fails with proxy first
t_proxy <- read_stars("MYD11A1.061_1km_aid0001.nc", sub = "LST_Day_1km", proxy = TRUE)
t_p <- st_as_stars(t_proxy)

# But the proxy can see the right values in y
t_proxy

# Can read the proxy in for the first value in y
t1 <- st_as_stars(t_proxy[, 1:10, 1,])

# But not others
t2 <- st_as_stars(t_proxy[, 1:10, 2,])


# Aggregate weirdness -----------------------------------------------------

library(stars)


# I'm getting continuing data in intervals that don't exist.
# using the demo
tif = system.file("tif/L7_ETMs.tif", package = "stars")
t1 = as.Date("2018-07-31")
x = read_stars(c(tif, tif, tif, tif), along = list(time = c(t1, t1+1, t1+2, t1+3)))[,1:30,1:30]

# Make them years so it's easier to make the breaks clearly between them
x = st_set_dimensions(x, 4, values = as.POSIXct(c("2018-01-01",
                                                  "2019-01-01",
                                                  "2020-01-01",
                                                  "2021-01-01")),
                      names = "time")
st_get_dimension_values(x, 'time')

# This should be sufficient (and there should be no data in the third returned
# sheet- the dates are start times, and all the data fits in the two intervals
# indexed by by_t[1] and by_t[2])
by_t = as.POSIXct(c("2017-07-01", "2019-07-01", "2021-07-01"))

x_suff <- aggregate(x, by_t, FUN = mean)
# it has the right names
st_get_dimension_values(x_suff, 'time')
# But why is there data in the last time?
x_suff[[1]][3,,,]

# Even stranger, if we increase by_t to have intervals well beyond the times in
# the data being aggregated, they still end up with data
by_t_long = as.POSIXct(c("2017-07-01", "2019-07-01", "2021-07-01", "2023-07-01", "2025-07-01"))
x_suff_l <- aggregate(x, by_t_long, FUN = mean)
x_suff_l[[1]][5,,,]

# What does findInterval do with this?
findInterval(st_get_dimension_values(x, 'time'), by_t)
findInterval(st_get_dimension_values(x, 'time'), by_t_long)
# So that seems to be behaving


soilmoist_2yr <- timeRoll(soilmoist_polys$aggdata,
                          FUN = RcppRoll::roll_min,
                          rolln = 183,
                          align = 'right',
                          na.rm = TRUE)
sm2s <- tempaggregate(starObj = soilmoist_2yr, by = datebreaks,
                      FUN = mean, na.rm = TRUE) |>
  aperm(c('geometry', 'time'))
sm2 <- sf_and_aggforce(sm2s, catchpoly, newname = 'area', funlist = \(x) sum(x, na.rm = T))
ggplot(sm2, aes(x = date, y = area)) + geom_line()
