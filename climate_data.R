## This script is used to build the datasets for the climate examples
library(tidyverse)
library(rnoaa)
## rnoaa vignette:  <https://cran.r-project.org/web/packages/rnoaa/vignettes/rnoaa_ropenaq.html>

library(lubridate)
# library(assertthat)

## Spatial stuff
library(sf)
library(ggmap)
library(ggspatial) ## Vignette:  <https://cran.r-project.org/web/packages/ggspatial/vignettes/ggspatial.html>

data_dir = 'data/'

## Retrieve GHCND data ----
## Interactive map of stations: <https://www.ncdc.noaa.gov/cdo-web/datatools/findstation>

stations = tribble(
    ~name, ~ghcnd, ~lat, ~lon, 
    'Davis', 'USC00042294', 38.5349, -121.7761, 
    # 'Sacramento', 'US1CASA0051', 38.5905, -121.3935, 
    'Sacramento', 'USW00023271', 38.5552, -121.4183,
    # 'Placerville', 'US1CAED0010', 38.7392, -120.7846,
    'Lake Tahoe', 'USW00093230', 38.8983, -119.9947,
    'Lake Berryessa', 'USC00045360', 38.4916, -122.1241
) %>% 
    st_as_sf(coords = c('lon', 'lat'), remove = FALSE, 
             crs = "+proj=longlat +ellps=WGS84")

stations.utm = st_transform(stations, '+proj=utm +zone=10 +datum=NAD83')

# meteo_pull_monitors('USW00093230', date_min = '2016-01-01', 
#                     date_max = '2016-01-31', var = 'TMAX')

## Variables (and dataset more broadly) documented here:  
## <https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt>

dataf_raw = meteo_pull_monitors(stations$ghcnd, 
                                date_min = '2009-01-01', 
                                date_max = '2018-12-31', 
                                var = c('TMAX', 'TMIN', 'PRCP'))

dataf = dataf_raw %>% 
    left_join(stations, by = c('id' = 'ghcnd')) %>% 
    mutate(year = year(date), 
           month = month.name[month(date)], 
           day = yday(date),
           temp_min = tmin / 10,
           temp_max = tmax / 10, 
           temp_delta = temp_max - temp_min,
           precipitation = prcp) %>% 
    select(id, name, date, year, month, day, 
           temp_min, temp_max, temp_delta, precipitation, 
           lat, lon)

## Write output ----
write_rds(paste0(data_dir, stations), 'stations.Rds')
write_rds(paste0(data_dir, stations.utm), 'stations.utm.Rds')
write_rds(paste0(data_dir, as.data.frame(dataf)), 'temp.Rds')

stop("Finished building datasets")

## High-density scatterplots, 5 ways ----
ggplot(dataf, aes(temp_max, temp_delta)) +
    # geom_point(alpha = .15) +
    # geom_count() +
    # geom_bin2d() +
    geom_hex(aes(color = ..count..)) +
    # geom_density2d(size = 1) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    xlab('Daily maximum temperature (ºC)') +
    ylab('Daily minimum temperature (ºC)') +
    theme_minimal() +
    ggtitle('Daily minima and maxima temperature', 
            subtitle = Sys.time())


## Time series with a model for reference ----
# model = loess(data = dataf, temp_delta ~ day)
model = lm(data = dataf, temp_delta ~ day + I(day^2), 
           subset = name == 'Sacramento')

predictions = predict(model, interval = 'confidence', level = .95)
predictions = as.data.frame(predictions)
predictions$day = model$model$day

ggplot(dataf, aes(day, temp_delta)) +
    geom_point(aes(color = month), show.legend = FALSE) +
    ## Method 1
    ## - Separate smooth for each panel
    ## - No access to model
    # geom_smooth(method = lm, formula = y ~ x + I(x^2),
    #             color = 'blue', size = 1) +
    ## Method 2
    ## - No confidence intervals
    # stat_function(fun = function (day) {
    #     newdata = data.frame(day)
    #     return(predict(model, newdata))},
    #     color = 'blue', size = 1) +
    ## Method 3
    ## - Requires some external wrangling
    geom_smooth(data = predictions, stat = 'identity',
                aes(y = fit, ymax = upr, ymin = lwr),
                color = 'blue', fill = 'blue') +
    xlab('Day of year') +
    scale_y_continuous(name = 'Daily temperature difference (max - min)', 
                       labels = scales::math_format(expr = .x*degree*C)) +
    scale_color_viridis_d(limits = month.name, 
                          option = 'E',
                          direction = -1) +
    ## Faceting repeats the annotation
    # annotate(geom = 'label', x = 200, y = 5, label = Sys.time()) +
    facet_wrap(~ name) +
    theme_minimal()


## Map ----
## ggmap approach
## - Services other than Google Maps are deprecated
## - Requires Google account, API key
## - Doesn't seem to play nicely with coordinates other than lat/lon

## See ?register_google for links and instructions
## ***DO NOT SHARE YOUR API KEY***
# register_google(key = 'notarealkey', write = TRUE)
has_google_key()

basemap = get_map(location = c(-121.4183, 38.5552), 
                  maptype = 'terrain',
                  zoom = 8)

ggmap(basemap) +
    geom_sf(data = stations, inherit.aes = FALSE) +
    geom_sf_label(data = stations, 
                  aes(label = name), nudge_y = -.1, size = 3)

## Example with UTM coordinates
# ggmap(basemap) +
#     geom_sf(data = stations.utm, inherit.aes = FALSE)

## ggspatial approach
## - annotation_map_tile() is slow, even with cached images
## See rosm::osm.types() for a list of basemap types
ggplot(data = stations) +
    annotation_map_tile(type = 'osm', zoom = 10) +
    geom_sf_label(data = stations, 
                  aes(label = name), nudge_y = .03, size = 3) +
    geom_sf() +
    annotation_scale(location = 'tl') +
    annotation_north_arrow(location = 'br', which_north = 'true', 
                           style = north_arrow_minimal)

## Example with UTM coordinates
ggplot(data = stations.utm) +
    annotation_map_tile(type = 'osm', zoom = 10,
                        cachedir = system.file('rosm.cache', package = 'ggspatial')) +
    geom_sf_label(data = stations,
                  aes(label = name), nudge_y = 1000, size = 3) +
    geom_sf()


## Other EDA / plot idea development ----
ggplot(dataf, aes(day, fill = name)) +
    geom_ribbon(aes(ymin = temp_min, ymax = temp_max), 
                alpha = .5) +
    facet_wrap(~ year) +
    scale_color_brewer(palette = 'Set1') +
    theme_minimal()

ggplot(dataf, aes(temp_min, temp_max)) +
    # geom_point() +
    # geom_hex(bins = 10) +
    geom_density2d() +
    stat_function(fun = identity) +
    scale_fill_viridis_c() +
    facet_wrap(~ name, scales = 'free') +
    theme_minimal()

ggplot(dataf, aes(temp_delta, fill = name)) +
    geom_density(alpha = .75)


# model = loess(data = dataf, temp_delta ~ day)
model = lm(data = dataf, temp_delta ~ day + I(day^2))

ggplot(dataf, aes(day, temp_delta)) +
    geom_point(aes(color = month), show.legend = FALSE) +
    # geom_smooth()
    stat_function(
        fun = function (day) {
            newdata = data.frame(day)
            return(predict(model, newdata))},
        color = 'red') +
    scale_color_viridis_d(limits = month.name, 
                          option = 'E',
                          direction = -1) +
    facet_wrap(~ name) +
    theme_minimal()

ggplot(dataf, aes(log10(precipitation), temp_delta)) +
    # geom_point() +
    geom_hex() +
    geom_smooth(method = 'lm', color = 'red') +
    scale_fill_viridis_c() +
    theme_minimal()

ggplot(dataf, aes(day, temp_max, color = year, group = year)) +
    geom_line() +
    geom_line(aes(y = precipitation)) +
    facet_wrap(~ name)

ggplot(dataf, aes(day, precipitation, color = year, group = year)) +
    geom_line() +
    facet_wrap(~ name)

ggplot(dataf, aes(temp_max, precipitation, color = day)) +
    # geom_path() +
    geom_point() +
    scale_color_viridis_c(direction = -1) +
    facet_wrap(~ name)
