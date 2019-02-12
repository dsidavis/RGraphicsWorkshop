#' ---
#' title: Visualization in R
#' author: "Dan Hicks, <djhicks@ucdavis.edu>"
#' ---

library(ggplot2)

data_dir = 'data/'
plots_dir = 'plots/'

#' # Plot 1: Dumbbell plot #
## Plot 1 ----
counts_df = readRDS(paste0(data_dir, 'paper_counts.Rds'))

ggplot(counts_df, aes(subject_area, full)) + 
    ## geoms
    geom_point(aes(color = 'Full'), size = 3) +
    geom_point(aes(y = sample, color = 'Sample'), size = 3) +
    geom_segment(aes(x = subject_area, xend = subject_area, 
                     y = full, yend = sample), 
                 arrow = arrow(angle = 15, type = 'closed', 
                               length = unit(.025, 'snpc'))) +
    ## scales
    xlab('ASJC subject area') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'Papers in dataset') +
    scale_color_brewer(palette = 'Set1', 
                       name = 'Dataset') +
    coord_flip() + 
    ## annotations and theme
    ggtitle('Sampling improves balance across subject areas', 
            subtitle = Sys.time()) +
    annotate(geom = 'label', x = 'HEAL', y = .12, label = Sys.time()) +
    theme_minimal() +
    theme(legend.position = c(.8, .2), 
          legend.background = element_rect(fill = 'white'))
ggsave(paste0(plots_dir, 'plot_1.png'), height = 7, width = 7)


#' # Plot 2: High-density scatterplots, 5 ways #
## Plot 2 ----
temp_df = readRDS(paste0(data_dir, 'temp.Rds'))

ggplot(temp_df, aes(temp_max, temp_delta)) +
    ## geoms
    # geom_point() +
    # geom_count() +
    # geom_bin2d() +
    geom_hex(aes(color = ..count..)) +
    # geom_density2d(size = 1) +
    ## facets
    facet_wrap(~ name) +
    ## scales
    xlab('Daily maximum temperature (ºC)') +
    ylab('Daily temperature difference (ºC)') +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    ## themes
    theme_bw() +
    ggtitle('Maximum temperature vs. temperature difference', 
            subtitle = Sys.time())
ggsave(paste0(plots_dir, 'plot_2.png'), height = 7, width = 7)


#' # Plot 3:  Time series + model predictions, 3 ways #
## Plot 3 ----
model = lm(data = temp_df, temp_delta ~ day + I(day^2), 
           subset = name == 'Sacramento')

predictions = predict(model, interval = 'confidence', level = .95)
predictions = as.data.frame(predictions)
predictions$day = model$model$day

ggplot(temp_df, aes(day, temp_delta)) +
    ## geoms
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
    ## facets
    ## Faceting repeats the annotation
    # annotate(geom = 'label', x = 200, y = 5, label = Sys.time()) +
    facet_wrap(~ name) +
    ## scales
    xlab('Day of year') +
    scale_y_continuous(name = 'Daily temperature difference (max - min)', 
                       labels = scales::math_format(expr = .x*degree*C)) +
    scale_color_viridis_d(limits = month.name, 
                          option = 'E',
                          direction = -1) +
    ## themes
    theme_bw() +
    ggtitle('Seasonal and regional variation in daily temperature difference', 
            subtitle = Sys.time())

## Note effect of aspect ratios on perception
ggsave(paste0(plots_dir, 'plot_3.png'), height = 7, width = 7)
ggsave(paste0(plots_dir, 'plot_3a.png'), height = 7, width = 12)


#' # Plot 4:  Visualizing spatial data in R #
## Plot 4 ----
library(sf)
#' `ggplot2` (version >=3) can plot vector spatial data from `sf` objects using `geom_sf()`.  
if (packageVersion('ggplot2') < package_version('3.0.0')) {
    stop('ggplot2 version 3 or later is required for this section')
}

#' This section looks at two different ways of adding basemaps under `geom_sf()` plots

stations = readRDS(paste0(data_dir, 'stations.Rds'))
stations.utm = readRDS(paste0(data_dir, 'stations.utm.Rds'))

#' ## `ggmap` approach ##
library(ggmap)
## - Services other than Google Maps are deprecated
## - Requires Google account, API key
## - Doesn't seem to play nicely with coordinates other than lat/lon

## See ?register_google for links and instructions
## ***DO NOT SHARE YOUR API KEY***
# register_google(key = 'notarealkey', write = TRUE)
if (!has_google_key()) {
    stop('You need a Google Maps API key.  See ?register_google')
}

basemap = get_map(location = c(-121.4183, 38.5552), 
                  maptype = 'terrain',
                  zoom = 8)

ggmap(basemap) +
    geom_sf(data = stations, inherit.aes = FALSE) +
    geom_sf_label(data = stations, 
                  aes(label = name), nudge_y = -.1, size = 3)
ggsave(paste0(plots_dir, 'plot_4_ggmap.png'), height = 7, width = 7)

## Example with UTM coordinates
# ggmap(basemap) +
#     geom_sf(data = stations.utm, inherit.aes = FALSE)


#' ## `ggspatial` approach ##
library(ggspatial)
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
ggsave(paste0(plots_dir, 'plot_4_ggspatial.png'), height = 7, width = 7)

## Example with UTM coordinates
# ggplot(data = stations.utm) +
#     annotation_map_tile(type = 'osm', zoom = 10,
#                         cachedir = system.file('rosm.cache', package = 'ggspatial')) +
#     geom_sf_label(data = stations,
#                   aes(label = name), nudge_y = 1000, size = 3) +
#     geom_sf()

