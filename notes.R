#' ---
#' title: Visualization in R
#' author: "Dan Hicks, <djhicks@ucdavis.edu>"
#' output:
#'   html_document:
#'     toc: true
#' ---

#' **Welcome!**
#' 
#' To get these notes and the datasets we'll be using, you'll need to clone or download this repository:  <https://github.com/dsidavis/RGraphicsWorkshop>
#' 
#' You'll also need the `ggplot2` library. 
library(ggplot2)
## NB Some other libraries are loaded below. These are all optional.  It's good practice to put your dependencies at the top of your code.  

data_dir = 'data/'
plots_dir = 'plots/'

#' # Base Graphics #
#' 
#' - R comes with a standard set of visualization tools, called *base graphics*.[^base]  
#' - Base graphics' approach to visualization is often called an "artist's palette" model
#'     - A set of low-level tools 
#'         - `points()`, `lines()`, `polygon()`
#'         - `abline()`, `curve()`
#'     - Define a canvas first, then use these tools to paint elements on the canvas
#' - Advantages of base graphics
#'     - One-liners
#'         - `plot(some_object)` often does something useful
#'             - Try it with dataframes, regression models, principal components results, hierarchical clusterings, igraph networks, ...
#'         - `hist()`, `pairs()`, `barplot()`, `boxplot()`, `pie()`(!)
#'     - Straightforward to extend to custom classes
#'     - No$^*$ dependencies; works on any$^*$ R installation
#'     - Very flexible for unusual visualizations or combinations of plots
#'         - triplots/barycentric coordinates
#'         - scatterplots with marginal densities
#'         - heatmaps with marginal dendrograms
#'         - map with inset plots of local data
#' - Disadvantages of base graphics
#'     - Parameters aren't perspicuous
#'         - fill color is `bg`
#'         - point shape is `pch`
#'         - point size is `cex`
#'     - "Ugly defaults"
#'     - Plots as [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science))
#' 
#' [^base]: Strictly speaking, `base` is the name of a particular package; "base graphics" aren't included in this package, but instead are in the `graphics` package.  So "base graphics" is strictly incorrect.  But "graphics graphics" sounds silly, and the `graphics` package is loaded automatically when you launch R, so everyone just calls it "base graphics."  
#' 

#' # Grammar of Graphics #
#' 
#' - The *grammar of graphics* is a model for visualization developed by Leland Wilkinson
#'     - [*The Grammar of Graphics*](https://link.springer.com/book/10.1007%2F978-1-4757-3100-2)
#'     - *Graphs are mappings*:  features of data (variables) are mapped to features of visual objects
#'     - These mappings are called *aesthetics*
#'         - Mapping height to x-coordinate
#'         - Mapping frequency to point size
#'         - Mapping gender to color
#' - `ggplot2` is an implementation of the grammar of graphics in R developed by Hadley Wickham
#'     - ["A Layered Grammar of Graphics"](https://www.tandfonline.com/doi/abs/10.1198/jcgs.2009.07098)
#' - Advantages of `ggplot2`
#'     - Conceptual model
#'     - Facets
#'     - Themes and palettes
#'     - Plots are [first-class objects](https://en.wikipedia.org/wiki/First-class_citizen)
#'     - "Pretty defaults"
#' - Disadvantages of `ggplot2`
#'     - Only$^*$ for data frames in [*long format*](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
#'     - Huge gap between basic use and developing extensions
#'     

#' # Plot 0:  Quick intro to ggplot #
## Plot 0 ----
library(datasauRus)
## This is a fun collection of datasets:  They look totally different when they're plotted, but have the same mean, standard deviation, and Pearson correlation coefficient.  

str(datasaurus_dozen_wide, give.attr = FALSE)
str(datasaurus_dozen, give.attr = FALSE)

ggplot(datasaurus_dozen_wide, 
       aes(dino_x, dino_y)) +
    geom_point()

ggplot(datasaurus_dozen, 
       aes(x, y)) +
    geom_point() +
    facet_wrap(~ dataset)


#' # Plot 1: Dumbbell plot #
## Plot 1 ----
## Derived from figure 2: <https://www.frontiersin.org/articles/10.3389/frma.2018.00024/full>
counts_df = readRDS(paste0(data_dir, 'paper_counts.Rds'))

counts_df

ggplot(counts_df, aes(subject_area)) + 
    ## geoms
    geom_point(aes(y = full, color = 'Full'), size = 3) +
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

#' ## Discussion ##
#' 1. What design decisions are being used to highlight the message of this plot?  
#' 2. What alternative designs could be used to communicate the same message?  
#'     - Are any of these alternative designs better than the one I used?  
#'     - Try implementing your idea in ggplot
#' 3. How does this plot omit, distort, or mislead?  
#' 


#' # Plot 2: High-density scatterplots #
## Plot 2 ----
## Code to construct this dataset is in `climate_data.R`
temp_df = readRDS(paste0(data_dir, 'temp.Rds'))

str(temp_df)

ggplot(temp_df, aes(temp_max, temp_delta)) +
    ## geoms
    # geom_point() +
    # geom_count() +
    # geom_bin2d() +
    # geom_hex(aes(color = ..count..)) +
    # geom_density2d(size = 1) +
    # stat_density2d(contour = FALSE, geom = 'raster', aes(fill = ..density..), show.legend = FALSE) +
    stat_density2d(geom = 'polygon', aes(fill = ..level..), show.legend = FALSE) +
    ## facets
    facet_wrap(~ name) +
    ## Annotations are repeated within each facet
    # annotate(geom = 'label', x = 200, y = 5, label = Sys.time()) +
    ## scales
    xlab('Daily maximum temperature (ºC)') +
    ylab('Daily temperature difference (ºC)') +
    scale_color_viridis_c(option = 'A') +
    scale_fill_viridis_c(option = 'A') +
    ## themes
    theme_bw() +
    ggtitle('Temperature difference is associated with maximum daily temperature', 
            subtitle = Sys.time())
ggsave(paste0(plots_dir, 'plot_2.png'), height = 7, width = 7)

#' ## Discussion ##
#' 1. What are the strengths and weaknesses of each different option?  
#' 


#' # Plot 3:  Time series + model predictions #
## Plot 3 ----
## Hack to get month labels on day-as-year
## <https://stackoverflow.com/a/39861306/3187973>
temp_df$day.date = as.Date(temp_df$day, '2009-01-01')

## Model **for Sacramento alone**
model = lm(data = temp_df, temp_delta ~ day + I(day^2), 
           subset = name == 'Sacramento')

predictions = predict(model, interval = 'confidence', level = .95)
predictions = as.data.frame(predictions)
predictions$day = model$model$day
predictions$day.date = as.Date(predictions$day, '2009-01-01')

ggplot(temp_df, aes(day.date, temp_delta)) +
    ## geoms
    geom_path(aes(color = month, group = year), alpha = .3, show.legend = FALSE) +
    ## Separate models for each panel
    geom_smooth(method = lm, formula = y ~ x + I(x^2),
                color = 'red', size = 1) +
    ## Sacramento-only model
    ## Use the smooth geom to get curve + ribbon
    ## But set `stat = 'identity'` because we don't want to fit a (new) model
    geom_smooth(data = predictions, stat = 'identity',
                aes(y = fit, ymax = upr, ymin = lwr),
                color = 'blue', fill = 'blue', size = 1) +
    ## facets
    facet_wrap(~ name) +
    ## scales
    scale_x_date(name = 'Month', date_breaks = '3 month', date_labels = '%b') +  
    scale_y_continuous(name = 'Daily temperature difference (max - min)', 
                       labels = scales::math_format(expr = .x*degree*C)) +
    scale_color_viridis_d(limits = month.name,
                          option = 'E',
                          direction = -1) +
    # coord_polar() +
    ## themes
    theme_bw() +
    ggtitle('Sacramento has smaller temperature differences than other sites', 
            subtitle = Sys.time())

## Note effect of aspect ratios on perception
ggsave(paste0(plots_dir, 'plot_3.png'), height = 7, width = 7)
ggsave(paste0(plots_dir, 'plot_3a.png'), height = 7, width = 12)

#' ## Discussion ##
#' 1. What are the dangers of using models internal to the plot?  
#' 2. Aspect ratios change our perception of the plot.  Does this mean one aspect ratio is misleading?  
#' 


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

stations

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

