library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

library(broom)
library(rgeos)
library(viridis)

# data source: https://www.cdc.gov/nchs/hus/contents2019.htm?search=,State

spdf <- 
  geojsonio::geojson_read('./Nov2021/us_states_hexgrid.geojson', what = 'sp')

# drop the '(United States)' part of the State name
spdf@data <- 
  spdf@data |> 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# fortify spdf using tidyr::tidy()
spdf_fortified <- tidy(spdf, region = 'google_name')
# get centroids for hexes
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE),
                                       id = spdf@data$iso3166_2))

# read in physician data
physicans <- 
  readxl::read_excel("./Nov2021/fig16_physicians_per_100k_2018.xlsx", skip = 2)
names(physicans) = c('state', 'physicians')

# map physician rates to states
spdf_fortified <- 
  spdf_fortified |> 
  left_join(physicans, by = c("id"="state"))

# bin into discrete groups
spdf_fortified$bin <-
  cut(spdf_fortified$physicians, 
      breaks = c(seq(188, 672, length.out = 8), Inf),
      labels = c('under 188', '188-257', '258-326', '327-395',
                 '396-465', '466-534', '535-603', '603-672'),
      include.lowest = TRUE)

# plot
ggplot(data = spdf_fortified) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = bin),
               size = 0, alpha = 0.9) +
  geom_text(data = centers, aes(x = x, y = y, label = id), 
            color = 'white') +
  scale_fill_viridis(option = 'viridis', discrete = TRUE) +
  theme_void() +
  labs(title = "The northeast has the highest amount of physicians per capita",
       subtitle = "Registered physicians by US State, 2018",
       fill = "physicians \nper 100,000",
       caption = 'source: National Center for Health Statistics, 2019') +
  theme(text = element_text(family = 'Merriweather'),
        plot.title = element_text(size = 14, family = 'Merriweather'),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(t = 1, l = 5, r = 5, b = 1),
        legend.title = element_text(size = 10),
        legend.margin = margin(t = 1, r = 5, b = 1, l = 2),
        legend.box.margin = margin(1, 1, 1, 1),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = 'black')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_map()
