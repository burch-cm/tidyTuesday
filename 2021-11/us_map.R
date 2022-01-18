library(readxl)
library(tidyverse)

library(tmap)
library(tigris)

us_geo <- tigris::states(class = 'sf')
glimpse(us_geo)

as_tibble(us_geo)

contig_states <- 
  us_geo |> 
  filter(REGION != 9) |> 
  shift_geometry()

tm_shape(contig_states, projection = 5070) +
  tm_borders(col = 'black', alpha = 0.4)

library(zipcodeR)

zip_loc <- zip_code_db |> select(zipcode, lng, lat)

# load faa data
inv_loc <- readxl::read_excel("./Nov2021/data/inv_locations.xlsx")
region_count <- 
  inv_loc |> 
  select(VIN, region = Region, state_name = `Garage State`) |> 
  group_by(region) |> 
  count() |> 
  ungroup()


tm_shape(contig_states, projection = 5070) +
  