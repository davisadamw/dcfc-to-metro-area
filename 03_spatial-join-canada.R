library(tidyverse)
library(sf)

# load afdc locations
afdc_locations <- read_rds("Data/afdc_locations.rds") %>% 
  filter(country == "Canada")

# load county boundaries
canada_metro_bounds <- read_sf("~/GIS DataBase/Canada/lcma000a16a_e.shp") %>% 
  select(canada_metro_name = CMANAME,
         province          = PRNAME)


# convert afdc to spatial dataset (assume crs is WGS84 since it appears to be GPS data, reproject to match county boundaries)
afdc_locations_sf <- afdc_locations %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(canada_metro_bounds))

# spatial join county data to charging locations
afdc_locations_canada <- afdc_locations_sf %>% 
  st_join(canada_metro_bounds) %>% 
  st_drop_geometry()

# write result to disk
afdc_locations_canada %>% 
  as_tibble() %>% 
  write_rds("Data/afdc_locations_with_canada.rds")

