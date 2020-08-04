library(tidyverse)
library(sf)

# load afdc locations
afdc_locations <- read_rds("Data/afdc_locations.rds") %>% 
  filter(country == "US")

# load county boundaries
county_bounds <- read_sf("~/GIS DataBase/tl_2019_us_county/tl_2019_us_county.shp")


# convert afdc to spatial dataset (assume crs is WGS84 since it appears to be GPS data, reproject to match county boundaries)
afdc_locations_sf <- afdc_locations %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(county_bounds))

# grab the relevant columns from county_bounds
county_bounds_narrow <- county_bounds %>% 
  select(GEOID, county_name = NAME, CBSAFP, CSAFP, geometry)

# spatial join county data to charging locations
afdc_locations_county <- afdc_locations_sf %>% 
  st_join(county_bounds_narrow) %>% 
  st_drop_geometry()

# write result to disk
afdc_locations_county %>% 
  as_tibble() %>% 
  write_rds("Data/afdc_locations_with_county.rds")

