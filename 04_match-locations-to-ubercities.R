library(tidyverse)
library(tidycensus)

# load county-uber link, assign worst accuracy category to each group
county_to_uber <- read_csv("Geocoding_Result/us_counties_uber.csv",
                           col_types = cols_only(GEOID = "c", 
                                                 UberCity = "i", 
                                                 UberCity2 = "c")) %>% 
  with_groups(UberCity2, mutate, UberCity = max(UberCity))

# load county populations
census_county_pops <- read_csv("Data/co-est2019-alldata.csv", 
                               col_types = cols_only(STATE = "c", COUNTY = "c", 
                                                     STNAME = "c", CTYNAME = "c",
                                                     POPESTIMATE2019 = "i")) %>% 
  mutate(GEOID = paste0(STATE, COUNTY), .keep = "unused")

# load county level charger totals, collapse per county
county_chargers <- read_rds("Data/afdc_locations_with_county.rds") %>% 
  with_groups(GEOID, summarize, charging_locations = n(), level2 = sum(level2), dcfc = sum(dcfc))

# combine all the data per county
chargers_alldata_county <- county_to_uber %>% 
  left_join(census_county_pops, by = "GEOID") %>% 
  left_join(county_chargers,    by = "GEOID") %>% 
  drop_na(STNAME, CTYNAME, POPESTIMATE2019) %>% 
  mutate(across(charging_locations:dcfc, replace_na, 0))

chargers_alldata_ubercity <- chargers_alldata_county %>% 
  group_by(UberCity, UberCity2) %>% 
  summarize(across(POPESTIMATE2019:dcfc, sum), .groups = "drop") %>% 
  mutate(across(charging_locations:dcfc, list(per_100kpeople = ~ . / POPESTIMATE2019 * 100000))) %>% 
  rename(UberCity_accuracy = UberCity,
         UberCity = UberCity2)

# ken's uber cities
uber_city_defs <- readxl::read_excel("Data/UberCities_ken_to_UberCities_Adam.xlsx") %>% 
  left_join(chargers_alldata_ubercity, by = "UberCity")

# Save results
chargers_alldata_ubercity %>% 
  write_csv("Outputs/all_ubercities_chargerinfo.csv")
uber_city_defs %>% 
  write_csv("Outputs/active_US_CAN_cities_chargerinfo.csv")
