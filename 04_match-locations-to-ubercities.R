library(tidyverse)
library(sf)

# load county areas (whoops, doesn't live in the right directory ... will correct this if there need to be updates in the future)
county_areas <- read_sf("~/GIS DataBase/tl_2019_us_county/tl_2019_us_county.shp") %>% 
  select(GEOID, ALAND) %>% 
  st_drop_geometry() %>% 
  mutate(area_km2 = ALAND / 1e6, .keep = "unused")

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
  # drop full state results
  filter(COUNTY != "000") %>% 
  mutate(GEOID = paste0(STATE, COUNTY), .keep = "unused") %>% 
  inner_join(county_areas, by = "GEOID")

# load county level charger totals, collapse per county
county_chargers <- read_rds("Data/afdc_locations_with_county.rds") %>% 
  with_groups(c(GEOID, CBSAFP, CSAFP), summarize, 
              charging_locations = n(), 
              level2 = sum(level2),
              dcfc = sum(dcfc))

# combine all the data per county
chargers_alldata_county <- county_to_uber %>% 
  left_join(census_county_pops, by = "GEOID") %>% 
  left_join(county_chargers,    by = "GEOID") %>% 
  drop_na(STNAME, CTYNAME, POPESTIMATE2019, area_km2) %>% 
  mutate(across(charging_locations:dcfc, replace_na, 0))

chargers_alldata_ubercity <- chargers_alldata_county %>% 
  group_by(UberCity, UberCity2) %>% 
  summarize(across(POPESTIMATE2019:dcfc, sum), .groups = "drop") %>% 
  mutate(across(charging_locations:dcfc, 
                list(per_100kpeople = ~ . / POPESTIMATE2019 * 100000,
                     per_1000km2    = ~ . / area_km2 * 1000))) %>% 
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


# Run CBSA and CSA too ####

# load CBSA/CSA names
cbsa_csa_names <- read_delim("Data/core-based-statistical-areas-cbsas-and-combined-statistical-areas-csas.csv",
                             delim = ";",
                             col_types = cols_only(`CBSA Code`  = "c",
                                                   `CSA Code`   = "c",
                                                   `CBSA Title` = "c",
                                                   `CSA Title`  = "c")) %>% 
  rename(CBSAFP = `CBSA Code`,
         CSAFP  = `CSA Code`,
         CBSA   = `CBSA Title`,
         CSA    = `CSA Title`) %>% 
  distinct()

# attach CBSA/CSA names to counties
chargers_alldata_cbsacsa <- chargers_alldata_county %>% 
  left_join(cbsa_csa_names, by = c("CBSAFP", "CSAFP")) %>% 
  mutate(CBSA = replace_na(CBSA, "non-CBSA"),
         CSA  = replace_na(CSA,  "non-CSA"))

# summarize by State/CBSA and State/CSA and save
chargers_alldata_cbsacsa %>% 
  group_by(STNAME, CBSAFP, CBSA) %>% 
  summarize(across(c(POPESTIMATE2019, area_km2, charging_locations:dcfc), sum), .groups = "drop") %>% 
  mutate(across(charging_locations:dcfc, 
                list(per_100kpeople = ~ . / POPESTIMATE2019 * 100000,
                     per_1000km2    = ~ . / area_km2 * 1000))) %>% 
  write_csv("Outputs/cbsa_chargerinfo.csv")

chargers_alldata_cbsacsa %>% 
  group_by(STNAME, CSAFP, CSA) %>% 
  summarize(across(c(POPESTIMATE2019, area_km2, charging_locations:dcfc), sum), .groups = "drop") %>% 
  mutate(across(charging_locations:dcfc, 
                list(per_100kpeople = ~ . / POPESTIMATE2019 * 100000,
                     per_1000km2    = ~ . / area_km2 * 1000))) %>%
  write_csv("Outputs/csa_chargerinfo.csv")

# might as well keep it at the county level as well
chargers_alldata_cbsacsa %>% 
  relocate(CBSAFP, CSAFP, CBSA, CSA, .after = CTYNAME) %>% 
  mutate(across(charging_locations:dcfc, 
                list(per_100kpeople = ~ . / POPESTIMATE2019 * 100000,
                     per_1000km2    = ~ . / area_km2 * 1000))) %>% 
  write_csv("Outputs/county_chargerinfo.csv")
