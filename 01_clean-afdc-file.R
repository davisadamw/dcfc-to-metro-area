library(tidyverse)
library(janitor)
library(lubridate)

# load afdc data, clean names (to snake case)
afdc_locations <- read_csv("Data/alt_fuel_stations (Aug 3 2020).csv",
                           col_types = cols_only(ID                   = "c",
                                                 ZIP                  = "c",
                                                 `Fuel Type Code`     = "c",
                                                 Latitude             = "d",
                                                 Longitude            = "d",
                                                 `EV Level1 EVSE Num` = "i",
                                                 `EV Level2 EVSE Num` = "i",
                                                 `EV DC Fast Count`   = "i",
                                                 `Open Date`          = "c")) %>% 
  clean_names()

# extract locations that have any EV charging
afdc_locations_ev <- afdc_locations %>% 
  filter(fuel_type_code == "ELEC")

# would be nice to be able to identify whether open date is before/after March 2019, but... mostly NAs there
# add a column identifying country from zip code (US is 5-digit, Canada starts with a letter)
# convert all NAs in charger count columns to 0s
locs_cleancols <- afdc_locations_ev %>% 
  mutate(open_mar19 = ymd(open_date) <= ymd("2019-03-31"),
         country    = if_else(str_detect(zip, "^[0-9]{5}$"), "US", "Canada"),
         across(ev_level1_evse_num:ev_dc_fast_count, replace_na, 0)) %>% 
  select(id, country,
         level2 = ev_level2_evse_num, dcfc = ev_dc_fast_count, 
         open_mar19, longitude, latitude)

locs_cleancols %>% 
  write_rds("Data/afdc_locations.rds")
