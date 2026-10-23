# Site temperature records ------------------------------------------------
# Builds daily temperature profiles for each region over the 2023 sampling
# season and writes Output/Output_data/temp_profiles.csv.
#   - Chesapeake: in situ buoy (Raw_data/site_temps/GR_OCEAN_2023_trimmed.csv)
#     https://buoybay.noaa.gov/data/data-download
#   - Florida: USGS gauge 023000095, downloaded with dataRetrieval
#   - Connecticut, Maine, Shediac, Miramichi: NOAA OISST v2.1 (0.25 deg daily SST),
#     downloaded with rerddap, averaged over a 1 x 1 deg box around each site
# Needs an internet connection. The current temp_profiles.csv is kept in
# Output_data/, so this only needs rerunning if the sources change.
#
# Requires: here, tidyverse, lubridate, dataRetrieval, rerddap

library(here)
library(tidyverse)
library(lubridate)
library(rerddap)

start_date <- "2023-06-01"
end_date   <- "2023-12-24"

# In situ records ----------------------------------------------------------
ches_temps <- read.csv(here("Raw_data", "site_temps", "GR_OCEAN_2023_trimmed.csv")) %>%
  filter(!(temp_qual %in% c(3, 4, 9))) %>%          # drop flagged values
  mutate(date = mdy_hm(time)) %>%
  select(date, temp_c) %>%
  mutate(site = "Chesapeake")

# Florida (low salinity): USGS gauge
# https://waterdata.usgs.gov/monitoring-location/023000095/
url <- dataRetrieval::constructNWISURL(siteNumbers = "023000095", parameterCd = "00010",
                                       startDate = start_date, endDate = "", service = "uv")
flor_temps <- dataRetrieval::importWaterML1(url, asDateTime = TRUE) %>%
  select(date = dateTime, temp_c = X_TOP_00010_00000) %>%
  mutate(date = as_datetime(date), site = "Florida")

insitu_temps <- bind_rows(ches_temps, flor_temps) %>%
  filter(date > start_date)

# Alternative in situ sources tried earlier (not used; files are in Raw_data/site_temps/):
#   Maine: NDBC station CASM1 (casm1*2023.txt)
#   Manatee River (high salinity): NDBC station PMAF1 (pmaf1*2023.txt)
#   Connecticut: USGS gauge 01194796 (Connecticut River at Old Lyme)

# Satellite SST (OISST) ----------------------------------------------------
OISST_data <- griddap(datasetx = "ncdcOisst21Agg_LonPM180",
                      url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                      time = c(start_date, end_date),
                      zlev = c(0, 0),
                      latitude = c(20, 50),
                      longitude = c(-85, -55),
                      fields = "sst")$data %>%
  mutate(time = as.Date(str_remove(time, "T00:00:00Z"))) %>%
  rename(t = time, temp = sst) %>%
  select(longitude, latitude, t, temp) %>%
  na.omit()

# Box centres for each region (+/- 0.5 deg)
oisst_boxes <- tribble(
  ~region,        ~lat, ~lon,
  "Connecticut",  41.5, -72.0,
  "Maine",        43.9, -69.8,
  "Shediac",      46.5, -64.5,
  "Miramichi",    47.1, -65.0
)

temp_profiles_sat <- oisst_boxes %>%
  pmap_dfr(function(region, lat, lon) {
    OISST_data %>%
      filter(between(latitude, lat - 0.5, lat + 0.5),
             between(longitude, lon - 0.5, lon + 0.5)) %>%
      mutate(region = region)
  }) %>%
  group_by(region, t) %>%
  summarise(temp_c = mean(temp), .groups = "drop") %>%
  mutate(date = as_date(t), doy = yday(date)) %>%
  select(-t)

temp_profiles_insitu <- insitu_temps %>%
  mutate(date = as_date(date), doy = yday(date)) %>%
  group_by(site, doy, date) %>%
  summarise(temp_c = mean(temp_c, na.rm = TRUE), .groups = "drop") %>%
  select(region = site, doy, date, temp_c)

# Write --------------------------------------------------------------------
temp_profiles <- bind_rows(temp_profiles_insitu, temp_profiles_sat) %>%
  filter(date > start_date)

write.csv(temp_profiles, here("Output", "Output_data", "temp_profiles.csv"), row.names = FALSE)
