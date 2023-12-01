
#### Chesapeake #### 
# https://buoybay.noaa.gov/data/data-download
ches_temps = read.csv(file = "Raw_data/site_temps/GR_OCEAN_2023_trimmed.csv") %>% 
  filter(!(temp_qual %in% c(3,4,9))) %>% 
  mutate(date = lubridate::mdy_hm(time)) %>% 
  select(date, temp_c) %>% 
  mutate("site" = "Chesapeake")

# #### Maine ####
# # https://www.ndbc.noaa.gov/station_history.php?station=casm1
# maine_1 = read.table(file = "Raw_data/site_temps/casm172023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# maine_2 = read.table(file = "Raw_data/site_temps/casm182023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# maine_3 = read.table(file = "Raw_data/site_temps/casm192023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# maine_4 = read.table(file = "Raw_data/site_temps/casm102023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# maine_temps = bind_rows(maine_1, maine_2, maine_3, maine_4) %>% 
#   filter(WTMP != 999) %>% 
#   mutate("date" = lubridate::make_datetime(year = YY, month = MM, day = DD, hour = hh, min = mm)) %>% 
#   select(date, "temp_c" = WTMP) %>% 
#   mutate("site" = "Maine")

# #### Connecticut #### 
# # https://waterdata.usgs.gov/monitoring-location/01194796/#parameterCode=00010&period=P365D&showMedian=true&timeSeriesId=66973
# # Connecticut River at Old Lyme
# siteNumber = "01194796"
# siteInfo = dataRetrieval::readNWISsite(siteNumber)
# parameterCd = "00010"
# startDate = "2023-06-01"
# endDate = ""
# 
# # Constructs the URL for the data wanted then downloads the data
# url = dataRetrieval::constructNWISURL(siteNumbers = siteNumber, parameterCd = parameterCd, 
#                                       startDate = startDate, endDate = endDate, service = "uv")
# 
# ct_temps = dataRetrieval::importWaterML1(url, asDateTime = T) %>%  
#   select("date" = dateTime, "temp_c" = X_Top_00010_00000) %>% 
#   mutate(date = lubridate::as_datetime(date)) %>% 
#   mutate("site" = "Connecticut")

#### Florida - Low Salinity #### 
# https://waterdata.usgs.gov/monitoring-location/023000095/#parameterCode=00010&period=P365D&showMedian=true&timeSeriesId=31853

siteNumber = "023000095"
siteInfo = dataRetrieval::readNWISsite(siteNumber)
parameterCd = "00010"
startDate = "2023-06-01"
endDate = ""

# Constructs the URL for the data wanted then downloads the data
url = dataRetrieval::constructNWISURL(siteNumbers = siteNumber, parameterCd = parameterCd,
                                      startDate = startDate, endDate = endDate, service = "uv")

flor_temps = dataRetrieval::importWaterML1(url, asDateTime = T) %>%
  select("date" = dateTime, "temp_c" = X_TOP_00010_00000) %>%
  mutate(date = lubridate::as_datetime(date)) %>%
  mutate("site" = "Florida")


# #### Florida - High Salinity #### 
# # https://www.ndbc.noaa.gov/station_history.php?station=pmaf1
# 
# man_riv_1 = read.table(file = "Raw_data/site_temps/pmaf162023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# man_riv_2 = read.table(file = "Raw_data/site_temps/pmaf172023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# man_riv_3 = read.table(file = "Raw_data/site_temps/pmaf182023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# man_riv_4 = read.table(file = "Raw_data/site_temps/pmaf192023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# man_riv_5 = read.table(file = "Raw_data/site_temps/pmaf102023.txt", header = T) %>% 
#   select(YY:mm, WTMP)
# 
# # flor_6 = read.table(file = "Raw_data/site_temps/pmaf182023.txt", header = T) %>% 
# #   select(YY:mm, WTMP) ### ADD THIS IN AFTER SAMPLING IN DECEMBER
# 
# man_riv_temps = bind_rows(man_riv_1, man_riv_2, man_riv_3, man_riv_4, man_riv_5) %>% 
#   filter(WTMP != 999) %>% 
#   mutate("date" = lubridate::make_datetime(year = YY, month = MM, day = DD, hour = hh, min = mm)) %>% 
#   select(date, "temp_c" = WTMP) %>% 
#   mutate("site" = "Manatee River")

insitu_temps = bind_rows(ches_temps, 
                           flor_temps) %>% 
  filter(date > "2023-06-01")

#### Version 2 ####
library(lubridate) # Useful functions for dealing with dates
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(tidyverse) # A staple for modern data management in R

#rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

OISST_data = griddap(datasetx = "ncdcOisst21Agg_LonPM180", 
        url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
        time = c("2023-06-01", "2023-11-15"), 
        zlev = c(0, 0),
        latitude = c(20, 50),
        longitude = c(-85, -55),
        fields = "sst")$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  dplyr::rename(t = time, temp = sst) %>% 
  dplyr::select(longitude, latitude, t, temp) %>% 
  na.omit()

# fl_temps = OISST_data %>% 
#   filter(latitude > 27.5 - 0.5, 
#          latitude < 27.5 + 0.5, 
#          longitude < -82.5 + 0.5,
#          longitude > -82.5 - 0.5) %>%  
#   mutate(region = "Florida")
# 
# ches_temps = OISST_data %>% 
#   filter(latitude > 38.5 - 0.5, 
#          latitude < 38.5 + 0.5, 
#          longitude < -76 + 0.5,
#          longitude > -76 - 0.5)  %>%  
#   mutate(region = "Chesapeake")
# 
ct_temps = OISST_data %>%
  filter(latitude > 41.5 - 0.5,
         latitude < 41.5 + 0.5,
         longitude < -72 + 0.5,
         longitude > -72 - 0.5)  %>%
  mutate(region = "Connecticut")

maine_temps = OISST_data %>% 
  filter(latitude > 43.9 - 0.5, 
         latitude < 43.9 + 0.5, 
         longitude < -69.8 + 0.5,
         longitude > -69.8 - 0.5)  %>%  
  mutate(region = "Maine")

shediac_temps = OISST_data %>% 
  filter(latitude > 46.5 - 0.5, 
         latitude < 46.5 + 0.5, 
         longitude < -64.5 + 0.5,
         longitude > -64.5 - 0.5)  %>%  
  mutate(region = "Shediac")

mira_temps = OISST_data %>% 
  filter(latitude > 47.1 - 0.5, 
         latitude < 47.1 + 0.5, 
         longitude < -65 + 0.5,
         longitude > -65 - 0.5)  %>%  
  mutate(region = "Miramichi")

temp_profiles2 = bind_rows(ct_temps, maine_temps, shediac_temps, mira_temps) %>% 
  group_by(region, t) %>% 
  summarise(temp_c = mean(temp)) %>% 
  mutate(date = as_date(t),
         doy = yday(date)) %>%  
  select(-t)

temp_profiles1 = insitu_temps %>% 
  mutate(date = as_date(date),
         doy = yday(date)) %>%
  group_by(site, doy, date) %>%  
  summarise(temp_c = mean(temp_c, na.rm = T)) %>%
  select("region" = site, doy, date, temp_c)
  
temp_profiles = bind_rows(temp_profiles1, temp_profiles2) %>% 
  filter(date > "2023-06-01")

write.csv(temp_profiles, file = "Output/Output_data/temp_profiles.csv", row.names = F)

# mira_temps %>% 
#   ggplot(aes(x = longitude, y = latitude)) +
#   facet_wrap(t~.) +
#   geom_tile(aes(fill = temp)) +
#   # borders() + # Activate this line to see the global map
#   scale_fill_viridis_c() +
#   coord_quickmap(expand = F) +
#   labs(x = NULL, y = NULL, fill = "SST (°C)") +
#   theme(legend.position = "bottom")
# 
