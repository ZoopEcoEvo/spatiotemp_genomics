library(tidyverse)

### Damariscotta River - Maine 
dsct = readxl::read_excel(path = "Background/Occurrence/damariscotta_river.xlsx") %>% 
  select(Date, Year, Month, Day, station, lat = `Lat (decimal Degrees)`, lon = `Lon (decimal Degrees)`, cast, depth,
         "tow_volume_m3" = `Volume filtered m3`, Split, "lab_dilution_ml" = `Lab Dilution (ml)`, "aliquot_ml" = `Aliquot size (ml)`, 
         "total_dilution_factor" = `Total Dilution factor`,
         starts_with("Acartia", ignore.case = T)) %>%  
  mutate('doy' = lubridate::yday(Date))

dsct_long = dsct %>% 
  pivot_longer(cols = starts_with("Acartia"),
               names_to = "species", 
               values_to = "number") %>% 
  mutate(abund = (number * total_dilution_factor) / (tow_volume_m3 / Split))

dsct_long %>%  
  filter(species %in% c("Acartia_hudsonicaF", "Acartia_tonsaF", "Acartia_longiremisF")) %>% 
ggplot(aes(x = doy, y = abund, colour = species)) + 
  facet_grid(Year~.) +
  geom_line()
  #geom_point() + 
  #geom_smooth(se = F) 

dsct_long %>%  
  filter(species %in% c("Acartia_tonsaF") & abund < 600) %>% 
  ggplot(aes(x = doy, y = abund)) + 
  facet_grid(Year~.) + 
  geom_line() + 
  theme(legend.position = "none")

dsct_long %>%  
  filter(species %in% c("Acartia_tonsaF") & Month %in% c(5:10)) %>%  
  ggplot(aes(x = doy, y = abund, colour = as.factor(Year))) + 
  geom_point()
