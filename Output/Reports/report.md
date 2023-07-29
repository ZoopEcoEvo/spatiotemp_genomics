Comparing seasonal and latitudinal patterns in thermal adaptation
================
2023-07-29

- [Site Characteristics](#site-characteristics)
- [Critical Thermal Limits](#critical-thermal-limits)
- [Body Size](#body-size)
- [Trait Correlations](#trait-correlations)
- [Next Steps](#next-steps)
- [Misc. Details](#misc-details)

## Site Characteristics

Copepods were collected by surface tow from sites across the Western
Atlantic at several times throughout the year. The sites are shown
below. Temperatures at the time of collection were measured using a
manual thermometer.

``` r
coords = site_data %>%
  select(site, long, lat) %>%
  distinct()

site_map = map_data("world") %>% 
  filter(region %in% c("USA", "Canada")) %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "lightgrey") + 
  coord_map(xlim = c(-85,-60),
            ylim = c(25, 48)) + 
  geom_point(data = coords,
             mapping = aes(x = long, y = lat, colour = site),
             size = 3) +
  scale_colour_manual(values = site_cols) + 
  labs(x = "Longitude", 
       y = "Latitude") + 
  theme_matt(base_size = 16)

site_temp_plot = full_data %>% 
  select(site, season, doy, collection_temp, collection_salinity) %>%  
  distinct() %>% 
  ggplot(aes(x = doy, y = collection_temp, colour = site)) + 
  geom_point(size = 6) + 
  geom_line(linewidth = 1) + 
  scale_colour_manual(values = site_cols) + 
  labs(y = "Temperature (°C)",
       x = "Day of the Year") +
  theme_matt() + 
  theme(legend.position = "right")

ggpubr::ggarrange(site_map, site_temp_plot, common.legend = T, legend = "bottom")
```

<img src="../Figures/markdown/site-chars-1.png" style="display: block; margin: auto;" />

Exact locations for the sites are provided here. The two sites in
Maryland are differentiated by their salinity: Tyler’s Cove has a
relatively high salinity (15 psu) compared to Ganey’s Wharf (2 psu).

``` r
site_data %>%  
  arrange(lat) %>%  
  select("Site" = site, "Region" = region, "Lat" = lat, "Long" = long) %>% 
  knitr::kable(align = "c")
```

|           Site           |    Region     |   Lat    |   Long    |
|:------------------------:|:-------------:|:--------:|:---------:|
|        Key Largo         |    Florida    | 25.28391 | -80.33014 |
|      Manatee River       |    Florida    | 27.50561 | -82.57277 |
|      St. Petersburg      |    Florida    | 27.70367 | -82.63969 |
|        Tyler Cove        |   Maryland    | 38.35083 | -76.22902 |
|      Ganey’s Wharf       |   Maryland    | 38.80555 | -75.90906 |
|       Esker Point        |  Connecticut  | 41.32081 | -72.00166 |
|       Sawyer Park        |     Maine     | 43.90698 | -69.87179 |
| St. Thomas de Kent Wharf | New Brunswick | 46.44761 | -64.63692 |
|        Miramichi         | New Brunswick | 47.02940 | -65.47312 |

## Critical Thermal Limits

Critical thermal maxima (CTmax) was measured using a custom setup. The
method uses a standard dynamic ramping assay to determine the maximum
temperature individuals could sustain normal functioning. This differs
from lethal temperatures, and indeed, all individuals observed so far
recovered following the assay.

Individuals were rested for one hour after collection before the assay.
During the assay, copepods were held in 0.2 um filtered seawater,
adjusted to match the salinity at the time of collection with bottled
spring water. During the assay, several ‘control’ individuals were
maintained in this adjusted salinity solution, but did not experience
the temperature ramp, to ensure that there was no background mortality.

Shown below are the measured CTmax values. Note: CTmax values for the
early season Key Largo copepods were collected at the end of February
2023 as part of a separate project. Body size values were not measured
during this project, nor were copepods individually preserved after the
experiments. These early season CTmax values are included as a point of
comparison.

``` r
ggplot(full_data, aes(x = season, y = ctmax, colour = site)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0,
                                             dodge.width = 0.5)) + 
  scale_colour_manual(values = site_cols) + 
  labs(y = "CTmax (°C)",
       x = "Season") +
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/seasonal-ct-max-1.png" style="display: block; margin: auto;" />

## Body Size

Following the CTmax assay, individuals were photographed for body size
measurements. Prosome lengths were measured from these photographs using
a scale micrometer and the software ImageJ. These measurements are shown
below.

``` r
ggplot(full_data, aes(x = season, y = size, colour = site)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0,
                                             dodge.width = 0.5)) + 
  scale_colour_manual(values = site_cols) + 
  labs(y = "Prosome Length (mm)",
       x = "Season") +
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/seasonal-body-size-1.png" style="display: block; margin: auto;" />

## Trait Correlations

We expect that collections from warmer waters should yield copepods with
higher thermal limits and smaller body sizes.

``` r
ctmax_temp_plot = ggplot(full_data, aes(x = collection_temp, y = ctmax)) + 
  geom_smooth(method = "lm", se = T,
              linewidth = 2, 
              colour = "grey") + 
  geom_point(aes(colour = site), 
             size = 2, alpha = 0.7) + 
  scale_colour_manual(values = site_cols) + 
  labs(y = "CTmax (°C)",
       x = "Collection Temp. (°C)") +
  theme_matt() + 
  theme(legend.position = "right")

size_temp_plot = ggplot(full_data, aes(x = collection_temp, y = size)) + 
  geom_smooth(method = "lm", se = T,
              linewidth = 2, 
              colour = "grey") + 
  geom_point(aes(colour = site), 
             size = 2, alpha = 0.7) + 
  scale_colour_manual(values = site_cols) + 
  labs(y = "Prosome Length (mm)",
       x = "Collection Temp. (°C)") +
  theme_matt() + 
  theme(legend.position = "right")

ggpubr::ggarrange(ctmax_temp_plot, size_temp_plot, common.legend = T, legend = "bottom")
```

<img src="../Figures/markdown/temp-cors-1.png" style="display: block; margin: auto;" />

Of particular interest is the relationship between prosome length and
CTmax. In many cases, larger body sizes are associated with cold
adaptation/acclimation. We may therefore see this pattern emerge across
populations or seasons. If populations contain a mix of cold- and
warm-adapted genotypes, however, we might also see this relationship
emerge **within** individual collections. Shown below is the
relationship between prosome length and CTmax for the individuals
measured thus far. Individual regression lines for each site are shown
along with a ‘universal’ regression in grey.

``` r
full_data %>%  
  filter(ctmax > 31) %>% 
  ggplot(aes(x = size, y = ctmax)) + 
  # geom_smooth(data = filter(full_data, ctmax > 31), 
  #             aes(x = size, y = ctmax),
  #             method = "lm", 
  #             colour = "grey60", 
  #             se = F,
  #             linewidth = 2) + 
  geom_smooth(method = "lm", se = F,
              linewidth = 2,
              colour = "grey70") + 
  geom_point(aes(colour = site),
             size = 2, alpha = 0.7) + 
  scale_colour_manual(values = site_cols) + 
  labs(y = "CTmax (°C)",
       x = "Prosome Length (mm)") +
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/ctmax-vs-size-1.png" style="display: block; margin: auto;" />

## Next Steps

After phenotyping, each individual was preserved in 95% ethanol.
Individual DNA libraries will be prepared using Twist Bio 96-plex prep
kits, then sequenced on an Illumina NovaSeq X Plus. Using the
low-coverage whole genome sequences, we will examine seasonal patterns
in allele frequency change, and compare these fine scale temporal
patterns with the larger latitudinal patterns in allele frequency to
determine whether the same alleles driving rapid seasonal adaptation are
in play over larger spatial (and longer temporal) scales.

## Misc. Details

``` r
ggplot(temp_record, aes(x = minute_passed, y = temp_C, group = factor(run))) + 
  geom_abline(slope = 0.3, intercept = mean(temp_record[temp_record$minute_interval == 0, 8])) + 
  geom_abline(slope = 0.1, intercept = mean(temp_record[temp_record$minute_interval == 0, 8])) + 
  geom_line(linewidth = 0.2, alpha = 0.8) + 
  geom_point(data = full_data, 
             aes(x = time, y = ctmax + 0.4),
             size = 2,
             shape = 25) +
  labs(x = "Time passed (minutes)",
       y = "Temperature (degrees C)",
       fill = "Trial Number") + 
  guides(colour = "none") + 
  theme_matt(base_size = 16) + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/temp-record-plot-1.png" style="display: block; margin: auto;" />

``` r
ramp_record2 = ramp_record %>% 
  group_by(run, minute_interval) %>% 
  summarise(mean_ramp = mean(ramp_per_minute)) %>% 
  ungroup()

ggplot(ramp_record2, aes(x = minute_interval, y = mean_ramp)) + 
  geom_hline(yintercept = 0.3) + 
  geom_hline(yintercept = 0.1) + 
  #geom_point() + 
  geom_hex(bins = 30) + 
  ylim(0, 0.35) + 
  labs(y = "Ramp Rate (deg. C / min.)",
       x = "Time into run (minute)") + 
  theme_matt(base_size = 16) 
```

<img src="../Figures/markdown/ramp-record-plot-1.png" style="display: block; margin: auto;" />
