# MUR SST at each site --------------------------------------------------------
# Downloads daily MUR SST (GHRSST Level 4, ~1 km, from June 2002) around each site
# for the environmental summaries of Stage 3.
#   Dataset: jplMURSST41 on the NOAA CoastWatch ERDDAP server
#   https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html
#
# Steps
#   1. Water pixels: one day's grid in a box around each site (land pixels are
#      missing). All water pixels within `radius_km` of the site are used. Copepods
#      are advected in and out of the sampling sites, so the surrounding water is the
#      relevant environment, and averaging also damps the error of single pixels next
#      to land. If no water pixel is that close (site's own water body not resolved),
#      the search box is widened and the water within `radius_km` of the nearest water
#      pixel is used instead; such sites are flagged (fallback_nearest).
#   2. Daily series: the grid around those pixels, downloaded one year at a time and
#      averaged across the pixels for each day (the mean is taken per day, before any
#      summary, so extremes and ranges come from one regional series). Each site-year
#      is cached in Raw_data/outside_sources/mur_sst/ (file name includes the radius),
#      so an interrupted run resumes where it stopped.
#
# Outputs (Output/Output_data/)
#   mur_pixels.csv     one row per pixel used: site, pixel coordinates, distance (km)
#   mur_sites.csv      per site: pixels used, nearest and farthest distance, fallback flag
#   mur_sst_daily.csv  site_code, date, sst_c (mean over pixels), sst_sd_pixels (spread
#                      among pixels that day), n_pixels, analysis_error, sea_ice_fraction
#
# MUR values are "foundation" temperatures (roughly night-time, below the surface
# skin): no daily heating cycle and no tidal swings. Ranges should therefore be
# computed from daily means, and compared with logger daily means, not raw readings.
#
# Needs an internet connection. Requires: here, tidyverse, rerddap

library(here)
library(tidyverse)
library(rerddap)

# Settings ------------------------------------------------------------------
erddap_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset    <- "jplMURSST41"
years      <- 2014:2023      # 10 years ending in the sampling year
radius_km  <- 5              # average water pixels within this distance of the site
box_deg    <- c(0.1, 0.2, 0.4) # search box half-widths for water pixels, tried in turn until one has water
probe_date <- "2023-07-15"   # day used to find water pixels (summer: no ice)
cache_dir  <- here("Raw_data", "outside_sources", "mur_sst")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

sites <- read_csv(here("Raw_data", "site_data.csv"), show_col_types = FALSE) %>%
  rename(lon = long)

haversine_km <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  6371 * 2 * asin(sqrt(pmin(1, a)))
}

# griddap can return array (matrix) columns; make them plain vectors
flatten_cols <- function(d) {
  as_tibble(d) %>% mutate(across(everything(), ~ if (is.list(.x)) unlist(.x) else as.vector(.x)))
}

# Retry a download a few times (ERDDAP occasionally times out) -----------------
with_retry <- function(expr_fun, tries = 4, wait = 20) {
  for (i in seq_len(tries)) {
    out <- tryCatch(expr_fun(), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    message("  attempt ", i, " failed: ", conditionMessage(out))
    if (i < tries) Sys.sleep(wait)
  }
  stop(out)
}

# 1. Water pixels around each site -------------------------------------------------
pixel_file <- here("Output", "Output_data", "mur_pixels.csv")
if (file.exists(pixel_file) && all(read_csv(pixel_file, show_col_types = FALSE)$radius_km == radius_km)) {
  pixels <- read_csv(pixel_file, show_col_types = FALSE)
} else {
  pixels <- pmap_dfr(sites, function(site, site_code, lat, lon, ...) {
    message("Finding water pixels for ", site_code)
    # widen the search box until it contains water (narrow estuaries are not resolved)
    water <- tibble()
    for (b in box_deg) {
      grid <- with_retry(function() {
        griddap(datasetx = dataset, url = erddap_url,
                time = c(probe_date, probe_date),
                latitude = c(lat - b, lat + b),
                longitude = c(lon - b, lon + b),
                fields = "analysed_sst")$data
      }) %>% flatten_cols()
      water <- grid %>%
        filter(!is.na(analysed_sst)) %>%
        mutate(dist_km = haversine_km(lat, lon, latitude, longitude)) %>%
        arrange(dist_km)
      if (nrow(water) > 0) break
      message("  no water within ", b, " deg of ", site_code)
    }
    if (nrow(water) == 0) {
      warning("No water pixel within ", max(box_deg), " deg of ", site_code, ": add a larger box_deg")
      return(NULL)
    }
    use <- filter(water, dist_km <= radius_km)
    fallback <- nrow(use) == 0
    if (fallback) {
      # site's own water body not resolved: average the water within radius_km of the
      # nearest water pixel instead (the adjacent water the site exchanges with)
      near <- slice(water, 1)
      use <- filter(water, haversine_km(near$latitude, near$longitude, latitude, longitude) <= radius_km)
    }
    tibble(site = site, site_code = site_code, site_lat = lat, site_lon = lon,
           pixel_lat = use$latitude, pixel_lon = use$longitude, dist_km = use$dist_km,
           radius_km = radius_km, fallback_nearest = fallback)
  })
  write_csv(pixels, pixel_file)
}

site_pixels <- pixels %>%
  group_by(site, site_code) %>%
  summarise(n_pixels = n(), nearest_km = min(dist_km), farthest_km = max(dist_km),
            fallback_nearest = any(fallback_nearest), .groups = "drop")
write_csv(site_pixels, here("Output", "Output_data", "mur_sites.csv"))
print(site_pixels, n = Inf)

# 2. Daily regional series per site and year ---------------------------------------
get_year <- function(code, year) {
  f <- file.path(cache_dir, sprintf("%s_%d_r%gkm.csv", code, year, radius_km))
  if (file.exists(f)) return(read_csv(f, show_col_types = FALSE))
  message("Downloading ", code, " ", year)
  px <- filter(pixels, site_code == code)
  start <- max(as.Date(sprintf("%d-01-01", year)), as.Date("2002-06-01"))
  end   <- min(as.Date(sprintf("%d-12-31", year)), Sys.Date() - 2)
  keep  <- paste(round(px$pixel_lat, 3), round(px$pixel_lon, 3))
  d <- with_retry(function() {
    griddap(datasetx = dataset, url = erddap_url,
            time = c(as.character(start), as.character(end)),
            latitude = range(px$pixel_lat),
            longitude = range(px$pixel_lon),
            fields = c("analysed_sst", "analysis_error", "sea_ice_fraction"))$data
  }) %>%
    flatten_cols() %>%
    filter(paste(round(latitude, 3), round(longitude, 3)) %in% keep) %>%
    mutate(date = as.Date(substr(time, 1, 10))) %>%
    group_by(date) %>%
    summarise(site_code = code,
              sst_c = mean(analysed_sst, na.rm = TRUE),
              sst_sd_pixels = sd(analysed_sst, na.rm = TRUE),
              n_pixels = sum(!is.na(analysed_sst)),
              analysis_error = mean(analysis_error, na.rm = TRUE),
              sea_ice_fraction = mean(sea_ice_fraction, na.rm = TRUE),
              .groups = "drop") %>%
    relocate(site_code)
  # ERDDAP serves MUR in degrees C; guard against a Kelvin response
  if (mean(d$sst_c, na.rm = TRUE) > 200) d <- mutate(d, sst_c = sst_c - 273.15)
  write_csv(d, f)
  d
}

daily <- site_pixels %>%
  select(code = site_code) %>%
  crossing(year = years) %>%
  pmap_dfr(get_year) %>%
  arrange(site_code, date)

write_csv(daily, here("Output", "Output_data", "mur_sst_daily.csv"))

# Quick check ----------------------------------------------------------------
daily %>%
  group_by(site_code) %>%
  summarise(days = n(), first = min(date), last = max(date),
            missing = sum(is.na(sst_c)), mean_sst = mean(sst_c, na.rm = TRUE),
            max_sst = max(sst_c, na.rm = TRUE),
            mean_pixels = mean(n_pixels), mean_sd_pixels = mean(sst_sd_pixels, na.rm = TRUE),
            .groups = "drop") %>%
  print(n = Inf)
