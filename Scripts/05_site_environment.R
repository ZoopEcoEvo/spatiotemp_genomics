# Site environment for Stage 3: MUR SST + checks against monitoring stations ----------
# Three parts, each can be switched off below:
#   Part 1 (get_mur)      daily MUR SST around each site, 2014-2023
#   Part 2 (get_stations) public monitoring stations near the sites, compared with MUR
#                         on matching days (needs Part 1's output)
#   Part 3 (seasonal_temps) temperature experienced by each seasonal sample: mean MUR SST
#                         over the generation(s) before collection -> seasonal_temperature.tsv,
#                         copied to the cluster (metadata/) for the temperature version of
#                         the per-SNP seasonal test (needs Part 1's output)
#   Station corrections   sites where MUR is measurably off and a station sits at the site
#                         get a month-varying offset (mur_corrections below; Parts 3-4 use
#                         the corrected series, written to mur_sst_daily_corrected.csv)
#   Part 4 (site_climate) long-term site climatology from MUR, 2014-2023 -> site_climatology.csv:
#                         the between-site temperature for the CTmax model (report section 4),
#                         Stage 3 and the space-vs-time comparisons (needs Part 1's output)
# Both parts cache every download in Raw_data/outside_sources/, so reruns only fetch
# what is missing. Needs an internet connection.
# Requires: here, tidyverse, rerddap (Part 1), dataRetrieval (Part 2)
#
# (Separate from 01_site_temps.R, which builds the 2023 sampling-season temperature
# profiles used for the phenotype analyses and generation counts.)

library(here)
library(tidyverse)

get_mur        <- F
get_stations   <- F
seasonal_temps <- TRUE
site_climate   <- TRUE

# Station-based corrections of MUR: site_code = station (as named in Part 2). GW: MUR has
# no water near Ganey's Wharf (uses water 14-19 km away); CBP station ET5.1 at the wharf
# is +2 to +4 deg C warmer in spring and 1-1.7 deg C colder in autumn (shallow upper estuary).
mur_corrections <- c(GW = "ET5.1 Ganey Wharf")

haversine_km <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  a <- sin((lat2 - lat1) * to_rad / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin((lon2 - lon1) * to_rad / 2)^2
  6371 * 2 * asin(sqrt(pmin(1, a)))
}

# =====================================================================================
# Part 1: MUR SST
# =====================================================================================
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

if (get_mur) {
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
}

# =====================================================================================
# Part 2: monitoring stations vs MUR
# =====================================================================================
# Downloads water temperature (and salinity where measured) from public monitoring
# stations near the sampling sites, and compares them with the MUR series from
# Part 1 on matching days.
#
# Sources (all programmatic except the DFO file):
#   USGS        Noank CT (Mystic R.), Manatee R. at Rye and at Devils Elbow (FL) - dataRetrieval
#   CBP         Chesapeake Bay Program monthly stations ET5.1 (Ganey Wharf), CB5.1
#               (Cedar Point) - Water Quality Portal via dataRetrieval
#   NOAA CO-OPS Port Manatee FL (= NDBC PMAF1), New London CT, Portland ME (= NDBC CASM1)
#               - CO-OPS data API
#   CBIBS       Gooses Reef buoy, 2023 (Raw_data/site_temps/GR_OCEAN_2023_trimmed.csv)
#   DFO         Coastal Temperature Monitoring, southern Gulf of St. Lawrence (manual
#               download: put the CSV in Raw_data/outside_sources/dfo_sgsl_temperature/)
#   Collection  temperatures measured at each site on each sampling day (cluster
#               export metadata/sites.tsv)
#
# Downloads are cached in Raw_data/outside_sources/stations/ (one file per station and
# year), so reruns only fetch what is missing. A source that fails is reported and
# skipped; the rest still run.
#
# Outputs (Output/Output_data/)
#   station_daily.csv          station, site_code, date, temp_c, salinity (daily means)
#   mur_check_daily.csv        station and MUR on the same days
#   mur_check_summary.csv      per station: overlap, mean bias (station - MUR), RMSE, r,
#                              and the Stage 3 metrics from each source on the shared days
#   mur_check_monthly.csv      mean bias by calendar month (seasonal pattern of the bias)
#   mur_check_collection.csv   collection-day temperatures vs MUR on those dates
#
# Needs an internet connection. Requires: here, tidyverse, dataRetrieval

if (get_stations) {

  years      <- 2014:2023
  start_date <- as.Date(sprintf("%d-01-01", min(years)))
  end_date   <- as.Date(sprintf("%d-12-31", max(years)))
  cache_dir  <- here("Raw_data", "outside_sources", "stations")
  dfo_dir    <- here("Raw_data", "outside_sources", "dfo_sgsl_temperature")
  dfo_max_km <- 30     # DFO buoys within this distance of a site are compared with it
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  mur <- read_csv(here("Output", "Output_data", "mur_sst_daily.csv"), show_col_types = FALSE)
  sites <- read_csv(here("Raw_data", "site_data.csv"), show_col_types = FALSE) %>%
    rename(lon = long)

  # run one source; report and skip it if it fails
  try_source <- function(label, expr_fun) {
    message("== ", label)
    out <- tryCatch(expr_fun(), error = function(e) {
      message("   FAILED: ", conditionMessage(e)); NULL
    })
    if (!is.null(out)) message("   ", nrow(out), " daily values")
    out
  }

  # cached download of one station-year
  cached <- function(name, fetch_fun) {
    f <- file.path(cache_dir, paste0(name, ".csv"))
    if (file.exists(f)) return(read_csv(f, show_col_types = FALSE))
    d <- fetch_fun()
    if (!is.null(d) && nrow(d) > 0) write_csv(d, f)
    d
  }

  # first column whose name matches any of the patterns (WQP and USGS names vary by version)
  pick_col <- function(d, patterns) {
    for (p in patterns) {
      hit <- grep(p, names(d), value = TRUE, ignore.case = TRUE)
      if (length(hit)) return(hit[1])
    }
    NA_character_
  }

  # Stations ----------------------------------------------------------------------
  stations <- tribble(
    ~station,                 ~source,  ~id,                ~site_code, ~type,
    "Noank (Mystic R.)",      "usgs",   "411940071590300",  "CT",       "continuous",
    "Manatee R. at Rye",      "usgs",   "023000095",        "FH",       "continuous",
    "Manatee R. Devils Elbow","usgs",   "02300009",         "FH",       "continuous",
    "ET5.1 Ganey Wharf",      "wqp",    "CBP_WQX-ET5.1",    "GW",       "monthly",
    "CB5.1 Cedar Point",      "wqp",    "CBP_WQX-CB5.1",    "MD",       "monthly",
    "Port Manatee",           "coops",  "8726384",          "MR",       "continuous",
    "New London",             "coops",  "8461490",          "CT",       "continuous",
    "Portland ME",            "coops",  "8418150",          "ME",       "continuous",
    "Gooses Reef buoy",       "cbibs",  "GR",               "MD",       "continuous"
  )

  # USGS (continuous) ---------------------------------------------------------------
  # 00010 water temperature, 00480 salinity. Instantaneous values, averaged per day.
  get_usgs <- function(id) {
    if (!requireNamespace("dataRetrieval", quietly = TRUE)) stop("install dataRetrieval")
    map_dfr(years, function(y) cached(sprintf("usgs_%s_%d", id, y), function() {
      uv <- dataRetrieval::readNWISuv(siteNumbers = id, parameterCd = c("00010", "00480"),
                                      startDate = sprintf("%d-01-01", y),
                                      endDate = sprintf("%d-12-31", y), tz = "UTC")
      if (is.null(uv) || nrow(uv) == 0) return(tibble())
      tcol <- grep("00010_00000$", names(uv), value = TRUE)
      scol <- grep("00480_00000$", names(uv), value = TRUE)
      tibble(date = as.Date(uv$dateTime),
             temp_c = if (length(tcol)) rowMeans(as.matrix(uv[tcol]), na.rm = TRUE) else NA_real_,
             salinity = if (length(scol)) rowMeans(as.matrix(uv[scol]), na.rm = TRUE) else NA_real_) %>%
        group_by(date) %>%
        summarise(temp_c = mean(temp_c, na.rm = TRUE), salinity = mean(salinity, na.rm = TRUE),
                  .groups = "drop")
    }))
  }

  # Chesapeake Bay Program monthly stations (Water Quality Portal) ---------------------
  # Shallowest sample of each date = surface value.
  get_wqp <- function(id) {
    if (!requireNamespace("dataRetrieval", quietly = TRUE)) stop("install dataRetrieval")
    cached(sprintf("wqp_%s_%d_%d", id, min(years), max(years)), function() {
      w <- dataRetrieval::readWQPdata(siteid = id,
                                      characteristicName = c("Temperature, water", "Salinity"),
                                      startDateLo = format(start_date), startDateHi = format(end_date))
      if (is.null(w) || nrow(w) == 0) return(tibble())
      dcol <- pick_col(w, c("^ActivityStartDate$", "Activity_StartDate"))
      ccol <- pick_col(w, c("^CharacteristicName$", "Result_Characteristic$"))
      vcol <- pick_col(w, c("^ResultMeasureValue$", "Result_Measure$"))
      zcol <- pick_col(w, c("^ActivityDepthHeightMeasure.MeasureValue$", "Activity_Depth$",
                            "^ResultDepthHeightMeasure.MeasureValue$", "Result_Depth"))
      tibble(date = as.Date(w[[dcol]]), what = w[[ccol]],
             value = suppressWarnings(as.numeric(w[[vcol]])),
             depth = if (is.na(zcol)) NA_real_ else suppressWarnings(as.numeric(w[[zcol]]))) %>%
        filter(!is.na(value)) %>%
        group_by(date, what) %>%
        filter(is.na(depth) | depth == min(depth, na.rm = TRUE)) %>%
        summarise(value = mean(value), .groups = "drop") %>%
        mutate(what = if_else(str_detect(what, regex("temp", ignore_case = TRUE)), "temp_c", "salinity")) %>%
        pivot_wider(names_from = what, values_from = value)
    }) %>%
      { if (!"salinity" %in% names(.)) mutate(., salinity = NA_real_) else . }
  }

  # NOAA CO-OPS (continuous; hourly values) ------------------------------------------------
  # The API limits water-temperature requests to about a month, so each year is fetched
  # month by month (cached per year). A month without data returns an error line, which is
  # treated as no data.
  coops_month <- function(id, start) {
    end <- lubridate::ceiling_date(start, "month") - 1
    url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                  "product=water_temperature&interval=h&time_zone=gmt&units=metric&format=csv",
                  "&application=aton_lcwg&station=", id,
                  "&begin_date=", format(start, "%Y%m%d"), "&end_date=", format(end, "%Y%m%d"))
    lines <- tryCatch(readLines(url, warn = FALSE), error = function(e) character())
    if (length(lines) < 2 || !grepl("^Date Time", lines[1])) return(NULL)
    d <- read.csv(text = lines, header = TRUE, check.names = FALSE)
    tibble(time = as.POSIXct(d[[1]], tz = "UTC", format = "%Y-%m-%d %H:%M"),
           temp_c = suppressWarnings(as.numeric(d[[2]])))
  }
  get_coops <- function(id) {
    map_dfr(years, function(y) cached(sprintf("coops_%s_%d", id, y), function() {
      months <- seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-01", y)), by = "month")
      h <- map_dfr(months, ~ coops_month(id, .x))
      if (nrow(h) == 0) return(tibble())
      h %>%
        filter(!is.na(temp_c), !is.na(time)) %>%
        group_by(date = as.Date(time)) %>%
        summarise(temp_c = mean(temp_c), n_hours = n(), .groups = "drop") %>%
        filter(n_hours >= 18) %>%                 # skip days with large gaps
        transmute(date, temp_c, salinity = NA_real_)
    }))
  }

  # CBIBS Gooses Reef (file already in the project) ------------------------------------
  get_cbibs <- function(id) {
    read_csv(here("Raw_data", "site_temps", "GR_OCEAN_2023_trimmed.csv"), show_col_types = FALSE) %>%
      filter(!(temp_qual %in% c(3, 4, 9))) %>%
      mutate(date = as.Date(lubridate::mdy_hm(time))) %>%
      group_by(date) %>%
      summarise(temp_c = mean(temp_c, na.rm = TRUE), .groups = "drop") %>%
      mutate(salinity = NA_real_)
  }

  station_daily <- pmap_dfr(stations, function(station, source, id, site_code, type) {
    d <- try_source(station, function() {
      switch(source, usgs = get_usgs(id), wqp = get_wqp(id), coops = get_coops(id),
             cbibs = get_cbibs(id))
    })
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d %>%
      mutate(across(c(temp_c, salinity), ~ if_else(is.nan(.x), NA_real_, .x))) %>%
      mutate(station = station, site_code = site_code, type = type, .before = 1)
  })

  # DFO southern Gulf of St. Lawrence (manual download) -----------------------------
  dfo_files <- list.files(dfo_dir, pattern = "\\.csv$", full.names = TRUE) %>%
    discard(~ str_detect(basename(.x), regex("dictionary", ignore_case = TRUE)))
  if (length(dfo_files) == 0) {
    message("== DFO: no CSV in ", dfo_dir, " (download 'Coastal Temperatures sGSL' there); skipped")
  } else {
    dfo <- try_source("DFO coastal temperature", function() {
      # the DFO file is Latin-1 encoded (bilingual column names with accents)
      raw <- map_dfr(dfo_files, ~ read_csv(.x, show_col_types = FALSE, guess_max = 1e5,
                                           locale = locale(encoding = "latin1")))
      cols <- c(lat = pick_col(raw, c("^lat", "latitude")),
                lon = pick_col(raw, c("^lon", "longitude")),
                time = pick_col(raw, c("date.?time", "^date", "^time", "timestamp")),
                year = pick_col(raw, c("^year")), month = pick_col(raw, c("^month")),
                day = pick_col(raw, c("^day")),
                temp = pick_col(raw, c("daily_average_temp", "^temp", "temperature", "^tw")),
                stn = pick_col(raw, c("^site$", "station", "site", "buoy", "location", "^id")),
                depth = pick_col(raw, c("log_position", "position", "log_depth", "depth", "level")))
      has_ymd <- !any(is.na(cols[c("year", "month", "day")]))
      if (any(is.na(cols[c("lat", "lon", "temp")])) || (is.na(cols[["time"]]) && !has_ymd))
        stop("could not identify columns; names are: ", paste(names(raw), collapse = ", "),
             ". Set them in the `cols` vector in this section.")
      message("   columns used: ", paste(names(cols), cols, sep = " = ", collapse = "; "))
      dates <- if (has_ymd) {
        as.Date(sprintf("%04d-%02d-%02d", as.integer(raw[[cols[["year"]]]]),
                        as.integer(raw[[cols[["month"]]]]), as.integer(raw[[cols[["day"]]]])))
      } else {
        as.Date(lubridate::parse_date_time(raw[[cols[["time"]]]],
                                           c("Ymd HMS", "Ymd HM", "Ymd", "dmY HM", "mdY HM")))
      }
      d <- tibble(lat = as.numeric(raw[[cols[["lat"]]]]), lon = as.numeric(raw[[cols[["lon"]]]]),
                  date = dates,
                  temp_c = as.numeric(raw[[cols[["temp"]]]]),
                  dfo_station = if (is.na(cols[["stn"]])) paste(round(lat, 3), round(lon, 3)) else as.character(raw[[cols[["stn"]]]]),
                  depth = if (is.na(cols[["depth"]])) "all" else as.character(raw[[cols[["depth"]]]])) %>%
        filter(!is.na(temp_c), date >= start_date, date <= end_date)
      # stations within dfo_max_km of TK or RW
      near <- d %>% distinct(dfo_station, depth, lat, lon) %>%
        crossing(filter(sites, site_code %in% c("TK", "RW")) %>%
                   select(site_code, s_lat = lat, s_lon = lon)) %>%
        mutate(dist_km = haversine_km(s_lat, s_lon, lat, lon)) %>%
        filter(dist_km <= dfo_max_km)
      print(arrange(near, site_code, dist_km), n = Inf)
      d %>% inner_join(select(near, dfo_station, depth, site_code, dist_km), by = c("dfo_station", "depth"),
                       relationship = "many-to-many") %>%
        group_by(site_code, dfo_station, depth, dist_km, date) %>%
        summarise(temp_c = mean(temp_c), .groups = "drop") %>%
        transmute(station = sprintf("DFO %s (%s, %.0f km)", dfo_station, depth, dist_km),
                  site_code, type = "continuous", date, temp_c, salinity = NA_real_)
    })
    station_daily <- bind_rows(station_daily, dfo)
  }

  write_csv(station_daily, here("Output", "Output_data", "station_daily.csv"))

  # Comparison with MUR on shared days -------------------------------------------------
  check <- station_daily %>%
    filter(!is.na(temp_c)) %>%
    inner_join(select(mur, site_code, date, mur_c = sst_c), by = c("site_code", "date")) %>%
    mutate(bias = temp_c - mur_c)
  write_csv(check, here("Output", "Output_data", "mur_check_daily.csv"))

  # Stage 3 metrics from a daily series (computed on the shared days only)
  metrics <- function(date, x) {
    m <- tibble(date, x, month = lubridate::floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(mean_m = mean(x), range_m = max(x) - min(x), n = n(), .groups = "drop")
    tibble(mean = mean(x),
           p95 = quantile(x, 0.95, names = FALSE),
           warmest_month = max(m$mean_m),
           coldest_month = min(m$mean_m),
           monthly_range = mean(m$range_m[m$n >= 20]))      # months with >= 20 shared days
  }

  summary_tab <- check %>%
    group_by(station, site_code, type) %>%
    group_modify(function(d, key) {
      base <- tibble(days = nrow(d), first = min(d$date), last = max(d$date),
                     bias = mean(d$bias), rmse = sqrt(mean(d$bias^2)),
                     r = if (nrow(d) > 2) cor(d$temp_c, d$mur_c) else NA_real_)
      if (key$type == "continuous" && nrow(d) >= 60) {
        st <- metrics(d$date, d$temp_c) %>% rename_with(~ paste0("station_", .x))
        mu <- metrics(d$date, d$mur_c) %>% rename_with(~ paste0("mur_", .x))
        base <- bind_cols(base, st, mu)
      }
      base
    }) %>%
    ungroup() %>%
    arrange(site_code, station)
  write_csv(summary_tab, here("Output", "Output_data", "mur_check_summary.csv"))

  check %>%
    mutate(month = lubridate::month(date)) %>%
    group_by(station, site_code, month) %>%
    summarise(days = n(), bias = mean(bias), .groups = "drop") %>%
    write_csv(here("Output", "Output_data", "mur_check_monthly.csv"))

  # Collection-day temperatures vs MUR ---------------------------------------------------
  export_root <- here("Raw_data", "cluster_exports")
  export_name <- getOption("cluster_export")
  if (is.null(export_name)) export_name <- tail(sort(dir(export_root)), 1)
  sites_meta <- file.path(export_root, export_name, "metadata", "sites.tsv")
  if (file.exists(sites_meta)) {
    coll <- read_tsv(sites_meta, show_col_types = FALSE) %>%
      select(site_code = site, matches("^(date|temp_C)_(early|peak|late)$")) %>%
      mutate(across(starts_with("date_"), as.character), across(starts_with("temp_C_"), as.numeric)) %>%
      pivot_longer(-site_code, names_to = c(".value", "season"), names_pattern = "(date|temp_C)_(.*)") %>%
      filter(!is.na(date), !is.na(temp_C)) %>%
      mutate(date = as.Date(date)) %>%
      left_join(select(mur, site_code, date, mur_c = sst_c), by = c("site_code", "date")) %>%
      mutate(diff = temp_C - mur_c)
    write_csv(coll, here("Output", "Output_data", "mur_check_collection.csv"))
    message("== Collection-day temperature minus MUR, by site")
    coll %>% group_by(site_code) %>%
      summarise(n = n(), mean_diff = mean(diff, na.rm = TRUE), .groups = "drop") %>%
      print(n = Inf)
  }

  message("== Station vs MUR (bias = station - MUR, deg C)")
  summary_tab %>% select(station, site_code, type, days, bias, rmse, r) %>% print(n = Inf)
}

# =====================================================================================
# Station corrections: month-varying offsets for sites listed in mur_corrections
# =====================================================================================
# Monthly mean bias (station - MUR) from mur_check_monthly.csv (Part 2), smoothed with a
# circular 3-month running mean, interpolated to day of year between mid-month anchors,
# and added to that site's daily MUR values. Raw MUR is kept as sst_c_mur.
read_mur_corrected <- function() {
  mur <- read_csv(here("Output", "Output_data", "mur_sst_daily.csv"), show_col_types = FALSE) %>%
    mutate(sst_c_mur = sst_c, correction = 0)
  mfile <- here("Output", "Output_data", "mur_check_monthly.csv")
  if (length(mur_corrections) == 0) return(mur)
  if (!file.exists(mfile)) stop("mur_check_monthly.csv missing: run Part 2 (get_stations) first")
  monthly <- read_csv(mfile, show_col_types = FALSE)
  for (code in names(mur_corrections)) {
    b <- monthly %>% filter(site_code == code, station == mur_corrections[[code]]) %>%
      arrange(month)
    if (nrow(b) != 12) stop("need 12 months of bias for ", code, " (", mur_corrections[[code]], ")")
    sm <- (b$bias + c(tail(b$bias, 1), head(b$bias, -1)) + c(b$bias[-1], b$bias[1])) / 3
    mid <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)   # mid-month day of year
    f <- approxfun(c(mid[12] - 365, mid, mid[1] + 365), c(sm[12], sm, sm[1]))
    message("   correction for ", code, " (", mur_corrections[[code]], "), deg C by month: ",
            paste(sprintf("%+.1f", sm), collapse = " "))
    i <- mur$site_code == code
    mur$correction[i] <- f(pmin(lubridate::yday(mur$date[i]), 365))
    mur$sst_c[i] <- mur$sst_c_mur[i] + mur$correction[i]
  }
  write_csv(mur, here("Output", "Output_data", "mur_sst_daily_corrected.csv"))
  mur
}

# =====================================================================================
# Part 3: temperature experienced by each seasonal sample
# =====================================================================================
# Allele frequencies reflect selection over the generations before sampling, so each
# sample gets the mean MUR SST over `n_gen` generations before its collection date.
# Generation time depends on temperature (gen_days = 5490 * (T + 1)^-2.05, as used for
# the generation counts), so the window is found iteratively: start from the 30-day mean,
# compute the generation time, average over that window, and repeat.
# Also written for comparison: the 30-day mean, MUR on the collection day, and the
# temperature measured at collection.
if (seasonal_temps) {
  n_gen       <- 1                      # generations before collection to average over
  window_lims <- c(5, 60)               # days
  gen_days    <- function(temp) 5490 * (temp + 1)^-2.05

  mur <- read_mur_corrected()
  export_root <- here("Raw_data", "cluster_exports")
  export_name <- getOption("cluster_export")
  if (is.null(export_name)) export_name <- tail(sort(dir(export_root)), 1)
  events <- read_tsv(file.path(export_root, export_name, "metadata", "sites.tsv"), show_col_types = FALSE) %>%
    select(site, matches("^(date|temp_C)_(early|peak|late)$")) %>%
    mutate(across(starts_with("date_"), as.character), across(starts_with("temp_C_"), as.numeric)) %>%
    pivot_longer(-site, names_to = c(".value", "season"), names_pattern = "(date|temp_C)_(.*)") %>%
    filter(!is.na(date)) %>%
    mutate(date = as.Date(date))

  window_mean <- function(code, end, days) {
    x <- mur$sst_c[mur$site_code == code & mur$date > end - days & mur$date <= end]
    if (length(x) < 0.8 * days) NA_real_ else mean(x, na.rm = TRUE)
  }

  seasonal_temperature <- events %>%
    rowwise() %>%
    mutate(temp_30d = window_mean(site, date, 30),
           temp_collection_mur = window_mean(site, date, 1),
           window_days = NA_real_, temp_c = temp_30d) %>%
    ungroup()
  for (it in 1:3) {
    seasonal_temperature <- seasonal_temperature %>%
      rowwise() %>%
      mutate(window_days = round(pmin(pmax(n_gen * gen_days(temp_c), window_lims[1]), window_lims[2])),
             temp_c = window_mean(site, date, window_days)) %>%
      ungroup()
  }
  seasonal_temperature <- seasonal_temperature %>%
    select(site, season, date, temp_c, window_days, temp_30d, temp_collection_mur,
           temp_measured = temp_C) %>%
    arrange(site, date)
  write_tsv(seasonal_temperature, here("Output", "Output_data", "seasonal_temperature.tsv"))
  message("== Temperature before each collection (copy seasonal_temperature.tsv to the cluster's metadata/)")
  print(seasonal_temperature, n = Inf)
}

# =====================================================================================
# Part 4: long-term site climatology (MUR, all years downloaded)
# =====================================================================================
# One row per site. temp_mean (mean of all days) is the between-site temperature used by
# the report's within/between CTmax model. The other columns are the Stage 3 summaries:
#   temp_season       mean over the sampling season (June-November)
#   warmest_month     mean of the warmest calendar month, averaged over years
#   coldest_month     mean of the coldest calendar month, averaged over years
#   temp_p95          95th percentile of daily values
#   monthly_range     mean over months of (max - min daily value), i.e. the range over
#                     roughly one generation
#   season_days       days per year at or above `active_temp` (time the species can be active)
# Uses the corrected series (sites in mur_corrections); a `corrected` column marks them.
if (site_climate) {
  active_temp <- 10      # deg C; threshold for season length (adjust if a better value is known)
  mur <- read_mur_corrected() %>%
    filter(!is.na(sst_c)) %>%
    mutate(year = lubridate::year(date), month = lubridate::month(date))
  monthly <- mur %>%
    group_by(site_code, year, month) %>%
    summarise(mean_m = mean(sst_c), range_m = max(sst_c) - min(sst_c), n = n(), .groups = "drop") %>%
    filter(n >= 25)
  site_climatology <- mur %>%
    group_by(site_code) %>%
    summarise(years = n_distinct(year), days = n(),
              temp_mean = mean(sst_c),
              temp_season = mean(sst_c[month %in% 6:11]),
              temp_p95 = quantile(sst_c, 0.95, names = FALSE),
              .groups = "drop") %>%
    left_join(monthly %>%
                group_by(site_code, year) %>%
                summarise(warm = max(mean_m), cold = min(mean_m), .groups = "drop") %>%
                group_by(site_code) %>%
                summarise(warmest_month = mean(warm), coldest_month = mean(cold), .groups = "drop"),
              by = "site_code") %>%
    left_join(monthly %>% group_by(site_code) %>% summarise(monthly_range = mean(range_m), .groups = "drop"),
              by = "site_code") %>%
    left_join(mur %>% group_by(site_code, year) %>% summarise(d = sum(sst_c >= active_temp), .groups = "drop") %>%
                group_by(site_code) %>% summarise(season_days = mean(d), .groups = "drop"),
              by = "site_code") %>%
    left_join(read_csv(here("Raw_data", "site_data.csv"), show_col_types = FALSE) %>% select(site_code, lat),
              by = "site_code") %>%
    mutate(corrected = site_code %in% names(mur_corrections)) %>%
    arrange(lat)
  write_csv(site_climatology, here("Output", "Output_data", "site_climatology.csv"))
  message("== Site climatology (MUR, ", min(mur$year), "-", max(mur$year), ")")
  print(site_climatology, n = Inf, width = Inf)
}
