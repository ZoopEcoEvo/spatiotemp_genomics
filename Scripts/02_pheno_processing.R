# Phenotype processing ----------------------------------------------------
# Combines the observation (time + size) and temperature logger files for each
# CTmax assay to estimate CTmax per individual.
#
# Inputs (one pair per assay run, named <yyyy_mm_dd_run>):
#   Raw_data/pheno_data/<run>_obs.csv   collection info, time of CTmax, body size
#   Raw_data/temp_data/<run>_temp.CSV   three temperature sensors, 5-s intervals
# Outputs (Output/Output_data/):
#   ctmax_runs/<run>_ctmax.csv  per-run results
#   full_data.csv               all individuals (one row per individual)
#   temp_record.csv             all logger data (long format)
#   ramp_record.csv             ramping rate per sensor and minute
#
# Every run is reprocessed each time (a few seconds); there is no incremental
# "previous runs" bookkeeping any more. Run IDs follow the sorted file names.
#
# Requires: here, tidyverse, lubridate

library(here)
library(tidyverse)
library(lubridate)

out_dir <- here("Output", "Output_data")
run_dir <- file.path(out_dir, "ctmax_runs")
dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)

runs <- sort(str_remove(dir(here("Raw_data", "pheno_data"), pattern = "_obs\\.csv$"), "_obs\\.csv$"))

process_run <- function(file_name, run_id) {
  # Temperature sensors (logging at 5 s intervals)
  temp_data <- read_csv(here("Raw_data", "temp_data", paste0(file_name, "_temp.CSV")),
                        show_col_types = FALSE) %>%
    mutate(Time = hms(Time),
           Date = as_date(Date)) %>%
    mutate(time_point = row_number(),
           second_passed = time_length(Time - first(Time)),   # s since logging began
           minute_passed = second_passed / 60,
           minute_interval = floor(second_passed / 60)) %>%
    pivot_longer(cols = c(Temp1, Temp2, Temp3),
                 names_to = "sensor", values_to = "temp_C") %>%
    ungroup()

  date <- paste(str_split_fixed(file_name, "_", 4)[1:3], collapse = "-")

  time_data <- read_csv(here("Raw_data", "pheno_data", paste0(file_name, "_obs.csv")),
                        show_col_types = FALSE) %>%
    drop_na(ctmax_minute) %>%
    mutate(time = (ctmax_minute + (ctmax_second / 60)) - 2,  # 2-min logger start-up delay
           rank = dense_rank(desc(time)),
           collection_date = as_date(collection_date, format = "%m/%d/%y"),
           exp_date = as_date(date),
           days_in_lab = as.numeric(exp_date - collection_date))

  # Rate of change for each sensor in each minute interval
  min_ramp <- temp_data %>%
    group_by(sensor, minute_interval) %>%
    group_modify(~ data.frame(
      ramp_per_second = unclass(coef(lm(data = .x, temp_C ~ second_passed))[2]))) %>%
    mutate(ramp_per_minute = ramp_per_second * 60,
           run = run_id)

  # CTmax = mean temperature over each individual's uncertainty window
  # (0.1 min x rank before the recorded time); ramp rate = mean over the prior 5 min
  ind_measurements <- time_data %>%
    group_by(tube) %>%
    summarise(
      ctmax = mean(filter(temp_data, minute_passed > (time - (0.1 * rank)) & minute_passed < time)$temp_C),
      ramp_rate = mean(filter(min_ramp, minute_interval > (time - 5) & minute_interval < time)$ramp_per_minute))

  ct_data <- inner_join(time_data, ind_measurements, by = "tube") %>%
    mutate(run = run_id,
           warming_tol = ctmax - collection_temp) %>%
    select(collection_date, site, season, collection_temp, collection_salinity, exp_date,
           days_in_lab, run, replicate, tube, rank, size, time, ramp_rate, ctmax, warming_tol)

  write.csv(ct_data, file.path(run_dir, paste0(file_name, "_ctmax.csv")), row.names = FALSE)

  list(ct = ct_data,
       temp = mutate(temp_data, run = run_id),
       ramp = min_ramp)
}

results <- map2(runs, seq_along(runs), process_run)

full_data   <- map_dfr(results, "ct")
temp_record <- map_dfr(results, "temp") %>% mutate(Time = as.character(Time))
ramp_record <- map_dfr(results, "ramp")

write.csv(full_data,   file.path(out_dir, "full_data.csv"),   row.names = FALSE)
write.csv(temp_record, file.path(out_dir, "temp_record.csv"), row.names = FALSE)
write.csv(ramp_record, file.path(out_dir, "ramp_record.csv"), row.names = FALSE)

message(length(runs), " assay runs, ", nrow(full_data), " individuals -> ", out_dir)
