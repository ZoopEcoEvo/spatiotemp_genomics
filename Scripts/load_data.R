# Load processed data -----------------------------------------------------
# Sourced at the top of report.qmd and the manuscripts so each document reads
# the same processed files the same way (they render in clean environments and
# cannot see objects from the controller). Reads only Output/Output_data/ and
# Raw_data/site_data.csv; run 02_pheno_processing.R and 03_sample_table.R first.
#
# Objects created
#   site_data       site, site_code, region, lat, long
#   sample_table    every individual, with inclusion flags (03_sample_table.R)
#   all_data        every phenotyped individual (incl. excluded ones)
#   join_data       phenotype analysis set: include_pheno == TRUE (A. tonsa, clade assigned)
#   seq_data        sequenced libraries, A. hudsonica removed
#   cx(...)         path inside the cluster export in use (export_name)
#   (sample_table also carries micro-screen calls when micro_calls.csv exists)
#   temp_profiles   daily temperature per region (01_site_temps.R)
#   temp_record     CTmax assay logger data; ramp_record: ramping rates
#   site_temps      one row per collection (site x season)
#   temp_summaries  collection temperatures per site, wide, with site means
#   collections     site_temps with site codes
#
# Requires: here, tidyverse

library(here)
library(tidyverse)

source(here("Scripts", "palettes.R"))

od <- function(...) here("Output", "Output_data", ...)

site_data <- read.csv(here("Raw_data", "site_data.csv")) %>%
  mutate(site = fct_reorder(site, lat))

sample_table <- read.csv(od("sample_table.csv")) %>%
  mutate(collection_date = as.Date(collection_date),
         exp_date = as.Date(exp_date),
         doy = lubridate::yday(collection_date),
         site = factor(site, levels = levels(site_data$site)),
         site_code = factor(site_code, levels = c("KL", site_order)),
         season = factor(season, levels = season_order),
         # names used by the older report code
         ind_id = str_replace_all(paste(site, season, replicate, tube, sep = "_"), " ", "_"),
         templates = raw_reads,
         mean_coverage = mean_depth_nuclear,
         pct_1x = breadth_ge1x,
         pct_5x = breadth_ge5x,
         pct_aligned = pct_mapped / 100,
         pf_hq_aligned_reads = raw_reads * pct_mapped / 100)

# Micro screen calls (04_micro_calls.R), if present: <group>_call, parasite_any, epibiont_any
mc <- od("micro_calls.csv")
if (file.exists(mc)) {
  sample_table <- sample_table %>%
    left_join(read.csv(mc) %>% dplyr::select(sample_id, ends_with("_call"), ends_with("_per_M"),
                                             parasite_any, epibiont_any),
              by = "sample_id")
}

all_data  <- filter(sample_table, phenotyped) %>% droplevels()
join_data <- filter(sample_table, include_pheno) %>% droplevels()   # A. tonsa only, clade assigned

# Sequenced A. tonsa libraries with a clade, plus failed libraries (kept for the
# sequencing QC plots; species unknown). Drops A. hudsonica and the two mixed
# (unassigned) libraries.
tonsa_clades <- c("X", "F", "S", "IV")
seq_data <- filter(sample_table, sequenced, !(species %in% "A_hudsonica"),
                   library_failed | clade %in% tonsa_clades) %>% droplevels()

# Cluster export, for figures built directly from cluster tables. The controller
# passes its choice through CLUSTER_EXPORT; otherwise the most recent export.
export_root <- here("Raw_data", "cluster_exports")
export_name <- Sys.getenv("CLUSTER_EXPORT")
if (export_name == "") export_name <- tail(sort(dir(export_root)), 1)
cx <- function(...) file.path(export_root, export_name, ...)

temp_profiles <- read.csv(od("temp_profiles.csv")) %>%
  mutate(date = lubridate::as_datetime(date),
         doy = lubridate::yday(date))

temp_record <- read.csv(od("temp_record.csv"))
ramp_record <- read.csv(od("ramp_record.csv"))

site_temps <- join_data %>%
  dplyr::select(site, site_code, lat, season, doy, collection_temp, collection_salinity) %>%
  distinct()

temp_summaries <- join_data %>%
  dplyr::select(site, season, collection_temp) %>%
  distinct() %>%
  drop_na(collection_temp) %>%
  pivot_wider(id_cols = site, names_from = season, values_from = collection_temp) %>%
  inner_join(site_data, by = "site") %>%
  rowwise() %>%
  mutate(site_mean = mean(c_across(any_of(season_order)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cent_site_mean = site_mean - mean(site_mean)) %>%
  dplyr::select(site, region, lat, long, any_of(season_order), site_mean, cent_site_mean)

collections <- site_temps

