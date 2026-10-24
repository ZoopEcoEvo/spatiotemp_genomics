# Central sample table ----------------------------------------------------
# One row per individual (phenotyped and/or sequenced), combining
#   - phenotypes          Output/Output_data/full_data.csv (02_pheno_processing.R)
#   - site information    Raw_data/site_data.csv
#   - sequencing / QC     the cluster export (Raw_data/cluster_exports/<date>/)
#   - clade calls         the cluster export (see "Clades" below)
#   - manual exclusions   Raw_data/sample_exclusions.csv
# and flags whether each individual goes into the phenotype and/or genomic
# analyses. Writes Output/Output_data/sample_table.csv and a short summary.
#
# Cluster export: a folder unpacked unchanged from workflow/export_tables.sh,
# so paths inside it mirror the cluster project (config/samples.tsv,
# metadata/..., results/...). Set options(cluster_export = "<folder>") in the
# controller to pin one; otherwise the most recent folder is used.
#
# Requires: here, tidyverse

library(here)
library(tidyverse)

# Which export ---------------------------------------------------------------
export_root <- here("Raw_data", "cluster_exports")
export_name <- getOption("cluster_export")
if (is.null(export_name)) export_name <- tail(sort(dir(export_root)), 1)
export_dir <- file.path(export_root, export_name)
if (!dir.exists(export_dir)) stop("Cluster export not found: ", export_dir)
message("Cluster export: ", export_name)

ex <- function(...) file.path(export_dir, ...)
read_tsv_q <- function(path) read_tsv(path, show_col_types = FALSE, na = c("", "NA"),
                                      col_types = cols(.default = col_character()))

seasons <- c("early", "peak", "late")
tonsa_clades <- c("X", "F", "S", "IV")

# Sites ----------------------------------------------------------------------
site_data <- read.csv(here("Raw_data", "site_data.csv"))

# Phenotypes -----------------------------------------------------------------
pheno <- read.csv(here("Output", "Output_data", "full_data.csv")) %>%
  left_join(select(site_data, site, site_code), by = "site") %>%
  mutate(sample_id = sprintf("%s_%s_%d_%02d", site_code, season, replicate, tube),
         phenotyped = TRUE)
stopifnot(!anyNA(pheno$site_code), !anyDuplicated(pheno$sample_id))

# Sequenced libraries (cluster) -----------------------------------------------
# samples.tsv IDs are mostly zero-padded (CT_early_1_01), but a few are not
# (ME_early_2_1), so the join uses site / event / assay / individual instead of
# the ID string. The cluster sample_id is kept as the canonical ID.
samples <- read_tsv_q(ex("config", "samples.tsv")) %>%
  transmute(cluster_id = sample_id,
            site_code = site,
            season = sampling_event,
            replicate = as.integer(assay),
            tube = as.integer(individual),
            plate, well,
            library_kept = is.na(keep) | keep != "no",
            sequenced = TRUE)

meta <- read_tsv_q(ex("metadata", "sample_metadata.tsv")) %>%
  select(cluster_id = sample_id, prior_clade, prior_clade_reads, seq_notes = notes)

qc <- read_tsv_q(ex("results", "00_qc", "sample_summary.tsv")) %>%
  transmute(cluster_id = sample_id,
            raw_reads = as.numeric(raw_reads),
            pct_mapped = as.numeric(pct_mapped),
            pct_properly_paired = as.numeric(pct_properly_paired),
            dup_rate = as.numeric(dup_rate),
            mean_depth_nuclear = as.numeric(mean_depth_nuclear),
            mean_depth_mito = as.numeric(mean_depth_mito),
            mito_best_contig = str_remove(mito_best_contig, "^mtDNA_"))

cov_file <- ex("results", "00_qc", "coverage_per_sample.tsv")
if (file.exists(cov_file)) {
  cov <- read_tsv_q(cov_file) %>%
    transmute(cluster_id = sample_id,
              breadth_ge1x = as.numeric(breadth_ge1x),
              breadth_ge5x = as.numeric(breadth_ge5x))
  qc <- left_join(qc, cov, by = "cluster_id")
}

# keep == "no" covers two different cases: failed libraries (too few reads) and
# A. hudsonica libraries (fine, but a different species)
seq <- samples %>%
  left_join(meta, by = "cluster_id") %>%
  left_join(qc, by = "cluster_id") %>%
  mutate(library_failed = !library_kept & !str_detect(coalesce(seq_notes, ""), "hudsonica"))

# Clades ---------------------------------------------------------------------
# Final calls come from metadata/clade_assignments.tsv once Stage 1 is done
# (column `clade`; "admixed"/"unassigned" are excluded). Until then the prior
# COI-based clade (sample_metadata.tsv) is used. Stage 1 only covers kept
# A. tonsa libraries, so A. hudsonica keeps its prior call either way. Calls
# from failed libraries are not trusted and are set to NA.
clade_file <- ex("metadata", "clade_assignments.tsv")
if (file.exists(clade_file)) {
  # columns: clade (X/F/S/IV, or "unassigned" for two-clade DNA mixtures) and
  # lineage (the Stage 2 analysis unit: X, S, IV, F-N, F-FH, F-MR)
  calls <- read_tsv(clade_file, comment = "#", show_col_types = FALSE,
                    col_types = cols(.default = col_character())) %>%
    select(cluster_id = sample_id, clade, any_of("lineage"))
  if (!"lineage" %in% names(calls)) calls$lineage <- calls$clade
  seq <- left_join(seq, calls, by = "cluster_id") %>%
    mutate(clade = coalesce(clade, if_else(prior_clade == "A_hudsonica", prior_clade, NA_character_)),
           lineage = coalesce(lineage, if_else(prior_clade == "A_hudsonica", prior_clade, NA_character_)),
           clade_source = if_else(prior_clade == "A_hudsonica" & !library_kept, "prior_coi", "stage1"))
} else {
  warning("No metadata/clade_assignments.tsv in the export; using prior_clade (COI skim).")
  seq <- mutate(seq, clade = prior_clade, lineage = prior_clade, clade_source = "prior_coi")
}
seq <- mutate(seq,
              clade = if_else(library_failed, NA_character_, clade),
              lineage = if_else(library_failed, NA_character_, lineage))

# Combine --------------------------------------------------------------------
keys <- c("site_code", "season", "replicate", "tube")
sample_table <- full_join(pheno, seq, by = keys) %>%
  mutate(sample_id  = coalesce(cluster_id, sample_id),
         phenotyped = coalesce(phenotyped, FALSE),
         sequenced  = coalesce(sequenced, FALSE),
         library_kept = coalesce(library_kept, FALSE),
         library_failed = coalesce(library_failed, FALSE)) %>%
  select(-site) %>%
  left_join(site_data, by = "site_code") %>%
  mutate(species = case_when(clade %in% tonsa_clades ~ "A_tonsa",
                             clade == "A_hudsonica" ~ "A_hudsonica",
                             TRUE ~ NA_character_))

# Exclusions -----------------------------------------------------------------
excl <- read.csv(here("Raw_data", "sample_exclusions.csv"))
unknown <- setdiff(excl$sample_id, sample_table$sample_id)
if (length(unknown)) stop("sample_exclusions.csv has unknown IDs: ", paste(unknown, collapse = ", "))
excl_pheno <- excl$sample_id[excl$exclude_from %in% c("phenotype", "all")]
excl_gen   <- excl$sample_id[excl$exclude_from %in% c("genomic", "all")]

sample_table <- sample_table %>%
  mutate(
    manual_exclusion = excl$reason[match(sample_id, excl$sample_id)],
    # Phenotype analyses: measured, confidently A. tonsa with a clade, not manually excluded
    include_pheno = phenotyped & !is.na(ctmax) & clade %in% tonsa_clades &
      !(sample_id %in% excl_pheno),
    # Genomic analyses: library passed QC, A. tonsa, not manually excluded
    include_genomic = sequenced & library_kept & clade %in% tonsa_clades &
      !(sample_id %in% excl_gen),
    pheno_status = case_when(
      !phenotyped ~ "not phenotyped",
      sample_id %in% excl_pheno ~ "manual exclusion",
      !sequenced ~ "not sequenced (no clade)",
      species == "A_hudsonica" ~ "A. hudsonica",
      library_failed ~ "failed library (no clade)",
      clade == "unassigned" ~ "mixed library (unassigned)",
      is.na(clade) | !(clade %in% tonsa_clades) ~ "no clade call",
      TRUE ~ "included"),
    site_code = factor(site_code, levels = site_data$site_code),
    season = factor(season, levels = seasons)) %>%
  arrange(site_code, season, replicate, tube) %>%
  select(sample_id, site_code, site, region, lat, long, season, replicate, tube,
         phenotyped, sequenced, library_kept, library_failed, species, clade, lineage, clade_source, prior_clade,
         mito_best_contig, include_pheno, include_genomic, pheno_status, manual_exclusion,
         collection_date, collection_temp, collection_salinity, exp_date, days_in_lab,
         run, rank, size, time, ramp_rate, ctmax, warming_tol,
         plate, well, raw_reads, pct_mapped, pct_properly_paired, dup_rate,
         mean_depth_nuclear, mean_depth_mito, any_of(c("breadth_ge1x", "breadth_ge5x")),
         prior_clade_reads, seq_notes)

# Write --------------------------------------------------------------------
out_dir <- here("Output", "Output_data")
write.csv(sample_table, file.path(out_dir, "sample_table.csv"), row.names = FALSE)

summary_lines <- c(
  paste("Cluster export:", export_name),
  paste("Clade source:", unique(na.omit(sample_table$clade_source))),
  paste("Individuals:", nrow(sample_table)),
  paste("Phenotyped:", sum(sample_table$phenotyped)),
  paste("Sequenced:", sum(sample_table$sequenced), "| libraries kept:", sum(sample_table$library_kept)),
  paste("Phenotyped and sequenced:", sum(sample_table$phenotyped & sample_table$sequenced)),
  paste("Included in phenotype analyses:", sum(sample_table$include_pheno)),
  paste("Included in genomic analyses:", sum(sample_table$include_genomic)),
  "", "Phenotyped individuals by status:",
  capture.output(print(as.data.frame(count(filter(sample_table, phenotyped), pheno_status)), row.names = FALSE)),
  "", "Prior (COI) clade vs. best mito contig (kept libraries):",
  capture.output(with(filter(sample_table, library_kept), table(prior_clade, mito_best_contig, useNA = "ifany"))))
writeLines(summary_lines, file.path(out_dir, "sample_table_summary.txt"))
message(paste(summary_lines, collapse = "\n"))
