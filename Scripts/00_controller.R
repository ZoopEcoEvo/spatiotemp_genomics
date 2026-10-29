# Project controller ------------------------------------------------------
# Run the whole workflow from here. Each step runs separately, so every script
# and document must load its own packages and read its own input files. That
# keeps each piece reproducible on its own. (Quarto documents are always
# rendered in a fresh R session, so they can't see objects created here.)
#
# Paths are built with here::here(), which finds the project root from the
# .Rproj file (or the .git folder), so this works regardless of the current
# working directory.
#
# Heavy genomic processing happens on the Unity cluster (Snakemake project
# aton_lcwg). Its small result tables are exported with
# workflow/export_tables.sh and unpacked, unchanged, into
# Raw_data/cluster_exports/<date>/. This project only reads them.

library(here)
library(quarto)

# Choose which steps to run -----------------------------------------------
process_site_temps       <- FALSE # downloads buoy / OISST data -> Output_data/temp_profiles.csv (needs internet)
process_site_env         <- FALSE # MUR SST 2014-2023 + monitoring-station checks -> Output_data/mur_*.csv (needs internet)
process_pheno            <- FALSE # Raw_data/pheno_data + temp_data -> CTmax per individual
process_samples          <- FALSE # phenotypes + cluster export -> Output_data/sample_table.csv
process_micro            <- FALSE # cluster SSU profile -> Output_data/micro_calls.csv
render_report            <- TRUE # Output/Reports/report.qmd -> figures + report
render_pheno_manuscript  <- FALSE # Manuscript/Sasaki_Pespeni_Phenotype.qmd -> Output/Drafts/
render_genome_manuscript <- FALSE # Manuscript/Sasaki_Pespeni_LCWG.qmd -> Output/Drafts/

# Which cluster export to use (a folder name in Raw_data/cluster_exports/).
# NULL = the most recent one. Pin a folder name to freeze a manuscript's inputs.
options(cluster_export = NULL)
# Quarto documents run in a fresh R session; they read the choice from here
Sys.setenv(CLUSTER_EXPORT = if (is.null(getOption("cluster_export"))) "" else getOption("cluster_export"))

# Step 1: Site temperature records -----------------------------------------
if (process_site_temps) {
  source(here("Scripts", "01_site_temps.R"), local = new.env())
}

# Step 1b: Site environment for Stage 3 (MUR SST, station checks) ----------
if (process_site_env) {
  source(here("Scripts", "05_site_environment.R"), local = new.env())
}

# Step 2: Phenotypes (CTmax, body size) ------------------------------------
if (process_pheno) {
  source(here("Scripts", "02_pheno_processing.R"), local = new.env())
}

# Step 3: Central sample table (phenotypes + sequencing + clades) ----------
if (process_samples) {
  source(here("Scripts", "03_sample_table.R"), local = new.env())
}

# Step 3b: Micro screen calls (parasites, epibionts) ------------------------
if (process_micro) {
  source(here("Scripts", "04_micro_calls.R"), local = new.env())
}

# Step 4: Render the project report ----------------------------------------
if (render_report) {
  quarto_render(input = here("Output", "Reports", "report.qmd"),
                output_format = "all")
}

# Step 5: Render the manuscripts -------------------------------------------
# Drafts are named by date. Files starting with "dev_" are ignored by git;
# remove the prefix if you want a draft committed to the repo.
render_draft <- function(qmd, prefix) {
  draft_name <- paste0(prefix, Sys.Date())
  quarto_render(input = here("Manuscript", qmd),
                output_format = "all", # PDF and Word
                metadata = list("output-file" = draft_name))
  # Quarto writes the drafts next to the .qmd; move them to Output/Drafts/
  drafts <- list.files(here("Manuscript"),
                       pattern = paste0("^", draft_name, "\\.(pdf|docx)$"),
                       full.names = TRUE)
  file.copy(drafts, here("Output", "Drafts"), overwrite = TRUE)
  file.remove(drafts)
}

if (render_pheno_manuscript) {
  render_draft("Sasaki_Pespeni_Phenotype.qmd", "dev_pheno_draft_")
}

if (render_genome_manuscript) {
  render_draft("Sasaki_Pespeni_LCWG.qmd", "dev_lcwg_draft_")
}

# Record the software environment ------------------------------------------
# Written on every run so the README never needs a hand-copied sessionInfo().
# If the project uses renv, renv.lock is the authoritative record instead.
writeLines(c(capture.output(sessionInfo()),
             "",
             paste("Quarto version:", quarto_version())),
           here("Output", "session_info.txt"))
