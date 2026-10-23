# Project controller ------------------------------------------------------
# Run the whole workflow from here. Each step runs in its own clean
# environment, so every script and document loads its own packages and
# reads its own input files. That keeps each piece reproducible on its own.
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
library(rmarkdown)

# Choose which steps to run -----------------------------------------------
process_site_temps     <- FALSE # downloads buoy / OISST data -> Output_data/temp_profiles.csv (needs internet)
process_pheno          <- T # Raw_data/pheno_data + temp_data -> CTmax per individual
process_samples        <- T # phenotypes + cluster export -> Output_data/sample_table.csv
make_report            <- T # Output/Reports/report.Rmd -> figures + report
knit_pheno_manuscript  <- T # Manuscript/Sasaki_Pespeni_Phenotype.Rmd -> Output/Drafts/
knit_genome_manuscript <- T # Manuscript/Sasaki_Pespeni_LCWG.Rmd -> Output/Drafts/

# Which cluster export to use (a folder name in Raw_data/cluster_exports/).
# NULL = the most recent one. Pin a folder name to freeze a manuscript's inputs.
options(cluster_export = NULL)

# Step 1: Site temperature records -----------------------------------------
if (process_site_temps) {
  source(here("Scripts", "01_site_temps.R"), local = new.env())
}

# Step 2: Phenotypes (CTmax, body size) ------------------------------------
if (process_pheno) {
  source(here("Scripts", "02_pheno_processing.R"), local = new.env())
}

# Step 3: Central sample table (phenotypes + sequencing + clades) ----------
if (process_samples) {
  source(here("Scripts", "03_sample_table.R"), local = new.env())
}

# Step 4: Render the project report ----------------------------------------
if (make_report) {
  render(input = here("Output", "Reports", "report.Rmd"),
         output_format = "all",
         envir = new.env())
}

# Step 5: Render the manuscripts -------------------------------------------
# Drafts are named by date. Files starting with "dev_" are ignored by git;
# remove the prefix if you want a draft committed to the repo.
if (knit_pheno_manuscript) {
  render(input = here("Manuscript", "Sasaki_Pespeni_Phenotype.Rmd"),
         output_file = paste0("dev_pheno_draft_", Sys.Date()),
         output_dir = here("Output", "Drafts"),
         output_format = "all",
         envir = new.env(),
         clean = TRUE)
}

if (knit_genome_manuscript) {
  render(input = here("Manuscript", "Sasaki_Pespeni_LCWG.Rmd"),
         output_file = paste0("dev_lcwg_draft_", Sys.Date()),
         output_dir = here("Output", "Drafts"),
         output_format = "all",
         envir = new.env(),
         clean = TRUE)
}

# Record the software environment ------------------------------------------
# Written on every run so the README never needs a hand-copied sessionInfo().
# If the project uses renv, renv.lock is the authoritative record instead.
writeLines(capture.output(sessionInfo()),
           here("Output", "session_info.txt"))
