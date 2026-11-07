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
process_site_env         <- FALSE # 01: MUR SST 2014-2023 + station checks -> Output_data/mur_*.csv, site_climatology.csv (needs internet)
process_pheno            <- FALSE # 02: Raw_data/pheno_data + temp_data -> CTmax per individual
process_samples          <- FALSE # 03: phenotypes + cluster export -> Output_data/sample_table.csv
process_micro            <- FALSE # 04: cluster SSU profile -> Output_data/micro_calls.csv
process_gwas_inputs      <- FALSE # 05: sample table -> Output_data/ctmax_gwas_pheno.tsv (copy to cluster metadata/)
process_mito_aa          <- FALSE # 06: mitogenome consensus + sample table -> Output_data/mito_aa/*.csv
render_report            <- T # Output/Reports/report.qmd -> analyses, report figures, Output_data/ms_objects.rds
render_ms_figures        <- T # Manuscript/ms_figures.qmd -> manuscript figures (plots only; seconds)
render_manuscript        <- T # Manuscript/Sasaki_Pespeni_LCWG.qmd -> Output/Drafts/

# Which cluster export to use (a folder name in Raw_data/cluster_exports/).
# NULL = the most recent one. Pin a folder name to freeze a manuscript's inputs.
options(cluster_export = NULL)
# Quarto documents run in a fresh R session; they read the choice from here
Sys.setenv(CLUSTER_EXPORT = if (is.null(getOption("cluster_export"))) "" else getOption("cluster_export"))

# Step 1: Site environment (MUR SST 2014-2023, station checks; needs internet) -
if (process_site_env) {
  source(here("Scripts", "01_site_environment.R"), local = new.env())
}

# Step 2: Phenotypes (CTmax, body size) ------------------------------------
if (process_pheno) {
  source(here("Scripts", "02_pheno_processing.R"), local = new.env())
}

# Step 3: Central sample table (phenotypes + sequencing + clades) ----------
if (process_samples) {
  source(here("Scripts", "03_sample_table.R"), local = new.env())
}

# Step 4: Micro screen calls (parasites, epibionts) -------------------------
if (process_micro) {
  source(here("Scripts", "04_micro_calls.R"), local = new.env())
}

# Step 5: Phenotype table for the CTmax association scan (copy to the cluster)
if (process_gwas_inputs) {
  source(here("Scripts", "05_ctmax_gwas_inputs.R"), local = new.env())
}

# Step 6: Mitochondrial amino-acid differences among clades ---------------
if (process_mito_aa) {
  source(here("Scripts", "06_mito_aa.R"), local = new.env())
}

# Step 7: Render the project report ----------------------------------------
if (render_report) {
  quarto_render(input = here("Output", "Reports", "report.qmd"),
                output_format = "all")
}

# Step 8: Manuscript figures (plots only; reads Output_data/ms_objects.rds from step 7)
if (render_ms_figures) {
  quarto_render(input = here("Manuscript", "ms_figures.qmd"))
  # keep the figure preview with the other rendered documents, out of Manuscript/
  file.rename(here("Manuscript", "ms_figures.html"), here("Output", "Reports", "ms_figures.html"))
}

# Step 9: Render the manuscript -------------------------------------------
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

if (render_manuscript) {
  render_draft("Sasaki_Pespeni_LCWG.qmd", "dev_lcwg_draft_")
}

# Record the software environment ------------------------------------------
# Written on every run so the README never needs a hand-copied sessionInfo().
# If the project uses renv, renv.lock is the authoritative record instead.
writeLines(c(capture.output(sessionInfo()),
             "",
             paste("Quarto version:", quarto_version())),
           here("Output", "session_info.txt"))
