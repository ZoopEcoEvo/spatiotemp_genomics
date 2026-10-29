# Phenotype table for the CTmax association scan (cluster, target `gwas`) ---------------
# One row per sequenced, phenotyped individual with a lineage call:
#   sample_id, lineage, clade, site_code, season, collection, ctmax, size
# collection = site x sampling event: everyone in a collection experienced the same
# conditions before the assay, so it is used as a covariate on the cluster to absorb
# acclimation (and population differences) - the scan compares individuals within
# collections.
# Copy the output to the cluster as metadata/ctmax_gwas_pheno.tsv.
#
# Requires: here, tidyverse; reads Output_data/sample_table.csv (03_sample_table.R)

library(here)
library(tidyverse)

pheno <- read_csv(here("Output", "Output_data", "sample_table.csv"), show_col_types = FALSE) %>%
  filter(include_pheno, sequenced, !library_failed, !is.na(lineage), lineage != "A_hudsonica",
         !is.na(ctmax), !is.na(size)) %>%
  transmute(sample_id, lineage, clade, site_code, season,
            collection = paste(site_code, season, sep = "_"), ctmax, size) %>%
  arrange(lineage, collection, sample_id)

write_tsv(pheno, here("Output", "Output_data", "ctmax_gwas_pheno.tsv"))

message("CTmax association inputs: ", nrow(pheno), " individuals")
pheno %>%
  group_by(lineage) %>%
  summarise(individuals = n(), collections = n_distinct(collection), sites = n_distinct(site_code),
            .groups = "drop") %>%
  print(n = Inf)
