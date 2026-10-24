# Micro screen: presence calls per individual ----------------------------
# Turns the cluster's taxon-neutral SSU profile (stage_micro) into presence /
# absence calls for the organism groups of interest, and writes
# Output/Output_data/micro_calls.csv (one row per sequenced library).
# load_data.R joins these onto sample_table as infection / epibiont covariates.
#
# Input (cluster export): results/10_micro/ssu_profile/ssu_pairs_long.tsv.gz
#   one row per sample x assigned lineage; lineages are already the shared
#   ancestor of all equally good SILVA hits, with low-identity reads kept only
#   at a broad rank and host (Acartia) rRNA removed.
#
# Call rule (decided 2026-10-24): a group is present when its reads total
#   >= min_pairs read pairs AND >= min_per_M pairs per million library pairs.
#   Calls just above the threshold count as real.
# Roles: parasite / epibiont / unresolved / background (non-pathogenic) / diet.
#   parasite_any and epibiont_any summarise the first two roles.
#
# Requires: here, tidyverse

library(here)
library(tidyverse)

min_pairs <- 5
min_per_M <- 2

groups <- tribble(
  ~group,             ~pattern,        ~role,
  "paradinium",       "Paradiniidae",  "parasite",     # rhizarian parasite of the copepod body cavity
  "trematode",        "Trematoda",     "parasite",     # mostly hemiuroid (Azygiida) larvae
  "apostome",         "Apostomatia",   "epibiont",     # apostome ciliates (on / in crustaceans; moult feeders)
  "peritrich",        "Peritrichia",   "epibiont",     # Zoothamnium and relatives, attached to the cuticle
  "cnidaria_myxozoa", "Cnidaria",      "unresolved",   # myxozoan parasite or cnidarian prey: not yet resolved
  "kinetoplastid",    "Kinetoplastea", "background",   # widespread at low level; treated as non-pathogenic
  "malacostraca",     "Malacostraca",  "diet"          # crustacean prey / plankton
)

# Cluster export (same choice as 03_sample_table.R)
export_root <- here("Raw_data", "cluster_exports")
export_name <- getOption("cluster_export")
if (is.null(export_name)) export_name <- tail(sort(dir(export_root)), 1)
prof <- file.path(export_root, export_name, "results", "10_micro", "ssu_profile")
if (!file.exists(file.path(prof, "ssu_pairs_long.tsv.gz"))) stop("No ssu_profile in export ", export_name)

qc <- read_tsv(file.path(prof, "ssu_sample_qc.tsv"), show_col_types = FALSE) %>%
  transmute(sample_id, library_pairs, ssu_pairs, host_rrna_pairs)
long <- read_tsv(file.path(prof, "ssu_pairs_long.tsv.gz"), show_col_types = FALSE)

totals <- groups %>%
  pmap_dfr(function(group, pattern, role) {
    long %>%
      filter(str_detect(lineage, pattern)) %>%
      group_by(sample_id) %>%
      summarise(pairs = sum(pairs), .groups = "drop") %>%
      mutate(group = group)
  })

calls <- qc %>%
  select(sample_id, library_pairs) %>%
  crossing(groups %>% select(group)) %>%
  left_join(totals, by = c("sample_id", "group")) %>%
  mutate(pairs = replace_na(pairs, 0),
         per_M = 1e6 * pairs / library_pairs,
         call = pairs >= min_pairs & per_M >= min_per_M)

wide <- calls %>%
  select(sample_id, group, pairs, per_M, call) %>%
  pivot_wider(names_from = group, values_from = c(pairs, per_M, call), names_glue = "{group}_{.value}")

role_any <- function(r) {
  g <- groups$group[groups$role == r]
  calls %>% filter(group %in% g) %>% group_by(sample_id) %>% summarise(v = any(call), .groups = "drop")
}

micro_calls <- qc %>%
  left_join(wide, by = "sample_id") %>%
  left_join(rename(role_any("parasite"), parasite_any = v), by = "sample_id") %>%
  left_join(rename(role_any("epibiont"), epibiont_any = v), by = "sample_id") %>%
  mutate(across(ends_with("per_M"), ~ round(.x, 3)),
         micro_export = export_name,
         micro_rule = sprintf(">= %d pairs and >= %g per million", min_pairs, min_per_M))

write_csv(micro_calls, here("Output", "Output_data", "micro_calls.csv"))
write_csv(groups, here("Output", "Output_data", "micro_groups.csv"))

message("micro_calls.csv: ", nrow(micro_calls), " libraries; positives per group:")
print(colSums(select(micro_calls, ends_with("_call"))))
