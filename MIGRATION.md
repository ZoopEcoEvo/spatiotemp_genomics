# Migration notes (delete once settled)

Old project (`spatiotemp_genomics`, pre-template) → new template layout, 2026-10-22.

## Where everything went

| Old | New | Change |
|---|---|---|
| `Raw_data/pheno_data/`, `temp_data/`, `site_temps/`, `outside_sources/` | same | unchanged |
| `Raw_data/site_data.csv` | same | added `site_code` column (KL, MR, ...); removed byte-order mark |
| `Raw_data/molecular/` (empty) | `Raw_data/cluster_exports/<date>/` | cluster tables, unpacked unchanged from `export_tables.sh` |
| exclusions hard-coded in `02_controller.R` | `Raw_data/sample_exclusions.csv` | 5 juveniles / low CTmax, converted to cluster IDs (see below) |
| `Scripts/00_site_temps.R` | `Scripts/01_site_temps.R` | `here()` paths; OISST boxes in one table; unused alternatives listed as comments |
| `Scripts/01_data_processing.R` (phenotype part) | `Scripts/02_pheno_processing.R` | always reprocesses every run (no `prev_runs.txt` append logic); per-run files in `Output_data/ctmax_runs/`. Output identical to the old `full_data.csv` (427 individuals, all values match) |
| `Scripts/01_data_processing.R` (molecular / inventory part) | `Scripts/03_sample_table.R` | rebuilt on the cluster export: `sample_table.csv` replaces `joined_data.csv`, `sample_inventory.csv`, `sample_coverage.csv`, `COI_clades_summary.csv` |
| `Scripts/02_controller.R` | `Scripts/00_controller.R` (template) + `Scripts/load_data.R` | controller only orchestrates; data loading moved to `load_data.R`, which the report and manuscripts source themselves |
| theme functions inside `report.Rmd` | `Scripts/themes.R` (template) | |
| colours inside `report.Rmd` | `Scripts/palettes.R` | palette option 2 (same file as the cluster's `R/palettes.R`) |
| `Output/Reports/report.Rmd` | same, ported | new setup chunk; figures to `Output/Figures/`; sequencing + clade section rebuilt on cluster QC tables; old PCA section removed (superseded by Stage 1); writes `range_changes.csv` |
| — | `Output/Reports/legacy/report_pre_migration.Rmd` | untouched copy of the old report, not run |
| `Output/Output_data/*` | regenerated | `temp_profiles.csv` and `range_changes.csv` carried over; the rest rebuilt by the scripts |
| `Output/Figures/markdown/` | `Output/Figures/` | regenerate with `make_report <- TRUE` |
| `Output/Drafts/` (old drafts), `Output/Reports/report.{html,md,pdf}` | not copied | keep in the old folder / git history |
| `Manuscript/Sasaki_Pespeni_Phenotype.Rmd` | same | sources `load_data.R`; figure paths `here("Output", "Figures", ...)`; exclusion counts now computed |
| — | `Manuscript/Sasaki_Pespeni_LCWG.Rmd` | template manuscript, renamed, header filled in |
| `Manuscript/references.bib`, templates | same | old bib kept (templates identical to the template's) |
| old `README.md` | new `README.md` | filled in |

Old individual IDs → cluster IDs in `sample_exclusions.csv`:
St._Thomas_de_Kent_Wharf_late_1_3 → TK_late_1_03; Ft._Hamer_late_2_3 → FH_late_2_03;
Tyler_Cove_peak_2_2 → MD_peak_2_02; Manatee_River_peak_2_7 → MR_peak_2_07;
Manatee_River_peak_2_6 → MR_peak_2_06. The two phenotyped *A. hudsonica*
(Esker_Point_early_2_3 = CT_early_2_03, Sawyer_Park_peak_1_4 = ME_peak_1_04) and the
three low-confidence clade calls (FH_peak_1_10, ME_early_2_07, RW_early_2_09, now failed
libraries) are excluded automatically, so they are not in the file.

## Things that changed in results (check before the next draft)

1. **Phenotype analysis set is 397 individuals, not 410.** Exclusions are now 30
   (5 juveniles, 23 failed libraries with no trusted clade, 2 *A. hudsonica*) instead of
   17 (5 / 10 / 2). The old analysis used COI clade calls for some individuals whose
   WGS libraries later failed (keep = no). 7 of the 23 have ≥ 20 COI reads in the old
   skim and a matching best mito contig, so their clades could be recovered if wanted
   (e.g. by running the Stage 1 mito COI counts on failed libraries too).
2. **Table 2 (`range_changes.csv`) shifts slightly** with the new set: clade-only
   scenario 24.6 → 25.5 % decrease; clade + acclimation 32.1 → 31.1 %. The numbers
   typed in the Results text ("~24%", "~32%", 9.8°C, 8.8°C) need updating.
3. The clade-proportion figure caption still describes the old "85% of reads" rule.
4. Clades come from `prior_clade` until `metadata/clade_assignments.tsv` is exported
   after Stage 1; then `03_sample_table.R` switches automatically (`clade_source`).

## Still to do

- [ ] Decide: migrate inside the existing GitHub repo (keeps history) or start a new repo from the template.
- [ ] `renv::init()` once packages are installed; commit `renv.lock`.
- [ ] Fill in `Background/analysis_plan.qmd` (the cluster roadmap `docs/aton_lcwg_roadmap.md` has most of the content).
- [ ] Knit the report and both manuscripts once on your machine (PDF needs LaTeX).
- [ ] After Stage 1: export, unpack to `Raw_data/cluster_exports/<date>/`, rebuild the sample table, add PCA / admixture to the report.

## Quarto update (2026-10-24)

Report and manuscripts moved from R Markdown to Quarto, following the updated lab template:
`report.Rmd` → `report.qmd`; `Sasaki_Pespeni_Phenotype.Rmd` / `Sasaki_Pespeni_LCWG.Rmd` → `.qmd`
(chunk options as `#|` lines; figure and table captions as `fig-cap` / `tbl-cap` with `fig-` / `tbl-` labels;
references printed before the supplement). New template files in `Manuscript/`: `word_template.docx`
(replaces `doc_template.docx`), `lab_style.csl`, `word_frontmatter.lua`, updated `preprint_template.tex`.
`00_controller.R` renders with `quarto::quarto_render()`; `.gitignore` and `themes.R` from the template.
