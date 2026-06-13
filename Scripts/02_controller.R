# Load in required packages
library(rmarkdown)
library(merTools)
library(ggpubr)
library(lme4)
library(tidyverse)

#Determine which scripts should be run
process_all_data = F #Runs data analysis 
process_site_temps = F #Compiles continuous temperature data for the sites
process_clades = F #Processes clade data
make_report = F #Runs project summary
knit_pheno_manuscript = F #Compiles manuscript draft
knit_genome_manuscript = F #Compiles manuscript draft

site_data = read.csv(file = "Raw_data/site_data.csv") %>% 
  mutate(site = fct_reorder(site, lat))

source(file = "Scripts/01_data_processing.R")

##################################
### Read in the PROCESSED data ###
##################################

temp_profiles = read.csv(file = "Output/Output_data/temp_profiles.csv") %>% 
  mutate(date = lubridate::as_datetime(date),
         doy = lubridate::yday(date))

temp_record = read.csv(file = "Output/Output_data/temp_record.csv")

ramp_record = read.csv(file = "Output/Output_data/ramp_record.csv")

all_data = read.csv(file = "Output/Output_data/full_data.csv") 

# Which individuals need to be removed

### Find low-confidence matches 
# clade_summary %>%  
#   group_by(sample) %>% 
#   filter(max(prop) < 0.85) %>% 
#   filter(prop == max(prop)) 

### Low Confidence Matches
# FH_peak_1_10 (50.6% match top clade)
# ME_early_2_07 (54.2% match top clade)
# RW_early_2_09 (68.8% match top clade)

### Juveniles or abnormally low CTmax 
# St._Thomas_de_Kent_Wharf_late_1_3
# Ft._Hamer_late_2_3
# Tyler_Cove_peak_2_2
# Manatee_River_peak_2_7
# Manatee_River_peak_2_6

### Find hudsonica individuals
# clade_summary %>%
#   group_by(sample) %>%
#   filter(max(prop) > 0.85) %>%
#   filter(prop == max(prop)) %>% 
#   filter(Clade == "A_hudsonica")

### Phenotyped hudsonica individuals (only two in the `join_data` object)
# CT_early_2_03
# ME_peak_1_04

join_data = read.csv(file = "Output/Output_data/joined_data.csv") %>% 
  mutate(site = fct_reorder(site, lat),
         season = fct_relevel(season, "early", "peak", "late"),
         site_code = fct_relevel(site_code, "FH", "MR", "MD", "GW", "CT", "ME", "TK", "RW")) %>% 
  mutate(clade = case_when( ### Manually removing the clade ID for the three individuals with low confidence estimates
    site_code == "FH" & season == "peak" & replicate == 1 & tube == 10 ~ NA,
    site_code == "ME" & season == "early" & replicate == 2 & tube == 7 ~ NA,
    site_code == "RW" & season == "early" & replicate == 2 & tube == 9 ~ NA,
    .default = clade)) %>% 
  drop_na(clade) %>% 
  filter(clade != "A_hudsonica") %>% 
  filter(!(ind_id %in% c("St._Thomas_de_Kent_Wharf_late_1_3",
                       "Ft._Hamer_late_2_3", 
                       "Tyler_Cove_peak_2_2", 
                       "Manatee_River_peak_2_7", 
                       "Manatee_River_peak_2_6"))) 

temp_summaries = join_data %>% 
  dplyr::select(site, season, collection_temp) %>% 
  distinct() %>% 
  drop_na(collection_temp) %>% 
  pivot_wider(id_cols = c("site"),
              names_from = season, 
              values_from = collection_temp) %>% 
  inner_join(site_data) %>% 
  group_by(site) %>% 
  mutate(site_mean = mean(c(early, peak, late), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(cent_site_mean = scale(site_mean, center = T, scale = F)[,1]) %>% 
  dplyr::select(site, region, lat, long, early, peak, late, site_mean, cent_site_mean)

site_temps = join_data %>% 
  dplyr::select(site, lat, season, doy, collection_temp, collection_salinity) %>%  
  distinct() 

######## Sequencing data

#tonsa_matrix = as.matrix(read.table("Raw_data/molecular/pcangsd/tonsa_exclusions.cov"))
iv_matrix = as.matrix(read.table("Raw_data/molecular/pcangsd/clade_IV.cov"))
s_matrix = as.matrix(read.table("Raw_data/molecular/pcangsd/clade_S.cov"))
f_matrix = as.matrix(read.table("Raw_data/molecular/pcangsd/clade_F.cov"))
x_matrix = as.matrix(read.table("Raw_data/molecular/pcangsd/clade_X.cov"))

clade_summary = read.csv(file = "Output/Output_data/COI_clades_summary.csv") %>% 
  mutate(population = fct_relevel(population, "FH", "MR", "MD", "GW", "CT", "ME", "TK", "RW"),
         season = fct_relevel(season, "early", "peak", "late")) %>% 
  group_by(sample) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()

bam_list = read.csv(file = "Raw_data/molecular/bam_list.txt", header = F) %>% 
  mutate(sample = str_remove_all(V1, pattern = "bam_files/"),
         sample = str_remove_all(sample, pattern = "_dd_clip.bam"), 
         site_code = str_split_fixed(sample, pattern = "_", n = 2)[,1],
         season = str_split_fixed(sample, pattern = "_", n = 3)[,2],
         replicate = as.numeric(str_split_fixed(sample, pattern = "_", n = 4)[,3]),
         tube = as.numeric(str_split_fixed(sample, pattern = "_", n = 4)[,4])) %>% 
  left_join(select(join_data, site_code, season, replicate, tube, clade), by = c("site_code", "season", "replicate", "tube")) %>% 
  mutate(row = row_number()) %>% 
  group_by(sample) %>% 
  mutate(beagle_ind = paste0("Ind", row - 1, collapse = "")) %>% 
  select(-row)

# Takes the bam list and subsets to clade, site, season, and site x season lists
for(c in unique(bam_list$clade)){
  # Subsets the list to just the clade (allows next steps to skip anything with no representatives)
  subset = bam_list %>%
    ungroup() %>%
    filter(clade == c)

  clade_list = subset %>%
    select(V1)

  write.table(clade_list, file = paste("Raw_data/molecular/lists/clade_lists/", c, "_bam.txt", sep = ""),
              quote = F,
              col.names = F,
              row.names = F)

  ### Create lists for each season
  for(s in unique(subset$season)){
    season_list = subset %>%
      ungroup() %>%
      filter(season == s) %>%
      select(V1)

    write.table(season_list, file = paste("Raw_data/molecular/lists/season_lists/", c, "_",  s, "_bam.txt", sep = ""),
                quote = F,
                col.names = F,
                row.names = F)
  }

  ### Create lists for each site AND site x season combos
  for(l in unique(subset$site_code)){
    site_subset = subset %>%
      ungroup() %>%
      filter(site_code == l)

    site_list = site_subset %>%
      select(V1)

    write.table(site_list, file = paste("Raw_data/molecular/lists/site_lists/", c, "_",  l, "_bam.txt", sep = ""),
                quote = F,
                col.names = F,
                row.names = F)

    for(ss in unique(site_subset$season)){
      site_season_list = site_subset %>%
        ungroup() %>%
        filter(season == ss) %>%
        select(V1)

      write.table(site_season_list, file = paste("Raw_data/molecular/lists/subset_lists/", c, "_",  l, "_", ss, "_bam.txt", sep = ""),
                  quote = F,
                  col.names = F,
                  row.names = F)
    }
  }
}

for(c in unique(bam_list$clade)){
  clade_list = bam_list %>%
    ungroup() %>%
    filter(clade == c) %>%
    select(beagle_ind)

  list_name = paste("Raw_data/molecular/beagle_inds/clade_", c, "_inds.txt", sep = "")

  write.table(clade_list, file = list_name, row.names = F, col.names = F, quote = F)
}

range_changes = read.csv(file = "Output/Output_data/range_changes.csv") %>% 
  select("Scenario" = Comparison, "WT Range" = WT.Range, "% Decrease" = X..Decrease)

fst_summary = read.table(file = "Raw_data/molecular/fst_summary.tsv", sep = "\t", header = T) %>%
  janitor::clean_names() %>% drop_na()

collections = site_temps %>%
  mutate(site_code = case_when(
    site == "Manatee River" ~ "MR",
    site == "Ft. Hamer" ~ "FH",
    site == "Tyler Cove" ~ "MD",
    site == "Ganey's Wharf" ~ "GW",
    site == "Esker Point" ~ "CT",
    site == "Sawyer Park" ~ "ME",
    site == "St. Thomas de Kent Wharf" ~ "TK",
    site == "Ritchie Wharf" ~ "RW"
  ))

fst_analysis = collections %>%
  right_join(fst_summary, join_by(site_code == site1, season == season1)) %>%
  select(clade, site1 = site_code, season1 = season, site2, season2, comparison, fst_weight, fst_unweight,
         lat1 = lat, doy1 = doy, temp1 = collection_temp, sal1 = collection_salinity) %>%
  left_join(collections, join_by(site2 == site_code, season2 == season)) %>%
  select(clade, site1, season1, site2, season2, comparison, fst_weight, fst_unweight,
         lat1, doy1, temp1, sal1,
         lat2 = lat, doy2 = doy, temp2 = collection_temp, sal2 = collection_salinity) %>%
  mutate(temp_diff = abs(temp2-temp1),
         sal_diff = abs(sal2-sal1),
         lat_diff = abs(lat2-lat1),
         doy_diff = abs(doy2-doy1), 
         season1 = fct_relevel(season1, "early", "peak", "late"),
         season2 = fct_relevel(season2, "early", "peak", "late"),
         site1 = fct_reorder(site1, .x = lat1),
         site2 = fct_reorder(site2, .x = lat2))

ggplot(fst_analysis, aes(x = comparison, y = fst_weight)) +
  facet_wrap(clade~.) + 
  geom_boxplot() + 
  geom_point() + 
  theme_bw() + 
  theme(panel.grid = element_blank())

fst_analysis %>%
  filter(comparison == "seasonal") %>%
ggplot(aes(x = season1, y = season2, fill = fst_weight)) +
  facet_grid(site1~clade) +
  geom_tile() +
  scale_fill_viridis_c()+ 
  theme_bw() + 
  theme(panel.grid = element_blank())

fst_analysis %>%
  filter(comparison == "spatial") %>%
  ggplot(aes(x = site1, y = site2, fill = fst_weight)) +
  facet_grid(season1~clade) +
  geom_tile() +
  scale_fill_viridis_c()+ 
  theme_bw() + 
  theme(panel.grid = element_blank())

fst_analysis %>%
  filter(comparison == "spatial") %>% 
  ggplot(aes(x = lat_diff, y = fst_weight, colour = season1)) +
  facet_wrap(.~clade, scales = "free") +
  geom_hline(yintercept = 0) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)+ 
  theme_bw() 

fst_analysis %>%
  ggplot(aes(x = temp_diff, y = fst_weight, colour = season1)) +
  facet_grid(comparison~clade) +
  geom_hline(yintercept = 0) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)+ 
  theme_bw() 

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
  
}


##################################
### Read in the PROCESSED data ###
##################################

if(knit_pheno_manuscript == T){
  render(input = "Manuscript/Sasaki_Pespeni_Phenotype.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
         #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}


if(knit_genome_manuscript == T){
  render(input = "Manuscript/Sasaki_Pespeni_LCWG.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
         #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
