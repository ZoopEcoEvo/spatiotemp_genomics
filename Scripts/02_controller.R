#### To Do 
# Update the model structure - remove the site stuff from random effects and replace with replicate


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
make_report = T #Runs project summary
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

all_data = read.csv(file = "Output/Output_data/full_data.csv") %>% 
  bind_rows(read.csv(file = "Raw_data/outside_sources/key_largo_winter.csv") %>% 
              filter(bopyrid == "no") %>% 
              dplyr::select(-bopyrid))

join_data = read.csv(file = "Output/Output_data/joined_data.csv") %>% 
  mutate(lat = if_else(site_code == "ME", 43.90698, lat),
    site = fct_reorder(site, lat),
         season = fct_relevel(season, "early", "peak", "late"),
         site_code = fct_relevel(site_code, "FH", "MR", "MD", "GW", "CT", "ME", "TK", "RW"))

inventory = read.csv(file = "Output/Output_data/sample_inventory.csv")

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

######## Sequencing data

tonsa_matrix = as.matrix(read.table("Raw_data/molecular/pcangsd/tonsa_exclusions.cov"))

clade_summary = read.csv(file = "Output/Output_data/COI_clades_summary.csv") %>% 
  mutate(population = fct_relevel(population, "FH", "MR", "MD", "GW", "CT", "ME", "TK", "RW"))

bam_list = read.csv(file = "Raw_data/molecular/bam_list.txt", header = F) %>% 
  mutate(sample = str_remove_all(V1, pattern = "bam_files/"),
         sample = str_remove_all(sample, pattern = "_dd_clip.bam"), 
         site_code = str_split_fixed(sample, pattern = "_", n = 2)[,1],
         season = str_split_fixed(sample, pattern = "_", n = 3)[,2],
         replicate = as.numeric(str_split_fixed(sample, pattern = "_", n = 4)[,3]),
         tube = as.numeric(str_split_fixed(sample, pattern = "_", n = 4)[,4])) %>% 
  left_join(select(join_data, site_code, season, replicate, tube, clade), by = c("site_code", "season", "replicate", "tube"))

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
