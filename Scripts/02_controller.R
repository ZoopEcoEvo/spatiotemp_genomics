# Load in required packages
library(rmarkdown)
library(tidyverse)
library(ggpubr)

#Determine which scripts should be run
process_all_data = F #Runs data analysis 
make_report = T #Runs project summary
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

source(file = "Scripts/01_data_processing.R")

##################################
### Read in the PROCESSED data ###
##################################
site_data = read.csv(file = "Raw_data/site_data.csv") %>% 
  mutate(site = fct_reorder(site, lat))

kl_winter = read.csv(file = "Raw_data/outside_sources/key_largo_winter.csv") %>% 
  filter(bopyrid == "no") %>% 
  mutate(warming_tol = ctmax - collection_temp,
         collection_date = as.character(as.Date(collection_date, "%m/%d/%y")),
         exp_date = as.character(as.Date(exp_date, "%m/%d/%y")))

all_data = read.csv(file = "Output/Output_data/full_data.csv") %>%  
  bind_rows(kl_winter) %>% 
  mutate(doy = lubridate::yday(collection_date),
         ind_id = str_replace_all(paste(site, season, replicate, tube, sep = "_"), pattern = " ", replacement = "_")) %>% 
  inner_join(site_data, by = c("site")) %>% 
  mutate(site = fct_reorder(site, lat),
         warming_tol = ctmax - collection_temp)

excluded_inds = c(
  "Esker_Point_early_2_3",
  "Manatee_River_peak_2_6",
  "Manatee_River_peak_2_7",
  "Tyler_Cove_peak_2_2"
)

 full_data = all_data %>%  
  filter(!(ind_id %in% excluded_inds))
  
 excluded = all_data %>% 
  filter(ind_id %in% excluded_inds)

temp_record = read.csv(file = "Output/Output_data/temp_record.csv")

ramp_record = read.csv(file = "Output/Output_data/ramp_record.csv")

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
