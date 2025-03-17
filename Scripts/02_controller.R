# Load in required packages
library(rmarkdown)
library(merTools)
library(ggpubr)
library(lme4)
library(tidyverse)

#Determine which scripts should be run
process_all_data = F #Runs data analysis 
process_site_temps = F #Compiles continuous temperature data for the sites
process_reads = F
align_reads = F
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

### Processes temperature data
if(process_site_temps == T){
  source(file = "Scripts/00_site_temps.R")
}

temp_profiles = read.csv(file = "Output/Output_data/temp_profiles.csv") %>% 
  mutate(date = lubridate::as_datetime(date),
         doy = lubridate::yday(date))

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
         season = fct_relevel(season, "early", "peak", "late"),
         warming_tol = ctmax - collection_temp) %>%  
  arrange(site) 

excluded_inds = c(
  "Esker_Point_early_2_3",
  "Manatee_River_peak_2_6",
  "Manatee_River_peak_2_7",
  "Tyler_Cove_peak_2_2",
  "Sawyer_Park_peak_1_4",
  "St._Thomas_de_Kent_Wharf_late_1_3",
  "Ft._Hamer_late_2_3"
)

full_data = all_data %>%  
  filter(!(ind_id %in% excluded_inds))

excluded = all_data %>% 
  filter(ind_id %in% excluded_inds)

temp_record = read.csv(file = "Output/Output_data/temp_record.csv")

ramp_record = read.csv(file = "Output/Output_data/ramp_record.csv")
 
# full_data %>%  
#   group_by(site, season, collection_date) %>% 
#   summarise(mean_ctmax = mean(ctmax)) %>% 
#   mutate(mean_ctmax = round(mean_ctmax, digits = 1)) %>% 
#   pivot_wider(names_from = season, 
#               values_from = mean_ctmax) %>% 
#   write.csv("Output/Output_data/lim_summary.csv")

full_data %>%
  group_by(site, season, collection_date, collection_temp, collection_salinity) %>%
  summarise(mean_ctmax = mean(ctmax)) %>%
  write.csv("Output/Output_data/collection_summary.csv")

temp_summaries = full_data %>% 
  select(site, season, collection_temp) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c("site"),
              names_from = season, 
              values_from = collection_temp) %>% 
  inner_join(site_data) %>% 
  group_by(site) %>% 
  mutate(season_mean = mean(c(early, peak, late), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(cent_season = scale(season_mean, center = T, scale = F)[,1]) %>% 
  select(site, region, lat, long, early, peak, late, season_mean, cent_season)

#########

if(process_reads == T){
  
  sample_map = readxl::read_excel(path = "Molecular/twist_map.xlsx") %>% 
    select(Sample_Name, Sample_Id, Sample_Barcode, "Well" = well) # %>% 
    # write.csv("Molecular/sample_map.csv", row.names = F)
  
  sequence_comp = sample_map %>% 
    select(Sample_Name) %>% 
    filter(!Sample_Name %in% c("ME_late_1_02", "ME_late_1_03", "ME_late_1_04", "ME_late_1_05", "ME_late_1_06",
                              "ME_late_1_07", "ME_late_1_08", "ME_late_1_09", "ME_late_1_10")) %>% 
    separate_wider_delim(Sample_Name, delim = "_", names = c("site", "season", "run", "tube")) %>% 
    group_by(site, season) %>%  
    summarise("seq_n" = n()) 
  
  data_comp = all_data %>%  
    filter(site != "Key Largo") %>% 
    select(site, season, run, tube) %>%  
    mutate(site = case_when(
      site == "Manatee River" ~ "MR", 
      site == "Ft. Hamer" ~ "FH", 
      site == "Tyler Cove" ~ "MD", 
      site == "Ganey's Wharf" ~ "GW", 
      site == "Esker Point" ~ "CT", 
      site == "Sawyer Park" ~ "ME", 
      site == "St. Thomas de Kent Wharf" ~ "TK", 
      site == "Ritchie Wharf" ~ "RW", 
    )) %>% 
    group_by(site, season) %>%  
    summarise("data_n" = n())
  
  comp_data = inner_join(sequence_comp, data_comp) 
  
  comp_data %>% 
    ggplot(aes(x = data_n, y = seq_n)) +  
    geom_point() + 
    geom_abline(intercept = 0, slope = 1)
  
}


if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
  
  # ### Summary figure for Melissa's MIRA proposal (7x8in portrait)
  # ggarrange(
  #   site_map + scale_x_continuous(breaks = c(-80, -70, -60)) + theme_matt(base_size = 16) + guides(colour=guide_legend(nrow=3,byrow=F)),
  #   ctmax_plot + theme_matt(base_size = 16) + guides(colour=guide_legend(nrow=3,byrow=F)),
  #   ctmax_temp_plot + theme_matt(base_size = 16) + guides(colour=guide_legend(nrow=3,byrow=F)),
  #   pop_effs_plot + theme_matt(base_size = 16) + guides(colour=guide_legend(nrow=3,byrow=F)), 
  #   ggplot() + theme_void(),
  #   nrow = 3, ncol = 2,
  #   heights = c(1,1,0.1),
  #   common.legend = T,
  #   align = "hv",
  #   legend = "bottom",
  #   labels = c("A", "B", "C", "D", "")
  # )

}

if(molecular_report == T){
  st_curve = readxl::read_excel(path = "Molecular/method_test/extraction_nanodrop.xlsx")
  
  render(input = "Output/Reports/mol_report.Rmd", #Input the path to your .Rmd file here
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
