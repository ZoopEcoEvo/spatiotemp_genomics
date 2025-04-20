# Load in required packages
library(rmarkdown)
library(merTools)
library(ggpubr)
library(lme4)
library(tidyverse)

#Determine which scripts should be run
process_all_data = F #Runs data analysis 
process_site_temps = F #Compiles continuous temperature data for the sites
process_clades = F #Runs the script to read in clade matches and compile the summary files (takes a long time)
make_report = T #Runs project summary
molecular_report = F
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
  dplyr::select(-bopyrid) %>% 
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

# full_data %>%
#   group_by(site, season, collection_date, collection_temp, collection_salinity) %>%
#   summarise(mean_ctmax = mean(ctmax)) %>%
#   write.csv("Output/Output_data/collection_summary.csv")

temp_summaries = full_data %>% 
  dplyr::select(site, season, collection_temp) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c("site"),
              names_from = season, 
              values_from = collection_temp) %>% 
  inner_join(site_data) %>% 
  group_by(site) %>% 
  mutate(season_mean = mean(c(early, peak, late), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(cent_season = scale(season_mean, center = T, scale = F)[,1]) %>% 
  dplyr::select(site, region, lat, long, early, peak, late, season_mean, cent_season)

######## Sequencing data
read_data = read.csv("Raw_data/molecular/read_metrics.csv") %>% 
  filter(sample_id != "unmatched") %>% 
  arrange(templates) %>% 
  mutate("site" = str_split_fixed(sample_id, pattern = "_", n = 2)[,1], 
         site = case_when(
           site == "KL" ~ "Key Largo",
           site == "MR" ~ "Manatee River",
           site == "FH" ~ "Ft. Hamer",
           site == "MD" ~ "Tyler Cove",
           site == "GW" ~ "Ganey's Wharf",
           site == "CT" ~ "Esker Point",
           site == "ME" ~ "Sawyer Park",
           site == "TK" ~ "St. Thomas de Kent Wharf",
           site == "RW" ~ "Ritchie Wharf"),
         "season" = str_split_fixed(sample_id, pattern = "_", n = 3)[,2], 
         "replicate" = str_split_fixed(sample_id, pattern = "_", n = 4)[,3],
         "tube" = str_split_fixed(sample_id, pattern = "_", n = 4)[,4],
         replicate = as.integer(replicate),
         tube = as.integer(tube))

# sample_map = readxl::read_excel(path = "Molecular/twist_map.xlsx") %>% 
#   select(Sample_Name, Sample_Id, Sample_Barcode, "Well" = well) # %>% 
# # write.csv("Molecular/sample_map.csv", row.names = F)
# 
# sequence_comp = sample_map %>% 
#   select(Sample_Name) %>% 
#   filter(!Sample_Name %in% c("ME_late_1_02", "ME_late_1_03", "ME_late_1_04", "ME_late_1_05", "ME_late_1_06",
#                              "ME_late_1_07", "ME_late_1_08", "ME_late_1_09", "ME_late_1_10")) %>% 
#   separate_wider_delim(Sample_Name, delim = "_", names = c("site", "season", "run", "tube")) %>% 
#   group_by(site, season) %>%  
#   summarise("seq_n" = n()) 
# 
# data_comp = all_data %>%  
#   filter(site != "Key Largo") %>% 
#   select(site, season, run, tube) %>%  
#   mutate(site = case_when(
#     site == "Manatee River" ~ "MR", 
#     site == "Ft. Hamer" ~ "FH", 
#     site == "Tyler Cove" ~ "MD", 
#     site == "Ganey's Wharf" ~ "GW", 
#     site == "Esker Point" ~ "CT", 
#     site == "Sawyer Park" ~ "ME", 
#     site == "St. Thomas de Kent Wharf" ~ "TK", 
#     site == "Ritchie Wharf" ~ "RW", 
#   )) %>% 
#   group_by(site, season) %>%  
#   summarise("data_n" = n())
# 
# comp_data = inner_join(sequence_comp, data_comp) 
# 
# comp_data %>% 
#   ggplot(aes(x = data_n, y = seq_n)) +  
#   geom_point() + 
#   geom_abline(intercept = 0, slope = 1)

if(process_clades == T){
  
  ref_clades = read_tsv("raw_data/molecular/reference_clade_labels.txt",  show_col_types = FALSE) %>% distinct()
  
  clade_matches = data.frame()
  no_matches = c()
  
  for(i in dir("Raw_data/molecular/clade_matches/")){
    
    blast_results = read.csv(file = paste0("Raw_data/molecular/clade_matches/", i, collapse = ""),
                             col.names = c("read", "ref", "pident", "length", "mismatch", "gapopen", 
                                           "qstart", "qend", "sstart", "send", "evalue", "bitscore")) 
    
    clade_counts = blast_results %>% 
      group_by(read) %>% 
      filter(evalue == min(evalue)) %>% 
      ungroup() %>% 
      filter(length > 100) %>% 
      filter(pident > 95) %>% 
      left_join(ref_clades, join_by(ref == label)) %>%  
      mutate("sample" = str_split_fixed(i, pattern = ".csv", n = 2)[,1], 
             "population" = str_split_fixed(sample, pattern = "_", n = 4)[,1], 
             "season" = str_split_fixed(sample, pattern = "_", n = 4)[,2], 
             "rep" = as.numeric(str_split_fixed(sample, pattern = "_", n = 4)[,3]), 
             "tube" = str_split_fixed(sample, pattern = "_", n = 4)[,4])
    
    if(dim(clade_counts)[1] == 0){
      no_matches = c(no_matches, str_split_fixed(i, pattern = ".csv", n = 2)[,1])
    }else{
      clade_matches = bind_rows(clade_matches, clade_counts)
    }
  }
  
  clade_matches = clade_matches %>% 
    ungroup() %>% 
    mutate(Clade = case_when(
      Clade == "I" ~ "A_hudsonica",
      .default = Clade
    ))
  
  write.table(clade_matches, file = "Output/Output_data/clade_matches.csv", 
              sep = ",", row.names = F, col.names = !file.exists("Output/Output_data/clade_matches.csv"), 
              append = T)
  
  best_matches = clade_matches %>% 
    group_by(sample) %>% 
    filter(length == max(length)) %>% 
    filter(evalue == min(evalue)) %>% 
    select(sample, population, season, rep, tube, Clade) %>% 
    distinct()
  
  write.table(best_matches, file = "Output/Output_data/best_matches.csv", 
              sep = ",", row.names = F, col.names = !file.exists("Output/Output_data/best_matches.csv"), 
              append = T)
}

clade_summary = read.csv(file = "Output/Output_data/COI_clades_summary.csv") %>% 
  mutate(population = fct_relevel(population, "MR", "FH", "MD", "GW", "CT", "ME", "TK", "RW"), 
         season = fct_relevel(season, "early", "peak", "late"), 
         sample = fct_reorder2(sample, .y = population, .x = season, .desc = F))

tonsa_samples = read.table("Raw_data/molecular/angsd_dists/bam_list.txt") %>% 
  separate(V1, into = c("drop", "file"), sep = "/") %>% 
  separate("file", into = c("sample", "drop2"), sep = "_dd") %>% 
  dplyr::select(sample) %>% 
  separate(sample, into = c("pop", "season", "replicate", "tube"), sep = "_") %>% 
  mutate(tube = as.numeric(tube), 
         replicate = as.numeric(replicate)) %>% 
  left_join(filter(clade_ctmax, Clade != "A_hudsonica"), join_by("pop", "season", "replicate", "tube")) %>% 
  ungroup() %>% 
  mutate(season = fct_relevel(season, "early", "peak", "late"),
         pop = fct_relevel(pop, "FH", "MR", "MD", "GW", "CT", "ME", "TK", "RW"))

ind_dist_matrix <- as.matrix(read.table("Raw_data/molecular/angsd_dists/snp_call.ibsMat"))


# clade_matches = read.csv(file = "Output/Output_data/clade_matches.csv") %>% 
#   mutate(population = fct_relevel(population, "MR", "FH", "MD", "GW", "CT", "ME", "TK", "RW"), 
#          rep = as.numeric(rep), 
#          tube = as.numeric(tube),
#          season = fct_relevel(season, "early", "peak", "late"), 
#          sample = fct_reorder2(sample, .y = population, .x = season, .desc = F))
# 
# best_matches = read.csv(file = "Output/Output_data/best_matches.csv") %>% 
#   mutate(population = fct_relevel(population, "MR", "FH", "MD", "GW", "CT", "ME", "TK", "RW"), 
#          rep = as.numeric(rep), 
#          tube = as.numeric(tube),
#          season = fct_relevel(season, "early", "peak", "late"), 
#          sample = fct_reorder2(sample, .y = population, .x = season, .desc = F))

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
