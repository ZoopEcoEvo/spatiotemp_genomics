################
## Site Temps ##
################

if(process_site_temps == T){
  source(file = "Scripts/00_site_temps.R")
}

################
## Phenotypic ##
################

# This script cycles through the time, temperature, and length data files collected for each individual replicate
# and combines them to estimate thermal limits (as CTmax)

cumul_data = data.frame()
temp_record = data.frame()
ramp_record = data.frame()
file_list = dir(path = "Raw_data/pheno_data/") # full set of time data files

all_runs = str_split_fixed(file_list, pattern = "_obs.csv", n = 2)[,1] # Pulls out just the date prefix from the file names
if(process_all_data == T){
  prev_runs = NA #Use this line the first time the script is run to process all files
  overwrite = "yes"
}else{
  prev_runs = read.table(file = "Output/Output_data/prev_runs.txt", header = T) # The time data files that have already been processed
  prev_runs = prev_runs$x
  overwrite = "no"
}

new_runs = all_runs[which(!(all_runs %in% prev_runs))] # Only the new time data files
runs = c()

if(length(new_runs) > 0){ # If there are new data files to process...
  for(f in 1:length(new_runs)){
    file_name = new_runs[f] 
    runs = c(runs, file_name)
    
    if(length(file_list) == 1){
      run_id = 1
    }else{
      run_id = sum(!is.na(prev_runs)) + f
    }
    
    # Loads data from temperature sensors (logging at 5 second intervals)
    temp_data = read_csv(paste("Raw_data/temp_data/", file_name, "_temp.CSV", collapse = "", sep = "")) %>% 
      mutate("Time" = lubridate::hms(Time),
             "Date" = lubridate::as_date(Date)) %>% 
      mutate("time_point" = row_number(), # Assigns each time point a sequential value
             "second_passed" = lubridate::time_length(Time - first(Time)), # Calculates the time passed in seconds since logging began
             "minute_passed" = second_passed / 60,
             "minute_interval" = floor(second_passed / 60)) %>% # Integer math to convert from seconds since logging began to minute time interval 
      pivot_longer(cols = c(Temp1, Temp2, Temp3), # Pivots data set so there's only one column of temperature data
                   names_to = "sensor",
                   values_to = "temp_C") %>% ungroup()
    
    name_split = str_split_fixed(file_name, pattern = "_", n = 4)
    date = paste(name_split[1], name_split[2], name_split[3], sep = "-")
    
    time_data = read_csv(paste("Raw_data/pheno_data/", file_name, "_obs.csv", collapse = "", sep = "")) %>% 
      drop_na(ctmax_minute) %>%
      mutate(time = (ctmax_minute + (ctmax_second / 60)) - 2, # Accounts for the two minute start up delay in the temperature logger
             "rank" = dense_rank(desc(time)),
             collection_date = lubridate::as_date(collection_date, format = "%m/%d/%y"),
             exp_date = lubridate::as_date(date),
             days_in_lab = as.numeric(exp_date - collection_date))
    
    min_ramp = temp_data  %>% 
      group_by(sensor, minute_interval) %>% 
      group_modify(~ data.frame(
        "ramp_per_second" = unclass(
          coef(lm(data = .x, temp_C ~ second_passed))[2]))) %>% # Calculates rate of change for each sensor during each of the minute time intervals
      mutate(ramp_per_minute = ramp_per_second * 60, # Converts from change per second to change per minute
             run = run_id) # Gives each run a unique numeric ID
    
    ### Combine with time data to get CTmax values 
    ind_measurements = time_data %>% 
      group_by(tube) %>% 
      summarise("ctmax" = mean(filter(temp_data, minute_passed > (time - (0.1 * rank)) & minute_passed < time)$temp_C), # Average temperature of the uncertainty window for each individual
                "ramp_rate" = mean(filter(min_ramp, minute_interval > (time - 5) & minute_interval < time)$ramp_per_minute))
    
    ct_data = inner_join(time_data, ind_measurements, by = c("tube")) %>% 
      mutate(run = run_id, 
             warming_tol = ctmax - collection_temp) %>% 
      select(collection_date, site, season, collection_temp, collection_salinity, exp_date, 
             days_in_lab, run, replicate, tube, rank, size, time, ramp_rate, ctmax, warming_tol)
    
    write.csv(ct_data, file = paste("Output/Output_data/", file_name, "_ctmax.csv", sep = "", collapse = ""), row.names = F)
    
    cumul_data = bind_rows(cumul_data, ct_data) 
    
    temp_data$run = run_id
    temp_record = bind_rows(temp_record, temp_data)
    
    ramp_record = bind_rows(ramp_record, min_ramp)
  }
  
  full_data = cumul_data
  
  if(overwrite == "yes"){
    #Records full data set
    write.table(x = runs, file = "Output/Output_data/prev_runs.txt", row.names = F) 
    write.table(x = full_data, file = "Output/Output_data/full_data.csv", sep = ",", row.names = F)
    write.table(x = temp_record, file = "Output/Output_data/temp_record.csv", sep = ",", row.names = F)
    write.table(x = ramp_record, file = "Output/Output_data/ramp_record.csv",  sep = ",", row.names = F)
  }else{
    #Records full data set
    write.table(x = runs, file = "Output/Output_data/prev_runs.txt", row.names = F, col.names = F, append = T) 
    write.table(x = full_data, file = "Output/Output_data/full_data.csv", sep = ",", row.names = F,col.names =F, append = T)
    write.table(x = temp_record, file = "Output/Output_data/temp_record.csv", sep = ",", row.names = F,col.names =F, append = T)
    write.table(x = ramp_record, file = "Output/Output_data/ramp_record.csv",  sep = ",", row.names = F, col.names =F,  append = T)
  }
}

###############
## Molecular ##
###############

### Processing Clades

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


### Bringing the different data sources together 
# Takes all the CTmax data and adds in info about the number of reads, clade assignments, and genome coverage
# Filters out individuals to be excluded, that are missing a clade assignment, and A. hudsonica individuals

if(process_all_data == T){

  all_data = read.csv(file = "Output/Output_data/full_data.csv") %>%  
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
  
  all_data %>% 
    mutate(ind_id = paste(str_replace_all(site, pattern = " ", replacement = "_"), 
                          season, replicate, tube, sep = "_")) %>% 
    filter(ind_id %in% excluded_inds)
  
  read_data = read.csv("Raw_data/molecular/read_metrics.csv") %>% 
    filter(sample_id != "unmatched") %>% 
    arrange(templates) %>% 
    mutate("site_code" = str_split_fixed(sample_id, pattern = "_", n = 2)[,1], 
           site = case_when(
             site_code == "KL" ~ "Key Largo",
             site_code == "MR" ~ "Manatee River",
             site_code == "FH" ~ "Ft. Hamer",
             site_code == "MD" ~ "Tyler Cove",
             site_code == "GW" ~ "Ganey's Wharf",
             site_code == "CT" ~ "Esker Point",
             site_code == "ME" ~ "Sawyer Park",
             site_code == "TK" ~ "St. Thomas de Kent Wharf",
             site_code == "RW" ~ "Ritchie Wharf"),
           "season" = str_split_fixed(sample_id, pattern = "_", n = 3)[,2], 
           "replicate" = str_split_fixed(sample_id, pattern = "_", n = 4)[,3],
           "tube" = str_split_fixed(sample_id, pattern = "_", n = 4)[,4],
           replicate = as.integer(replicate),
           tube = as.integer(tube)) %>% 
    select(-sample_id)
  
  clade_summary = read.csv(file = "Output/Output_data/COI_clades_summary.csv") %>% 
    mutate(population = fct_relevel(population, "MR", "FH", "MD", "GW", "CT", "ME", "TK", "RW"), 
           season = fct_relevel(season, "early", "peak", "late"), 
           sample = fct_reorder2(sample, .y = population, .x = season, .desc = F))
  
  clade_assignments = clade_summary %>% 
    group_by(sample) %>% 
    filter(n == max(n)) %>% 
    mutate(tube = parse_number(str_split_fixed(individual, pattern = "_", n = 2)[2]),
           replicate = parse_number(str_split_fixed(individual, pattern = "_", n = 2)[1]),
           season = str_split_fixed(sample, pattern = "_", n = 4)[2])
  
  ### Reads in post-alignment metrics, including genome size, mean coverage, and the percent represented at 1x and 5x coverage
  sample_coverage = data.frame()
  sample_mapping = data.frame()
  for(i in dir("Raw_data/molecular/coverage_metrics/")){
    
    if(str_detect(i, pattern = "_wgs_")){
      sample = str_split_1(i, pattern = "_wgs_")[1]
      
      sample_data = read.delim(file = paste0("Raw_data/molecular/coverage_metrics/", i, collapse = ""), 
                               comment.char = "#", nrows = 1) %>% 
        mutate("sample" = sample) %>% 
        select(sample, GENOME_TERRITORY, MEAN_COVERAGE, SD_COVERAGE, PCT_1X, PCT_5X) %>% 
        janitor::clean_names()
      
      sample_coverage = bind_rows(sample_coverage, sample_data)
      
    }else{
      sample = str_split_1(i, pattern = "_map_")[1]
      
      sample_data = read.delim(file = paste0("Raw_data/molecular/coverage_metrics/", i, collapse = ""), 
                               comment.char = "#", nrows = 3) %>% 
        filter(CATEGORY == "PAIR") %>% 
        mutate("sample" = sample) %>% 
        select(sample, TOTAL_READS, PF_HQ_ALIGNED_READS) %>% 
        janitor::clean_names()
      
      sample_mapping = bind_rows(sample_mapping, sample_data) 
    }
  }
  
  sample_coverage %>% 
    full_join(sample_mapping, by = c("sample")) %>% 
    write.csv(file = "Output/Output_data/sample_coverage.csv")
  
  sample_coverage = read.csv(file = "Output/Output_data/sample_coverage.csv") %>% 
    select(-X) %>% 
    mutate(site_code = str_split_fixed(sample, pattern = "_", n = 4)[,1],
           season = str_split_fixed(sample, pattern = "_", n = 4)[,2],
           replicate = parse_number(str_split_fixed(sample, pattern = "_", n = 4)[,3]),
           tube = parse_number(str_split_fixed(sample, pattern = "_", n = 4)[,4]))
  
  join_data = all_data %>%  
    select(-days_in_lab, -run, -time, -ramp_rate) %>% 
    mutate(site_code = case_when(
      site == "Key Largo" ~ "KL",
      site == "Manatee River" ~ "MR",
      site == "Ft. Hamer" ~ "FH",
      site == "Tyler Cove" ~ "MD",
      site == "Ganey's Wharf" ~ "GW",
      site == "Esker Point" ~ "CT",
      site == "Sawyer Park" ~ "ME",
      site == "St. Thomas de Kent Wharf" ~ "TK",
      site == "Ritchie Wharf" ~ "RW")) %>% 
    left_join(read_data) %>%
    mutate(templates = templates * 2) %>% 
    left_join(select(ungroup(clade_assignments), -sample, -individual, "clade" = Clade, "num_clade_matches" = n, "site_code" = population, tube, replicate, season)) %>% 
    left_join(select(sample_coverage, site_code, season, replicate, tube, mean_coverage, sd_coverage, pct_1x, pct_5x, total_reads, pf_hq_aligned_reads)) %>% 
    mutate(pct_aligned = pf_hq_aligned_reads / templates)
    
  bam_list = read.table("Raw_data/molecular/bam_list.txt") %>% 
    mutate(sample = str_split_fixed(V1, pattern = "/", n = 2)[,2],
           sample = str_split_fixed(sample, pattern = "_dd_", n = 2)[,1]) %>% 
    select(sample) %>% 
    mutate(site_code = str_split_fixed(sample, pattern = "_", n = 4)[,1],
           season = str_split_fixed(sample, pattern = "_", n = 4)[,2],
           replicate = parse_number(str_split_fixed(sample, pattern = "_", n = 4)[,3]),
           tube = parse_number(str_split_fixed(sample, pattern = "_", n = 4)[,4]), 
           "bam" = "yes")
  
  excluded = data.frame(
    site_code = c("CT", "MR", "MR", "MD", "ME", "TK", "FH"),
    season = c("early", "peak", "peak", "peak", "peak", "late", "late"),
    replicate = c(2, 2, 2, 2, 1, 1, 2), 
    tube = c(3, 6, 7, 2, 4, 3, 3), 
    exclude = "yes")
  
  inventory = all_data %>% 
    mutate(phenotype = "yes",
           site_code = case_when(
             site == "Key Largo" ~ "KL",
             site == "Manatee River" ~ "MR",
             site == "Ft. Hamer" ~ "FH",
             site == "Tyler Cove" ~ "MD",
             site == "Ganey's Wharf" ~ "GW",
             site == "Esker Point" ~ "CT",
             site == "Sawyer Park" ~ "ME",
             site == "St. Thomas de Kent Wharf" ~ "TK",
             site == "Ritchie Wharf" ~ "RW")) %>% 
    select(site, site_code, season, replicate, tube, phenotype) %>% 
    mutate(phenotype = if_else(phenotype == "<NA>", "no", "yes")) %>% 
    full_join(select(read_data, site, site_code, season, replicate, tube, "reads" = templates)) %>% 
    mutate(reads = if_else(is.na(reads), "no", "yes")) %>% 
    full_join(select(ungroup(clade_assignments), "site_code" = population, season, replicate, tube, "clade" = Clade)) %>% 
    mutate(hudsonica = if_else(clade == "A_hudsonica", "yes", "no"),
           hudsonica = if_else(is.na(hudsonica), "no", hudsonica),
           clade_id = if_else(is.na(clade), "no", "yes")) %>% 
    full_join(select(sample_coverage, site_code, season, replicate, tube, "coverage" = pct_1x)) %>% 
    mutate(coverage = if_else(is.na(coverage), "no", "yes")) %>% 
    full_join(select(join_data, site, site_code, season, replicate, tube, "joined" = ctmax)) %>% 
    mutate(joined = if_else(is.na(joined), "no", "yes")) %>% 
    full_join(select(bam_list, site_code, season, replicate, tube, bam)) %>% 
    mutate(bam = if_else(is.na(bam), "no", "yes")) %>% 
    full_join(excluded) %>% 
    mutate(exclude = if_else(is.na(exclude), "no", "yes"), 
           exclude = if_else(hudsonica == "yes", "yes", exclude)) %>% 
    drop_na(replicate)
  
  dim(inventory)
  length(which(inventory$phenotype == "yes"))
  length(which(inventory$exclude == "yes"))
  length(which(inventory$hudsonica == "yes"))
  length(which(inventory$reads == "yes"))
  length(which(inventory$clade_id == "yes"))
  length(which(inventory$coverage == "yes"))
  length(which(inventory$joined == "yes"))
  length(which(inventory$bam == "yes"))
  length(which(inventory$phenotype == "yes" & inventory$site_code != "KL"))
  length(which(inventory$joined == "no" & inventory$bam == "yes"))
  
  inventory %>% group_by(site_code, season) %>% count() %>%  pivot_wider(names_from = season, values_from = n)
  
  inventory %>% filter(joined == "no")
  
  ### This is the subset that should be used for the PCAs? 
  inventory %>% filter(bam == "yes", phenotype == "yes", exclude == "no", site_code != "KL") %>% arrange(site_code, season, replicate, tube) %>% dim()
  
  write.csv(inventory, file = "Output/Output_data/sample_inventory.csv", row.names = F)
  write.csv(join_data, file = "Output/Output_data/joined_data.csv", row.names = F)
}

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
