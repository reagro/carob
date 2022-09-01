#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 african countries
#################################################################################
  

carob_script <- function(path){
  uri <- "doi.org/10.25502/DGQZ-YP49/D"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "variety_performance"
  
  #dataset level data
  
  dset <- data.frame(
  dataset_id = dataset_id,
  group = group,
  uri = uri,
  publication = "",
  data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, 
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, 
  L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, 
  K., Baars, E., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - 
  Kenya, 2012 [Data set]. International Institute of Tropical Agriculture (IITA).
  https://doi.org/10.25502/DGQZ-YP49/D",
  carob_contributor = "Effie Ochieng",
  experiment_type = "variety_performance",
  has_weather =  FALSE,
  has_management = FALSE
  )
 
  # download and read data 
  
  ff <- carobiner::get_data(uri,path,group)
  js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
  dset$license <- carobiner::get_license(js) 
  
  # read the data
  f <- ff[basename(ff) == "data.csv"]
  d <- data.frame(read.csv2(f, sep = ","))
  
  f1 <- ff[basename(ff) == "general.csv"]
  d1 <- data.frame(read.csv2(f1, sep = ","))
  
  f2 <- ff[basename(ff) == "soil_properties.csv"]
  d2 <- data.frame(read.csv2(f2, sep = ",")) 
  
  #create a "variable 'crop' and fill it using the varieties, data set had no crop variable
  d[ , 'crop'] <- 
  d$crop[d$variety == "SB 19"] <- "soybean"
  d$crop[d$variety == "SB 24"] <- "soybean"
  d$crop[d$variety == "SB 25"] <- "soybean"
  d$crop[d$variety == "Squire"] <- "soybean"
  d$crop[d$variety == "Sequele"] <- "soybean"
  d$crop[d$variety == "Saga"] <- "soybean"
  d$crop[d$variety == "KK 8"] <- "common bean"
  d$crop[d$variety == "KK072"] <- "common bean"
  d$crop[d$variety == "Kenya Umoja"] <- "common bean"
  d$crop[d$variety == "Okwodo"] <- "common bean"
  d$crop[d$variety == "KK 15"] <- "common bean"
  d$crop[d$variety == "KK 071"] <- "common bean"
  d$crop[d$variety == "KK 072"] <- "common bean"
  d$crop[d$variety == "Okuodo"] <- "common bean"
  d$crop[d$variety == "Kenya umoja"] <- "common bean"
  d$crop[d$variety == "Gasirida"] <- "common bean"
  d$crop[d$variety == "Omubano"] <- "common bean"
  d$crop[d$variety == "RWV 1129"] <- "common bean"
  d$crop[d$variety == "RWV 51348"] <- "common bean"
  d$crop[d$variety == "Mac 44"] <- "common bean"
  d$crop[d$variety == "Mammesa"] <- "common bean"
  d$crop[d$variety == "Kenya Tamu"] <- "common bean"
  d$crop[d$variety == "Kenya Mavuno"] <- "common bean"
  d$crop[d$variety == "Newroscoco"] <- "common bean"
  d$crop[d$variety == "KAT 56"] <- "common bean"
  d$crop[d$variety == "Ayaki"] <- "common bean"
  d$crop[d$variety == "KAT B1"] <- "common bean"
  d$crop[d$variety == "TGX 1987-10F"] <- "soybean"
  d$crop[d$variety == "TGX 1987-18F"] <- "soybean"
  d$crop[d$variety == "TGX 1987-6F"] <- "soybean"
  d$crop[d$variety == "TGX 1987-62F"] <- "soybean"
  d$crop[d$variety == "RWV51348"] <- "common bean"
  d$crop[d$variety == "SB19"] <- "soybean"
  d$crop[d$variety == "Kenya tamu"] <- "common bean"
  d$crop[d$variety == "RWV 51353"] <- "common bean"
  d$crop[d$variety == "RWV 51355"] <- "common bean"
  d$crop[d$variety == "RWV 51354"] <- "common bean"
  d$crop[d$variety == "RWV 51352"] <- "common bean"
  d$crop[d$variety == "RWV 51351"] <- "common bean"
  d$crop[d$variety == "RWV 51350"] <- "common bean"
  d$crop[d$variety == "RWV 51349"] <- "common bean"
  d$crop[d$variety == "SB 8"] <- "soybean"
  d$crop[d$variety == "EAI 3600"] <- "soybean"

  
  #process data sets separately identifying the variables of interest
  d$trial_id <- d$experiment_id
  d$rep <- d$replication_no
  d$on_farm <-"yes"
  d$start_date <-paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")
  d$end_date <- paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")
  d$treatment <- paste(d$main_treatment,d$sub_treatment_inoc,d$sub_treatment_fert, sep = "-")
  
  #adding fertilizer information
   d$f1<- d$main_treatment
   d$f2<- d$sub_treatment_inoc
   d$f3<- d$sub_treatment_fert
   d$K_fertilizer[d$f1 == "TSP/KCL/Urea"|d$f2 == "TSP/KCL/UREA" |d$f2 =="TSP/KCL"|d$f2 == "TSP/KCL/Urea"| d$f3 == "TSP/KCL"|d$f3 == "TSP/KCL/Urea"]<- 30
   d$P_fertilizer[d$f1 == "TSP/KCL/Urea"|d$f2 == "TSP/KCL/UREA" |d$f2 =="TSP/KCL"|d$f2 == "TSP/KCL/Urea"|d$f3 == "TSP/KCL"|d$f3 == "TSP/KCL/Urea"]<- 30
   d$N_fertilizer[d$f1 == "TSP/KCL/Urea"|d$f2 == "TSP/KCL/UREA"|d$f3 == "TSP/KCL/Urea"]<- 60
   d$N_splits[d$f1 =="TSP/KCL/Urea"|d$f2 == "TSP/KCL/Urea"|d$f2 == "TSP/KCL/UREA"|d$f3 == "TSP/KCL/Urea" ]<-2
  
  #changing the character variables to numeric using lapply
  
  d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules",
         "nodule_dry_weight")] <- lapply(d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules",
                                                "nodule_dry_weight")], as.numeric)
  
  
  
  d$biomass_roots <- d$root_dry_weight_roots_no_nodules
  d$biomass_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules +
                        d$nodule_dry_weight)
  
  #subset the processed variables
  
  d <- d[,c("trial_id","rep","on_farm","start_date","end_date","treatment","biomass_roots","biomass_total", "crop", "K_fertilizer","P_fertilizer","N_fertilizer","N_splits")]
  
  
  d1$trial_id <- d1$experiment_id
  d1$country <-"Kenya"
  d1$site <- d1$action_site
  
  #subset the processed variables
  d1 <- d1 [, c("trial_id", "country", "site")]
  
  d2$trial_id <- d2$experiment_id
  d2$soil_pH <- d2$ph
  d2$soil_K <- d2$k
  d2$soil_sand <- d2$sand
  d2$soil_clay <- d2$clay
  d2$soil_SOC <- d2$tot_carbon
  d2$soil_N <- d2$tot_nitrogen 
  
  #subset the processed variables
  d2 <- d2[, c("trial_id", "soil_pH", "soil_K", "soil_sand","soil_clay","soil_SOC","soil_N")]
  
  # combining the processed data sets to one
  s <- merge(d, d1, by = "trial_id")
  q <- merge(s, d2, by = "trial_id")
  #add the gps information
  q$latitude <- -0.02356
  q$longitude <- 37.90619
  
  q$dataset_id <- dataset_id
  # all scripts should end like this
  carobiner::write_files(dset, q, path, dataset_id, group)
  TRUE
}  
  