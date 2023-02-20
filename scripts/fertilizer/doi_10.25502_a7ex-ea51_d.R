#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 african countries
#################################################################################

carob_script <- function(path){
uri <- "doi.org/10.25502/a7ex-ea51/d"
dataset_id <- carobiner::simple_uri(uri)
group <- "fertilizer"

#dataset level data

dset <- data.frame(
  dataset_id = dataset_id,
  group = group,
  uri = uri,
  publication = "doi.org/10.1016/j.agee.2017.08.015",
  data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, 
  E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, 
  P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, 
  T., Ronner, E., Kanampiu, F., Giller, K., Baars, E., 
  & Heerwaarden, J. van. (2020). N2Africa agronomy 
  trials - Rwanda, 2012 [Data set]. International Institute 
  of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/A7EX-EA51/D",
  carob_contributor = "Effie Ochieng",
  experiment_type = "variety_trials",
  has_weather =  FALSE,
  has_management = FALSE
)


## download and read data 

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
 d$crop  <-
 d$crop[d$variety == "Peka 6"] <- "soybean"
 d$crop[d$variety == "RWA 1668"] <- "common bean"
 d$crop[d$variety == "Peka6"] <- "soybean"
 d$crop[d$variety == "Sc. Saga"] <- "soybean"
 d$crop[d$variety == "Sc. Square"] <- "soybean"
 d$crop[d$variety == "Sc. Squille"] <- "soybean"
 d$crop[d$variety == "SB8"] <- "soybean"
 d$crop[d$variety == "SB24"] <- "soybean"
 d$crop[d$variety == "Gasirida "] <- "common bean"
 d$crop[d$variety == "RWR 1668"] <- "common bean"
 
 #process data sets separately identifying the variables of interest
 d$trial_id <- d$experiment_id
 d$rep <- d$replication_no
 d$on_farm <-"yes"
 d$start_date <-paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")
 d$end_date <- paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")
 d$treatment <- paste(d$main_treatment,d$sub_treatment_inoc,d$sub_treatment_fert, sep = "-")
 d$biomass_roots <- d$root_dry_weight_roots_no_nodules
 d$biomass_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules + d$nodule_dry_weight)

 # adding the fertilizer information
 d$fertilizer_combination <- d$sub_treatment_inoc
 d$P_fertilizer[d$fertilizer_combination == "DAP "| d$fertilizer_combination == "DAP"|
                  d$fertilizer_combination == "TSP" | d$fertilizer_combination == "TSP/KCL"] <- 30
 
 d$K_fertilizer[d$fertilizer_combination == "TSP/KCL"]<- 30

 #subset the processed variables
 d <- d[, c("trial_id", "rep", "on_farm","start_date","end_date", "treatment", "biomass_roots","biomass_total", "crop","P_fertilizer","K_fertilizer")]
 
 d1$trial_id <- d1$experiment_id
 d1$country <-"Rwanda"
 d1$adm2 <- d1$mandate_area_name
 d1$site <- d1$action_site
 

#subset the processed variables
 d1 <- d1 [, c("trial_id", "country", "site","adm2")]
 
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
 q$latitude <-  -1.94028
 q$longitude <- 29.87389
 
 q$dataset_id <- dataset_id
 
 # all scripts should end like this
 carobiner::write_files(dset, q, path, dataset_id, group)
 TRUE
}



