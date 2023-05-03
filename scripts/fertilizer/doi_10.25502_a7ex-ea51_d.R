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
  project="N2Africa",
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
  experiment_type = "on farm",
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

 d1$trial_id <- d1$experiment_id
 d1$country <-"Rwanda"
 d1$first_name <- sapply(strsplit(as.character(d1$action_site), "/"), `[`, 1)
 d1$last_name <- sapply(strsplit(as.character(d1$action_site), "/"), `[`, 2)


 d1$adm2 <- ifelse(d1$first_name %in% c("Bugesera","Kamonyi","Kayonza","Burera","Musenyi"), d1$first_name,
                   ifelse(d1$last_name %in% "Musanze",d1$last_name,NA))
 
 d1$adm3 <- ifelse(d1$first_name %in% c("Rwaza", "Cyabingo","Cyuve","Nyamata", "Mareba", "Musenyi","Nyamirama","Muko","cyuve", "Musambira"), d1$first_name,
                   ifelse(d1$last_name %in% c(" Musenyi"," Mareba", " Musambira"," Nyarubaka","Nyarubaka"," Nyamiyaga"," Nyamirama", " Rukara", " Rukara ", " Kinoni "),d1$last_name,NA))
 
 d1$adm3 <- carobiner::fix_name(d1$adm3, "title")

 d1$latitude <- ifelse(d1$adm3 == "Musenyi",-2.18864,
                       ifelse(d1$adm3 == "Mareba",-2.25791,
                              ifelse(d1$adm3 == "Musambira",-2.02396,
                                     ifelse(d1$adm3 == "Nyarubaka",-2.08663,
                                           ifelse(d1$adm3 == "Nyamiyaga",-2.43333,
                                                  ifelse(d1$adm3 == "Nyamirama",-1.95002,
                                                         ifelse(d1$adm3 == "Rukara",-1.8005,
                                                                ifelse(d1$adm3 == "Rwaza",-1.55886,
                                                                       ifelse(d1$adm3 ==  "Kinoni",-1.46114,
                                                                              ifelse(d1$adm3 == "Cyabingo",-1.58553,
                                                                                     ifelse(d1$adm3 == "Nyamata",-2.14395,
                                                                                            ifelse(d1$adm3m == "Muko",-2.05674,-1.47401))))))))))))
 
 d1$longitude <- ifelse(d1$adm3 =="Musenyi", 30.0297,
                        ifelse(d1$adm3 =="Mareba",30.06256,
                               ifelse(d1$adm3 == "Musambira",29.8395,
                                      ifelse(d1$adm3 == "Nyarubaka",29.83537,
                                             ifelse(d1$adm3 == "Nyamiyaga", 29.76667,
                                                    ifelse(d1$adm3 == "Nyamirama",30.52925,
                                                           ifelse(d1$adm3 == "Rukara", 30.4725,
                                                                 ifelse(d1$adm3 ==  "Rwaza",29.66713,
                                                                        ifelse(d1$adm3 ==  "Kinoni",29.73912,
                                                                               ifelse(d1$adm3 == "Cyabingo",29.69453,
                                                                                      ifelse(d$adm3 ==  "Nyamata",30.09802,
                                                                                             ifelse(d1$adm3 == "Muko",30.60027,29.65572))))))))))))
 
 
 d1 <- d1 [, c("trial_id", "country","adm2","adm3", "latitude","longitude")]
 
#create a "variable 'crop' and fill it using the varieties, data set had no crop variable
 e <- data.frame(variety = trimws(d$variety))
 e$variety[e$variety==""] <- NA

## RH: there are many records for which the crop is not know (and neither is the variety)
## That needs to be resolved
 e$crop <- "no crop"
 e$crop[d$variety %in% c("Peka 6", "Peka6", "Sc. Saga", "Sc. Square", "Sc. Squille", "SB8", "SB24")] <- "soybean"
 e$crop[d$variety %in% c("RWA 1668", "Gasirida", "RWR 1668")] <- "common bean"
 # e$crop[d$variety %in% NA] <- "no crop"
 
 #process data sets separately identifying the variables of interest
 e$trial_id <- d$experiment_id
 e$rep <- d$replication_no
 e$on_farm <-TRUE
 e$start_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
 e$end_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
 e$treatment <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, sep = "-")
 e$biomass_roots <- as.numeric(d$root_dry_weight_roots_no_nodules)
 e$biomass_total <- as.numeric((d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules + d$nodule_dry_weight))
 e$yield <- as.numeric(d$grain_yield_ha_calc)

 # adding the fertilizer information
 fert <- trimws(d$sub_treatment_inoc)

 e$P_fertilizer[fert %in% c("DAP", "TSP", "TSP/KCL")] <- 30
 e$K_fertilizer[fert == "TSP/KCL"]<- 30
 ## fix
 ## d$N_fertilizer[fert %in% c("DAP")] <- ????
 
 #subset the processed variables
# d <- d[, c("trial_id", "rep", "on_farm","start_date","end_date", "treatment", "biomass_roots","biomass_total", "yield", "crop","P_fertilizer","K_fertilizer")]
 
 # combining the processed data sets to one
 e <- merge(e, d1, by = "trial_id")
 
 e$dataset_id <- dataset_id
 
 # all scripts should end like this
 carobiner::write_files(dset, e, path, dataset_id, group)
}



