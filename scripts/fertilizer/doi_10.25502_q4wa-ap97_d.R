#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

uri <- "doi.org/10.25502/q4wa-ap97/d"
dataset_id <- carobiner::simple_uri(uri)
group <- "fertilizer"

#dataset level data

dset <- data.frame(
  dataset_id = dataset_id,
  group = group,
  project="N2Africa",  
  uri = uri,
  publication = NA,
  data_citation ="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, 
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., 
  Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Baars, E., & Heerwaarden,
  J. van. (2020). N2Africa agronomy trials - Rwanda, 2011 [Data set]. 
  International Institute of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/Q4WA-AP97/D",
  carob_contributor = "Effie Ochieng",
  experiment_type = "fertilizer",
  has_weather =  FALSE,
  has_management = FALSE
)


# download and read data 

ff <- carobiner::get_data(uri,path,group)
js <- carobiner::get_metadata(dataset_id, path, group, major = 1, minor = 0)
dset$license <- carobiner::get_license(js) 

# read the data
f <- ff[basename(ff) == "data.csv"]
d <- read.csv(f)

f1 <- ff[basename(ff) == "general.csv"]
d1 <- read.csv(f1)

f2 <- ff[basename(ff) == "soil_properties.csv"]
d2 <- read.csv(f2)
                           
#process data sets separately identifying the variables of interest 

d$trial_id <- d$experiment_id
d$rep <- d$replication_no
d$on_farm <- TRUE
d$start_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
d$end_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
d$inoculated <- grepl("INO", d$experiment_id)
d$yield <- d$grain_yield_ha_calc
## EGB:
## there should be a way to cathch biomass variables... But the values do not make sense...
# d$biomass_total <- ((d$above_ground_fresh_biomass/d$area_biomass_sampling)*10)*0.86 # g/m2 -> kg/ha to dry weight (~16% moisture content)
# d$residue_yield <- ((d$above_ground_fresh_biomass/d$area_biomass_sampling)*10) - ((d$total_fresh_weight_all_pods/d$plot_area_harvest)*10)

#adding fertilizer information
## RH. Initialize to zero. I assume that all the other cases 
## that are not changed below are zero, not NA!
d$P_fertilizer <- 0
d$K_fertilizer <- 0
d$N_fertilizer <- 0
d$N_splits <- 0

d$P_fertilizer[d$sub_treatment_inoc %in% c("TSP", "TSP/KCL+UREA", "TSP/KCL", "TSP/KCL +UREA")]<- 30
d$K_fertilizer[d$sub_treatment_inoc %in% c("TSP/KCL+UREA", "TSP/KCL", "TSP/KCL +UREA")] <- 30
d$N_fertilizer[d$sub_treatment_inoc %in% c("TSP/KCL+UREA","TSP/KCL +UREA","PK6+Urea","SB24+Urea")] <- 60
d$N_splits[d$N_fertilizer > 0] <- 2

# subsetting the processed variables
d <- d [, c("trial_id","rep","on_farm","start_date","end_date","inoculated","yield","P_fertilizer","K_fertilizer","N_fertilizer","N_splits")]

d1$trial_id <- d1$experiment_id
d1$country <- "Rwanda"
d1$location <- gsub('[0-9]+', '', sub("(.*),.*", "\\1",  sub(" .*", "", carobiner::fix_name(d1$action_site, "title"))))
d1$longitude <- ifelse(d1$location == "Musanze", 29.569,
                       ifelse(d1$location == "Nemba", 29.786,
                              ifelse(d1$location == "Kinoni", 29.739,
                                     ifelse(d1$location == "Rwinkwavu", 30.615,
                                            ifelse(d1$location == "Rukara", 30.504,
                                                   ifelse(d1$location == "Nyarubaka", 29.843,
                                                          ifelse(d1$location == "Nyamata", 30.120,
                                                                 ifelse(d1$location == "Kamonyi", 29.902,
                                                                        ifelse(d1$location == "Nyamirama", 30.503,
                                                                               ifelse(d1$location == "Musenyi", 30.180,
                                                                                      ifelse(d1$location == "Nyamiyaga", 29.664,
                                                                                             ifelse(d1$location == "Musambira", 29.841,
                                                                                                    ifelse(d1$location == "Mareba", 29.718,
                                                                                                           ifelse(d1$location == "Bugesera", 30.158, 30.510))))))))))))))
d1$latitude <- ifelse(d1$location == "Musanze", -1.474,
                      ifelse(d1$location == "Nemba", -1.642,
                             ifelse(d1$location == "Kinoni", -1.468,
                                    ifelse(d1$location == "Rwinkwavu", -1.959,
                                           ifelse(d1$location == "Rukara", -1.798,
                                                  ifelse(d1$location == "Nyarubaka", -2.085,
                                                         ifelse(d1$location == "Nyamata", -2.151,
                                                                ifelse(d1$location == "Kamonyi", -2.028,
                                                                       ifelse(d1$location == "Nyamirama", -1.932,
                                                                              ifelse(d1$location == "Musenyi", -3.377,
                                                                                     ifelse(d1$location == "Nyamiyaga", -0.922,
                                                                                            ifelse(d1$location == "Musambira", -2.045,
                                                                                                   ifelse(d1$location == "Mareba", -1.675,
                                                                                                          ifelse(d1$location == "Bugesera", -2.232, -1.905))))))))))))))

d1$crop[grepl("SOY", d1$crop)] <- "soybean"
d1$crop[d1$crop %in% c("Bush BEANS INPUT", "Climbing BEANS INPUT")] <- "common bean"

d1 <- d1 [, c("trial_id","country","location","longitude", "latitude","crop")]

# combining the processed data sets to one
s <- merge(d, d1, by = "trial_id")

s$dataset_id <- dataset_id

# all scripts should end like this
carobiner::write_files(dset, s, path, dataset_id, group)

}
