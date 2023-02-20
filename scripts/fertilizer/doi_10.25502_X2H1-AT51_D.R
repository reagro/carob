
carob_script <- function(path){
  
  "
Title: N2Africa agronomy trials - Ethiopia, 2013
  
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity 
of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
improving household nutrition and increasing income levels of smallholder farmers. As a vision of success,
N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit 
from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants
and fertilizers adapted to local settings. A strong national expertise in grain legume production and 
N2-fixation research and development will be the legacy of the project. The project is implemented in 
five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, 
Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.

" 
  uri <- "doi:10.25502/X2H1-AT51/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  ## dataset level data 
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= "https://doi.org/10.1080/23311932.2020.1722353",
    data_citation = "Vanlauwe, Bernard, Samuel, A.-N., Endalkachew, W., Peter, E., Freddy, B., Jean-Marie, S., Paul, W., Regis, C., Lloyd, P., Nkeki, K., Theresa, A.-B., Esther, R., Fred, K., Ken, G., Edward, B., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - Ehtiopia, 2013 [Data set].
    International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/X2H1-AT51/D",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    experiment_type="Symbiotic N2 fixation",
    has_weather=TRUE,
    has_management=TRUE)
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)

  # processing activities.csv
  f <- ff[basename(ff) == "activities.csv"]
  d <- read.csv(f)
  d$start_date <- as.Date(d$planting,"%m/%d/%Y")
  d<- d[,c("trial_id","start_date")]
  
  # processing crop_observations.csv
  f1 <- ff[basename(ff) == "crop_observations.csv"]
  d1 <- read.csv(f1)
  d1$rep <- d1$replication_no
  d1$treatment <- d1$main_treatment # sub_treatment and sub_sub_treatment have null values hence will be ignored
  d1$start_date <- as.Date(d1$date_planting,"%m/%d/%Y")
  d1$end_date <- as.Date(d1$date_harvest, "%m/%d/%Y")
  d1$yield <- d1$grain_yield_kgperha
  d1$residue_yield<- d1$total_yield_stover_kg_per_ha
  d1$biomass_total <- d1$calc_weight_a_ground_biomass_kg
  d1$grain_weight <- d1$dry_weight_100_seed_g * 10
  d1 <- d1[,c("trial_id","rep","treatment","variety","start_date","end_date","yield","residue_yield","biomass_total","grain_weight")]

  # processing field_history.csv
  f2 <- ff[basename(ff) == "field_history.csv"]
  d2 <- data.frame(read.csv(f2))
  d2 <- d2[3]
  
  # processing general.csv
  f3 <- ff[basename(ff) == "general.csv"]
  d3 <- data.frame(read.csv2(f3, sep = "," ))
  d3$adm1 <- d3$district_county
  d3$adm2 <- d3$village
  d3$location <- d3$site
  d3$crop <- d3$type_of_experiment
  d3$crop <- ifelse(d3$crop == "commonbean_babytrial"|d3$crop == "common bean_input"|d3$crop == "commonbean_input"|d3$crop == "Commonbean_input"
                    |d3$crop == "common bean_var"|d3$crop == "Commonbean_variety"|d3$crop == "commonbean_variety","common bean",
                    ifelse(d3$crop == "Chickpea_input"|d3$crop == "Chickpea_variety","chickpea",
                           ifelse(d3$crop == "Fababean_input"|d3$crop == "Fababean_variety","faba bean","soybean")))
  d3 <- d3[,c("trial_id","country","adm1","adm2","location","crop")]
  
  # processing nutr_deficiency_pest_disease.csv
  f4 <- ff[basename(ff) == "nutr_deficiency_pest_disease.csv"]
  d4 <- data.frame(read.csv2(f4, sep = "," ))
  d4$rep <- d4$replication_no
  d4 <- d4[,c("trial_id","rep")]
  
  # processing pesticide_biocide_use.csv
  f5 <- ff[basename(ff) == "pesticide_biocide_use.csv"]
  d5 <- data.frame(read.csv2(f5, sep = "," ))
  d5<- d5[3]
  
  # processing rainfall.csv
  f6 <- ff[basename(ff) == "rainfall.csv"]
  d6 <- data.frame(read.csv2(f6, sep = "," ))
  d6$rain <- d6$rain_mm
  d6 <- d6[,c("trial_id","rain")]
  
  # processing soil_data.csv
  f7 <- ff[basename(ff) == "soil_data.csv"]
  d7<- data.frame(read.csv2(f7, sep = "," )) 
  d7$trial_id <- d7$farm_id
  d7$soil_pH <- d7$ph
  d7$soil_SOC <- d7$tc_perc
  d7$soil_N <- d7$n_perc
  d7$soil_sand <- d7$sand_perc
  d7$soil_clay <- d7$clay_perc
  d7 <- d7[,c("trial_id","soil_pH","soil_SOC","soil_N","soil_sand","soil_clay")]

  # compiling into a single final dataset
  f <- carobiner::bindr(d,d1,d2,d3,d4,d5,d6,d7)
  f$dataset_id <- dataset_id
  f$country <- replace(f$country,1:nrow(f),"Ethiopia")
  f$latitude <- 9.14500
  f$longitude <- 40.48967
  f$on_farm <- "yes"
  f$crop[is.na(f$crop)] <- "common bean" 
  f$row_spacing <- 40 
  f$plant_spacing <-10
  # Fertilizer rates: DAP will be applied using a rate of 25 kg DAP per hectare; DAP has 18:46:0 composition
  # calculating amount of P in DAP applied assuming that any P input refers to DAP appication; 
  
  P2O5 <- 25 * 0.46
  # to acquire the amount of P in P2O5, P has atomic weight 31 while O has atomic weight 16.
  P <- P2O5*((2*31)/(2*31+5*16))  
  N <- 25 * 0.18
  
  f$fertilizer_type <- "none"

  #P or +P shows presence of phosphorus
  
  f$treatment <- trimws(f$treatment)
  f$P_fertilizer[f$treatment %in% c("+P+ +R","+P+ -R", "-I +P", "+I +P", "+I  + +P", "-I  +  +P",
	"+p and +I", "+p and -I", "-I,+P", "With P, I", "P", "w/out I, P", "+P,+I", "variety + +I & +P", "I, P",
	"I, P", "25kgDAP", "25kgDAP&Inoculant")] <- P
  f$N_fertilizer[f$treatment %in% c("+P+ +R", "+P+ -R", "-I +P", "+I +P", "+I  + +P", "-I  +  +P",
		"+p and +I", "+p and -I", "-I,+P", "With P, I", "P", "w/out I, P", "+P,+I", "variety + +I & +P", "I, P", "I, P", "25kgDAP", "25kgDAP&Inoculant")] <- N

  f$inoculated <- grepl("\\+R", f$treatment) | grepl("\\+I", f$treatment) 
		| grepl("With P, I", f$treatment) | grepl("^I", f$treatment) 
  
  ## RH see: 
  ## unique(trimws(f$treatment))
  ## what about "SARI" "Hawassa Dume" "Awash 1" "ECAB0081" "GLP2", etc??
  ## are those inoculants?
  ## if so, set inoculated to TRUE, and set inoculant to these names
  
  
  [f$treatment %in% c("+P+ +R","+P+ -R"," -I +P"," +I +P"," +I  + +P "," -I  +  +P ",
		" +p and +I", " +p and -I"," -I,+P","With P, I","P","w/out I, P"," +P,+I","variety + +I & +P","I, P"," I, P","25kgDAP","25kgDAP&Inoculant")]

  f$fertilizer_type[f$N_fertilizer > 0] <- "DAP"


	f$start_date <- as.character(f$start_date)
	f$end_date <- as.character(f$end_date)

## RH variety names can be normalized more.
	f$variety[f$variety == ""] <- NA

## RH many records have no yield. These are not useful. Is that an error in the data processing?
## If so, please fix. If not, we should remove these rows.

  carobiner::write_files(dset, f, path, dataset_id, group)
}

