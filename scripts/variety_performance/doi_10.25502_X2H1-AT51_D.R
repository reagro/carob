
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
  dataset_id <- agro::get_simple_URI(uri)
  group <- "variety_performance"
  
  ## dataset level data 
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= "https://www.tandfonline.com/doi/pdf/10.1080/23311932.2020.1722353",
    data_citation = "vanlauwe, bernard, Samuel, A.-N., Endalkachew, W., Peter, E., Freddy, B., Jean-Marie, S., Paul, W., Regis, C., Lloyd, P., Nkeki, K., Theresa, A.-B., Esther, R., Fred, K., Ken, G., Edward, B., & Heerwaarden, J. van. (2020). N2Africa agronomy trials - Ehtiopia, 2013 [Data set].
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
  d <- data.frame(read.csv2(f, sep = "," ))
  d$start_date <- as.Date(d$planting,"%m/%d/%Y")
  d <- d[,c("trial_id","start_date")]
  
  # processing crop_observations.csv
  f1 <- ff[basename(ff) == "crop_observations.csv"]
  d1 <- data.frame(read.csv2(f1, sep = "," ))
  d1$rep <- d1$replication_no
  d1$treatment <- d1$main_treatment # sub_treatment and sub_sub_treatment have null values hence will be ignored
  d1$start_date <- as.Date(d1$date_planting, "%m/%d/%Y")
  d1$end_date <- as.Date(d1$date_harvest, "%m/%d/%Y")
  d1$yield <- d1$grain_yield_kgperha
  d1$residue_yield<- d1$total_yield_stover_kg_per_ha
  d1$biomass_total <- d1$calc_weight_a_ground_biomass_kg
  d1$grain_weight <- d1$dry_weight_100_seed_g * 10
  d1 <- d1[,c("trial_id","rep","treatment","variety","start_date","end_date","yield","residue_yield","biomass_total","grain_weight")]
  
  # merging activities.csv,field_history.csv and general.csv into d5 due to common trial_id and dropping irrelevant variables
  f2 <- ff[basename(ff) == "field_history.csv"]
  d2 <- data.frame(read.csv2(f2, sep = "," ))
  f3 <- ff[basename(ff) == "general.csv"]
  d3 <- data.frame(read.csv2(f3, sep = "," ))
  d4 <- merge(d,d2, by = c("trial_id"), all = TRUE)
  d5 <- merge(d4,d3,by = c("trial_id","instanceid"), all = TRUE)
  drop <- c("SN.x","SN.y","id.x","id.y","instanceid")
  d5 <- d5[ , !(names(d5) %in% drop)]
  d5$adm1 <- d5$district_county
  d5$adm2 <- d5$village
  d5$location <- d5$site
  d5$crop <- d5$type_of_experiment
  d5$crop <- ifelse(d5$crop == "commonbean_babytrial"|d5$crop == "common bean_input"|d5$crop == "commonbean_input"|d5$crop == "Commonbean_input"
                    |d5$crop == "common bean_var"|d5$crop == "Commonbean_variety"|d5$crop == "commonbean_variety","common bean",
                    ifelse(d5$crop == "Chickpea_input"|d5$crop == "Chickpea_variety","chickpea",
                           ifelse(d5$crop == "Fababean_input"|d5$crop == "Fababean_variety","faba bean","soybean")))
  d5 <- d5[,c("trial_id","country","adm1","adm2","location","crop","start_date")]
  
  # processing nutr_deficiency_pest_disease.csv
  f6 <- ff[basename(ff) == "nutr_deficiency_pest_disease.csv"]
  d6 <- data.frame(read.csv2(f6, sep = "," ))
  d6$rep <- d6$replication_no
  d6 <- d6[,c("trial_id","rep")]
  
  # processing pesticide_biocide_use.csv
  f7 <- ff[basename(ff) == "pesticide_biocide_use.csv"]
  d7 <- data.frame(read.csv2(f7, sep = "," ))
  d7 <- d7[3]
  
  # processing rainfall.csv
  f8 <- ff[basename(ff) == "rainfall.csv"]
  d8 <- data.frame(read.csv2(f8, sep = "," ))
  d8$rain <- d8$rain_mm
  d8 <- d8[,c("trial_id","rain")]
  
  # processing soil_data.csv
  f9 <- ff[basename(ff) == "soil_data.csv"]
  d9 <- data.frame(read.csv2(f9, sep = "," ))
  d9$trial_id <- d9$farm_id
  d9$soil_pH <- d9$ph
  d9$soil_SOC <- d9$tc_perc
  d9$soil_N <- d9$n_perc
  d9$soil_sand <- d9$sand_perc
  d9$soil_clay <- d9$clay_perc
  d9 <- d9[,c("trial_id","soil_pH","soil_SOC","soil_N","soil_sand","soil_clay")]
  
  # compiling into a single final dataset
  z <- carobiner::bindr(d1,d5,d6,d7,d8,d9)
  z
  colnames(z)
  z$country <- replace(z$country,1:nrow(z),"Ethiopia")
  unique(z$country)
  z$latitude <- 9.14500
  z$longitude <- 40.48967
  z$on_farm <- "yes"
  z$dataset_id <- dataset_id
  z$crop[is.na(z$crop)] <- "common bean" # assuming all null values in crops are common beans
 
  
  z <- z[,c("dataset_id","trial_id","on_farm","country","adm1","adm2","location","crop","variety","start_date",
  "end_date","rep","treatment","rain","soil_pH","soil_SOC","soil_N","soil_sand","soil_clay","grain_weight",
  "biomass_total","yield","residue_yield","latitude","longitude")]

  carobiner::write_files(dset, z, path, dataset_id, group)
  TRUE
}
