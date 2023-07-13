
carob_script <- function(path) {

  "
  	The project proposes to use win-win public-private partnership approaches to disseminate improved
  	legume seeds and complementary crop management practices developed under PARTI collaboration over 
  	the past six years. Through PARTI partnerships, 5 varieties of soybean were released. 
  	Most of the varieties are drought tolerant, resistant to endemic pests and diseases, have end-user preferred 
  	traits, and show significant increases in yields on farmers’ fields.
  
  "
  
  uri <- "doi:10.25502/HNKM-Y645/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="N2Africa",
    uri=uri,
    publication= NA,
    data_citation="Engoke, C. N. S., Stephen, K.-B., Wiredu, A. N., & John, O. (2022). Inoculant, nitrogen and phosphorus improves photosynthesis and water use efficiency in soybean production- Legume cropping systems [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/HNKM-Y645/D",
    data_institutions = "IITA",
    carob_contributor="Effie Ochieng'",
    data_type="on-farm experiments"
  )

  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "Canon data.csv"]
  d1 <- read.csv(f)
  f <- ff[basename(ff) == "Biomass Analysis.csv"]
  d2 <- read.csv(f)
  
  
  d1 <- carobiner::change_names(d1, 
                                c("Country","Loc","Treatment","Variety","pH_value","Nitrogen","Rep","Yld_FW_kg_ha","Harvested_Biomass_kg_ha","Days_to_Flowering_R1","Season"),  
                                c("country","adm1","treatment","variety","P_fertilizer","N_fertilizer","rep","yield","residue_yield","flowering","season"))
  d1$crop <- "soybean"
  d1$K_fertilizer <- 0
  d1$inoculated <- ifelse(d1$Inoculant == "Yes",TRUE,FALSE)
  d1$grain_weight <- (1000/d1$Seeds_sqm) * d1$Seed_Weiht_sqm_g #to get for 1000seeds
  d1$plant_density <- 10000*d1$PLST #to get plant population/ha
  d1$on_farm <- TRUE
  d1$is_survey <- FALSE
  d1$sn <- 1:1440
  d1$trial_id <- paste(d1$sn,d1$adm1, d$treatment, sep = "_")
  d1 <- d1[,c("country","adm1","treatment","crop","variety","N_fertilizer","P_fertilizer","K_fertilizer","rep","yield","residue_yield","plant_density","trial_id","on_farm","is_survey","season","inoculated","flowering","grain_weight")]
  
  # second data set
  d2 <- carobiner::change_names(d2, 
                                c("Country","Loc","Treatment","Variety","pH_value","Nitrogen","Rep","Yld_FW_kg_ha","Harvested_Biomass_kg_ha","NOD_WT","Season"), 
                                c("country","adm1","treatment","variety","P_fertilizer","N_fertilizer","rep","yield","residue_yield","nodule_weight","season"))
  d2$crop <- "soybean"
  d2$K_fertilizer <- 0
  d2$plant_density <- 10000*d2$PLST
  d2$on_farm <- TRUE
  d2$is_survey <- FALSE
  d2$sn <- 1:160
  d2$trial_id <- paste(d2$sn,d2$adm1, d2$treatment, sep = "_")
  d2 <- d2[,c("country","adm1","treatment","crop","variety","N_fertilizer","P_fertilizer","K_fertilizer","rep","yield","residue_yield","plant_density","trial_id","on_farm","is_survey","season","nodule_weight")]
  
  
  common_colnames <- intersect(colnames(d1), colnames(d2))
  
  #split d1 and d2, then rbind, then merge
  v <- subset(d1, select = common_colnames)
  v1 <- subset(d1, select = c("trial_id","inoculated","flowering","grain_weight"))
  
  v2 <- subset(d2, select = common_colnames)
  v3 <- subset(d2, select = c("trial_id","nodule_weight"))
  
  dd1 <- rbind(v, v2)
  
  dd1 <- merge(dd1,v1, all.x = T)
  dd1 <- merge(dd1,v3, all.x = T)
  
  
  
  t <- data.frame(country = c("Malawi","Mozambique","Mozambique", "Zambia","Zambia","Malawi","Mozambique"),
                  adm1 = c("Bvumbwe","Angonia","Ruace", "Chipata","Lusaka","Chitedze","Namarripe"),
                  longitude = c(35.0267,34.1445,36.7011, 32.6458,29.3143,33.6538, 38.92),
                  latitude = c(-15.9428,-14.7690, -15.1963,-13.6391,-15.3066,-13.9788,-15.23056))
  
  dd1 <- merge(dd1, t, by = c("country","adm1"), all.x = TRUE, sort = FALSE)
  
  
  dd1 <- dd1[, c("country","adm1","treatment","crop","variety","N_fertilizer","P_fertilizer","K_fertilizer","rep","yield","plant_density","residue_yield","on_farm","is_survey","season","inoculated","flowering",
                 "grain_weight","nodule_weight","longitude","latitude","trial_id")]
  
  #start and end date info obtained from the dictonary 
  dd1$planting_date <- ifelse(dd1$season =="Y1617S", "2016", "2017")
  dd1$harvest_date  <- ifelse(dd1$season =="Y1617S", "2017", "2018")
  dd1$yield_part <- "seed"
  dd1$dataset_id <- dataset_id
  
  
  
  #what is the nodule_weight unit you are using? efyrouwa: nodule weight here is dry weight in milligrams
  
  # all scripts must end like this
  
  carobiner::write_files(dset, dd1, path=path)
  
   

}
  
   