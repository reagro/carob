
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

	return(FALSE)
  
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
  d1 <- d1[,c("country","adm1","treatment","crop","variety","N_fertilizer","P_fertilizer","K_fertilizer","inoculated","rep","yield","residue_yield","flowering","grain_weight","plant_density","on_farm","is_survey","season")]
  
  # second data set
  d2 <- carobiner::change_names(d2, 
	c("Country","Loc","Treatment","Variety","pH_value","Nitrogen","Rep","Yld_FW_kg_ha","Harvested_Biomass_kg_ha","NOD_WT","Season"), 
	c("country","adm1","treatment","variety","P_fertilizer","N_fertilizer","rep","yield","residue_yield","nodule_weight","season"))
  d2$crop <- "soybean"
  d2$K_fertilizer <- 0
  d2$nodule_weight <- d2$nodule_weight
  d2$plant_density <- 10000*d2$PLST
  d2$on_farm <- TRUE
  d2$is_survey <- FALSE
 
## it makes no sense to use "merge" here. 
## the datasets are not for the same records.
## they have different records for different countries and need to be rbind-ed 
##  common_colnames <- intersect(colnames(d1), colnames(d2))
## dd1 <- merge(d1, d2, by = common_colnames, all = TRUE, sort = FALSE)

# but d2 is not ready for rbind
  dd <- rbind(d1, d2)
  
  # filling in the longitude and latitude 
  
  u <- unique(dd1[, c("country", "adm1")])
  ## g <- carobiner::geocode(country = u$country, location = u$adm1,service = "nominatim")
  ## you cannot have carobiner::geocode in a script. 
  ## you need to run this and then use dput on the data.frame with coordinates
  ## and add that to the script.
  
  g <- data.frame(g$df)
  g <- carobiner::change_names(g,c("location","lon","lat"),c("adm1","longitude","latitude"))
  
  #common_colnames <- intersect(colnames(dd1), colnames(g))
  # it is better to just spell out what the variables are
  dd <- merge(dd, g, by = common_colnames, all.x = TRUE, sort = FALSE)
  i <- which(dd1$adm1 == "Namarripe")
  dd$latitude[i] <- -15.23056
  dd$longitude[i] <- 38.92
  
  dd <- dd[, c("country","adm1","treatment","crop","variety","N_fertilizer","P_fertilizer","K_fertilizer","rep","yield","plant_density","residue_yield","on_farm","is_survey","season","inoculated","flowering",
                 "grain_weight","nodule_weight","longitude","latitude")]
                                                                                                                                                                                                                  
 #start and end date info obtained from the dictonary 
  dd$planting_date <- ifelse(dd$season =="Y1617S", "2016", "2017")
  dd$harvest_date  <- ifelse(dd$season =="Y1617S", "2017", "2018")
  dd$yield_part <- "seed"
  dd$dataset_id <- dataset_id
  dd$trial_id <- paste(dd$dataset_id, dd$treatment, sep = "_")
  
#  message("add to records: nodule_weight") = done
#  what is the nodule_weight unit you are using?
    # all scripts must end like this

    carobiner::write_files(dset, dd1, path=path)

}
  
   