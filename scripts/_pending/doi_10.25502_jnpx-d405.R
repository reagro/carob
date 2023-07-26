
carob_script <- function(path){
  
  ### yield is given in kg/plot with unknown plot sizes hence can't be assumed to be in kg/ha
  "
 Title: N2Africa demonstration trial, 2012 - 2019
 
 Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity 
 of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, 
 improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, 
 N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit 
 from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants 
 and fertilizers adapted to local settings. A strong national expertise in grain legume production and 
 N2-fixation research and development will be the legacy of the project.The project is implemented in 
 five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, 
 Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
  "
  
  uri <- "doi.org/10.25502/jnpx-d405"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  ## dataset level data
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="N2Africa",
    uri=uri,
    publication= NA,
    data_citation = "Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa demonstration trial, 2012 - 2019 [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/JNPX-D405",
    data_institutions = "IITA",
    carob_contributor="Rachel Mukami",
    data_type="on_farm survey demonstration trials"
  )
  
  ## download and read data
  
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  
  ###download and read data
  
  f <- ff[basename(ff) == "data_table.csv"]
  d <- data.frame(read.csv(f))
  
  r0 <- d
  
  r0$trial_id <- r0$id
  r0$planting_date <- as.character(as.Date(r0$start, format = "%m/%d/%Y"))
  r0$harvest_date <- as.character(as.Date(r0$end, format = "%m/%d/%Y"))
  
  # fixing planting dates that occur after harvesting dates
  m <- which(r0$planting_date > r0$harvest_date)
  r0$planting_date[m] <- as.character(as.Date(r0$date_hhsurvey_1.date[m], format = "%m/%d/%Y")) 
  r0$harvest_date[m] <- as.character(as.Date(r0$submissiondate[m], format = "%m/%d/%Y")) 
  
  r0$crop <- carobiner::fix_name(r0$legume_planted_in_the_n2africa_trial,"lower")
  
  r0$country <- carobiner::fix_name(r0$country,"title")
  r0$adm1 <- as.character(NA)
  r0$adm2 <- carobiner::fix_name(r0$lga_district_woreda,"title")
  r0$adm2[r0$adm2 == "Other"] <- NA
  r0$adm3 <- as.character(NA)
  r0$location <- carobiner::fix_name(r0$other_lga_district_woreda,"title")
  r0$site <- carobiner::fix_name(r0$sector_ward,"title")
  
  r0$elevation <- as.numeric(r0$gps_altitude_field.m)
  r0$elevation <- ifelse(is.na(r0$elevation),r0$gps_field_device_altitude.m,r0$elevation)
  
  r0$latitude <- as.numeric(r0$gps_latitude_field.decimal_degrees)
  r0$latitude <- ifelse(is.na(r0$latitude),r0$gps_field_device_latitude.decimal_degrees,r0$latitude)
  
  r0$longitude <- as.numeric(r0$gps_longitude_field.decimal_degrees)
  r0$longitude <- ifelse(is.na(r0$longitude),r0$gps_field_device_longitude.decimal_degrees,r0$longitude)
  
  r0$previous_crop <- r0$crop_1_previous_season
  
  r0$cntry <- carobiner::fix_name(r0$country,"title")
  
  r0$inoculated <- ifelse(r0$inoculation_n2africa_field == "y",TRUE,FALSE)
  
  # converting from wide to long based on treatments and their characteristics
  
  vars <- c("spacing", "grain", "biomass")
  i <- grepl(paste(vars, collapse = "|"), names(r0))
  gh <- names(r0)[i]
  ghh <- gh[1:48] # more information on treatments 1 - 12 properties
  
  rw <- grepl("row", ghh)
  plt <- grepl("plant", ghh)
  grn <- grepl("grain", ghh)
  mass <- grepl("biomass", ghh)
  
  treatments <- names(r0)[16:27]
  
  # converting from wide to long
  b3 <- reshape(r0,
                direction = "long",
                varying = list(c(treatments),
                               ghh[rw],
                               ghh[plt],
                               ghh[grn],
                               ghh[mass]),
                v.names = c("treatment", "row_spacing_cm", "plant_spacing_cm", "grain_weight_kg", "above_ground_biomass_kg/plot"),
                idvar = "id",
                timevar = "treatment_number")
  
  # subsetting to variables of interest
  b3 <- b3[,291:ncol(b3)]
  rownames(b3) <- NULL
  
  b3$variety <- as.character(NA)
  b3$variety_type <- NA
  b3$variety_type <- ifelse(b3$crop %in% c("climbing_bean","bush_bean"),b3$crop,NA)
  
  names(b3)[14] <- "country"
  
  b3$fertilizer_type <- as.character(NA)
  b3$N_fertilizer <- 0
  b3$P_fertilizer <- 0
  b3$K_fertilizer <- 0
  
  b3$yield_part <- "seed"
  b3$yield <- NA
  
  b3$crop <- carobiner::replace_values(b3$crop,
                                       c("soya_bean","pigeon_pea","faba_bean","climbing_bean","bush_bean"),
                                       c("soybean","pigeon pea","faba bean","common bean","common bean"))
  
  b3$previous_crop <- carobiner::replace_values(b3$previous_crop,
                                                c("soyabean","sweet_potato","pigeon_pea","irish_potato","bush_bean","climbing_bean","bambara_bean","fallow","green_gram"),
                                                c("soybean","sweetpotato","pigeon pea","potato","common bean","common bean","bambara groundnut","no crop","mung bean"))
  
  b3$on_farm <- TRUE
  b3$is_survey <- TRUE
  
  names(b3)[18] <- "row_spacing"
  names(b3)[19] <- "plant_spacing"
  b3$row_spacing <- as.numeric(b3$row_spacing)
  b3$plant_spacing <- as.numeric(b3$plant_spacing)
  
  # Grain weight is used as yield. It is in kg/plot. Plot size is unknown hence yield value in kg/ha can't be extrapolated.
  
  b3$yield <- as.numeric(b3$grain_weight_kg) 
  
  # Above ground biomass is assumed to be wet and not dry, hence used as residue yield. It is in kg/plot. 
  # Plot size is unknown hence residue yield value in kg/ha can't be extrapolated.
  
  b3$residue_yield <- as.numeric(b3$`above_ground_biomass_kg/plot`)
  b3$dataset_id <- dataset_id
  b3$previous_crop[b3$previous_crop == "other"] <- NA

  # subset variables of interest 
  
  z <- b3[,c("dataset_id","trial_id","country","adm1","adm2","adm3","location","site","latitude","longitude","elevation",
             "crop","variety","variety_type","treatment","planting_date","harvest_date","inoculated",
             "previous_crop","row_spacing","plant_spacing","fertilizer_type","N_fertilizer","P_fertilizer",
             "K_fertilizer","residue_yield","yield_part","yield","on_farm","is_survey")]
  
  # dropping inputs without treatment, residue yield and yield information
  i <- complete.cases(z[,c("treatment","residue_yield","yield"),])
  z <- z[i,]
  
  # correcting spacing variables that are out of bounds
  z$row_spacing <- ifelse(z$row_spacing > 150,NA,z$row_spacing)
  z$row_spacing[z$trial_id == "3f6ebe42-9306-4e7f-a47b-a1549b24f97b"] <- 75
  
  z$plant_spacing <- ifelse(z$plant_spacing > 100,NA,z$plant_spacing)
  z$plant_spacing[z$trial_id == "3f6ebe42-9306-4e7f-a47b-a1549b24f97b"] <- 10
  
  z$adm2[z$adm2 == "Bwari Area Council"] <- "Bwari"
  z$location[z$location == "Epdra Saboba"] <- "Saboba"
  
  # extracting coordinates from geocode
  coords(z)
  z <- hh[,1:30]
  
  # final dataset
  z <- z[,c("dataset_id","trial_id","country","adm1","adm2","adm3","location","site","latitude","longitude","elevation",
             "crop","variety","variety_type","treatment","planting_date","harvest_date","inoculated",
             "previous_crop","row_spacing","plant_spacing","fertilizer_type","N_fertilizer","P_fertilizer",
             "K_fertilizer","residue_yield","yield_part","yield","on_farm","is_survey")]
  
  carobiner::write_files(dset, z, path=path)
  # resulting error is from pigeon pea's yield. It's low amount could result from lack of proper yield units due to unknown plot size .
}


## USING GEOCODE TO EXTRACT COORDINATES
coords <- function(z){

# Extracting NA coordinates from adm2 geocode coordinates
v <- z
p <- complete.cases(v[,c("country","adm2")])
vv <- v[p,]
v1 <- unique(vv[,c("country","adm2")])
gg <- carobiner::geocode(country = v1$country,location = v1$adm2,service = "nominatim")
gg1 <- gg[[1]]
names(gg1)[2] <- "adm2"

b <- merge(v,gg1,by = c("country","adm2"),all.x = TRUE)
b$latitude <- b$lat
b$longitude <- b$lon

# Extracting remaining NA coordinates from site geocode coordinates
pp <- b[is.na(b$latitude),]

b0 <- pp
p <- complete.cases(b0[,c("country","site")])
v1 <- unique(b0[p,c("country","site")])
gg <- carobiner::geocode(country = v1$country,location = v1$site,service = "nominatim")
gg1 <- gg[[1]]
names(gg1)[2] <- "site"

b1 <- merge(b[,1:30],gg1,by = c("country","site"),all.x = TRUE)

i <- is.na(b1$latitude)
j <- is.na(b1$longitude)

replacement_values <- b1$lat[i]
b1$latitude[i] <- replacement_values

replacement_values <- b1$lon[j]
b1$longitude[j] <- replacement_values

# Extracting remaining NA coordinates from location geocode coordinates
pp <- b1[is.na(b1$latitude),]

b2 <- pp
p <- complete.cases(b2[,c("country","location")])
v1 <- unique(b2[p,c("country","location")])
gg <- carobiner::geocode(country = v1$country,location = v1$location,service = "nominatim")
gg1 <- gg[[1]]
names(gg1)[2] <- "location"

bb <- merge(b1[,1:30],gg1,by = c("country","location"),all.x = TRUE)

i <- is.na(bb$latitude)
j <- is.na(bb$longitude)

replacement_values <- bb$lat[i]
bb$latitude[i] <- replacement_values

replacement_values <- bb$lon[j]
bb$longitude[j] <- replacement_values

# Extracting remaining NA coordinates from country geocode coordinates
tt <- bb[is.na(bb$latitude),]
v1 <- unique(tt[,"country"])
gg <- carobiner::geocode(country = v1,location = v1,service = "nominatim")
gg1 <- gg[[1]]
gg1 <- gg1[,c("country","lat","lon")]

hh <- merge(bb[,1:30],gg1,by = "country",all.x = TRUE)

i <- is.na(hh$latitude)
j <- is.na(hh$longitude)

replacement_values <- hh$lat[i]
hh$latitude[i] <- replacement_values

replacement_values <- hh$lon[j]
hh$longitude[j] <- replacement_values
}

