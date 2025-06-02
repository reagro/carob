# R script for "carob"


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
  
  uri <- "doi:10.25502/jnpx-d405"
  group <- "fertilizer"
  ff <- carobiner::get_data(uri, path, group)
  

  
  meta <- data.frame(
  	carobiner::get_metadata(uri, path, group, major=1, minor=0),
    project="N2Africa",
    publication= NA,
    data_institute = "IITA",
    carob_contributor="Rachel Mukami",
    carob_date="2023-07-25",
    data_type="on_farm survey demonstration trials"
  )
  
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
  
  # # EGB: add plot size using country protocols
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # !!!!!!!!!!!! Please Review !!!!!!!!!!!!!
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  b3$plotsize <- NA
  b3$plotsize[b3$country == "Nigeria" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2014] <- 0.01 # In protocol (2014) plot size is indicated 10m*10m = 100m2 = 0.01ha
  b3$plotsize[b3$country == "Nigeria" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2015 & b3$crop %in% c("cowpea", "groundnut")] <- 0.375 # In protocol (2015) plot size is indicated 75m*50m = 3750m2 = 0.375ha
  b3$plotsize[b3$country == "Nigeria" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2015 & b3$crop %in% c("soybean")] <- 0.125 # In protocol (2015) plot size is indicated 50m*25m = 1250m2 = 0.125ha
  b3$plotsize[b3$country == "Nigeria" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2016 & b3$crop %in% c("soybean")] <- 0.01 # In protocol (2016) plot size is indicated 10m*10m = 100m2 = 0.01ha
  b3$plotsize[b3$country == "Uganda" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2014] <- 0.0025 # In protocol plot size is indicated 5m*5m = 25m2 = 0.0025ha
  b3$plotsize[b3$country == "Uganda" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2017 & b3$crop %in% c("common bean")] <- 0.0036 # In protocol plot size is indicated 6m*6m = 36m2 = 0.0036ha
  b3$plotsize[b3$country == "Uganda" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2017 & b3$crop %in% c("soybean")] <- 0.01 # In protocol plot size is indicated 10m*10m = 100m2 = 0.01ha
  b3$plotsize[b3$country == "Ghana" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2017] <- 0.01 # In protocol (2017) plot size is indicated 10m105m = 100m2 = 0.01ha
  b3$plotsize[b3$country == "Ghana" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2015] <- 0.012 # In protocol (2015) plot size is indicated 12m*10m = 120m2 = 0.012ha
  b3$plotsize[b3$country == "Ethiopia" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2015] <- 0.01 # In protocol (2015) plot size is indicated 10m*10m = 100m2 = 0.01ha
  b3$plotsize[b3$country == "Tanzania" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2015 & b3$crop %in% c("common bean")] <- 0.0036 # In protocol (2015) plot size is indicated 6m*6m = 36m2 = 0.0036ha
  b3$plotsize[b3$country == "Tanzania" & format(as.Date(b3$planting_date, "%Y-%m-%d"), "%Y") == 2015 & b3$crop %in% c("groundnut")] <- 0.01 # In protocol (2015) plot size is indicated 10m*10m = 36m2 = 0.01ha
  
  # Grain weight is used as yield. It is in kg/plot. Plot size is unknown hence yield value in kg/ha can't be extrapolated.
  
  b3$yield <- as.numeric(b3$grain_weight_kg)/b3$plotsize
  
  # Above ground biomass is assumed to be wet and not dry, hence used as residue yield. It is in kg/plot. 
  # Plot size is unknown hence residue yield value in kg/ha can't be extrapolated.
  
  b3$fwy_residue <- as.numeric(b3$`above_ground_biomass_kg/plot`)/b3$plotsize
  b3$dataset_id <- dataset_id
  b3$previous_crop[b3$previous_crop == "other"] <- NA

  # subset variables of interest 
  
  z <- b3[,c("dataset_id","trial_id","country","adm1","adm2","adm3","location","site","latitude","longitude","elevation",
             "crop","variety","variety_type","treatment","planting_date","harvest_date","inoculated",
             "previous_crop","row_spacing","plant_spacing","fertilizer_type","N_fertilizer","P_fertilizer",
             "K_fertilizer","fwy_residue","yield_part","yield","on_farm","is_survey")]
  
  # dropping inputs without treatment, residue yield and yield information
  i <- complete.cases(z[,c("treatment","fwy_residue","yield"),])
  z <- z[i,]
  
  # correcting spacing variables that are out of bounds
  z$row_spacing <- ifelse(z$row_spacing > 150,NA,z$row_spacing)
  z$row_spacing[z$trial_id == "3f6ebe42-9306-4e7f-a47b-a1549b24f97b"] <- 75
  
  z$plant_spacing <- ifelse(z$plant_spacing > 100,NA,z$plant_spacing)
  z$plant_spacing[z$trial_id == "3f6ebe42-9306-4e7f-a47b-a1549b24f97b"] <- 10
  
  z$adm2[z$adm2 == "Bwari Area Council"] <- "Bwari"
  z$location[z$location == "Epdra Saboba"] <- "Saboba"
  
  # extracting coordinates from geocode
  # coords(z)
  # z <- hh[,1:30]
  # Adding the processed coordinates
  zz <- data.frame(country = c("Nigeria", "Malawi", "Kenya", "Kenya", 
                               "Uganda", "Tanzania", "Uganda", "Uganda", "Uganda", "Uganda", 
                               "Tanzania", "Tanzania", "Tanzania", "Nigeria", "Nigeria", "Tanzania", 
                               "Nigeria", "Nigeria", "Ghana", "Ghana", "Nigeria", "Nigeria", 
                               "Nigeria", "Tanzania", "Nigeria", "Nigeria", "Nigeria", "Uganda", 
                               "Uganda", "Uganda", "Tanzania", "Uganda", "Uganda", "Uganda", 
                               "Uganda", "Uganda", "Uganda", "Ghana", "Ghana", "Ghana", "Ghana", 
                               "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ghana", "Ethiopia", 
                               "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia"),
                   adm2 = c("Makurdi", 
                            "Dedza", "Matayos", "Bungoma Central", "Kanungu", "Lushoto", 
                            "Kapchorwa", "Bugiri", "Kapchorwa", "Kibaale", "Mvomero", "Moshi", 
                            "Rombo", "Hawul", "Kwaya Kusar", "Gairo", "Soba", "Biu", "Yendi", 
                            "Savelugu", "Mokwa", "Bayo", "Bwari", "Kilosa", "Kauru", "Chikun", 
                            "Paikoro", "Bukedea", "Arua", "Kumi", "Kongwa", "Yumbe", "Gulu", 
                            "Zombo", "Lira", "Kisoro", "Nebbi", "Chereponi", "Chereponi", 
                            "Chereponi", "Chereponi", "Chereponi", "Chereponi", "Chereponi", 
                            "Chereponi", "Chereponi", "Chereponi", "Damot Gale", "Halaba", 
                            "Soddo", "Pawe", "Boricha"),
                   lat = c(7.73375, -14.3779, 
                           0.35958, 0.78007, -0.75, -4.54528, 1.3333, 0.5333, 1.3333, 1.42123, 
                           -6.3, -3.35, -3.09292, 10.4331, 10.44317, -6.13841, 10.87005, 
                           10.61285, 9.44272, 9.62441, 9.2, 10.42254, 9.218, -6.83333, 10.22988, 
                           10.315, 9.4681, 1.36667, 3.02013, 1.5, -6.2, 3.52354, 2.77457, 
                           2.52031, 2.2499, -1.28538, 2.47826, 10.13417, 10.13417, 10.13417, 
                           10.13417, 10.13417, 10.13417, 10.13417, 10.13417, 10.13417, 10.13417, 
                           7, 7.45, 8.7, 11.3333, 8.361),
                   lon = c(8.52139, 34.33322, 
                           34.17005, 34.5528, 29.73, 38.43927, 34.42, 33.79, 34.42, 31.51593, 
                           37.45, 37.33333, 37.45626, 12.24682, 11.99089, 36.88079, 7.96443, 
                           12.19458, -0.00991, -0.8253, 5.33333, 11.70624, 7.408, 36.98333, 
                           8.29422, 7.274, 6.85565, 34.13333, 30.91105, 33.95, 36.417, 31.28243, 
                           32.29899, 30.88824, 32.89985, 29.68497, 31.08893, 0.28806, 0.28806, 
                           0.28806, 0.28806, 0.28806, 0.28806, 0.28806, 0.28806, 0.28806, 
                           0.28806, 37.83333, 38.052, 38.38333, 36.3333, 36.647))
  z <- merge(z, zz, by = c("country", "adm2"))
  z$latitude <- ifelse(is.na(z$latitude), z$lat, z$latitude)
  z$longitude <- ifelse(is.na(z$longitude), z$lon, z$longitude)
  
  # final dataset
  z <- z[,c("dataset_id","trial_id","country","adm1","adm2","adm3","location","site","latitude","longitude","elevation",
             "crop","variety","variety_type","treatment","planting_date","harvest_date","inoculated",
             "previous_crop","row_spacing","plant_spacing","fertilizer_type","N_fertilizer","P_fertilizer",
             "K_fertilizer","fwy_residue","yield_part","yield","on_farm","is_survey")]
  
  carobiner::write_files(meta, z, path=path)
  # resulting error is from pigeon pea's yield. It's low amount could result from lack of proper yield units due to unknown plot size .
}

## EGB: Updating the geocoding process
  ## USING GEOCODE TO EXTRACT COORDINATES
# v <- z
# p <- complete.cases(v[,c("country","adm2")])
# vv <- v[p,]
# v1 <- unique(vv[is.na(vv$latitude) | is.na(vv$longitude), c("country","adm2", "latitude", "longitude")])
# for (i in 1:nrow(v1)) {
#   ll <- carobiner::geocode(country = v1$country[i], adm1 = v1$adm2[i], location = v1$adm2[i], service = "geonames", username = "efyrouwa")
#   ii <- unlist(jsonlite::fromJSON(ll))
#   c <- as.integer(ii["totalResultsCount"][[1]])
#   v1$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
#   v1$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
# }
# v1$latitude[v1$adm2 == "Halaba"] <- 7.450 # https://www.google.com/maps/place/Halaba,+Ethiopia
# v1$longitude[v1$adm2 == "Halaba"] <- 38.052 # https://www.google.com/maps/place/Halaba,+Ethiopia
# v1$latitude[v1$adm2 == "Boricha"] <- 8.361 # https://www.google.com/maps/place/%E1%89%A6%E1%88%AD%E1%89%BB,+Ethiopia
# v1$longitude[v1$adm2 == "Boricha"] <- 36.647 # https://www.google.com/maps/place/%E1%89%A6%E1%88%AD%E1%89%BB,+Ethiopia
# sss <- dput(v1)
# 
# # Additional coordinate extraction from RM
# # coords <- function(z){
# # # Extracting NA coordinates from adm2 geocode coordinates
# # v <- z
# # p <- complete.cases(v[,c("country","adm2")])
# # vv <- v[p,]
# # v1 <- unique(vv[,c("country","adm2")])
# # gg <- carobiner::geocode(country = v1$country,location = v1$adm2,service = "nominatim")
# # gg1 <- gg[[1]]
# # names(gg1)[2] <- "adm2"
# # 
# # b <- merge(v,gg1,by = c("country","adm2"),all.x = TRUE)
# # b$latitude <- b$lat
# # b$longitude <- b$lon
# # 
# # # Extracting remaining NA coordinates from site geocode coordinates
# # pp <- b[is.na(b$latitude),]
# # 
# # b0 <- pp
# # p <- complete.cases(b0[,c("country","site")])
# # v1 <- unique(b0[p,c("country","site")])
# # gg <- carobiner::geocode(country = v1$country,location = v1$site,service = "nominatim")
# # gg1 <- gg[[1]]
# # names(gg1)[2] <- "site"
# # 
# # b1 <- merge(b[,1:30],gg1,by = c("country","site"),all.x = TRUE)
# # 
# # i <- is.na(b1$latitude)
# # j <- is.na(b1$longitude)
# # 
# # replacement_values <- b1$lat[i]
# # b1$latitude[i] <- replacement_values
# # 
# # replacement_values <- b1$lon[j]
# # b1$longitude[j] <- replacement_values
# # 
# # # Extracting remaining NA coordinates from location geocode coordinates
# # pp <- b1[is.na(b1$latitude),]
# # 
# # b2 <- pp
# # p <- complete.cases(b2[,c("country","location")])
# # v1 <- unique(b2[p,c("country","location")])
# # gg <- carobiner::geocode(country = v1$country,location = v1$location,service = "nominatim")
# # gg1 <- gg[[1]]
# # names(gg1)[2] <- "location"
# # 
# # bb <- merge(b1[,1:30],gg1,by = c("country","location"),all.x = TRUE)
# # 
# # i <- is.na(bb$latitude)
# # j <- is.na(bb$longitude)
# # 
# # replacement_values <- bb$lat[i]
# # bb$latitude[i] <- replacement_values
# # 
# # replacement_values <- bb$lon[j]
# # bb$longitude[j] <- replacement_values
# # 
# # # Extracting remaining NA coordinates from country geocode coordinates
# # tt <- bb[is.na(bb$latitude),]
# # v1 <- unique(tt[,"country"])
# # gg <- carobiner::geocode(country = v1,location = v1,service = "nominatim")
# # gg1 <- gg[[1]]
# # gg1 <- gg1[,c("country","lat","lon")]
# # 
# # hh <- merge(bb[,1:30],gg1,by = "country",all.x = TRUE)
# # 
# # i <- is.na(hh$latitude)
# # j <- is.na(hh$longitude)
# # 
# # replacement_values <- hh$lat[i]
# # hh$latitude[i] <- replacement_values
# # 
# # replacement_values <- hh$lon[j]
# # hh$longitude[j] <- replacement_values
# # }

