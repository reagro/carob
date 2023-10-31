
carob_script <- function(path){
  
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
    carob_date="2023-07-25",
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
  
  b3$residue_yield <- as.numeric(b3$`above_ground_biomass_kg/plot`)/b3$plotsize
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
  
  # assigning lon and lat NAs in order to extract correct coordinates and get rid of mismatching country-coordinates 
  z$latitude <- NA
  z$longitude <- NA
  
  # filling NA coordinates using adm2

  zz <- data.frame(country = c("Ghana", "Nigeria", "Tanzania", "Ghana", 
                               "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", "Tanzania", "Ethiopia", 
                               "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", 
                               "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia", 
                               "Ethiopia", "Ghana", "Uganda", "Uganda", "Uganda", "Uganda", 
                               "Uganda", "Uganda", "Ghana", "Uganda", "Uganda", "Uganda", "Uganda", 
                               "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Ghana", "Ghana", 
                               "Uganda", "Uganda", "Uganda", "Ghana", "Uganda", "Uganda", "Uganda", 
                               "Uganda", "Nigeria", "Nigeria", "Tanzania"), 
                   adm2 = c("Binduri","Makurdi", "Moshi", "Nadowli", "Damot Gale", "Halaba", "Shala",
                            "Ada'a", "Lushoto", "Gimbichu", "Soddo", "Enemay", "Farta", "Goba",
                            "Boricha", "Yilmana Densa", "Sinana", "Agarfa", "Mandura", "Kersa", 
                            "Pawe", "Dibate", "Tiroafeta", "Yendi", "Bugiri", "Kabale", "Kanungu", 
                            "Kisoro", "Rakai", "Manafwa", "Bawku Municipal", "Kiryandongo", 
                            "Bukedea", "Lira", "Kapchorwa", "Kumi", "Apac", "Bulambuli", 
                            "Arua", "Oyam", "Wa-West", "Savelugu", "Adjumani", "Moyo", "Gulu", 
                            "Bawku West", "Yumbe", "Koboko", "Zombo", "Nebbi", "Hawul", "Biu","Kongwa"), 
                   lat = c(10.97214, 7.73375, -3.35, 10.28835, 7, 7.45, 7.48333, 8.58333, -4.54528, 9.0679, 
                           8.7, 10.66667, 12,7.01667, 8.361, 11.5, 7.08333, 7.28333, 11, 9.32314, 11.3333, 
                           10.65, 7.9167, 9.44272, 0.5333, -1.24857, -0.75, -1.28538, -0.7093, 
                            0.88333, 11.0616, 2.01568, 1.36667, 2.2499, 1.3333, 1.5, 1.97556, 
                           1.32055, 3.02013, 2.38129, 10.06069, 9.62441, 3.37786, 3.6444, 
                           2.77457, 10.91667, 3.52354, 3.5, 2.52031, 2.47826, 10.4331, 10.61285,-6.2), 
                   lon = c(-0.30837, 8.52139, 37.33333, -2.60889, 37.83333, 
                           38.052, 38.53333, 38.91667, 38.43927, 39.25945, 38.38333, 38, 
                           38, 39.98333, 36.647, 37.33333, 40.2, 39.81667, 36.25, 39.46395, 
                           36.3333, 36.21667, 37.3333, -0.00991, 33.79, 29.98993, 29.73, 
                           29.68497, 31.41309, 34.33333, -0.24169, 32.07034, 34.13333, 32.89985,
                           34.42, 33.95, 32.53861, 34.28062, 30.91105, 32.50071, -2.50192, 
                           -0.8253, 31.7909, 31.76276, 32.29899, -0.51667, 31.28243, 31, 
                           30.88824, 31.08893, 12.24682, 12.19458, 36.417))
  
  z <- merge(z, zz, by = c("country", "adm2"),all.x = TRUE)

  z$latitude <- ifelse(is.na(z$latitude), z$lat, z$latitude)
  z$longitude <- ifelse(is.na(z$longitude), z$lon, z$longitude)
  
  # filling NAs using location
  
  zzz <- data.frame(country = c("Ghana", "Ghana", "Uganda", "Uganda"), 
                    location = c("Kumbungu", "Karaga", "Nebbi", "Adjumani"), 
                    lat = c(9.43333, 9.90568, 2.47826, 3.37786), 
                    lon = c(-1.06667, -0.53521,31.08893, 31.7909))
  
  z <- merge(z[,1:30], zzz, by = c("country", "location"),all.x = TRUE)
  
  z$latitude <- ifelse(is.na(z$latitude), z$lat, z$latitude)
  z$longitude <- ifelse(is.na(z$longitude), z$lon, z$longitude)
  
  z$latitude[is.na(z$latitude) & z$site == "Logshegu"] <- 	9.43341645
  z$longitude[is.na(z$longitude) & z$site == "Logshegu"] <- -0.889753797926527 
  
  z$latitude[is.na(z$latitude) & z$site == "Zangbanllung-Kukuo"] <- 	8.8493897
  z$longitude[is.na(z$longitude) & z$site == "Zangbanllung-Kukuo"] <- 0.1586035 
  
  
  # filling NAs using country
  z$latitude[is.na(z$latitude) & z$country == "Ghana"] <- 8.0300284
  z$longitude[is.na(z$longitude) & z$country == "Ghana"] <- -1.0800271
  
  z$latitude[is.na(z$latitude) & z$country == "Nigeria" & z$site == "Naka"] <-  7.582744
  z$longitude[is.na(z$longitude) & z$country == "Nigeria" & z$site == "Naka"] <- 8.204895

  # assigning NAs to yield and residue yield that is above carob's maximum threshold
  z$yield[(z$crop == "common bean" & z$yield > 9000)] <- NA
  z$yield[(z$crop == "faba bean" & z$yield > 6500)] <- NA
  z$residue_yield[z$residue_yield > 60000] <- NA

  # final dataset
  z <- z[,c("dataset_id","trial_id","country","adm1","adm2","adm3","location","site","latitude","longitude","elevation",
             "crop","variety","variety_type","treatment","planting_date","harvest_date","inoculated",
             "previous_crop","row_spacing","plant_spacing","fertilizer_type","N_fertilizer","P_fertilizer",
             "K_fertilizer","residue_yield","yield_part","yield","on_farm","is_survey")]
  
  carobiner::write_files(dset, z, path=path)
}
