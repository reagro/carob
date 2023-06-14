# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
  N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among 
  African smallholder farmers which will contribute to enhancing soil fertility, improving household 
  nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will 
  build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic 
  N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers 
  adapted to local settings. A strong national expertise in grain legume production and N2-fixation 
  research and development will be the legacy of the project.
 
  "
  
  uri <- "doi:10.25502/1anr-k002"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    publication= NA,
    data_institutions = "IITA",
    carob_contributor="Effie Ochieng'",
    experiment_type="___",
    has_weather=FALSE,
    has_soil=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=2)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "data_table.csv"]
  
  d <- read.csv(f)
  
  
  d$trial_id <- d$id
  d$irrigated <- FALSE 
  d$is_survey <- TRUE
  d$on_farm <- FALSE
  
  #subsetting each treatment with their respective observations into data different datasets then appending the rows later
  old_nms <- c("trial_id","treatmentX", "row_spacing_crop_1_plot_X.cm", "plant_spacing_crop_1_plot_X.cm","no_plants_hole_crop_1_plot_X.nr", "width_of_harvested_plot_crop_1_plot_X.m","number_of_rows_crop_1_plot_X.nr", "grain_weight_crop_1_plot_X.kg", "pod_weight_groundnut_crop_1_plot_X.kg","above_ground_biomass_weight_crop_1_plot_X.kg")
  new_names <- c("trial_id","treatment","row_spacing","plant_spacing","plants_hole","width_harvest","no_rows","grain_weight","pod_weight","residue_yield")

	old_nms3 = gsub("number_of_rows_crop_1_plot_X.nr", "number_of_rows_crop_1_plot_.nr", old_nms)

	lst <- lapply(1:12, function(i) {
			if (i == 3) {
				di <- d[, gsub("X", i, old_nms3)]
			} else {
				di <- d[, gsub("X", i, old_nms)]
			}
			colnames(di) <- new_names
			di
		})
	dd1 <- data.frame(do.call(rbind, lst))
	dd1 <- dd1[!is.na(dd1$treatment), ]
	
	dd1$row_spacing <- as.numeric(dd1$row_spacing)
	dd1$plant_spacing <- as.numeric(dd1$plant_spacing)
  
  #subsetting for the other variables of interest
	dd2 <- d[,c("trial_id","submissiondate","start","irrigated","on_farm","is_survey","end","country","lga_district_woreda","sector_ward","legume_planted_in_the_n2africa_trial","crop_1_previous_season","inoculation_n2africa_field")]

	dd3 <- merge(dd2, dd1, by = "trial_id", all = TRUE) 
  
  
 # bringing in the groundnut yield entries with the rest of the yield entries 
	i <- is.na(dd3$grain_weight) & !is.na(dd3$pod_weight)
	dd3$grain_weight[i] <- dd3$pod_weight[i]
    
	dd3$inoculated <- ifelse(dd3$inoculation_n2africa_field == "y", TRUE, FALSE)
	dd3$crop <- carobiner::replace_values(dd3$legume_planted_in_the_n2africa_trial, c("soya_bean", "climbing_bean", "bush_bean"), c("soybean","common bean","common bean"))
	dd3$start_date <- dd3$start
	dd3$end_date <- dd3$end
	dd3$country <- carobiner::fix_name(dd3$country, case = "title")
	dd3$adm1 <- carobiner::fix_name(dd3$lga_district_woreda, case = "title")
	dd3$site <- carobiner::fix_name(dd3$sector_ward, case = "title")

 #to get the varieties and the fertilizer inputs
	ft <- strsplit(dd3$treatment , "[,+]+")
  
	max_splits <- max(sapply(ft, length))
	split_list <- list()
	for (i in 1:max_splits) {
		split_list[[i]] <- sapply(ft, function(x) ifelse(length(x) >= i, x[i], NA))
	}
	dd4 <- cbind(dd3, as.data.frame(split_list))
  
  #to get the latitude 
  lat_long <- d[, c("trial_id","gps_latitude_field.decimal_degrees","gps_field_device_latitude.decimal_degrees","gps_field_device_longitude.decimal_degrees","gps_longitude_field.decimal_degrees")]
  
	i <- is.na(lat_long$gps_latitude_field.decimal_degrees) & (!is.na(lat_long$gps_field_device_latitude.decimal_degrees))
	lat_long$gps_latitude_field.decimal_degrees[i] <- lat_long$gps_field_device_latitude.decimal_degrees[i]
  
  # to get the longitude
	i <- is.na(lat_long$gps_longitude_field.decimal_degrees) & (!is.na(lat_long$gps_field_device_longitude.decimal_degrees))
    lat_long$gps_longitude_field.decimal_degrees[i] <- lat_long$gps_field_device_longitude.decimal_degrees[i]
  
	lat_long <- lat_long[, c("trial_id", "gps_latitude_field.decimal_degrees","gps_longitude_field.decimal_degrees")]  

    
  # The NAs in the lat and lon comes to 109 entries, find a way of doing this efficiently
 # n1 <- which(is.na(lat_long$gps_latitude_field.decimal_degrees & lat_long$gps_longitude_field.decimal_degrees))
 # n2 <- lat_long[n1, ]
  
  lat_long$latitude <- lat_long$gps_latitude_field.decimal_degrees
  lat_long$longitude <- lat_long$gps_longitude_field.decimal_degrees
 
  dd5 <- merge(dd4, lat_long, by = "trial_id")
 
 
  #filling in the fertilizer info using the split columns
 
  ft <- c(dd5$c..TGX1951.3F....TGX1951.3F....TGX1951.3F....TGX1951.3F....TGX1951.3F...,dd5$c...I......I......control.....I......I......I.Only.....I......I....,dd5$c...I......I......control.....I......I......I.Only.....I......I....,dd5$c...K...NA..NA....K.....K...NA....K...NA....K...NA....K...NA..,dd5$c...Micronutrients...NA..NA..NA....Micronutr...NA....Micronutr...,dd5$c.NA..NA..NA..NA...OM...NA...OM...NA..NA..NA..NA..NA..NA..NA..,dd5$c.NA..NA..NA..NA...OM...NA...OM...NA..NA..NA..NA..NA..NA..NA..,
         dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...1,dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...2,dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...3,dd5$c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA...4)

  ft <- gsub(" ","",ft)
  ft <- gsub("POnly","P",ft)
  ft <- gsub("Manure","OM",ft)
  ft <- gsub("FYM","OM",ft)
  ft[ grepl("icro", ft) ] <- "micronutrients" 
  ft[ grepl("psum", ft) ] <- "gypsum" 
  ft[ grepl("rol", ft) ] <- "control" 
  ft[ grepl("OM", ft) ] <- "OM" 

  # making the rows in the vector the same as the rows
  num_rows <- nrow(dd5)
  ft <- ft[1:num_rows]
  dd5$mix <- ft
  
  dd5$N_fertilizer <- 0
  dd5$P_fertilizer <- 0
  dd5$K_fertilizer <- 0
  dd5$Zn_fertilizer <-0
  dd5$S_fertilizer <- 0
  
  dd5$variety <- ifelse(dd5$mix %in% c("TGX1951-3F","Nabe12C","TGX1955-4F","Pendo","Tumaini","SAMNUT24","Songotra","Maksoy3N", "Lyamungu90",
                                              "Local","Samnut23","SAMNUT22","Jenguma","Minjingu"),dd5$mix,NA) 
  dd5$P_fertilizer <- ifelse(dd5$mix %in% c("P","PK","NPK"),30,0)
  dd5$K_fertilizer <- ifelse(dd5$mix %in% c("NPK","PK","K"),20,0)
  dd5$OM_used <- ifelse(dd5$mix %in% "OM", TRUE, FALSE)
  dd5$Zn_fertilizer <- ifelse(dd5$mix %in%  "Zn",2.5,0)
  dd5$S_fertilizer <- ifelse(dd5$mix %in%  "S", 12, 0)
  dd5$S_fertilizer <- ifelse(dd5$mix %in%  "S", 12, 0)
  
  # putting in the yield information
    # 1) get the length and width of farm size in meters
     dd5$length <- ((dd5$row_spacing)*(dd5$no_rows-1))/100
     
     
     #2) get plot size in m2
     dd5$plot_size <- dd5$length * dd5$width_harvest
     
     #3) get the yield/ha
     dd5$yield <- (10000*dd5$grain_weight)/dd5$plot_size
     dd5$residue_yield <- (10000*dd5$residue_yield)/dd5$plot_size
     
  dd5$dataset_id <- dataset_id
  dd5$start_date <- as.character(as.Date(dd5$start_date, format = "%d/%m/%Y"))
  dd5$end_date <- as.character(as.Date(dd5$end_date, format = "%d/%m/%Y"))

  dd5 <- dd5[, c("trial_id","dataset_id","irrigated","on_farm","is_survey","country","adm1","site","latitude","longitude","start_date","end_date","crop","variety","inoculated","row_spacing","N_fertilizer","P_fertilizer","K_fertilizer","Zn_fertilizer","S_fertilizer","OM_used","plant_spacing","yield","residue_yield")]
  
  # all scripts must end like this
  carobiner::write_files(dset, dd5, path, dataset_id, group)
  
  }  
  
  
  
  
  
  
  
  
  

  

  
  
