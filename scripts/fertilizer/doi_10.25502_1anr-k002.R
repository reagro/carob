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
  
  uri <- "doi.org/10.25502/1anr-k002"
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
  d1 <- d[,c("trial_id","treatment1","row_spacing_crop_1_plot_1.cm","plant_spacing_crop_1_plot_1.cm","no_plants_hole_crop_1_plot_1.nr","width_of_harvested_plot_crop_1_plot_1.m","number_of_rows_crop_1_plot_1.nr","grain_weight_crop_1_plot_1.kg","pod_weight_groundnut_crop_1_plot_1.kg","above_ground_biomass_weight_crop_1_plot_1.kg")]
  d2 <- d[,c("trial_id","treatment2","row_spacing_crop_1_plot_2.cm","plant_spacing_crop_1_plot_2.cm","no_plants_hole_crop_1_plot_2.nr","width_of_harvested_plot_crop_1_plot_2.m","number_of_rows_crop_1_plot_2.nr","grain_weight_crop_1_plot_2.kg","pod_weight_groundnut_crop_1_plot_2.kg","above_ground_biomass_weight_crop_1_plot_2.kg")]
  d3 <- d[,c("trial_id","treatment3","row_spacing_crop_1_plot_3.cm","plant_spacing_crop_1_plot_3.cm","no_plants_hole_crop_1_plot_3.nr","width_of_harvested_plot_crop_1_plot_3.m","number_of_rows_crop_1_plot_.nr","grain_weight_crop_1_plot_3.kg","pod_weight_groundnut_crop_1_plot_3.kg","above_ground_biomass_weight_crop_1_plot_3.kg")]
  d4 <- d[,c("trial_id","treatment4","row_spacing_crop_1_plot_4.cm","plant_spacing_crop_1_plot_4.cm","no_plants_hole_crop_1_plot_4.nr","width_of_harvested_plot_crop_1_plot_4.m","number_of_rows_crop_1_plot_4.nr","grain_weight_crop_1_plot_4.kg","pod_weight_groundnut_crop_1_plot_4.kg","above_ground_biomass_weight_crop_1_plot_4.kg")]
  d5 <- d[,c("trial_id","treatment5","row_spacing_crop_1_plot_5.cm","plant_spacing_crop_1_plot_5.cm","no_plants_hole_crop_1_plot_5.nr","width_of_harvested_plot_crop_1_plot_5.m","number_of_rows_crop_1_plot_5.nr","grain_weight_crop_1_plot_5.kg","pod_weight_groundnut_crop_1_plot_5.kg","above_ground_biomass_weight_crop_1_plot_5.kg")]
  d6 <- d[,c("trial_id","treatment6","row_spacing_crop_1_plot_6.cm","plant_spacing_crop_1_plot_6.cm","no_plants_hole_crop_1_plot_6.nr","width_of_harvested_plot_crop_1_plot_6.m","number_of_rows_crop_1_plot_6.nr","grain_weight_crop_1_plot_6.kg","pod_weight_groundnut_crop_1_plot_6.kg","above_ground_biomass_weight_crop_1_plot_6.kg")]
  d7 <- d[,c("trial_id","treatment7","row_spacing_crop_1_plot_7.cm","plant_spacing_crop_1_plot_7.cm","no_plants_hole_crop_1_plot_7.nr","width_of_harvested_plot_crop_1_plot_7.m","number_of_rows_crop_1_plot_7.nr","grain_weight_crop_1_plot_7.kg","pod_weight_groundnut_crop_1_plot_7.kg","above_ground_biomass_weight_crop_1_plot_7.kg")]
  d8 <- d[,c("trial_id","treatment8","row_spacing_crop_1_plot_8.cm","plant_spacing_crop_1_plot_8.cm","no_plants_hole_crop_1_plot_8.nr","width_of_harvested_plot_crop_1_plot_8.m","number_of_rows_crop_1_plot_8.nr","grain_weight_crop_1_plot_8.kg","pod_weight_groundnut_crop_1_plot_8.kg","above_ground_biomass_weight_crop_1_plot_8.kg")]
  d9 <- d[,c("trial_id","treatment9","row_spacing_crop_1_plot_9.cm","plant_spacing_crop_1_plot_9.cm","no_plants_hole_crop_1_plot_9.nr","width_of_harvested_plot_crop_1_plot_9.m","number_of_rows_crop_1_plot_9.nr","grain_weight_crop_1_plot_9.kg","pod_weight_groundnut_crop_1_plot_9.kg","above_ground_biomass_weight_crop_1_plot_9.kg")]
  d10 <- d[,c("trial_id","treatment10","row_spacing_crop_1_plot_10.cm","plant_spacing_crop_1_plot_10.cm","no_plants_hole_crop_1_plot_10.nr","width_of_harvested_plot_crop_1_plot_10.m","number_of_rows_crop_1_plot_10.nr","grain_weight_crop_1_plot_10.kg","pod_weight_groundnut_crop_1_plot_10.kg","above_ground_biomass_weight_crop_1_plot_10.kg")]
  d11 <- d[,c("trial_id","treatment11","row_spacing_crop_1_plot_11.cm","plant_spacing_crop_1_plot_11.cm","no_plants_hole_crop_1_plot_11.nr","width_of_harvested_plot_crop_1_plot_11.m","number_of_rows_crop_1_plot_11.nr","grain_weight_crop_1_plot_11.kg","pod_weight_groundnut_crop_1_plot_11.kg","above_ground_biomass_weight_crop_1_plot_11.kg")]
  d12 <- d[,c("trial_id","treatment12","row_spacing_crop_1_plot_12.cm","plant_spacing_crop_1_plot_12.cm","no_plants_hole_crop_1_plot_12.nr","width_of_harvested_plot_crop_1_plot_12.m","number_of_rows_crop_1_plot_12.nr","grain_weight_crop_1_plot_12.kg","pod_weight_groundnut_crop_1_plot_12.kg","above_ground_biomass_weight_crop_1_plot_12.kg")]
  
  # removing the rows with no entries
  d7 <- d7[!is.na(d7$treatment7),]
  d8 <- d8[!is.na(d8$treatment8),]
  d9 <- d9[!is.na(d9$treatment9),]
  d10<- d10[!is.na(d10$treatment10),]
  d11<- d11[!is.na(d11$treatment11),]
  d12<- d12[!is.na(d12$treatment12),]

  new_names <- c("trial_id","treatment","row_spacing","plant_spacing","plants_hole","width_harvest","no_rows","grain_weight","pod_weight","residue_yield")
  
  # changing the column names 
  colnames(d1) <- new_names
  colnames(d2) <- new_names
  colnames(d3) <- new_names
  colnames(d4) <- new_names
  colnames(d5) <- new_names
  colnames(d6) <- new_names
  colnames(d7) <- new_names
  colnames(d8) <- new_names
  colnames(d9) <- new_names
  colnames(d10) <- new_names
  colnames(d11) <- new_names
  colnames(d12) <- new_names
  
  #stacking the rows
  dd1 <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
  
  dd1$row_spacing <- as.numeric(dd1$row_spacing)
  dd1$plant_spacing <- as.numeric(dd1$plant_spacing)
  
  #subsetting for the other variables of interest
  dd2 <- d[,c("trial_id","submissiondate","start","irrigated","on_farm","is_survey","end","country","lga_district_woreda","sector_ward","legume_planted_in_the_n2africa_trial","crop_1_previous_season","inoculation_n2africa_field")]

  dd3 <- merge(dd2,dd1, by = "trial_id", all = TRUE) 
  
  
 # bringing in the groundnut yield entries with the rest of the yield entries 
  for (i in 1:length(dd3$grain_weight)){
    if(is.na(dd3$grain_weight[i]) & !is.na(dd3$pod_weight[i])){
     k<-dd3$pod_weight[i]
     dd3$grain_weight[i]<- k
    }
  }
    
  dd3$inoculated <- ifelse(dd3$inoculation_n2africa_field == "y", TRUE, FALSE)
  v <- carobiner::replace_values(dd3$legume_planted_in_the_n2africa_trial,c("soya_bean", "climbing_bean", "bush_bean"),
                                 c("soybean","common bean","common bean"))
  dd3$crop <- v
  dd3$start_date <- dd3$start
  dd3$end_date <- dd3$end
  v <- carobiner::fix_name(dd3$country, case = "title")
  dd3$country <- v
  v <- carobiner::fix_name(dd3$lga_district_woreda, case = "title")
  dd3$adm1 <- v
  v <- carobiner::fix_name(dd3$sector_ward, case = "title")
  dd3$site <- v
  dd3$dataset_id <- dataset_id
  
 #to get the varieties and the fertilizer inputs
  ft <- strsplit(dd3$treatment , "[,+]+")
  
  for (i in 1:max_splits) {
    split_list[[i]] <- sapply(ft, function(x) ifelse(length(x) >= i, x[i], NA))
  }
  dd4<- cbind(dd3, as.data.frame(split_list))
  
  #to get the latitude 
  lat_long <- d[, c("trial_id","gps_latitude_field.decimal_degrees","gps_field_device_latitude.decimal_degrees","gps_field_device_longitude.decimal_degrees","gps_longitude_field.decimal_degrees")]
  
  for (i in 1:length(lat_long$gps_latitude_field.decimal_degrees)){
   if(is.na(lat_long$gps_latitude_field.decimal_degrees[i]) & !is.na(lat_long$gps_field_device_latitude.decimal_degrees[i])){
     k<-lat_long$gps_field_device_latitude.decimal_degrees[i]
     lat_long$gps_latitude_field.decimal_degrees[i]<- k
    }
  }
  
  # to get the longitude
  for (i in 1:length(lat_long$gps_longitude_field.decimal_degrees)){
   if(is.na(lat_long$gps_longitude_field.decimal_degrees[i]) & !is.na(lat_long$gps_field_device_longitude.decimal_degrees[i])){
     k<-lat_long$gps_field_device_longitude.decimal_degrees[i]
     lat_long$gps_longitude_field.decimal_degrees[i]<- k
    }
  }
  
  lat_long <- lat_long[, c("trial_id","gps_latitude_field.decimal_degrees","gps_longitude_field.decimal_degrees")]  

   
 
  # The NAs in the lat and lon comes to 109 entries, find a way of doing this efficiently
  n1 <- which(is.na(lat_long$gps_latitude_field.decimal_degrees & lat_long$gps_longitude_field.decimal_degrees))
  n2 <- lat_long[n1, ]
  
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
  
  dd5$N_fertilizer <-0
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
  
  
  
  
  
  
  
  
  

  

  
  
