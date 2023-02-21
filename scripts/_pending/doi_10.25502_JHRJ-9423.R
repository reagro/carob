


#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

  uri <- "doi.org/10.25502/JHRJ-9423"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "variety_performance"
  
  
  dset <- data.frame (
    dataset_id = dataset_id,
    group = group,
    uri = uri,
    publication = NA,
    data_citation =" Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat,
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira,
  L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, 
  K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm 
  monitoring - Mozambique, 2011 - 2012 [Data set]. International 
  Institute of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/JHRJ-9423",
    carob_contributor = "Effie Ochieng",
    experiment_type = "variety_performance",
    has_weather = FALSE,
    has_management = FALSE)
  
  #extract the data
  ff <- carobiner::get_data(uri,path,group)
  js <- carobiner::get_metadata(dataset_id, path, group)
  dset$license <- carobiner::get_license(js)
  
  #read the data
  f <- ff[basename(ff) == "a_general.csv"]
  d <- data.frame(read.csv2(f, sep = ","))
  f1 <- ff[basename(ff)== "b_info_site_2.csv"]
  d1 <- data.frame(read.csv2(f1, sep = ","))
  f2 <- ff[basename(ff) =="c_use_of_package_2.csv" ]
  d2 <- data.frame(read.csv2(f2, sep = ","))
  f3 <- ff[basename(ff) =="c_use_of_package_4.csv" ]
  d3 <- data.frame(read.csv2(f3, sep = ","))
  f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
  d4<- data.frame(read.csv2(f4, sep = ",")) 
  f5 <- ff[basename(ff) == "e_harvest.csv"]
  d5 <- data.frame(read.csv2(f5, sep = ","))
  
  
  
  d$season <- as.character(d$season)
  d$country <-stringr::str_to_title(d$country)
  d$adm3 <- d$district
  d$site <- d$sector_ward
  d$trial_id <- d$farm_id
  d$longitude <- as.numeric(d$gps_longitude)
  d$latitude <- as.numeric(d$gps_latitude)
  
  
  d <- d[, c("trial_id","season","country","adm3","site","longitude","latitude")]
  
  
  d1$previous_crop <- tolower(d1$main_crop_last_season)
  d1$previous_crop[d1$previous_crop == ""] <- NA # to remove white space 
  d1$trial_id <- d1$farm_id
  d1 <- d1[,c("trial_id","previous_crop")]
  
  
  
  #cleaning fertilizer types
  d2$mineral_fert_type[d2$mineral_fert_type == "UREA"|d2$mineral_fert_type == "Ureia"|d2$mineral_fert_type == "ureia"|d2$mineral_fert_type == "Urea"]<- "urea" 
  d2$mineral_fert_type[d2$mineral_fert_type == "Ssp" |d2$mineral_fert_type == "SSp"|d2$mineral_fert_type == "SSP" ]<- "NSP"
  d2$mineral_fert_type[d2$mineral_fert_type == "Lime"|d2$mineral_fert_type == "LIME"| d2$mineral_fert_type == "Cal"] <- "lime"
  d2$mineral_fert_type[d2$mineral_fert_type == "None" |d2$mineral_fert_type == "none" |d2$mineral_fert_type == "None "|d2$mineral_fert_type == "No"] <- ""
  d2$mineral_fert_type[d2$mineral_fert_type == "Gypsum"]<- "gypsum"
  d2$crop_1[d2$crop_1 == "Groundnut"] <- "groundnut"
  d2$trial_id <- d2$farm_id
  d2$crop <- d2$crop_1
  d2$variety <-d2$variety_1 
  d2$fertilizer_type <- (d2$mineral_fert_type)
  d2$variety[d2$variety == "Mamane"| d2$variety == "Mamane "]<- "mamane"
  d2$variety[d2$variety == "Chitala"]<-"chitala"
  d2$variety[d2$variety == "Nametil"]<-"nametil"
  d2$fertilizer_type[d2$fertilizer_type == ""]<- NA
  d2 <- d2[,c("trial_id","crop","variety","fertilizer_type")]
  
  
  
  d3$trial_id <- d3$farm_id
  d3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
  d3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)
  d3 <- d3[,c("trial_id","row_spacing","plant_spacing")] 
  
  
  
  d4$trial_id <- d4$farm_id
  d4$start_date <- as.character(as.Date(paste(d4$date_planting_yyyy, d4$date_planting_mm, d4$date_planting_dd, sep = "-")))
  d4$end_date <- as.character(as.Date(paste(d4$date_harvest_yyyy,d4$date_harvest_mm,d4$date_harvest_dd, sep = "-")))
  d4 <- d4[, c("trial_id","start_date","end_date")]
  
  
  d5$trial_id <- d5$farm_id
  d5$rep <- d5$plot_no
  d5$yield <- as.numeric(d5$crop_1_weight_grain)*100 #calculating kg/ha
  d5 <- d5[, c("trial_id","rep","yield")]
  
  
  
  z <- carobiner::bindr(d,d1,d2,d3,d4,d5)
  z$country <- "Mozambique"
  z$crop <- "groundnut"
  z$on_farm <- FALSE
  z$is_survey <- TRUE
  z$dataset_id <- dataset_id
  z$latitude <- -18.66569
  z$longitude <- 35.52956
  
  
 # all scripts should end like this
  carobiner::write_files(dset, z, path, dataset_id, group)
  TRUE
  
  
}
