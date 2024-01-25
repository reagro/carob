


#################################################################################
#N2Africa was aimed at increasing biological nitrogen fixation and productivity
#of grain legumes through effective production technologies including inoculants
#and fertilizers adapted to local settings which was aimed at increasing soil
#fertility.The trails were conducted in 11 African countries
#################################################################################

carob_script <- function(path){

  uri <- "doi:10.25502/JHRJ-9423"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "variety_performance"
  
  
  dset <- data.frame (
    dataset_id = dataset_id,
    group = group,
    uri = uri,
    project="N2Africa",
    publication = NA,
    data_citation =" Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat,
  P., Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira,
  L., Kamai, N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, 
  K., Ampadu-Boakye, T., & Heerwaarden, J. van. (2020). N2Africa farm 
  monitoring - Mozambique, 2011 - 2012 [Data set]. International 
  Institute of Tropical Agriculture (IITA). 
  https://doi.org/10.25502/JHRJ-9423",
    carob_contributor = "Effie Ochieng'",
    carob_date="2022-09-22",
    data_type = "variety_performance"
    )
  
  #extract the data
  ff <- carobiner::get_data(uri,path,group)
  js <- carobiner::get_metadata(dataset_id, path, group)
  dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
  
  #read the data
  f <- ff[basename(ff) == "a_general.csv"]
  d <- read.csv(f)
  f1 <- ff[basename(ff)== "b_info_site_2.csv"]
  d1 <- read.csv(f1)
  f2 <- ff[basename(ff) =="c_use_of_package_2.csv" ]
  d2 <- read.csv(f2)
  f3 <- ff[basename(ff) =="c_use_of_package_4.csv" ]
  d3 <- read.csv(f3)
  f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
  d4 <- read.csv(f4) 
  f5 <- ff[basename(ff) == "e_harvest.csv"]
  d5 <- read.csv(f5)
  

  d <- merge(d, d1, by="farm_id")
  d <- merge(d, d4, by="farm_id")

## RH: I think it in this case it is better to start a new data.frame
## and add what you need to avoid forgetting to include variables in the end
## when you would subset the data.frame 

  x <- data.frame(trial_id=d$farm_id)
  x$season <- as.character(d$season)
  x$country <- carobiner::fix_name(d$country, "title")
  x$adm2 <- carobiner::fix_name(d$district, "title")
  x$adm2[x$adm2 == "Mogovolas/nametil"] <- "Mogovolas"
  x$adm3 <- carobiner::fix_name(d$sector_ward, "title")
  x$site <- carobiner::fix_name(d$vilage, "title")
  x$longitude <- d$gps_longitude
  x$latitude <- d$gps_latitude
  x$elevation <- as.numeric(d$gps_altitude)

# from d1 
  x$previous_crop <- tolower(d$main_crop_last_season)
  x$previous_crop[x$previous_crop == ""] <- NA 

# from d4  
  x$planting_date <- as.character(as.Date(paste(d$date_planting_yyyy, d$date_planting_mm, d$date_planting_dd, sep = "-")))
  x$harvest_date <- as.character(as.Date(paste(d$date_harvest_yyyy,d$date_harvest_mm,d$date_harvest_dd, sep = "-")))
 
#   d <- d[, c("trial_id","season","country","adm2", "adm3", "site", "longitude", "latitude", "elevation", "previous_crop", "planting_date", "harvest_date")]
  

## d3 has four more records than d2 and d5, but there is no point keeping 
## this records without the values in d2 and d5
  dd <- merge(d2, d3, by=c("farm_id", "plot_no"), all=FALSE)
  dd <- merge(dd, d5, by=c("farm_id", "plot_no"))
 
 # from d2
## RH: it is better to start a new data.frame and add what you need
## to avoid forgetting variables in the end
  y <- data.frame(trial_id = dd$farm_id)
  
  #cleaning fertilizer types
  ft <- tolower(dd$mineral_fert_type)
  ft[grepl("^ure", ft)] <- "urea"
  ft[grepl("^no", ft)] <- "none"
  ft[ft=="ssp"] <- "SSP"
  ft[ft=="cal"] <- "lime"
  ft[ft==""] <- NA
  #unique(ft)
  y$fertilizer_type <- 	ft
  #table(dd$inoculant_used)
  y$inoculated <- FALSE 
  
## RH: this is the start, but based on the fertilizer type (and amount) used, 
## these numbers need to be changed  
  y$N_fertilizer <- 0
  y$P_fertilizer <- 0
  y$K_fertilizer <- 0
  
  y$crop <- tolower(dd$crop_1)
  y$variety <- carobiner::fix_name(dd$variety_1, "title", lowothers=FALSE) 

## RH: these are proper names and should be capitalized
#  d2$variety[d2$variety == "Mamane"| d2$variety == "Mamane "] <- "mamane"
#  d2$variety[d2$variety == "Chitala"] <- "chitala"
#  d2$variety[d2$variety == "Nametil"] <- "nametil"
 
  # from d3
  y$row_spacing <- as.numeric(dd$crop_1_spacing_row_to_row)
  y$plant_spacing <- as.numeric(dd$crop_1_spacing_plant_to_plant)

  # from d5
  ## RH: these are treatments, not reps!
  ##  dd$rep <- d5$plot_no
  y$treatment <- as.character(dd$plot_no)
  y$yield <- dd$crop_1_weight_grain * 100 #calculating kg/ha

  z <- merge(x, y, by="trial_id")

##RH ???
##  z$on_farm <- FALSE
##  z$is_survey <- TRUE
  z$on_farm <- TRUE
  z$is_survey <- FALSE

  z$dataset_id <- dataset_id

## RH ???? 
## you are overwriting the lon/lat data
## and this could never be good, since we have multiple locations.
##  z$latitude <- -18.66569
##  z$longitude <- 35.52956
  
 # all scripts should end like this
  carobiner::write_files(dset, z, path, dataset_id, group)
 
}
