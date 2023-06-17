# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
Data sets included in the database evaluated Site-Specific Nutrient Management (SSNM) and Farmers’ 
Fertilizer Practice (FFP) on maize, rice and wheat in Africa and Asia. Information captured includes,
with countries, cropping systems, crop type, seasons, climatic conditions, number of replications, 
number of nitrogen (N) splits, N, phosphorus (P) and potassium (K) fertilizer rates, grain yield, agronomic efficiency of N (AEN), partial factor of productivity N (PFP N), total fertilizer cost (TFC), gross return, and gross return above fertilizer cost (GRF).
(2020-10-27)
   
"
  
  uri <- "doi:10.7910/DVN/H23MVL"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation ="Dobermann A, Witt C, Dawe D, et al (2002) Site-specific nutrient management for intensive rice cropping systems in Asia. F Crop Res 74:37–66. 
    doi: doi: 10.1016/S0378- 4290(01)00197-6" ,
    data_institutions = "IRRI",
    carob_contributor="Cedric Ngakou",
    experiment_type="NA",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  lic <- carobiner::get_license(js)
  dset$license <- lic$name
  
  f <- ff[basename(ff) =="SSNM_Meta-analysis_data.csv"] 
  
  # read the dataset
  r <- read.csv(f,header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
   colnames(r) <- r[1,]
  d <- r[,c(4,7,8,9,10,14,15,17,28,29,31,32,34,35,38,41,43,44,49,53,57,62,64)]
  d <- d[-1,] # drop the first rows
  #normalize columns names
  colnames(d) <-  c("reference","country","location","longitude","latitude","soil_type","soil_pH","soil_SOC","previous_crop","crop",
                  "water_mangement","variety","observation_date","season","tillage","treatment","N_splits","N_fertilizer",
                  "P_fertilizer","K_fertilizer","Zn_fertilizer","rep","yield")
  
  d[c('start_date', 'end_date')] <- stringr::str_split_fixed(d$observation_date, "-", 2) 
  
  d[c('latitude1', 'Latitude2')] <- stringr::str_split_fixed(d$latitude, "\u0096", 2)
  d[c('longitude1', 'Longitude2')] <- stringr::str_split_fixed(d$longitude, "\u0096", 2)
  
  d[c('soil_pH', 'soil_pH1')] <- stringr::str_split_fixed(d$soil_pH, "-", 2)
  
  d[c('soil_SOC', 'soil_SOC1')] <- stringr::str_split_fixed(d$soil_SOC, "-", 2)
 
  d$k <- d$end_date
  d$end_date <- ifelse((d$start_date=="2006 and 2007")& is.na(d$end_date),"2007",
                     ifelse((d$start_date=="2005 and 2006")& is.na(d$end_date),"2006",d$k))
  #fix start date 
  p <- carobiner::fix_name(d$start_date)
  p <- gsub("2006 and 2007","2006",p)
  p <- gsub("2005 and 2006","2005",p)
  d$start_date <- p
  
  d[d==""] <- NA
  rownames(d) <- NULL
  lat <- trimws(d$longitude1)
  lon <- trimws(d$latitude1)
  # Fix long and lat columns
#RH?  d <- d[c(361,362),]
  
  lon <- gsub("\n46\u0092", "46'", lon)
  lon <- gsub("\002", "", lon)
  lon <- gsub("75°02' E to 77°04' E", "76°03'E", lon)
  lon <- gsub(" ", "", lon)
  lon <- gsub("°","-", lon)
  lon <- gsub("'","-", lon)
  lon <- gsub("\\?","-", lon)
  lon <- stringr::str_split_fixed(lon, "-", 3)
  lon[lon[,2]=="", 2] <- NA
  d$longitude = as.numeric(lon[,1]) + as.numeric(lon[,2])/60
  d$longitude <- ifelse(lon[,3] == "W", -d$longitude, d$longitude)
  
  lat <- gsub("29°07' N to 30°08' N", "29°33' N", lat)
  lat <- gsub("\u0092", "'",lat)
  lat <- gsub(" ", "", lat)
  lat <- gsub("°","-", lat)
  lat <- gsub("'N","", lat)
  lat <- gsub("-N","", lat)
  lat <- gsub("'","", lat)
  lat <- gsub("\\?N", "", lat)
  lat <- stringr::str_split_fixed(lat, "-", 2)
  lat[lat[,2]=="", 2] <- NA
  d$latitude = as.numeric(lat[,1]) + as.numeric(lat[,2])/60
  
  # extract relevant columns 
  d <- d[c("reference","country","location","longitude","latitude","crop","previous_crop","variety","yield","tillage","N_fertilizer", "P_fertilizer","K_fertilizer","N_splits","start_date","end_date","season","soil_pH",'soil_type',"soil_SOC")]
  # Add columns
  d$dataset_id <- dataset_id
  d$trial_id <- paste0(d$dataset_id,"_",d$country)
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  # fill missing data in long and lat columns

  yx <- c("latitude", "longitude")
  d$location[is.na(d$location)] <- ""
  d[d$location=="Bihar", yx] <- c(25.6440, 85.9065)
  d[d$location=="Haryana", yx] <- c(29, 76)
  d[d$location=="Punjab", yx] <- c(30.9293, 75.50048)
  d[d$location=="Can Tho, Mekong River Delta", yx] <- c(10.03642, 105.7875219)
  d[d$location=="An Giang, Mekong River Delta", yx] <- c(10.5392, 105.2312822)  
  d[d$location=="Tien Giang, Mekong River Delta", yx] <- c(10.4030368, 106.3616) 
  d[d$location=="Norman E. Borlaug Crop Research Centre of G.B. Pant University of Agriculture and Technology, Pantnagar", yx] <- c(29.0284405, 79.4832094) 
  d[d$location=="Andhra Pradesh", yx] <- c(14.8432502746, 78.7590408325)
  d[d$location=="Karnataka", yx] <- c(14.5203896, 75.7223521)
  d[d$location=="Tamil Nadu", yx] <- c(10.9094334, 78.3665347)
  d[d$location=="Odisha", yx] <- c(20.5431241, 84.6897321)
  d[d$location=="Karnal, Kurukshetra, Kaithal, Ambala,\nYamunanagar, Panipat, and Sonepat districts of Haryana", yx] <- c(29.6803266, 76.9896254)  
  d[d$location=="Gurdaspur, Hoshiarpur, Ludhiana, Patiala, Faridkot, and Firozpur in Punjab province in Northwest India", yx] <- c(32.16667, 75.316667)
  
  d[d$location=="Site 1 in Siruguppa, Bijapur, and \nNavalgund Talukas of Northern Karnataka", yx] <- c(15.6336064, 76.8939231)  
  d[d$location=="Kpong", yx] <- c(9.7018896, -0.8277126)
  d$location[d$location == ""] <- NA
  d$location <- gsub("\n", "", d$location)
  # fix crop name 
  p <- carobiner::fix_name(d$crop, "lower")
  d$crop <- p
  p1 <- carobiner::fix_name(d$previous_crop,"lower")
  p1 <- gsub("upland crop","no crop",p1) # no specification about upland crop
  p1 <- gsub("mungbean","mung bean",p1)
  d$previous_crop <- p1
  #fix white space in soil_SOC column
  e <- carobiner::fix_name(d$soil_type)
  e <- gsub(" ",NA,e)
  d$soil_type <- e
  #data type
  d$yield <-  (as.numeric(d$yield)) 
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$N_fertilizer <- as.double(d$N_fertilizer)
  d$P_fertilizer <- as.double(d$P_fertilizer)
  d$K_fertilizer <- as.double(d$K_fertilizer)
  d$N_splits <- as.numeric(d$N_splits)
  d$soil_pH <- as.double(d$soil_pH)
  d$soil_SOC <- as.double(d$soil_SOC)
  carobiner::write_files(dset, d, path, dataset_id, group)
  
}



