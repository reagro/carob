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
  dset1<- carobiner::get_license(js)
  dset$license <- dset1$uri
  
  f <- ff[basename(ff) =="SSNM_Meta-analysis_data.csv"] 
  
  # read the dataset
  r <- read.csv(f,header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
   colnames(r)<-r[1,]
  d<-r[,c(4,7,8,9,10,14,15,17,28,29,31,32,34,35,38,41,43,44,49,53,57,62,64)]
  d<-d[-1,] # drop the first rows
  #normalize columns names
  colnames(d)<- c("reference","country","location","longitude","latitude","soil_type","soil_pH","soil_SOC","previous_crop","crop",
                  "water_mangement","variety","observation_date","season","tillage","treatment","N_splits","N_fertilizer",
                  "P_fertilizer","K_fertilizer","Zn_fertilizer","rep","yield")
  
  d[c('start_date', 'end_date')] <- stringr::str_split_fixed(d$observation_date, "-", 2)
  
  
  d[c('latitude1', 'Latitude2')] <- stringr::str_split_fixed(d$latitude, "\u0096", 2)
  d[c('longitude1', 'Longitude2')] <- stringr::str_split_fixed(d$longitude, "\u0096", 2)
  
  d[c('soil_pH', 'soil_pH1')] <- stringr::str_split_fixed(d$soil_pH, "-", 2)
  
  d[c('soil_SOC', 'soil_SOC1')] <- stringr::str_split_fixed(d$soil_SOC, "-", 2)
 
  d$k<-d$end_date
  d$end_date<-ifelse((d$start_date=="2006 and 2007")& is.na(d$end_date),"2007",
                     ifelse((d$start_date=="2005 and 2006")& is.na(d$end_date),"2006",d$k))
  #fix start date 
  p<-carobiner::fix_name(d$start_date)
  p<-gsub("2006 and 2007","2006",p)
  p<-gsub("2005 and 2006","2005",p)
  d$start_date<-p
  
  d$latitude<-d$latitude1
  d$longitude<-d$longitude1
  rownames(d) <- NULL
  #fill the white space 
  d[d==""]<-NA
  # Fix long and lat columns
  d<-d[-c(361,362),]
  
  d$latitude<-gsub("\n46\u0092","46",d$latitude)
  d$latitude<-gsub("\002","",d$latitude)
  d$latitude<-gsub("to 77°04' E ","",d$latitude)
  d$latitude<-gsub(" ","",d$latitude)
  d$latitude<-gsub("°"," ",d$latitude)
  d$latitude<-gsub("'E"," 0",d$latitude)
  d$latitude<-gsub("'W"," 0",d$latitude)
  d$latitude<-gsub("E"," 0",d$latitude)
  d$latitude<-gsub("'"," 0",d$latitude)
  # calculate latitude value in decimal
  d$latitude<-measurements::conv_unit(d$latitude,"deg_min_sec","dec_deg")
  
  d$longitude<-gsub("to 30°08' N","",d$longitude)
  d$longitude<-gsub("\u0092","",d$longitude)
  d$longitude<-gsub(" ","",d$longitude)
  d$longitude<-gsub("°"," ",d$longitude)
  d$longitude<-gsub("'N"," 0",d$longitude)
  d$longitude<-gsub("'"," 0",d$longitude)
  d$longitude<-gsub("?N"," 0",d$longitude)
  # calculate longitude value in decimal 
  d$longitude<-measurements::conv_unit(d$longitude,"deg_min_sec","dec_deg")
  
  # extract relevant columns 
  d<-d[c("reference","country","location","longitude","latitude","crop","previous_crop","variety","yield","tillage","N_fertilizer",
         "P_fertilizer","K_fertilizer","N_splits","start_date","end_date","season","soil_pH",'soil_type',"soil_SOC")]
  # Add columns
  d$dataset_id <- dataset_id
  d$trial_id<-paste0(d$dataset_id,"_",d$country)
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  # fill missing data in long and lat columns
  d$latitude[d$location=="Bihar"]<-25.6440
  d$longitude[d$location=="Bihar"]<-85.9065
  d$latitude[d$location=="Haryana"]<-29
  d$longitude[d$location=="Haryana"]<-76
  d$latitude[d$location=="Punjab"]<-30.9293
  d$longitude[d$location=="Punjab"]<-75.50048
  d$latitude[d$location=="Can Tho, Mekong River Delta"]<-10.03642
  d$longitude[d$location=="Can Tho, Mekong River Delta"]<-105.7875219
  d$latitude[d$location=="An Giang, Mekong River Delta"]<-10.5392
  d$longitude[d$location=="An Giang, Mekong River Delta"]<-105.2312822
  
  d$latitude[d$location=="Tien Giang, Mekong River Delta"]<-10.4030368
  d$longitude[d$location=="Tien Giang, Mekong River Delta"]<-106.3616
  
  d$latitude[d$location=="Norman E. Borlaug Crop Research Centre of G.B. Pant University of Agriculture and Technology, Pantnagar"]<-29.0284405
  d$longitude[d$location=="Norman E. Borlaug Crop Research Centre of G.B. Pant University of Agriculture and Technology, Pantnagar"]<-79.4832094
  
  d$latitude[d$location=="Andhra Pradesh"]<-14.8432502746
  d$longitude[d$location=="Andhra Pradesh"]<-78.7590408325
  
  d$latitude[d$location=="Karnataka"]<-14.5203896
  d$longitude[d$location=="Karnataka"]<-75.7223521
  
  d$latitude[d$location=="Tamil Nadu"]<-10.9094334
  d$longitude[d$location=="Tamil Nadu"]<-78.3665347
  
  d$latitude[d$location=="Odisha"]<-20.5431241
  d$longitude[d$location=="Odisha"]<-84.6897321
  
  d$latitude[d$location=="Karnal, Kurukshetra, Kaithal, Ambala,\nYamunanagar, Panipat, and Sonepat districts of Haryana"]<-29.6803266
  d$longitude[d$location=="Karnal, Kurukshetra, Kaithal, Ambala,\nYamunanagar, Panipat, and Sonepat districts of Haryana"]<-76.9896254
  
  d$latitude[d$location=="Gurdaspur, Hoshiarpur, Ludhiana, Patiala, Faridkot, and Firozpur in Punjab province in Northwest India"]<-32.16667
  d$longitude[d$location=="Gurdaspur, Hoshiarpur, Ludhiana, Patiala, Faridkot, and Firozpur in Punjab province in Northwest India"]<-75.316667
  
  d$latitude[d$location=="Site 1 in Siruguppa, Bijapur, and \nNavalgund Talukas of Northern Karnataka"]<-15.6336064
  d$longitude[d$location=="Site 1 in Siruguppa, Bijapur, and \nNavalgund Talukas of Northern Karnataka"]<-76.8939231
  
  d$latitude[d$location=="Kpong"]<-9.7018896
  d$longitude[d$location=="Kpong"]<--0.8277126
  
  # fix crop name 
  p<-carobiner::fix_name(d$crop,"lower")
  d$crop<-p
  p1<-carobiner::fix_name(d$previous_crop,"lower")
  p1<-gsub("upland crop","no crop",p1) # no specification about upland crop
  p1<-gsub("mungbean","mung bean",p1)
  d$previous_crop<-p1
  #fix white space in soil_SOC column
  e<-carobiner::fix_name(d$soil_type)
  e<-gsub(" ",NA,e)
  d$soil_type<-e
  #data type
  d$yield<- (as.numeric(d$yield)) 
  d$longitude<-as.numeric(d$longitude)
  d$latitude<-as.numeric(d$latitude)
  d$N_fertilizer<-as.double(d$N_fertilizer)
  d$P_fertilizer<-as.double(d$P_fertilizer)
  d$K_fertilizer<-as.double(d$K_fertilizer)
  d$N_splits<-as.numeric(d$N_splits)
  d$soil_pH<-as.double(d$soil_pH)
  d$soil_SOC<-as.double(d$soil_SOC)
  carobiner::write_files(dset, d, path, dataset_id, group)
  
}



