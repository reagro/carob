# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
The objectives of this study were to examine yield gains
in the cultivars and to investigate inter-trait relationships
and yield stability under six drought and 17 rainfed conditions
in West Africa from 2013 to 2016.

"
  
  uri <- "https://doi.org/10.25502/20181101/1228/BB"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "international_maize_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    data_citation ="Baffour Badu-Apraku. (2018). Gains in Grain Yield
    of Extra-early Maize during Three Breeding Periods under 
    and Rain-fed Conditions [Data set]. International Institute
    of Tropical Agriculture (IITA).
    https://doi.org/10.25502/20181101/1228/BB",
    publication= NA,
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    experiment_type=NA, # please have a look on experiment type 
    has_weather=FALSE,
    has_soil=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f <- ff[basename(ff) == "20181025aao_Combd_BA15150_Final_DS_data.csv"]
  f1 <- ff[basename(ff) == "20181029aao_Combd_BA15150_Final_WW_data.csv"]
  
  r <- read.csv(f)
  r1 <- read.csv(f1)
  #r <- readxl::read_excel(f) |> as.data.frame()
  
  
  ## process file(s)
  rr <- r[,c("Country","Location","Rep","YR","YIELD","POLLEN","DYSK","PLHT","EHT","PASP","EROT","ASI","EASP")]  
  colnames(rr)<-c("country","location","rep","start_date","yield","dy_tass","dy_sk","pl_ht","e_ht","p_asp","erot","asi","e_asp")
  
  rr$trial_id <- paste0(r$Pedigree, '-', r$ENV)
  rr$season<-r$Study
  
  r11 <- r1[,c("Country","LOC","Rep","YEAR","YIELD","POLLEN","DYSK","PLHT","EHT","PASP","EROT","ASI","EASP")]  
  colnames(r11)<-c("country","location","rep","start_date","yield","dy_tass","dy_sk","pl_ht","e_ht","p_asp","erot","asi","e_asp")
  
  r11$trial_id <- paste0(r1$Pedigree, '-', r1$ENV)
  r11$season<-r1$Study
  
  d<-rbind(rr,r11)
  d$dataset_id <- dataset_id
  d$country<- "Nigeria"
  # Fix country name base on location 
  p <- carobiner::fix_name(gsub("/", "; ", d$location))
  p <- gsub("IKENNE", "Ikenne", p)
  d$location<- p
  
  d$country[d$location=="KPEVE"]<- "Ghana"
  d$country[d$location=="NYANKPALA"]<- "Ghana"
  d$country[d$location=="FUMESUA"]<- "Ghana"
  d$country[d$location=="ANGARADEBOU"]<-"Benin"
  d$country[d$location=="MANGA"]<-"Burkina Faso"
  ## each site must have corresponding longitude and latitude
  d$longitude[d$location=="Ikenne"]<-3.6977469 
  d$latitude[d$location=="Ikenne"]<-6.9010051
  d$longitude[d$location=="KPEVE"]<- 0.3326709
  d$latitude[d$location=="KPEVE"]<-6.6851678
  d$longitude[d$location=="NYANKPALA"]<- -0981456
  d$latitude[d$location=="NYANKPALA"]<- 9.400463 
  d$longitude[d$location=="FUMESUA"]<--1.5119402 
  d$latitude[d$location=="FUMESUA"]<- 6.7143898   
  d$longitude[d$location=="ANGARADEBOU"]<- 3.0412812
  d$latitude[d$location=="ANGARADEBOU"]<-11.3228338
  d$longitude[d$location=="ZARIA"]<- 7.6518533
  d$latitude[d$location=="ZARIA"]<- 11.0248119
  d$longitude[d$location=="MOKWA"]<- 5.0544281
  d$latitude[d$location=="MOKWA"]<- 9.2957202
  d$longitude[d$location=="DUSU"]<- 12.366667
  d$latitude[d$location=="DUSU"]<-  8.816667
    d$longitude[d$location=="BAGAUDA"]<- 8.38546
    d$latitude[d$location=="BAGAUDA"]<-11.5696
    d$longitude[d$location=="IFE"]<- 4.5604451
    d$latitude[d$location=="IFE"]<- 7.482824
    d$longitude[d$location=="MAINA-HARI"]<- 12.1577
    d$latitude[d$location=="MAINA-HARI"]<-10.6788
    d$longitude[d$location=="MANGA"]<- -1.0723972
    d$latitude[d$location=="MANGA"]<-11.6673837
    
  d$striga_trial <- FALSE
  d$striga_infected <- FALSE
    ##### Crop #####
  d$crop <- "maize"
    ##### Time Format #####
  d$start_date <- as.character(d$start_date)
  ## data type
  d$striga_infected<- as.logical(d$striga_infected)
  d$striga_trial<- as.logical(d$striga_trial)
    # all scripts must end like this
    carobiner::write_files(dset, d, path, dataset_id, group)
}




