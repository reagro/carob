# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   The ‘Sustainable Weed Management Technologies for Nigeria’ 
   was a 5-year project that was developed and assessed with 
   smallholder farmer participation modern, relevant and appropriate
   cassava weed management technologies suitable for sustainable 
   intensification in major agro-ecological (humid rainforest,
   forest transition savanna and southern Guinea savanna) and
   socio-economic conditions of Nigeria. An important goal of 
   the project was to help smallholder cassava growers achieve 
   sustainable increases in their productivity and incomes through 
   the development and adoption of improved weed control methods
"
  
  uri <- "doi.org/10.25502/2RB2-6439/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation = "Hauser, S. (2020). Cassava Weed Management Data - Agronomy Trials 2015 [Data set]. International Institute of Tropical 
    Agriculture (IITA). https://doi.org/10.25502/2RB2-6439/D",
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  f <- ff[basename(ff) == "Agro2015_1st_Season_All_Locations_Cas_Datafile_Rft.csv"] 
  f1 <- ff[basename(ff) == "Agro2015_2nd_Season_All_Locations_Cas_Datafile_Rft.csv"] 
  
  # read the dataset
  r1 <- read.csv(f)
  r2 <- read.csv(f1)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  d1 <- r1[, c(2,4,5,6,7,8,9,10,11,12,15,25,26,27,28)] 
  colnames(d1) <- c("trial_id","season","location","site","rep","tillage","crop","treatment","variety","plant_density","yield","latitude","longitude","start_date","end_date")
  # soil information
  d1$soil_SOC<-(r1$OC_d10+r1$OC_d20)/2
  d1$soil_pH<-(r1$pH_d10+r1$pH_d20)/2
  d1$soil_P_available<-(r1$P_d10+r1$P_d20)/2
  d1$soil_K<-(r1$K_d10+r1$K_d20)/2
  d1$soil_N<-(r1$N_d10+r1$N_d20)/2
  
  d2 <- r2[, c(2,4,5,6,12,7,8,10,9,11,15,24,25,26,27)]
  colnames(d2) <- c("trial_id","season","location","site","rep","tillage","crop","treatment","variety","plant_density","yield","latitude","longitude","start_date","end_date")
  # fill soil information for second season base on $site.
  # I assume that soil information is the same for the same long and lat position
  for (i in 1:length(d1$season)){
    
    j<-grepl(d1$site[i],d2$site)
    d2$soil_SOC[j]<-d1$soil_SOC[i]
    d2$soil_pH[j]<-d1$soil_pH[i]
    d2$soil_P_available[j]<-d1$soil_P_available[i]
    d2$soil_K[j]<-d1$soil_K[i]
    d2$soil_N[j]<-d1$soil_N[i]
  }
  # combine d1 and d2
  d<-rbind(d1,d2)
  # fill soil in information in the second season
  
  # Add columns
  d$dataset_id <- dataset_id
  d$country <- "Nigeria"
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  # NPK Apply  75-20-90 means 15% N, 15% P2O5, 15% K2O
  d$N_fertilizer <- ifelse(d$treatment == "NoFert", 0, 75)
  
  d$K_fertilizer <- ifelse(d$treatment == "NoFert", 0,90/1.2051 )
                           
  
  d$P_fertilizer <- ifelse(d$treatment == "NoFert", 0, 20/2.29)
                           
  # fix crop names 
  d$intercrops <- ifelse(d$crop=="CasMz","maize","no crop") # add intercrops column
  p<- carobiner::fix_name(d$crop)
  p<-gsub("CasMz","cassava",p)
  p<-gsub("Cassava","cassava",p)
  d$crop<-p
  #fix Long and lat
  d$longitude[d$site=="Makurdi"]<-7.6736
  d$latitude[d$site=="Makurdi"]<-12.906337
  d$longitude[d$site=="Otobi"]<-8.0701088
  d$latitude[d$site=="Otobi"]<-7.103026
  #data type
  d$yield<- (as.numeric(d$yield))*10000 # convert into kg/ha 
  d$plant_density<- as.numeric(d$plant_density)
  #date format
  d$start_date <- format(as.Date(d$start_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  d$end_date <- format(as.Date(d$end_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
  
}

