# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel 
   sites across sub-Saharan Africa using the Land Degradation
   Surveillance framework and included also multi-location diagnostic
   trials in selected sentinel sites to determine nutrient limitations
   and response to improved soil management practices (soil amendments)
"
  
  uri <- "doi:10.25502/20180814/1219/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= NA,
    data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Kontela [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1219/HJ",
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    experiment_type="fertilizer",
    has_weather=FALSE
     
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f1 <- ff[basename(ff) == "Kontela_DT2009_field.csv"]# get the field data
  f2 <- ff[basename(ff) == "Kontela_DT2009_Plant.csv"] # get the plant data
  f3 <- ff[basename(ff) == "Kontela_DT2009_Plot.csv"] # get the plot data 
  
  
  d1 <- read.csv(f1)
  d2 <- read.csv(f2)
  d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)
  
 # process field data 
  d1$latitude<- d1$Flat
  d1$site<- d1$Site
  d1$longitude<- d1$Flong
  d1$planting_date<-d1$PlntDa_mac
  d1$harvest_date<-d1$HarvDa
  d1$longitude<- d1$Flong
  d1$location<- d1$Village
  d1$variety_type<- d1$TCVariety
  d1$season<- d1$Season
  
  d1<-d1[,c("site","longitude","latitude","location","variety_type",
            "planting_date","harvest_date","season")]
  
  # process plot data 
  d3$trial_id<- paste0(d3$dataset_id,"-",d3$ID)
  d3$rep<- d3$Rep
  d3$treatment<- d3$TrtDesc
  d3$residue_yield <- (d3$TStoverYld)*1000
  d3$yield <- (d3$TGrainYld)*1000
  
  d3$N_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                  ifelse(d3$TrtDesc=="PK",0,100))
  
  d3$K_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                         ifelse(d3$TrtDesc=="NP",0,60))
  
  d3$P_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                         ifelse(d3$TrtDesc=="NK",0,30))
  
  d3$Zn_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",3,0)
  
  d3$S_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",5,0)
  
  d3$N_splits <- ifelse(d3$N_fertilizer > 0,3,0)
 
 # merge all the dataset
 d <- merge(d1, d3, all=TRUE)
 
 d$country <- "Mali"
 d$crop <- "sorghum"
 
 d <-d[,c("site","season","country","location","rep","trial_id","treatment","longitude","latitude","planting_date","harvest_date","variety_type",
         "crop","residue_yield","yield","N_fertilizer","K_fertilizer","P_fertilizer","Zn_fertilizer",
         "S_fertilizer","N_splits")]
 
#type of data
## RH season "1" is not very meaningful. 
## needs to be replaced 
  d$season <- as.character(d$season) 
  # date format
 d$harvest_date <- format(as.Date(d$harvest_date, format = "%m/%d/%Y"), "%Y-%m-%d")
 d$planting_date <- format(as.Date(d$planting_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  # all scripts must end like this

  d$dataset_id <- dataset_id
 
  carobiner::write_files(dset, d, path, dataset_id, group)
}

