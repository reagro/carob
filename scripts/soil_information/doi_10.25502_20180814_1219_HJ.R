# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel 
   sites across sub-Saharan Africa using the Land Degradation
   Surveilllance framework and inlcuded also multi-location diagnostic
   trials in selected sentiale sites to determine nutrient limitations
   and response to improved soil management practices (soil amendments)
"
  
  uri <- "doi:10.25502/20180814/1219/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "soil_information"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= NA,
    data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Kontela [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1219/HJ",
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    experiment_type="soil_information",
    has_weather=FALSE,
    has_management=TRUE
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
  d3$dataset_id <- dataset_id
  d1$dataset_id <- dataset_id
  
 # process field data 
  d1$latitude<- d1$Flat
  d1$site<- d1$Site
  d1$longitude<- d1$Flong
  d1$start_date<-d1$PlntDa_mac
  d1$end_date<-d1$HarvDa
  d1$longitude<- d1$Flong
  d1$location<- d1$Village
  d1$variety_type<- d1$TCVariety
  d1$season<- d1$Season
  
  d1<-d1[,c("dataset_id","site","longitude","latitude","location","variety_type",
            "start_date","end_date","season")]
  
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
  
  d3=transform(d3,N_splits=ifelse(d3$N_fertilizer>0,3,0)) 
 
 # merge all the dataset
 d_n<-list(d1,d3)
 d<-Reduce(function(x,y) merge(x,y, all=TRUE),d_n)
 
 
 # add column
 d$country<- "Mali"
 d$crop<- "sorghum"
 
 d<-d[,c("dataset_id","site","season","country","location","rep","trial_id","treatment","longitude","latitude","start_date","end_date","variety_type",
         "crop","residue_yield","yield","N_fertilizer","K_fertilizer","P_fertilizer","Zn_fertilizer",
         "S_fertilizer","N_splits")]
 
#type of data
 d$season<-as.character(d$season) 
  # date format
 d$end_date<-format(as.Date(d$end_date, format = "%m/%d/%Y"), "%Y-%m-%d")
 d$start_date<-format(as.Date(d$start_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
  TRUE
}

