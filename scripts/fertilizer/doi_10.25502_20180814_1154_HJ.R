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
  
  uri <- "doi:10.25502/20180814/1154/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Koloko [Data set]. International Institute of Tropical Agriculture 
    (IITA). doi:10.25502/20180814/1154/HJ" ,
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f1 <- ff[basename(ff) == "Koloko_DT2009_field.csv"] # get Field dataset 
  f2 <- ff[basename(ff) == "Koloko_DT2009_plant.csv"] # get plant dataset
  f3 <- ff[basename(ff) == "Koloko_DT2009_plot.csv"] # get plot dataset
  
  # read the dataset
  d1 <- read.csv(f1)
  d2 <- read.csv(f2)
  d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)
  d1$dataset_id <- dataset_id
  d3$dataset_id <- dataset_id
  
  #process field dataset
  
  d1$trial_id <- c(paste0(d1$dataset_id,"-",d1$ID))
  
  d1$location <- d1$Village
  
  d1$latitude <- d1$Flat
  
  d1$longitude <- d1$Flong
  
  d1$variety_type <- d1$TCVariety
  
  d1$previous_crop <- d1$PCrop1
  d1$planting_date <- d1$PlntDa
  d1$harvest_date <- d1$HarvDa
  #d1$fertilizer_type <- d1$FType1
  # add column
  d1$site  <- d1$Site
  d1$country <- "Mali"
  d1$crop <- "sorghum"
  
  # previous crop name normalization 
  d1$previous_crop[d1$previous_crop==""] <- "no crop"
  
  d1$previous_crop[d1$previous_crop=="Groundnuts(Aracide)"] <- "groundnut"
  
  d1$previous_crop[d1$previous_crop=="Sorghum"] <- "sorghum"
  d1$previous_crop[d1$previous_crop=="Kolokoland"] <- "kola"
  d1$previous_crop[d1$previous_crop=="Millet"] <- "pearl millet"
 
   d1 <- d1[,c("dataset_id","trial_id","location","site","country",
             "latitude","longitude","planting_date","harvest_date","crop","variety_type","previous_crop" )]
  
  #process plot data 
  
  d3$rep <- d3$Rep
  
  d3$treatment <- d3$TrtDesc
  
  d3$yield <- (d3$TGrainYld)*1000
  
  d3$residue_yield <- (d3$TStoverYld) * 1000
  
  d3$season  <- d3$Season
  
  
  d3$N_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                          ifelse(d3$TrtDesc=="PK",0,100))
  
  d3$K_fertilizer <- ifelse(d3$TrtDesc=="Control", 0,
                          ifelse(d3$TrtDesc=="NP", 0, 60))
  
  d3$P_fertilizer <- ifelse(d3$TrtDesc=="Control", 0,
                          ifelse(d3$TrtDesc=="NK", 0, 30))
  
  d3$Zn_fertilizer <- ifelse(d3$TrtDesc=="NPK+MN", 3, 0)
  
  d3$S_fertilizer <- ifelse(d3$TrtDesc=="NPK+MN", 5, 0)
  
  d3$N_splits <- ifelse(d3$N_fertilizer > 0, 3L, 0L)
  
  d3 <- d3[,c("dataset_id","rep","treatment","season","yield","residue_yield","N_fertilizer",
            "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]
  
  #merge all the data
  d <- merge(d1,d3,by="dataset_id", all.x = TRUE)
  
  
  # data type
  d$season <- as.character(d$season)
  # change date format
  d$planting_date <- format(as.Date(d$planting_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  
  d$harvest_date <- format(as.Date(d$harvest_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  # fill whitespace in observation 
  d <- replace(d,d=='',NA)
  # all scripts must end like this
  carobiner::write_files(dset, d, path=path)
  #TRUE
}

