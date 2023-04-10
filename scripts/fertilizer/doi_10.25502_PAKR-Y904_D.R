# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   Characterising soils of the maize belt in Nigeria to
   deteminie limiting nutrients based on which new fertilzer 
   formulations are developed that are tested on farmer's fields 
   in validation trials in a large number of locations 
   against the commonly used NPK 15-15-15 fertilizer
"
  
  uri <- "https://doi.org/10.25502/pakr-y904/d"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation = "Huising, J. (2019). OCP validation trials for maize fertilizers, OCP - Nigeria [Data set]. International Institute of Tropical 
    Agriculture (IITA). https://doi.org/10.25502/PAKR-Y904/D",
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
  
  f <- ff[basename(ff) == "OCP_Yld-data&covariates_complete.csv"] 
  
  # read the dataset
  d <- read.csv(f)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  d <- d[, c(1,2,3,4,5,7,8,13,47)] 
  colnames(d) <- c("location","adm2","rep","latitude","longitude","Team","treatment", "yield","soil_pH")
  d<-d[d$Team!="BUK3" & d$Team!="BUK1" & d$Team!="BUK2",] # Team BUK1,BUK2,BUK3 data are already include in doi_10.25502_RGB5_GA15_D.R
  # drop Team column
  d<-d[,c("location","adm2","rep","latitude","longitude","treatment","yield","soil_pH")]
  # Add columns
  d$plant_spacing<- 25 # get from VT protocol OCP Project Document 
  d$row_spacing<- 75   # get from VT protocol OCP Project Document
  d$dataset_id <- dataset_id
  d$country <- "Nigeria"
  d$crop <- "maize"
  d$variety<- "Sammaz 15" # get from VT protocol OCP Project Document
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  # NPK Apply  15-15-15 means 15% N, 15% P2O5, 15% K2O
  #OCPF1  (N) : 11% (P2O5): 21% (k2O): 22%  S: 5%  Zn: 1% 
  #OCPF2 (N) : 14% (P205): 31% (k2O): 0%   S: 9%  Zn: 0.9%
  
  d$N_fertilizer <- ifelse(d$treatment == "Control", 0, 
                           ifelse(d$treatment == "OCPF1", 11,
                                  ifelse(d$treatment=="OCPF2",14,15)))
  
  d$K_fertilizer <- ifelse(d$treatment == "Control", 0, 
                           ifelse(d$treatment == "OCPF1", 22/1.2051,
                                  ifelse(d$treatment=="OCPF2",0,15/1.2051)))
  
  d$P_fertilizer <- ifelse(d$treatment == "Control", 0, 
                           ifelse(d$treatment == "OCPF1", 21/2.29,
                                  ifelse(d$treatment=="OCPF2",31/2.29,15/2.29)))
  
  d$Zn_fertilizer <- ifelse(d$treatment == "Control", 0, 
                            ifelse(d$treatment == "OCPF1", 1,
                                   ifelse(d$treatment=="OCPF2",0.9,0)))
  
  d$S_fertilizer <- ifelse(d$treatment == "Control", 0, 
                           ifelse(d$treatment == "OCPF1", 5,
                                  ifelse(d$treatment=="OCPF2",9,0)))
  # add Columns 
  
  # planting date is June 2017  get from VT protocol
  d$start_date <- "2017-06-01"
  d$end_date <- "2017-11-01"
  d$season<- "2017"
  
  d$trial_id <- paste0(dataset_id, '-', d$Location)
  #data type
  d$yield<- as.numeric(d$yield)
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
  
}

