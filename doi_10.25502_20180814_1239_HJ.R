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
  
  uri <- "doi:10.25502/20180814/1239/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "soil_information"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="doi:10.25502/20180814/1239/HJ",
    data_citation = "APA",
    data_institutions = "IITA",
    carob_contributor="cedric Ngakou",
    experiment_type="soil_information",
    has_weather=FALSE,
    has_management=TRUE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f1 <- ff[basename(ff) == "Mbinga_DT2010_field.csv"]
  f2 <- ff[basename(ff) == "Mbinga_DT2010_plant.csv"]
  f3 <- ff[basename(ff) == "Mbinga_DT2010_plot.csv"]
  
  
  d1 <- read.csv(f1)
  d2 <- read.csv(f2)
  d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)
  d2$dataset_id <- dataset_id
  d<- d2[,c(14,2,7)]
  colnames(d)<- c("dataset_id","site","season")
  # add column
  d$country<- "Tanzania"
  d$trial_id<- c(paste0(d1$ID,"-",d1$FieldID),rep(NA,nrow(d)-length(paste0(d1$ID,"-",d1$FieldID))))
  d$rep<-c(d3$Rep,rep(NA,nrow(d)-length(d3$Rep)) )
  d$treatment<-c(d3$TrtDesc,rep(NA,nrow(d)-length(d3$TrtDesc)))
  d$yield<-c(d3$TGrainYld_adj,rep(NA,nrow(d)-length(d3$TGrainYld)))
  d$residue_yield<-c(d3$AdjTStoverYld,rep(NA,nrow(d)-length(d3$TGrainYld)))
  d$latitude<-c(d1$Flat,rep(NA,nrow(d)-length(d1$Flat)))
  d$longitude<-c(d1$Flong,rep(NA,nrow(d)-length(d1$Flong)))
  d$crop<-"maize"
  d$start_date<-c(d1$Basal.fertilizer.application,rep(NA,nrow(d)-length(d1$Basal.fertilizer.application)))
  d$end_date<-c(d1$HarvDa,rep(NA,nrow(d)-length(d1$HarvDa)))
  d$season<-as.character(d$season)
  d$p_h<-c(d2$Plant.height..cm.,rep(NA,nrow(d)-length(d2$Plant.height..cm.)))
  d$N_leaves<-c(d2$Number.of.leaves,rep(NA,nrow(d)-length(d2$Number.of.leaves)))
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
  TRUE
}
