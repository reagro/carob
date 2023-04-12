# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:

  The AFSIS project aimed to establish an Africa Soil Information 
  system. Data was collected in sentinel sites across sub-Saharan 
  Africa using the Land Degradation Surveillance framework and 
  included also multi-location diagnostic trials in selected sentinel
  sites to determine nutrient limitations and response to improved soil management 
  practices (soil amendments).

"
  
  uri <- "https://doi.org/10.25502/20180814/0923/HJ"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    uri=uri,
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "IITA",
    carob_contributor="Cedric Ngakou",
    data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1,
    Kasungu [Data set]. International Institute of Tropical Agriculture (IITA). 
    https://doi.org/10.25502/20180814/0923/HJ",
    ## something like randomized control...
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_soil=FALSE,
    has_management=FALSE
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  
  f1 <- ff[basename(ff) == "Kasungu_DT2011_field.csv"]
  f2 <- ff[basename(ff) == "Kasungu_DT2011_plant.csv"]
  f3 <- ff[basename(ff) == "Kasungu_DT2011_plot.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  #r <- readxl::read_excel(f) |> as.data.frame()
  
  
  ## process file(s)
  d1<-r1[,c("Site","Flat","Flong","Village","Season","Soil.texture.class","TCrop","PCrop1","FType1","MType1")]
  colnames(d1)<-c("site","latitude","longitude","location","season","soil_type","crop","previous_crop","fertilizer_type","OM_type")
  d1$dataset_id<-dataset_id
  d2<-r3[,c(8,10,20,27)]
  colnames(d2)<-c("rep","treatment","residue_yield","yield")
  d2$dataset_id<-dataset_id
  #merge d1 and d2
  d<-merge(d1,d2,by="dataset_id")
  # Add columns
  d$country<-"Malawi"
  d$start_date<-"2015-12-29"
  d$end_date<-"2016-06-01"
  d$trial_id<-paste0(d$dataset_id,"-",d$location)
  d$crop<-"maize"
  # fill whitespace 
  d <- replace(d, d=='', NA)
  #Add column
  d$OM_used<-ifelse(d$OM_type=="NA",FALSE,TRUE)
  # fix name 
  p <- carobiner::fix_name(d$previous_crop)
  p <- gsub("Sweetpotatoes","sweetpotato",p)
  p <- gsub("SWEET POTATOES","sweetpotato",p)
  p <- gsub("No","no crop",p)
  p <- gsub("G/nuts","groundnut",p)
  p <- gsub("Cassava","cassava",p)
  p <- gsub("Maize","maize",p)
  p <- gsub("Soyabean","soybean",p)
  p <- gsub("Tobacco","tobacco",p)
  d$previous_crop <- p
  
  # fix fertilizer_type name
  d$fertilizer_type<-ifelse(d$fertilizer_type=="NPK+Urea","urea",
                     ifelse(d$fertilizer_type=="CAN-DAP+NPK","CAN; DAP",
                     ifelse(d$fertilizer_type=="NPK+CAN+Urea","CAN; urea",
                     ifelse(d$fertilizer_type=="D.Comp;NPK;Urea;CAN","D compound; urea; CAN",
                     ifelse(d$fertilizer_type=="Urea+CAN","urea; CAN",
                     ifelse(d$fertilizer_type=="CAN+NPK","CAN",
                     ifelse(d$fertilizer_type=="NPK+Urea+CAN","urea; CAN",
                     ifelse(d$fertilizer_type=="NPK++CAN","CAN",
                     ifelse(d$fertilizer_type=="NPK;D Compound;Urea&CAN","D compound; urea; CAN","unknown")))))))))
  #### about the data #####
  ## (TRUE/FALSE)
  
  d$dataset_id <- dataset_id
  d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    #d$elevation <- NA
    ##### Fertilizers #####
    d$N_fertilizer<-ifelse(d$treatment=="Control",0,
                            ifelse(d$treatment=="PK",0,
                            ifelse(d$treatment=="0N40P60K",0,
                             ifelse(d$treatment=="45N40P60K",45,
                                    ifelse(d$treatment=="90N40P60K",90,
                                           ifelse(d$treatment=="120N40P60K",120,
                                                  ifelse(d$treatment=="150N40P60K",150,
                                                         ifelse(d$treatment=="120N0P60K",120,
                                                                ifelse(d$treatment=="120N15P60K",120,
                                                                       ifelse(d$treatment=="120N30P60K",120,100))))))))))
 d$K_fertilizer<-ifelse(d$treatment=="Control",0,
                            ifelse(d$treatment=="NP",0,60))
                            
 d$P_fertilizer<-ifelse(d$treatment=="Control",0,
                         ifelse(d$treatment=="NK",0,
                                ifelse(d$treatment=="120N0P60K",0,
                                       ifelse(d$treatment=="0N40P60K",40,
                                              ifelse(d$treatment=="45N40P60K",40,
                                                     ifelse(d$treatment=="90N40P60K",40,
                                                            ifelse(d$treatment=="120N40P60K",40,
                                                                   ifelse(d$treatment=="150N40P60K",40,
                                                                          ifelse(d$treatment=="120N15P60K",15,30)))))))))
                            
 d$Zn_fertilizer<-ifelse(d$treatment=="NPK+Mn",3,0)
                            
 d$S_fertilizer<-ifelse(d$treatment=="NPK+Mn",5,0)
  
 # data type and date format
  d$start_date <- as.character(as.Date( d$start_date  ))
  d$end_date  <- as.character(as.Date(  d$end_date  ))
  d$season  <- as.character(d$season) 
  d$fertilizer_type <- as.character(d$fertilizer_type) 
 
    # all scripts must end like this
  
    carobiner::write_files(dset, d, path, dataset_id, group)
}

