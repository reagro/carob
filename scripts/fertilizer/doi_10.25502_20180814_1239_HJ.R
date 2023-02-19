
# this is already included in doi_10.7910_DVN_UNLRGC

# R script for "carob"

### ISSUES
# ....


# carob_script <- function(path) {
  
  # "
	# Description:
   # The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel 
   # sites across sub-Saharan Africa using the Land Degradation
   # Surveillance framework and included also multi-location diagnostic
   # trials in selected sentinel sites to determine nutrient limitations
   # and response to improved soil management practices (soil amendments)
# "
  
  # uri <- "doi:10.25502/20180814/1239/HJ"
  # dataset_id <- carobiner::simple_uri(uri)
  # group <- "fertilizer"
  ### dataset level data 
  # dset <- data.frame(
    # dataset_id = dataset_id,
    # group=group,
    # uri=uri,
    # publication= NA,
    # data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Mbinga [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1239/HJ",
    # data_institutions = "IITA",
    # carob_contributor="Cedric Ngakou",
    # experiment_type="fertilizer",
    # has_weather=FALSE,
    # has_management=TRUE
  # )
  
  ### download and read data 
  
  # ff  <- carobiner::get_data(uri, path, group)
  # js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  # dset$license <- carobiner::get_license(js)
  
  
  # f1 <- ff[basename(ff) == "Mbinga_DT2010_field.csv"]## get field data 
  # f2 <- ff[basename(ff) == "Mbinga_DT2010_plant.csv"]##get plant data 
  # f3 <- ff[basename(ff) == "Mbinga_DT2010_plot.csv"]## get plot data 
  
  
  # d1 <- read.csv(f1)
  # d2 <- read.csv(f2)
  # d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)
  # d1$dataset_id <- dataset_id
  # d3$dataset_id <- dataset_id

    # process plot data
  
  # d3$trial_id<- c(paste0(d3$dataset_id,"-",d3$ID))
  # d3$rep<-d3$Rep
  # d3$season<-d3$Season
  # d3$treatment<-d3$TrtDesc
  # d3$yield<-(d3$TGrainYld_adj)*1000
  # d3$residue_yield<-(d3$AdjTStoverYld)*1000
  # d3$grain_weight<-d3$Wgt100grain
  # d3$N_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                          # ifelse(d3$TrtDesc=="PK",0,100))
  
  # d3$K_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                          # ifelse(d3$TrtDesc=="NP",0,60))
  
  # d3$P_fertilizer<-ifelse(d3$TrtDesc=="Control",0,
                          # ifelse(d3$TrtDesc=="NK",0,30))
  
  # d3$Zn_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",3,0)
  
  # d3$S_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",5,0)
  
  # d3=transform(d3,N_splits=ifelse(d3$N_fertilizer>0,3,0))
  
  # d3<-d3[,c("dataset_id","rep","season","treatment","trial_id","yield","residue_yield","grain_weight","N_fertilizer",
            # "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]
  # process field data
  
  # d1$latitude<-d1$Flat
  # d1$longitude<-d1$Flong
  # d1$start_date<-d1$Basal.fertilizer.application
  # d1$end_date<-d1$HarvDa
  # d1$OM_type<-d1$MType1
  # d1$previous_crop<- d1$PCrop1
  # d1$site<-d1$Site
  # d1<-d1[,c("dataset_id","site","longitude","latitude","start_date","end_date","previous_crop","OM_type")]
 
  # merge dataset
  
  # d<-merge(d1,d3,by="dataset_id", all.x = TRUE)
  
  #add column
  # d$country<- "Tanzania"
  # d$crop<-"maize"
  
  #d<- transform(d,OM_used=ifelse(d$OM_type=="None", "FALSE","TRUE"))
  
  # p <- carobiner::fix_name(gsub("/", "; ", d$previous_crop), "lower")
  # p <- gsub("beans.", "common bean", p)
  # p <- gsub("beans", "common bean", p)
  # p <- gsub("beans and cassava", "common bean; cassava", p)
  # p <- gsub("fallowed", "no crop", p)
  # p <- gsub("-", NA, p)
  # p <- gsub("common beanand cassava", "common bean; cassava", p)
  # d$previous_crop <- p
  
  #data type
  # d$season<-as.character(d$season)
  # d$grain_weight<- as.numeric(d$grain_weight)
  
  #d$OM_used<-as.character(d$OM_used)
  # d<-d[,c("dataset_id","rep","season","country","site","treatment","longitude","latitude","start_date",
          # "end_date","trial_id","crop","yield","residue_yield","grain_weight","previous_crop","OM_type","N_fertilizer",
            # "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]
  # change date format
  # d$start_date <- format(as.Date(d$start_date, format = "%d/%m/%Y"), "%Y-%m-%d")
  
  # d$end_date <- format(as.Date(d$end_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  
  # all scripts must end like this
  
  # carobiner::write_files(dset, d, path, dataset_id, group)
  
# }

