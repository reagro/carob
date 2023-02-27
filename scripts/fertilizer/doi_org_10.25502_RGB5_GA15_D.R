# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
	Description:
   Developing efficient and affordable fertilizer products for 
   increased and sustained yields in the maize belt of Nigeria. 
   Maize belt of Nigeria that covers 8 states: Bauchi,
   Kaduna, Kano, Katsina, Nasarawa, Niger, Plateau and Taraba
"
  
  uri <- "https://doi.org/10.25502/RGB5-GA15/D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication=NA,
    data_citation = "Huising, J. (2019). OCP validation trials for maize fertilizers, Bayero University Kano - Nigeria [Data set]. International Institute of Tropical Agriculture (IITA).
    https://doi.org/10.25502/RGB5-GA15/D" ,
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
  
  
  
  f1 <- ff[basename(ff) == "BUK_T1_VT_yieldatharvest_summ.csv"] # dataset from team1
  f2 <- ff[basename(ff) == "BUK_T3_VT_yieldatharvest_final.csv"]# dataset from team2
  f3 <- ff[basename(ff) == "BUK-T2_VT_yieldatharvest_summ.csv"]# dataset from team3
  # read the dataset
  d1 <- read.csv(f1)
  d2 <- read.csv(f2)
  d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)
  d1$dataset_id <- dataset_id
  d2$dataset_id <- dataset_id
  
  #process team1 dataset
  
  d1$treatment<- d1$Section_B.repeat.trt_name
  d1$yield <- d1$yld.plot.full.pd
  
  #d1$plant_density<-d1$
  
  d1$N_fertilizer<-ifelse(d1$Section_B.repeat.trt_name=="Control",0,100)
                          
  
  d1$K_fertilizer<-ifelse(d1$Section_B.repeat.trt_name=="Control",0,60)
                          
  
  d1$P_fertilizer<-ifelse(d1$Section_B.repeat.trt_name=="OCPF1",0,
                          ifelse(d1$Section_B.repeat.trt_name=="OCPF2",20,0))
  
  #d1$Zn_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",3,0)
  
  #d1$S_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",5,0)
  
  d1=transform(d1,N_splits=ifelse(d1$N_fertilizer>0,3,0))
  
  d1<- d1[,c("dataset_id","treatment","yield","N_fertilizer","K_fertilizer","P_fertilizer","N_splits")]
  
  #process Team2 data 
  
  d2$yield<-d2$yld.full.pd
  d2$treatment<-d2$trt_name
  
  d2$N_fertilizer<-ifelse(d2$trt_name=="Control",0,100)
  
  
  d2$K_fertilizer<-ifelse(d2$trt_name=="Control",0,60)
  
  
  d2$P_fertilizer<-ifelse(d2$trt_name=="OCPF1",0,
                          ifelse(d2$trt_name=="OCPF2",20,0))
  
  #d1$Zn_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",3,0)
  
  #d1$S_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",5,0)
  
  d2=transform(d2,N_splits=ifelse(d2$N_fertilizer>0,3,0))
  
  d2 <- d2[,c("dataset_id","yield","treatment","N_fertilizer","K_fertilizer","P_fertilizer","N_splits")]
  
  #process Team3 data 
  
  d3$yield<-d3$yld.corr.pd..kg.ha.
  
  d3$treatment<-d3$trt_name
  
  d3$N_fertilizer<-ifelse(d3$trt_name=="Control",0,100)
  
  
  d3$K_fertilizer<-ifelse(d3$trt_name=="Control",0,60)
  
  
  d3$P_fertilizer<-ifelse(d3$trt_name=="OCPF1",0,
                          ifelse(d3$trt_name=="OCPF2",20,0))
  
  #d1$Zn_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",3,0)
  
  #d1$S_fertilizer<-ifelse(d3$TrtDesc=="NPK+MN",5,0)
 
  d3=transform(d3,N_splits=ifelse(d3$N_fertilizer>0,3,0))
   
  d3<-d3[,c("treatment","N_fertilizer","K_fertilizer","P_fertilizer","N_splits")]
  
  #merge all the data
  list = list(d1,d2,d3)
  d=Reduce(function(x, y) merge(x, y, all=TRUE), list)
  
  # add column
  # replication column rep
  for (i in 1:nrow(d)){# 
    if(i==1){ # first replication value
      d$rep[i]<-1
      r<-1
    }else if( d$treatment[i]==d$treatment[i-1]){ # give the same replication value for  the same treatment
      d$rep[i]<-r
      
      
    }
    else {
      d$rep[i]<-r+1
      r<-1+r
    }
  }
  
  d$country<- "Nigeria"
  d$crop<- "maize"
  d$latitude<-12.000000
  d$longitude<-8.516667
  d$start_date<-as.character(2018)
  d$end_date<-as.character(2019)
  d$location<-"Bayero"
  d$adm1<-"kano"
  d$trial_id <- paste0(dataset_id, '-', d$Location)
  # data type
  d$yield<-as.numeric(d$yield)
  d$rep<-as.integer(d$rep)
  # fill whitespace in observation 
  d<- replace(d,d=='',NA)
  # all scripts must end like this
  carobiner::write_files(dset, d, path, dataset_id, group)
  #TRUE
  }

