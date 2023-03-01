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
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
  dset$license <- carobiner::get_license(js)
  
  f1 <- ff[basename(ff) == "BUK_T1_VT_yieldatharvest_summ.csv"] # dataset from team1
  f2 <- ff[basename(ff) == "BUK-T2_VT_yieldatharvest_summ.csv"] # dataset from team2
  f3 <- ff[basename(ff) == "BUK_T3_VT_yieldatharvest_final.csv"]# dataset from team3
  # read the dataset
  d1 <- read.csv(f1)
  d2 <- read.csv(f2)
  d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)

##RH add plant density
  
	d1 <- d1[, c("Section_B.repeat.trt_name", "X_parent_index", "yld.plot.kg.ha")] 
	colnames(d1) <- c("treatment", "rep", "yield")
	d2 <- d2[, c("trt_name", "parent_index", "plot.yld..kg.ha.")] 
	colnames(d2) <- c("treatment", "rep", "yield")
	d3 <- d3[, c("trt_name", "parent_index", "yld.plot")] 
	colnames(d3) <- c("treatment", "rep", "yield")
	d <- rbind(d1, d2, d3)
	
				
## RH: that is *not* the right number for yield
#  d1$yield <- d1$yld.plot.full.pd
# RH: I think it should be
#  d1$yield <- d1$yld.plot.kg.ha 
 
  #d1$plant_density<-d1$



## RH: I see:  

#table(d$treatment)
#Control     NPK   OCPF1   OCPF2 
#    250     250     250     250 

## the suggests that the below is not correct. Or at least it should be commented on
## What are the contents of OCPF1 and OCPF2?
## is it true that both contain 100 kg/ha of N and 60 kg/ha of K, but zero P??
## None of that makes sense given that the NPK appears to be 15-15-15
## on the face of it, that makes no sense
## same applies to d2 and d3

  d$N_fertilizer <- ifelse(d$treatment == "Control", 0, 100)
  d$K_fertilizer <- ifelse(d$treatment == "Control", 0, 60)
  d$P_fertilizer <- ifelse(d$treatment == "OCPF1", 0,
                        ifelse(d$treatment == "OCPF2", 20, 0))
  
# RH where does this come from? Would be good to comment on.
  d$N_splits <- ifelse(d$N_fertilizer > 0, 3, 0)
  
  
## RH: wow, why would you do that???
## merge by what? (always specify the variables used to join the data ? 
## These are three different datasets that need to be rbind-ed, I think.
#merge all the data
#  list <- list(d1, d2, d3)
#  d <- Reduce(function(x, y) merge(x, y, all=TRUE), list)
 
  d$dataset_id <- dataset_id
  d$country <- "Nigeria"
  d$crop <- "maize"

#RH how do you know this? are these all in one location??
  d$latitude <- 12.000000
  d$longitude <- 8.516667

  d$start_date <- "2018"
  d$end_date <- "2019"
  d$location <- "Bayero"
  d$adm1 <- "Kano"
 
  d$trial_id <- paste0(dataset_id, '-', d$Location)

  # data type
  # fill whitespace in observation 
  ## do not blindly do things like this
  # d <- replace(d, d=="",NA)

  # all scripts must end like this
 carobiner::write_files(dset, d, path, dataset_id, group)
}

