
##CN
### Soil information is missing in the data ( not clear in the protocol)
#### 

carob_script <- function(path) {
   
"
Characterising soils of the maize belt in Nigeria to deteminie limiting nutrients based on which new fertilzer formulations 
are developed that are tested on farmer's fields in validation trials in a large number of locations against the commonly used 
NPK 15-15-15 fertilizer.
   
"
   uri <- "doi:10.25502/3348-6831/D"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer"
   ff <- carobiner::get_data(uri, path, group)
   
   dset <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2),
      project="OCP project", 
      publication= NA, 
      data_institute = "IITA", 
      carob_contributor="Cedric Ngakou", 
      carob_date="2024-06-04", 
      data_type="experiment",
      treatment_vars = "treatment"
   )
   
   ### yield data 
   r <- read.csv(ff[basename(ff)=="IAR_VT_yield harvest_summ.csv"])
   d <- r[,c("trt_name","Yld_harvest_plot.kg.ha")]
   colnames(d) <- c("treatment","yield")
   d$rep <- as.integer(1)
   d$location <- "Niger-kaduma" 
   #### 
   ## Add columns
   d$crop <- "maize"
   d$variety <- "Sammaz 15" # From VT protocol
   d$row_spacing <- 75
   d$plant_spacing <- 25 
   d$trial_id<- "1"
   d$country <- "Nigeria"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"  
   
   
   ## add fertilizer 
   
   
   # NPK Apply  15-15-15 
   #OCPF1   : N   P2O5  k2O   S    Zn  B2O3
   #          11  22    21   5     1   1
   
   #OCPF2  :  N   P2O5  k2O   S    Zn  B2O3
#             14   31    0    9    1    1
# More information can be found in the VT protocol ( see raw data)  
   
fert <- data.frame(treatment=c("Control","NPK","OCPF1","OCPF2" ),N_fertilizer=c(0,15,11,14),
                  P_fertilizer=c(0,15/2.29,22/2.29,31/2.29),K_fertilizer=c(0,15/1.2051,21/1.2051,0),Zn_fertilizer=c(0,0,1,1),
                   S_fertilizer=c(0,0,5,9),B_fertilizer=c(0,0,1,1))  

  d <- merge(d,fert,by="treatment",all.x = TRUE) 
   ## Add longitude and latitude    
  d$longitude <- 5.6511088  
  d$latitude <- 9.9326083    
  # planting and harvest date ( from VT protocol )
  d$planting_date <- "2017-06-01"
  d$harvest_date <- "2017-11-01"
 
   carobiner::write_files(path, dset, d)   
}