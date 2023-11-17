
# R script for "carob"

## ISSUES
# ....

## should also be written to fertilizer


carob_script <- function(path) {
 " 
Description:
Final dataset from agronomic experiment in Gumara Maksegnit (2016), as elaborated by GARC researcher in charge for this trial (Baye Ayalew). 
Please contact author and contact person at ICARDA to obtain more detailed metadata or to propose collaboration.

"
   
   uri <- "hdl:20.500.11766.1/FK2/LYKEFM"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "fertilizer"
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id, 
      group=group, 
      project=NA, 
      uri=uri, 
      data_citation="Ayalew, Baye, 2020, Determination of rate and timing of N application on bread wheat,
      https://hdl.handle.net/20.500.11766.1/FK2/LYKEFM, MELDATA, V2", 
      publication= NA, 
      data_institutions = "ICARDA", 
      data_type="on-farm experiment", 
      carob_contributor="cedric Ngakou", 
      carob_date="2023-11-10" 
      
   )
   
   ## download and read data 
   ff  <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
   dset$license <- carobiner::get_license(js)
   
   bn <- basename(ff)
   ## process file(s)
    
    r <- read.csv(ff[bn=="Mandie_s_Farm.csv"],sep = ";")
    r1 <- read.csv(ff[bn=="Worku_s_Farm.csv"],sep = ";") 
    newname<- c("year","treatment","rep","plant_height","biomass_total","yield")
    oldname<- c("Year","Treatment","Replicate","Plant_Height","Biomass","Yield")
    d1<- carobiner::change_names(r[,c("Year","Treatment","Replicate","Plant_Height","Biomass","Yield")],oldname,newname)
    d2<- carobiner::change_names(r1[,c("Year","Treatment","Replicate","Plant_Height","Biomass","Yield")],oldname,newname)
    
    # append d1 and d2
    d<- rbind(d1,d2)
    d$planting_date[d$year==1]<- "2013-06-03"
    d$harvest_date[d$year==1]<- "2013-10-25"
    
    ## process the treatment file 
    r2<- read.csv(ff[bn=="Treatment.csv"],sep = ";") 
    r2$Treatment1<- paste("planting_tillering_Booting (",r2$N_Planting,r2$N_Tillering,r2$N_Booting,")",sep = "_")
    d3<- r2[,c("Treatment","Nitrogen_Rate","Treatment1")]
    colnames(d3)<- c("treatment","N_fertilizer","Treatment1")
    d3<- d3[!is.na(d3$Treatment),]
    
    # merge d3 with d
    d<- merge(d,d3,by="treatment",all.x = TRUE)
    d$treatment<- d$Treatment1
    d$Treatment1<- d$year <- NULL
    #add columns
   d$crop<- "wheat"
   d$dataset_id <- dataset_id
   d$country <- "Ethiopia"
   d$location<- "Gumara Maksegnit"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain" 
   d$trial_id <- paste(d$rep,d$location,sep = "_")
   ## add lon and lat
   d$longitude<- 37.5
   d$latitude <- 12.28333
   
   ##  fix biomass_total range
   #d$dmy_total[d$dmy_total>20000]<- NA
   ## CN 
   # the plant_height values are lower than the valmin in record.csv (10cm) which doesn't make sense. The unit is given in cm, but I suppose this is probably an error.Should we remove the variable ?
   
   d$plant_height<-d$plant_height*10 # assuming it was in dm and convert into cm
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)	
}


