# R script for "carob"


## CN
## Could the valid_max of AUDPC be review ? valid_max=150 in the records_crops.csv but this data set is showing very high values

# Some variables names perhaps interesting such as NTP, SAUDPC, NoMTWP, TTWP, MTWP, MTYNA are exluded 


carob_script <- function(path) {
   
"The objective of this study was determinate the phenotypic stability for resistance to late blight of 15 late blight resistant B3 clones during 4 seasons in two locations in Peru. The experiments were performed under a randomized complete block designs (RCBD) with 4 replications of 40 hill-plots. All the plants were exposed to natural infection in the field 	and disease spread was enhanced by spreader rows planted systematically throughout the field and also by favorable weather conditions."
   
   uri <-  "doi:10.21223/P3/4FTDO8"
   group <- "disease"
   ff <- carobiner::get_data(uri, path, group)
  
   meta <- data.frame(
	  carobiner::read_metadata(uri, path, group, major=1, minor=3),
      publication= NA,# 
      data_institute = "CIP",
      carob_contributor="Cedric Ngakou",
      carob_date="2023-10-26",
      data_type="experiment",
      project=NA 
   )
   
   
   bn <- basename(ff)
   
   lst <- list()
   for (i in c(5:11)){

      name <- c("REP","INSTN","AUDPC","rAUDPC","TTYNA")
      
      Newname <- c("rep","variety","AUDPC","rAUDPC","yield")
      
      if (i %in% c(5:7)){
         r <- readxl::read_excel(ff[bn==bn[i]],sheet=9) |> as.data.frame()
         ri <- r[,name]
         colnames(ri) <- Newname
         lst[[i]] <- ri
      }
      else{
         r <- readxl::read_excel(ff[bn==bn[i]],sheet=8) |> as.data.frame()
         ri <- r[,name]
         colnames(ri) <- Newname
         lst[[i]] <- ri
      }
      
      
   }
   # append all the data
   d <- do.call(rbind, lst)
 
   d$yield <- d$yield*1000 ## in kg/ha
   d$yield_part <- "tubers" 
   d$crop <- "potato"
   d$pathogen <- "Phytophthora infestans"
   ## add columns
   
   d$country <- "Peru"
   d$adm1 <- "Junin"
   d$adm2 <- "concepcion"
   d$adm3 <- "Mariscal Castilla"
   d$site <- "viena"
   d$trial_id <- paste(d$adm1,d$dataset_id,sep = "-")
   d$harvest_date <- "2002-03-25"
   d$planting_date <- "2001-12-11"
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   ### add lon and lat coordinate
   d$longitude <-  -75.131448
   d$latitude <-  -11.523716
   
   ##data type
   d$rep <- as.integer(d$rep)
	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
   	d$is_survey = FALSE
   
   carobiner::write_files(meta, d, path=path)
   
}

