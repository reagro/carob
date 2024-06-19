

# R script for "carob"


carob_script <- function(path) {
   
   "
   Experiments were installed in Huancavelica, with the objective of identifying clones with high potential for being varieties applying the Participatory Varietal Selection methodology. For the period 2016-2017, 18 clones with high resistance to late blight were planted, belonging to the B population and developed in the International Potato Center together with Two control varieties, Canchan and Yungay (susceptible).
   Finally, in the harvest 5 clones with high yield, low glycoalkaloid content and good organoleptic quality were selected as a result of the Participatory Variety Selection of the farmers and the analysis of mixed models and BLUPs for the yield data. The 5 selected clones were planted again in the period 2017-2018 and through the Participatory Varietal Selection, three promising clones were selected (CIP308488.92, CIP308495.227, and CIP308478.59).
   "
   uri <- "doi:10.21223/JBIBBT"
   group <- "potato_trials"
   ff  <- carobiner::get_data(uri, path, group)
   
   dset <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2),
      data_institute = "CIP",
      publication="doi:10.1007/s11540-021-09495-z",
      project=NA,
      data_type= "experiment",
      treatment_vars = "variety_code",
      carob_contributor= "Cedric Ngakou",
      carob_date="2024-06-19"
   )
   
   ff <- ff[grep("exp",basename(ff))]
   
   process <- function(f) {
      r <- carobiner::read.excel(f, sheet="F4_harvest_mother")
      
      d <- data.frame(
         rep= as.integer(r$REP),
         variety_code= r$INSTN,
         yield= r$TTYNA* 1000, ## kg/ha
         season= as.character(substr(basename(f), start = 8, stop = 11)),
         country= "Peru",
         adm1= "Huancavelica",
         adm2= "Huancavelica" ,
         adm3= "yauli", 
         location= "canaypata",
         latitude= -12.747283,
         longitude= -74.8089,
         trial_id= "1",
         irrigated=  FALSE,
         inoculated= FALSE,
         is_survey= FALSE,
         on_farm= TRUE,
         crop = "potato",
         yield_part = "tubers"
      )
      
      d
   }
   
   d <- lapply(ff, process) 
   d <- do.call(rbind, d)
   d$row_spacing <- 100  # From "DOI:10.1007/s11540-021-09495-z"
   d$plant_spacing <- 30 
   d$harvest_days <- 120
   ## Add date column 
   d$planting_date <- "2017-11-25"
   d$planting_date[d$season=="2016"]<- "2016-11-07"
   
   carobiner::write_files(path, dset, d)
   
}


