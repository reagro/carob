# R script for "carob"

carob_script <- function(path) {
   
"Dataset for yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in San Ramón. 150 advanced clones of the LBHT and heat-tolerant population, with four control varieties Yungay, Kory, Amarilis and Desiree, and 23 parents were planted in San Ramón Peru between 2021 and 2022. (16 Rows x 12 Columns)"
   
   uri <- "doi:10.21223/BMWFX2"
   group <- "variety_trials"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2),
      data_institute = "CIP",
      publication= NA,
      project=NA,
      data_type= "experiment",
      treatment_vars = "variety",
      carob_contributor= "Cedric Ngakou",
      carob_date="2024-06-26"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="01_data_advanced_trial_LBHTC2_SanRamon2021.xlsx"])
   d <- data.frame(
      country="Peru",
      crop="potato",
      variety= r$CIPN,
      rep= as.integer(r$Rep),
      yield=r$MTYNA*1000, # to kg/ha
      on_farm= TRUE,
      inoculated= FALSE,
      irrigated= NA,
      yield_part= "tubers",
      trial_id= "1",
	  is_survey = FALSE
   )
   
   ## location
   d$adm1 <- "San Ramón"
   d$longitude <- -75.3521
   d$latitude <- -11.1214
   
   d$planting_date <- "2021-08-11"
   d$harvest_date  <- "2021-11-16"
   
   ## fertilizer
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(path, meta, d)  
}

