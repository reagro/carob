# R script for "carob"


carob_script <- function(path) {
   
 " As part of the GIZ-PZTM project, agronomic trials were conducted under a strip-plot design in the locations of Fultola, Moshamari and Hogalbunia of Khulna district in Bangladesh. Two varieties of potato (BARI Alu 72 and BARI Alu 78) under zero tillage with  (ZTSM) and conventional treatments were tested. This dataset corresponds to the results of yield (marketable tuber yield and fresh tuber yield) and soil analysis (characterization, fertility, salinity and carbon organic) of the samples collected at the end season 2021-2022."
   
   uri <- "doi:10.21223/CAQQ9N"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=2), 
      data_institute ="CIP", 
      publication = NA, 
      project = "GIZ-PZTM", 
      data_type = "experiment",
      response_vars = "yield;makatable_yield",
      treatment_vars = "land_prep_method;variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-09-06"
   )
   
   f <- ff[basename(ff)=="02_Yield data.xlsx"]
   f1 <- ff[basename(ff)=="01_Soil analysis.xlsx"]
   
  # Read files
   r <- carobiner::read.excel(f) 
   r1 <- carobiner::read.excel(f1)
   
# Processing yield data 
   d <- data.frame(
     location= r$location,
     rep= as.integer(r$replication),
     variety= r$variety,
     land_prep_method= ifelse(grepl("CM", r$management), "conventional", "none"),
     yield_marketable= r$MTY*1000, # kg/ha
     yield= r$FTY*1000, # kg/ha
     trial_id= paste(r$location, r$variety, r$plot_number, sep="-")
   )
  
   # Processing soil data 
   d1 <- data.frame(
      location= r1$location,
      rep= as.integer(r1$replication),
      variety= r1$variety,
      land_prep_method= ifelse(grepl("CM", r1$management), "conventional", "none"),
      soil_depth= r1$soil_depth,
      soil_sand= r1$sand,
      soil_silt= r1$silt,
      soil_clay= r1$clay,
      soil_type= r1$TC,
      soil_CEC= r1$CEC, 
      soil_pH= r1$pH,
      soil_EC= r1$EC,
      #soil_ECE= r1$Ece,
      soil_SOM= r1$OM,
      soil_N= r1$N...18,
      soil_P_available= r1$P,
      soil_K= r1$K...21*200, ## 1meq/100g = 200mg/kg
      soil_Zn= r1$Zn,
      soil_Cu= r1$Cu,
      soil_Fe= r1$Fe,
      soil_Mn= r1$Mn,
      soil_Ca= r1$Ca,
      soil_Mg= r1$Mg,
      soil_Na= r1$Na,
      soil_B= r1$B,
      soil_SOC= r1$OC
   )
   
   d  <- merge(d, d1, by= c("location", "land_prep_method", "rep", "variety"), all.x = TRUE)
    
   d$crop <- "potato"
   d$mulch_type <- "straw"
   d$country <- "Bangladesh"
   d$planting_date <- "2021"
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$on_farm <- TRUE
   d$yield_part <- "tubers"
   d$geo_from_source <- FALSE
   
   ## Add longitude and latitude 
   geo <- data.frame(
      location= c("Fultola", "Hogalbunia", "Moshamari"),
      latitude= c(23.8690465, 22.21285, 24.5038),
      longitude= c(90.1476495, 90.43229, 89.164334)
   )
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files (path, meta, d)
   
}


