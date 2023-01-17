# R script for "carob"

## ISSUES
# need to at lon/lat 


carob_script <- function(path) {

"
	Description:

    The dataset is meant for developing fertilizer management decision support tool for an effective crop-nutrient management. The dataset is developed on the basis of landscape targeting on-farm trials on crop-nutrient response and crop yield gap assessment across the Africa Rising target districts and other scaling up locations in the Ethiopian highlands.

"

	uri <- "doi:10.7910/DVN/ZXH0R8"
	dataset_id <- agro::get_simple_URI(uri)
	group <- ""
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi:10.1017/S1742170519000504",
	   data_citation = "",
	   data_institutions = "International Crops Research Institute for the Semi-Arid Tropics (ICRISAT); Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ); Amhara Agricultural Research Institute (ARARI); International Livestock Research Institute (ILRI)",
	   carob_contributor="Siyabusa Mkhulani & Eduardo Garcia Bendito",
	   experiment_type="On farm experimental trials",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	# No License specified in metadata. Only Terms of use available. It is registered as "CC BY 4.0"
	# dset$license <- carobiner::get_license(js)
	dset$license <- "Custom Dataset Terms"

## process 001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx"]
 
 	# suppress variable renaming warning
	suppressMessages(d <- readxl::read_excel(f))
  
  d <- data.frame(d)
  dd <- data.frame(country = d$Country) # Create parallel dataframe
  dd$country <- d$Country
  dd$adm1 <- d$Region.state
  dd$adm3 <- d$LGA.District
  dd$location <- d$village.Kebele
  dd$trial_id <- paste0('001_2014-2015_Wheat_ICRISAT-AR_ETH', '.', d$village.Kebele)
  
  dd$start_date <- as.Date(d$Planting.date)
  dd$end_date <- as.Date(d$Harvest.date)
  # dd$longitude <- # Removed due to PII
  # dd$latitude <- # Removed due to PII
  dd$on_farm <- "yes"
  dd$is_survey <- "no"
  dd$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in dd
    if(r == 1){ # If it is the first row...
      dd$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      dd$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      dd$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  dd$crop <- tolower(d$Crop)
  dd$variety <- d$Variety
  dd$crop_rotation <- tolower(d$Crop.system)
  # dd$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  dd$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  dd$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  dd$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                               ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                      ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  dd$N_fertilizer <- d$N.fertilizer.amount...19
  dd$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  dd$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
  dd$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount), 0, d$P.fertilizer.amount) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dd$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..K2O.kg.ha..), 0, d$K.fertilizer.amount..K2O.kg.ha..) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dd$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..Zn.kg.ha..), 0, d$Zn.fertilizer.amount..Zn.kg.ha..) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dd$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..S.kg.ha..), 0, d$S.fertilizer.amount..S.kg.ha..) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dd$soil_type <- d$Soil.type
  # dd$soil_pH <- # To be requested from the author
  # dd$soil_SOC <- # To be requested from the author
  # dd$soil_sand <- # To be requested from the author
  # dd$soil_clay <- # To be requested from the author
  # dd$soil_N <- # To be requested from the author
  # dd$soil_K <- # To be requested from the author
  # dd$soil_P_total <- # To be requested from the author
  # dd$soil_P_available <- # To be requested from the author
	  
## process 002_2016_Wheat_ ICRISAT-AR_ETH.xlsx
  f <- ff[basename(ff) == "002_2016_Wheat_ ICRISAT-AR_ETH.xlsx"]

 	# suppress variable renaming warning
	suppressMessages(d <- readxl::read_excel(f))
  d <- data.frame(d)
  ddd <- data.frame(country = d$Country) # Create parallel dataframe
  ddd$country <- d$Country
  ddd$adm1 <- d$Region.state
  ddd$adm3 <- d$LGA.District
  ddd$location <- d$village.Kebele
  ddd$trial_id <- paste0("002_2016_Wheat_ ICRISAT-AR_ETH", '.', d$village.Kebele)
  ddd$start_date <- as.Date(d$Planting.date)
  ddd$end_date <-  as.Date(d$Harvest.date)
  # ddd$longitude <- # was removed from data
  # ddd$latitude <- # was removed from data
  ddd$on_farm <- "yes"
  ddd$is_survey <- "no"
  ddd$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in dd
    if(r == 1){ # If it is the first row...
      ddd$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      ddd$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      ddd$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  ddd$crop <- tolower(d$Crop)
  ddd$variety <- d$Variety
  ddd$crop_rotation <- tolower(d$Crop.system)
  # ddd$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  ddd$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  ddd$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  ddd$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                               ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                      ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  ddd$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
  ddd$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  ddd$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
  ddd$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddd$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddd$Zn_fertilizer <- "" # Not present in the dataframe
  ddd$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddd$soil_type <- d$Soil.type
  # ddd$soil_pH <- # To be requested from the author
  # ddd$soil_SOC <- # To be requested from the author
  # ddd$soil_sand <- # To be requested from the author
  # ddd$soil_clay <- # To be requested from the author
  # ddd$soil_N <- # To be requested from the author
  # ddd$soil_K <- # To be requested from the author
  # ddd$soil_P_total <- # To be requested from the author
  # ddd$soil_P_available <- # To be requested from the author
  
## process 003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx
  f <- ff[basename(ff) == "003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx"]
  
  suppressMessages(d <- readxl::read_excel(f))
  d <- data.frame(d)
  dddd <- data.frame(country = d$Country) # Create parallel dataframe
  dddd$country <- d$Country
  dddd$adm1 <- d$Region.state
  dddd$adm3 <- d$LGA.District
  dddd$location <- d$village.Kebele
  dddd$trial_id <- paste0("003_2017_Sorghum+Tef_ ICRISAT-AR_ETH", '.', d$village.Kebele)
  dddd$start_date <- as.Date(d$Planting.date)
  dddd$end_date <- as.Date(d$Harvest.date)
  # dddd$longitude <- # Removed by PI?
  # dddd$latitude <- # Removed by PI?
  dddd$on_farm <- "yes"
  dddd$is_survey <- "no"
  dddd$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in dd
    if(r == 1){ # If it is the first row...
      dddd$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      dddd$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      dddd$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  dddd$crop <- tolower(d$Crop)
  dddd$variety <- d$Variety
  dddd$crop_rotation <- tolower(d$Crop.system)
  # dddd$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  dddd$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  dddd$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  dddd$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                                ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                       ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  dddd$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
  dddd$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  dddd$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
  dddd$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dddd$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dddd$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..kg.ha.), 0, d$Zn.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dddd$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  dddd$soil_type <- d$Soil.type
  # dddd$soil_pH <- # To be requested from the author
  # dddd$soil_SOC <- # To be requested from the author
  # dddd$soil_sand <- # To be requested from the author
  # dddd$soil_clay <- # To be requested from the author
  # dddd$soil_N <- # To be requested from the author
  # dddd$soil_K <- # To be requested from the author
  # dddd$soil_P_total <- # To be requested from the author
  # dddd$soil_P_available <- # To be requested from the author
  
## process 004_2019_Wheat_ ICRISAT-AR_ETH.xlsx
  f <- ff[basename(ff) == "004_2019_Wheat_ ICRISAT-AR_ETH.xlsx"]
  
  d <- suppressMessages(readxl::read_excel(f))
  d <- data.frame(d)
  ddddd <- data.frame(country = d$Country) # Create parallel dataframe
  ddddd$country <- d$Country
  ddddd$adm1 <- d$Region.state
  ddddd$adm3 <- d$LGA.District
  ddddd$location <- d$village
  ddddd$trial_id <- paste0("004_2019_Wheat_ ICRISAT-AR_ETH", '.', d$village)
  ddddd$start_date <- as.Date(d$Planting.date)
  ddddd$end_date <- as.Date(d$Harvest.date)
  # ddddd$longitude <- # Removed by PI?
  # ddddd$latitude <- # Removed by PI?
  ddddd$on_farm <- "yes"
  ddddd$is_survey <- "no"
  ddddd$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in dd
    if(r == 1){ # If it is the first row...
      ddddd$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      ddddd$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      ddddd$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  ddddd$crop <- tolower(d$Crop)
  ddddd$variety <- d$Variety
  ddddd$crop_rotation <- tolower(d$Crop.system)
  # ddddd$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  ddddd$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  ddddd$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  ddddd$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                                 ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                        ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  ddddd$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
  ddddd$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  ddddd$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
  ddddd$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddddd$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddddd$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..kg.ha.), 0, d$Zn.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddddd$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  ddddd$soil_type <- d$Soil.type
  # ddddd$soil_pH <- # To be requested from the author
  # ddddd$soil_SOC <- # To be requested from the author
  # ddddd$soil_sand <- # To be requested from the author
  # ddddd$soil_clay <- # To be requested from the author
  # ddddd$soil_N <- # To be requested from the author
  # ddddd$soil_K <- # To be requested from the author
  # ddddd$soil_P_total <- # To be requested from the author
  # ddddd$soil_P_available <- # To be requested from the author
  
## Append the tables together
  d <- rbind(dd,ddd,dddd,ddddd)
## Filter only relevant variables
  d <- dd[,c("country", "adm1", "adm3", "location", "trial_id", "start_date", "end_date", "on_farm", "is_survey", "treatment", "rep", "crop", "variety", "crop_rotation", "yield", "residue_yield", "fertilizer_type", "N_fertilizer", "N_splits", "observation_date", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "S_fertilizer","soil_type")]
## Add dataset ID
  d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
