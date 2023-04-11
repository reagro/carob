# R script for "carob"

## ISSUES
# need to at lon/lat 


carob_script <- function(path) {

"
	Description:

    The dataset is meant for developing fertilizer management decision support tool for an effective crop-nutrient management. The dataset is developed on the basis of landscape targeting on-farm trials on crop-nutrient response and crop yield gap assessment across the Africa Rising target districts and other scaling up locations in the Ethiopian highlands.

"

	uri <- "doi:10.7910/DVN/ZXH0R8"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi:10.1017/S1742170519000504",
	   data_citation = NA,
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
 
 	d <- carobiner::read.excel(f)
  
  d <- data.frame(d)
  
  d2 <- data.frame(country = d$Country) # Create parallel dataframe
  d2$country <- d$Country
  d2$adm1 <- d$Region.state
  d2$adm3 <- d$LGA.District
  d2$location <- d$village.Kebele
  d2$trial_id <- paste0('001_2014-2015_Wheat_ICRISAT-AR_ETH', '.', d$village.Kebele)
  
  d2$start_date <- as.character(as.Date(d$Planting.date))
  d2$end_date <- as.character(as.Date(d$Harvest.date))
  d2$longitude <- ifelse(d$village.Kebele == "Goshe Bado", 39.446,
                         ifelse(d$village.Kebele == "Tsibet", 39.482,
                                ifelse(d$village.Kebele == "Lemo", 37.851,
                                       ifelse(d$village.Kebele == "Selka", 40.289, 39.682))))
  d2$latitude <- ifelse(d$village.Kebele == "Goshe Bado", 9.740,
                        ifelse(d$village.Kebele == "Tsibet", 12.860,
                               ifelse(d$village.Kebele == "Lemo", 5.436,
                                      ifelse(d$village.Kebele == "Selka", 6.857, 9.799))))
  d2$on_farm <- TRUE
  d2$is_survey <- FALSE
  d2$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in d2
    if(r == 1){ # If it is the first row...
      d2$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]) { # If the treatment is the same name, it is a replicate...
      d2$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      d2$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  d2$rep <- as.integer(d2$rep)
  d2$crop <- tolower(d$Crop)
  d2$variety <- d$Variety
  d2$previous_crop <- tolower(d$Crop.system)
  # d2$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  d2$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  d2$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  d2$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                               ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                      ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  d2$N_fertilizer <- d$N.fertilizer.amount...19
  d2$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  
  # Date of the application of the 2nd split
  ### RH: this is not an observation! 
  ### d2$observation_date <- as.character(as.Date(d$N.topdressing.date))
  
  # I think is more correct to indicate 0 than NA, since the treatment does not contain the element
  d2$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount), 0, d$P.fertilizer.amount) 
  d2$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..K2O.kg.ha..), 0, d$K.fertilizer.amount..K2O.kg.ha..)
  d2$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..Zn.kg.ha..), 0, d$Zn.fertilizer.amount..Zn.kg.ha..)
  d2$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..S.kg.ha..), 0, d$S.fertilizer.amount..S.kg.ha..)
  d2$soil_type <- d$Soil.type
  
  # d2$soil_pH <- # To be requested from the author
  # d2$soil_SOC <- # To be requested from the author
  # d2$soil_sand <- # To be requested from the author
  # d2$soil_clay <- # To be requested from the author
  # d2$soil_N <- # To be requested from the author
  # d2$soil_K <- # To be requested from the author
  # d2$soil_P_total <- # To be requested from the author
  # d2$soil_P_available <- # To be requested from the author
	  
## process 002_2016_Wheat_ ICRISAT-AR_ETH.xlsx
  f <- ff[basename(ff) == "002_2016_Wheat_ ICRISAT-AR_ETH.xlsx"]

	d <- carobiner::read.excel(f)
  d <- data.frame(d)
  d3 <- data.frame(country = d$Country) # Create parallel dataframe
  d3$country <- d$Country
  d3$adm1 <- d$Region.state
  d3$adm3 <- d$LGA.District
  d3$location <- d$village.Kebele
  d3$trial_id <- paste0("002_2016_Wheat_ ICRISAT-AR_ETH", '.', d$village.Kebele)
  d3$start_date <- as.character(as.Date(d$Planting.date))
  d3$end_date <-  as.character(as.Date(d$Harvest.date))
  d3$longitude <- ifelse(d3$location == "Lemo", 37.851,
                         ifelse(d3$location == "Tsibet", 39.482, 39.460))
  d3$latitude <- ifelse(d3$location == "Lemo", 5.436,
                        ifelse(d3$location == "Tsibet", 12.860, 10.826))
  d3$on_farm <- TRUE
  d3$is_survey <- FALSE
  d3$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in d2
    if(r == 1){ # If it is the first row...
      d3$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      d3$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      d3$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  d3$crop <- tolower(d$Crop)
  d3$variety <- d$Variety
  d3$previous_crop <- tolower(d$Crop.system)
  # d3$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  d3$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  d3$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  d3$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                               ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                      ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  d3$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
  d3$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  ### not obs d3$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
  d3$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) 
  d3$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) 
  d3$Zn_fertilizer <- NA # Not present in the dataframe
  d3$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) 
  d3$soil_type <- d$Soil.type
  # d3$soil_pH <- # To be requested from the author
  # d3$soil_SOC <- # To be requested from the author
  # d3$soil_sand <- # To be requested from the author
  # d3$soil_clay <- # To be requested from the author
  # d3$soil_N <- # To be requested from the author
  # d3$soil_K <- # To be requested from the author
  # d3$soil_P_total <- # To be requested from the author
  # d3$soil_P_available <- # To be requested from the author
  
## process 003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx
  f <- ff[basename(ff) == "003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx"]
  
  d <- carobiner::read.excel(f)
  d <- data.frame(d)
  d4 <- data.frame(country = d$Country) # Create parallel dataframe
  d4$country <- d$Country
  d4$adm1 <- d$Region.state
  d4$adm3 <- d$LGA.District
  d4$location <- d$village.Kebele
  d4$trial_id <- paste0("003_2017_Sorghum+Tef_ ICRISAT-AR_ETH", '.', d$village.Kebele)
  d4$start_date <- as.character(as.Date(d$Planting.date))
  d4$end_date <- as.character(as.Date(d$Harvest.date))
  d4$longitude <- ifelse(d4$location == "Sirinka", 39.607, 39.684)
  d4$latitude <- ifelse(d4$location == "Sirinka", 11.748, 11.316)
  d4$on_farm <- TRUE
  d4$is_survey <- FALSE
  d4$treatment <- d$Treatment
  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in d2
    if(r == 1){ # If it is the first row...
      d4$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      d4$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      d4$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  d4$crop <- tolower(d$Crop)
  d4$variety <- d$Variety
  d4$previous_crop <- tolower(d$Crop.system)
  # d4$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  d4$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  d4$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  d4$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                                ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                       ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  d4$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
  d4$N_splits <- 2 
	# There were two N_splits: Basal (50%) and top dressing (50%)

  d4$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.) 
  d4$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.)
  d4$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..kg.ha.), 0, d$Zn.fertilizer.amount..kg.ha.) 
  d4$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) 
  d4$soil_type <- d$Soil.type

  # d4$soil_pH <- # To be requested from the author
  # d4$soil_SOC <- # To be requested from the author
  # d4$soil_sand <- # To be requested from the author
  # d4$soil_clay <- # To be requested from the author
  # d4$soil_N <- # To be requested from the author
  # d4$soil_K <- # To be requested from the author
  # d4$soil_P_total <- # To be requested from the author
  # d4$soil_P_available <- # To be requested from the author
  
## process 004_2019_Wheat_ ICRISAT-AR_ETH.xlsx
  f <- ff[basename(ff) == "004_2019_Wheat_ ICRISAT-AR_ETH.xlsx"]
  
  d <- carobiner::read.excel(f)
  d <- data.frame(d)
  
  d5 <- data.frame(country = d$Country) # Create parallel dataframe
  d5$country <- d$Country
  d5$adm1 <- d$Region.state
  d5$adm3 <- d$LGA.District
  d5$location <- d$village
  d5$trial_id <- paste0("004_2019_Wheat_ ICRISAT-AR_ETH", '.', d$village)
  d5$start_date <- as.character(as.Date(d$Planting.date))
  d5$end_date <- as.character(as.Date(d$Harvest.date))
  d5$longitude <- ifelse(d5$location == "Goshebado", 39.446,
                         ifelse(d5$location == "Lemo", 37.851, 40.215))
  d5$latitude <- ifelse(d5$location == "Goshebado", 9.740,
                        ifelse(d5$location == "Lemo", 5.436, 7.068))
  d5$on_farm <- TRUE
  d5$is_survey <- FALSE
  d5$treatment <- d$Treatment

  # Generate replicate column
  for (r in 1:nrow(d)) { # For each row in d2
    if(r == 1){ # If it is the first row...
      d5$rep[r] <- 1 # Assign 1st replicate...
      t <- 1 # And start a counter.
    } else if(d$Treatment[r] == d$Treatment[r-1]){ # If the treatment is the same name, it is a replicate...
      d5$rep[r] <- t+1 # Therefore add the next replicate...
      t <- t+1 # And add to the counter.
    } else {
      d5$rep[r] <- 1 # Else, it is a new treatment, therefore assign the 1st replicate...
      t <- 1 # And start a new counter.
    }
  }
  d5$crop <- tolower(d$Crop)
  d5$variety <- d$Variety
  d5$previous_crop <- tolower(d$Crop.system)
  # d5$biomass <- d$Biomass..kg.ha. # Biomass is collected as fresh biomass
  d5$yield <- d$Yield..kg.ha. + (d$Yield..kg.ha.*0.13) # Yield data is measured as dry weight 
  d5$residue_yield <- d$Stover.yield..kg.ha # Residue data is measured as dry weight
  # Type of fertilizer applied is not clear when several are applied
  d5$fertilizer_type <- ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "Urea + DAP",
                                 ifelse(d$Treatment == "NPK", "Urea + DAP + potassium nitrate",
                                        ifelse(d$Treatment == "NPKS", "Urea + DAP + potassium sulfate", "Urea + DAP + potassium sulfate + zinc sulfate")))
  d5$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
  d5$N_splits <- 2 # There were two N_splits: Basal (50%) and top dressing (50%)
  ###d5$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
  d5$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount..kg.ha.), 0, d$P.fertilizer.amount..kg.ha.)
  d5$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..kg.ha.), 0, d$K.fertilizer.amount..kg.ha.) 
  d5$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..kg.ha.), 0, d$Zn.fertilizer.amount..kg.ha.) 
  d5$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..kg.ha.), 0, d$S.fertilizer.amount..kg.ha.) 
  d5$soil_type <- d$Soil.type
  # d5$soil_pH <- # To be requested from the author
  # d5$soil_SOC <- # To be requested from the author
  # d5$soil_sand <- # To be requested from the author
  # d5$soil_clay <- # To be requested from the author
  # d5$soil_N <- # To be requested from the author
  # d5$soil_K <- # To be requested from the author
  # d5$soil_P_total <- # To be requested from the author
  # d5$soil_P_available <- # To be requested from the author
  
## Append the tables together
  d <- rbind(d2, d3, d4, d5)
  
## Filter only relevant variables
  d <- d2[,c("country", "adm1", "adm3", "location", "trial_id", "longitude", "latitude", "start_date", "end_date", "on_farm", "is_survey", "treatment", "rep", "crop", "variety", "previous_crop", "yield", "residue_yield", "fertilizer_type", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "S_fertilizer","soil_type")]
## Add dataset ID
  d$dataset_id <- dataset_id

	d$previous_crop <- gsub("tef", "teff", d$previous_crop)
	d$previous_crop <- gsub("fababean", "faba bean", d$previous_crop)

	ff <- gsub(" \\+ ", "; ", d$fertilizer_type)
	ff <- gsub("Urea", "urea", ff)
	ff <- gsub("potassium sulfate", "SOP", ff)
	ff <- gsub("potassium nitrate", "KNO", ff)
	ff <- gsub("zinc sulfate", "ZSO", ff)
	d$fertilizer_type <- ff

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)

}
