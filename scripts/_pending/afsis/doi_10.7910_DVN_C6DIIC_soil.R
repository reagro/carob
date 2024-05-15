# R script for "carob"


carob_script <- function(path) {
  
"Omission trials conducted in 5 countries under AfSIS Phase 1 under CIAT"
  
	uri <- "doi:10.7910/DVN/C6DIIC"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=5),
		data_institutions = "CIAT",
		publication= "doi:10.1007/s1070",
		project="AfSIS",
		data_type= "experiment",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-14"
	)
  
	f1 <- ff[basename(ff) == "03. DiagnosticTrials_2009-2012_SSA_Wetchem.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "02. DiagnosticTrials_2009-2012_SSA_Yld.xlsx"]
	r2 <- carobiner::read.excel(f2)
  
	r <- merge(r2, r1, all=TRUE)
	r <- unique(r)
  
  ## process file(s)
  
  ## select the variables of interest and assign them to the correct name
  # M3.EC to be converted from uS/cm to mS/cm divide by 1000
  d <- data.frame(
    trial_id = r$FieldID,
    soil_pH = r$pH,
    soil_EC = r$m3.ECS/1000,
    soil_Al = r$m3.Al,
    soil_B = r$m3.B,
    soil_Ca = r$m3.Ca,
    soil_Cu = r$m3.Cu,
    soil_Fe = r$m3.Fe,
    soil_K = r$m3.K,
    soil_Mg = r$m3.Mg,
    soil_Mn = r$m3.Mn,
    soil_Na = r$m3.Na,
    soil_P_total = r$m3.P,
    soil_S = r$m3.S,
    soil_Zn = r$m3.Zn,
    soil_Ex_acidity = r$m3.Hp,
    soil_PSI = r$PSI,
    soil_Ex_Na = r$ExNa,
    soil_Ex_Ca = r$ExCa,
    soil_Ex_K = r$ExK,
    soil_Ex_Bas = r$ExBas,
    soil_clay = r$psa.c4clay,
    soil_silt = r$psa.c4silt,
    soil_sand = r$psa.c4sand,
    soil_C = r$Total_Carbon,
    soil_SOC = r$Acidified_Carbon,
    soil_N = r$Total_Nitrogen
	country <- r$Country,
    site <- r$Site,
	longitude <- r$Flong,
	latitude <- r$Flat,
  )
  
  
  d$on_farm <- TRUE
  d$irrigated <- FALSE
    ## the treatment code	
   # d$treatment <- 
    
  d$latitude[d$latitude == 0] <- NA
  d$longitude[d$longitude == 0] <- NA
  d$longitude[d$site == "Finkolo"] <- -5.5113
  d$latitude[d$site == "Finkolo"] <- 11.2692

  d$soil_PSI <- ifelse(d$soil_PSI < 0, 0,d$soil_PSI)

#  missing <- which(is.na(d$latitude) == TRUE)
 
#  for(i in 1:length(missing)){
 #   latlon <- as.data.frame(carobiner::geocode(d$country[missing[i]], d$site[missing[i]])$df)
  #  d$latitude[missing[i]] <- latlon$lat
#  }
  
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  # d$crop <- 
  #   d$variety <- 
  #   
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  # d$planting_date <- as.character(as.Date(   ))
  # d$harvest_date  <- as.character(as.Date(    ))
  # 
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  # d$P_fertilizer <- 
  #   d$K_fertilizer <-
  #   d$N_fertilizer <- 
  #   d$S_fertilizer <- 
  #   d$lime <- 
  #   ## normalize names 
  #   d$fertlizer_type <- 
  #   
  #   d$inoculated <- TRUE or FALSE
  # d$inoculant <- 
    
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  # d$yield <- 
  #   #what plant part does yield refer to?
  #   d$yield_part <- 
  #   
    # all scripts must end like this
    carobiner::write_files(path, dset, d)
}
