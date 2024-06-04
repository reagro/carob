# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...

"
  
  uri <- "doi:SA-PlantingDate-Validation"
  group <- "eia"
  
  dset <- data.frame(
    # Need to fill-in metadata...
    # carobiner::read_metadata(uri, path, group, major=2, minor=0),
    uri = carobiner::simple_uri(uri),
    dataset_id = uri,
    authors = "Amit Srivastava",
    data_institute = "IRRI",
    title = NA,
    description = "Validations of the Planting Date SA Use Case MVP",
    group = group,
    license = 'Some license here...',
    carob_contributor = 'IITA Biometric Unit',
    data_citation = '...',
    project = 'Excellence in Agronomy - Planting Date SA validations',
    data_type = "on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
    carob_date="2024-04-25",
    treatment_vars = "fertilizer_type"
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/SA-PlantingDate-Validation/", full.names = T))
  
  # Retrieve relevant file
  f <- ff[basename(ff) == "EiA_Rabi_Wheat_Production_survey_data_2022-23.csv"]
  # Read relevant file
  r <- read.csv(f)
  # Build initial DF ... Start from here
  r <- Filter(function(x)!all(is.na(x)), r)# remove empty columns
  r1 <- r[,2:162] # separate T1
  names(r1) <- substr(names(r1),3,nchar(names(r1))) # drop the first two characters from colnames
  names(r1) <- sub("_T1","", names(r1))
  names(r1) <- sub("T1","", names(r1))
  r1$treatment <- "T1" # create variable T1
  r2 <- r[,c(2:21,163:300)] # separate T2
  names(r2) <- substr(names(r2),3,nchar(names(r2))) # drop the first two characters from colnames 
  names(r2) <- sub("_T2","", names(r2))
  names(r2) <- sub("T2","", names(r2))
  r2$treatment <- "T2" # create variable T2
  
  r <- carobiner::bindr(r1,r2)

  r$cropDurationDays[r$cropDurationDays <0 ] <- NA # convert negative values to NA
  
  d <- data.frame(
    country = "India",
    crop = "wheat",
    yield_part = "grain",	
    on_farm = TRUE,
    is_survey = FALSE,
    adm1=r$state,
    adm2=r$district,
    adm3=r$subDistrict,
    location=r$village,
    trial_id = as.character(rep(1:71, 2)),
    # plot_name=r$plot, # Not in carob # # EGB: Is it really necessary??
    # location=r$location,
    season=r$season,
    latitude=r$PlotGPS.Latitude,
    longitude=r$PlotGPS.Longitude,
    elevation=r$PlotGPS.Altitude,
    # crop_cut=r$cropCutDone, # # EGB: Is it really necessary??
    treatment=ifelse(r$treatment == "T1", "MVP recommendation", "Local recommendation"),
    variety=r$VarName,
    planting_date=as.character(as.Date(r$harvDate)-r$cropDurationDays),
    harvest_date=r$harvDate,
    plot_area=r$EiAcropAreaAcre*4046.86, # Acre to m2
    soil_texture=tolower(r$soilTexture),
    soil_quality=tolower(r$soilPerception),
    previous_crop=tolower(r$prevCrop),
    # EGB:
    # # Aligning towards terminag, probably need to add "no-tillage"
    land_prep_method=ifelse(r$LandPrep == "NoTillage", "no-tillage", "conventional"),
    transplanting_date=r$seedingSowingTransDate,
    seed_source=r$seedSource,
    seed_amount=r$cropSeedAmt,
    irrigated=ifelse(r$irrigate == "yes", TRUE, FALSE),
    # EGB
    # # Would be good to check and standardize this (?)
    irrigation_source=r$irrigSource,
    # EGB
    # # Would be good to check and standardize this (?)
    # irrigation_stage=r$irrigGrthStage, # Removing for now ...
    irrigation_number = r$irrigTimes,
    # EGB
    # # Need to review this...
    season_constraint = paste0(ifelse(r$drought == "yes", "drought", NA), "; ", ifelse(r$flood == "yes", "flood", NA)),
    fertilizer_type=paste0(gsub("totAmt", "", colnames(r)[grep("totAmt", colnames(r))]), collapse = "; "),
    N_fertilizer=r$Nitrogen_Kg_ha,
    P_fertilizer=r$Phosphorus_Kg_ha,
    K_fertilizer=r$Potassium_Kg_ha,
    OM_used = ifelse(r$FYM == "yes", TRUE, FALSE),
    OM_amount = r$FYMAmount * 1000, # Assuming ton/ha
    OM_type = ifelse(r$FYM == "yes", "farmyard manure", NA),
    drought_stress=r$drought,
    # drought_stage=r$droughtGS, # not in carob
    # crop_area=r$EiAcropAreaAcre, # Not in carob
    harvest_days=r$cropDurationDays,# assumed to be days to harvest
    # harvestMethod=r$harvestMethod, # not in carob
    insecticide_used=r$insecticides,
    # pesticide_used=r$pesticides,
    lodging=r$lodgingPercent,# not in carob
    threshing_method=r$threshing, # not in carob
    yield=r$tonPerHectare*1000 # assume to be yield. Convert from tons/ha to kg/ha
  )

  # EGB: 
  # # Carob admits "fallow" as an alternative to "none" (https://github.com/reagro/terminag/blob/43c69063ec93ba1805f83dd51b76d6b9748bda75/values/values_crop.csv#L293)
  d$previous_crop[d$previous_crop=="fallow"] <- "none" # Fallow not a crop 
  d$insecticide_used[d$insecticide_used=="no"] <- "none"
  # Replace empty cells with NAs
  d[d==""] <- NA # Empty cells assumed to be missing
  carobiner::write_files(dset, d, path=path)
}

# carob_script(path)
