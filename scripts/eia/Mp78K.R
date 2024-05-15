# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. ...

carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...

"
  
  uri <- "Mp78K"
  group <- "eia"
  
  dset <- data.frame(
    # Need to fill-in metadata...
    # carobiner::read_metadata(uri, path, group, major=2, minor=0),
    uri = uri,
    dataset_id = uri,
    authors = "Amit Srivastava",
    data_institutions = "IRRI",
    title = NA,
    description = "Validations of the Planting Date SA Use Case MVP",
    group = group,
    license = 'Some license here...',
    carob_contributor = 'IITA Biometric Unit',
    data_citation = '...',
    project = 'Excellence in Agronomy',
    use_case = "Planting Date SA",
    data_type = "on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
    carob_date="2024-04-25"
  )
  
  # Manually build path (this can be automated...)
  ff <- list.files(paste0(getwd(), '/data/raw/', group, '/', uri), full.names = TRUE)
  
  # Retrieve relevant file
  f <- ff[basename(ff) == "EiA_Rabi_Wheat_Production_survey_data_2022-23.csv"]
  # Read relevant file
  # r <- read.csv(f, colClasses = "character") # convert everything to character so it be pivoted longer
  r <- read.csv(f)
  # Build initial DF ... Start from here
  r <- Filter(function(x)!all(is.na(x)), r)# remove empty columns
  r1 <- r[,2:162] # separate T1
  names(r1) <- stringr::str_sub(names(r1),3) # remove the first two characters from colnames 
  names(r1) <- sub("_T1","", names(r1))
  names(r1) <- sub("T1","", names(r1))
  r1$treatment <- "T1" # create variable T1
  r2 <- r[,c(2:21,163:300)] # separate T2
  names(r2) <- stringr::str_sub(names(r2),3) # remove the first two characters from colnames 
  names(r2) <- sub("_T2","", names(r2))
  names(r2) <- sub("T2","", names(r2))
  r2$treatment <- "T2" # create variable T2
  r <- dplyr::bind_rows(r1,r2)
  d <- data.frame(
    country = "India",
    crop = "wheat",
    yield_part = "grain",	
    on_farm = TRUE,
    is_survey = TRUE,
    adm1=r$state,
    adm2=r$district,
    adm3=r$subDistrict,
    adm4=r$village,
    trial_id = rep(1:71, 2),
    plot_name=r$plot, # Not in carob
    location=r$location,
    season=r$season,
    latitude=r$PlotGPS.Latitude,
    longitude=r$PlotGPS.Longitude,
    elevation=r$PlotGPS.Altitude,
    gps_accuracy=r$PlotGPS.Accuracy, # Not in carob
    crop_cut=r$cropCutDone, 
    gender=r$fGender, # Not found in carob
    treatment=r$treatment,
    variety=r$VarName, 
    harvest_date=r$harvYear,
    plot_area=r$EiAcropAreaAcre,
    soil_texture=r$soilTexture,
    soil_quality=r$soilPerception,
    previous_crop=r$prevCrop,
    land_prep_method=r$LandPrep,
    transplanting_date=r$seedingSowingTransDate,
    seed_source=r$seedSource,
    seed_amount=r$cropSeedAmt,
    irrigated=r$irrigate,
    irrigation_source=r$irrigSource,
    irrigation_stage=r$irrigGrthStage,
    fertilizer_type="DAP;urea;NPK;NPKS;gypsum",
    N_fertilizer=r$Nitrogen_Kg_ha,
    P_fertilizer=r$Phosphorus_Kg_ha,
    K_fertilizer=r$Potassium_Kg_ha,
    drought_stress=r$drought,
    drought_stage=r$droughtGS, # not in carob
    crop_area=r$EiAcropAreaAcre, # Not in carob
    harvest_days=r$cropDurationDays,# assumed to be days to harvest
    harvestMethod=r$harvestMethod, # not in carob
    insecticide_used=r$insecticides,
    pesticide_used=r$pesticides,
    lodging=r$lodgingPercent,# not in carob
    threshing_method=r$threshing, # not in carob
    yield=r$tonPerHectare # assume to be yield
  )
# Convert from tons/ha to kg/ha
  d$yield <- d$yield*1000

  # Recode variables
  d$previous_crop <- dplyr::recode(d$previous_crop,
                            "Rice"="rice","Wheat"="wheat")
  d$previous_crop[d$previous_crop=="Fallow"] <- NA # Fallow not a crop
  d$land_prep_method <- ifelse(d$land_prep_method == "NoTillage","no-till","tillage")

  carobiner::write_files(dset, d, path=path)
}
