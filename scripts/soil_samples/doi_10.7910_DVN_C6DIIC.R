# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
Omission trials conducted in 5 countries under AfSIS Phase 1 under CIAT"
  
  #### Identifiers
  uri <- "doi:10.7910/DVN/C6DIIC"
  group <- "soil_samples"
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ##### dataset level metadata 
  dset <- data.frame(
    # change the major and minor versions if you see a warning
    carobiner::read_metadata(uri, path, group, major=2, minor=5),
    data_institutions = "CIAT",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= "doi:10.1007/s1070",
    # data_type can be e.g. "on-farm experiment", "survey", "compilation"
    project="AfSIS",
    data_type= "experiment",
    carob_contributor= "Andrew Sila",
    carob_date="2024-05-14"
  )
  
  ##### PROCESS data records
  
  # read data 
  
  f0 <- ff[basename(ff) == "03. DiagnosticTrials_2009-2012_SSA_Wetchem.csv"]
  r0 <- read.csv(f0)
  # read field data
  f1 <- ff[basename(ff) == "02. DiagnosticTrials_2009-2012_SSA_Yld.xlsx"]
  r1 <- carobiner::read.excel(f1)
  
  r <- merge(r1[, c('FieldID', 'Flat', 'Flong')], r0, by = 'FieldID')
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
    soil_Na = r$m3.Mn,
    soil_P_available = r$m3.P,
    soil_S = r$m3.Zn,
    soil_Zn = r$m3.Zn,
    #soil_Acidity = r$m3.Hp,
    #soil_PSI = r$PSI,
    soil_Ex_Na = r$ExNa,
    soil_Ex_Ca = r$ExCa,
    soil_Ex_K = r$ExK,
    #soil_ExBas = r$ExBas,
    soil_clay = r$psa.c4clay,
    soil_silt = r$psa.c4silt,
    soil_sand = r$psa.c4sand,
    soil_C = r$Total_Carbon,
    soil_SOC = r$Acidified_Carbon,
    soil_N = r$Total_Nitrogen
  )
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  #d$on_farm <- TRUE
   # d$is_survey <- FALSE
    #d$irrigated <- FALSE
    ## the treatment code	
   # d$treatment <- 
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- r$Country
    d$site <- r$Site
    ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  d$longitude <- r$Flong
    d$latitude <- r$Flat
    
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

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
