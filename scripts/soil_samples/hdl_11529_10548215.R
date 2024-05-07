# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "
Crop cut survey in 2015 conducted by EIAR and CIMMYT. Replicated crop cuts of 16m2 in farmers fields along with additional data on nutrient use and variety, and soil sample.\r\n

"
  
  #### Identifiers
  uri <- "hdl:11529/10548215"
  group <- "soil_samples"
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ##### dataset level metadata 
  dset <- data.frame(
    # change the major and minor versions if you see a warning
    carobiner::read_metadata(uri, path, group, major=2, minor=1),
    #data_institutions = c("CIMMYT","EIAR"),
    data_institutions = "CIMMYT",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= "",
    # data_type can be e.g. "on-farm experiment", "survey", "compilation"
    project='TAMASA',
    data_type= "survey",
    carob_contributor= "Andrew Sila",
    carob_date="2024-05-06"
  )
  
  ##### PROCESS data records
  
  # read data 
  
  f0 <- ff[basename(ff) == "ET_Baseline_EIAR_2015.xls"]
  #r <- read.csv(f)
  # or  r <- carobiner::read.excel(f)
  r <- readxl::read_xls(f0,  sheet="Revised_data")
  
  ## process file(s)
  
  ## select the variables of interest and assign them to the correct name
  d <- data.frame(
    crop="wheat", 
    latitude = r$Latitude,
    longitude = r$Longitude,
    id = r$Barcodes.for.soil.sample..0.20.,
    soil_SOC = r$Carbon....,
    soil_pH = r$pH,
    soil_Al = r$Al..mg.kg.,
    soil_Ca = r$Ca...mg.kg.,
    soil_EC = r$EC.S..dS.m.,
    soil_S = r$S...mg.kg.,
    soil_Mn = r$Mn...mg.kg.,
    soil_P = r$P...mg.kg.,
    soil_Zn = r$Zn...mg.kg.,
    soil_K = r$K...mg.kg.,
    soil_M = r$Mg...mg.kg.,
    soil_Na = r$Na...mg.kg.,
    soil_Fe = r$Fe...mg.kg.,
    soil_B = r$Boron...mg.kg.,
    soil_N = r$Nitrogen....
  )
  
  #### about the data #####
  ## (TRUE/FALSE)
  d$on_farm <- FALSE
    d$is_survey <- TRUE
    d$irrigated <- FALSE
    ## the treatment code	
   # d$treatment <- 
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- "Ethiopia"
   # d$site <- 
   # d$adm1 <- 
   # d$adm2 <- 
   # d$adm3 <- 
    d$elevation <- r$Altitude
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  #d$longitude <- 
  # d$latitude <- 
    
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  #d$crop <- 
   d$variety <- r$Name.of.variety
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  #d$planting_date <- as.character(as.Date(   ))
  #d$harvest_date  <- as.character(as.Date(    ))
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  #d$P_fertilizer <- 
   # d$K_fertilizer <-
    #d$N_fertilizer <- 
    #d$S_fertilizer <- 
   # d$lime <- 
    ## normalize names 
    #d$fertlizer_type <- 
    
   # d$inoculated <- TRUE or FALSE
  #d$inoculant <- 
    
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  #d$yield <- 
    #what plant part does yield refer to?
   # d$yield_part <- 
    
    # all scripts must end like this
    d <- na.omit(d)
    carobiner::write_files(path, dset, d)
}
