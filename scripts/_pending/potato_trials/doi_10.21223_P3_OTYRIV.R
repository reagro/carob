# R script for "carob"


carob_script <- function(path) {
  
  "

    The B3C3 population came from crossing the elite clones of B3C2 population, in 2011 
    under quarantine greenhouses at La Molina. For this, 30,000 genotypes (60 families with 
    500 seeds each) were planted. At harvest, 21685 genotypes were selected (72%). These 
    selected clones were planted in La Victoria, Huancayo, to increase the number of tubers 
    and make selection for agronomic attributes, where 3123 clones (10.41% of the original 
    population) were selected. This dataset is part of the following selection where the 
    selected clones were evaluated for three seasons (2012, 2013 and 2014), in Oxapampa where 
    the environmental conditions (rain, relative humidity, temperature) are great to have a 
    high pressure of late blight allowing us to select clones with resistance to this disease. 
    In 2012-2013 at Oxapampa, 3005 clones were planted in observation plots of 5 plants each 
    with two replications. In this experiment, the resistance to late blight of the plants was 
    recorded. And at harvest, the marketable and total tuber yield were measured. 
    Finally, 507 clones were selected.

"
  
  uri <- "doi:10.21223/P3/OTYRIV"
  group <- "potato_trials"
 
  ff <- carobiner::get_data (uri, path, group)
  
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=3),
    publication= "",
    data_institute = "CIP",
    data_type="experiment", 
    carob_contributor="Stephen Gichuhi",
    carob_date="2024-03-08"
  )
  
  
  f <- ff[basename(ff) == "PTLate_blight092012_OXAPMP_exp8.xlsx"]
  
  r <- readxl::read_excel(f) |> as.data.frame()
  
  ## process file(s)
  r <- carobiner::read.excel(f)
  d <- r[,c("Trial_name","Country","Latitude", "Longitude")]
  colnames(d) <- c("trial_id","country","latitude","longitude")
  
  ## use a subset
  d <- carobiner::change_names(r, from, to)
  
  
  #### about the data #####
  ## (TRUE/FALSE)
  
  
  d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$is_experiment <- TRUE
    d$irrigated <- TRUE
    ## the treatment code	
    d$treatment <- 
    
    ##### Location #####
  ## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
  ## you can use carobiner::fix_name()
  d$country <- "Peru"
  d$location <- "La Victoria, Huancayo" ##from description
  d$row_spacing<- as.numeric(n$`Distance_between_rows_(m)`)*100 # cm
  d$plant_spacing<- as.numeric(n$`Distance_between_plants_(m)`)*100 # cm
  d$plant_density <- as.numeric(n$`Planting_density_(plants/Ha)`)
  ## each site must have corresponding longitude and latitude
  ## see carobiner::geocode
  ##from r
  d$longitude <- -75.4043
  d$latitude <- -10.57745
    
    ##### Crop #####
  ## normalize variety names
  ## see carobiner::fix_name
  d$crop <- "potato"
  d$yield_part <- "tubers"
  d$variety <- 
    
    ##### Time #####
  ## time can be year (four characters), year-month (7 characters) or date (10 characters).
  ## use 	as.character(as.Date()) for dates to assure the correct format.
  d$planting_date <- as.character(as.Date(n$Planting,format= "%d/%m/%Y"),"%Y-%m-%d")
  d$harvest_date  <- as.character(as.Date(n$Harvest,format= "%d/%m/%Y"),"%Y-%m-%d")
  
  ##### Fertilizers #####
  ## note that we use P and K, not P2O5 and K2O
  ## P <- P2O5 / 2.29
  ## K <- K2O / 1.2051
  d$P_fertilizer <- NA
    d$K_fertilizer <- NA
    d$N_fertilizer <- NA
    d$S_fertilizer <- NA
    d$lime <- 
    ## normalize names 
    d$fertlizer_type <- NA
    d$inoculated <- TRUE/FALSE
  d$inoculant <- FALSE
    
    ##### in general, add comments to your script if computations are
    ##### based on information gleaned from metadata, a publication, 
    ##### or when they are not immediately obvious for other reasons
    
    ##### Yield #####
  d$biomass_total <- 
    
    d$yield <- 
    #what plant part does yield refer to?
    d$yield_part <- 
    
    # all scripts must end like this
    carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

