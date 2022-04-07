# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:Maize response to N and P. Tofa, A., Kamara, A. Y., Babaji, B. A., Ademulegun, T. D., & Aliyu, K. T. (2021). Maize response to N and P [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/07RT-7A40/D"

    
####
  uri <- "https://doi.org/10.25502/07rt-7a40/d"
  dataset_id <- agro::get_simple_URI(uri)
  group <- "fertilizer"
  ## dataset level data. Internal annotation for CAROB 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication="",
    carob_contributor="Henry Juarez",
    experiment_type="fertilizer",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## download and read data (Path is important)
    
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)
  
  # Process the trial/farm sites 
    
    f <- ff[basename(ff) == "N and P maize trial_2015_16.csv"]
    d <- read.csv(f)
    
    # process file(s)
    d$dataset_id <- dataset_id
    d$country <- "Nigeria"
    # enrichment with spatial data is better done on the aggregated data for Adm1, Adm2 & Adm3
    d$site <- d$loc
    d$trial_id <- paste0(dataset_id, "-", d$ID) ###
       
    d$latitude <- 10.26858
    d$longitude <- 7.78896
 
    d$season <- d$year
    d$on_farm <- "yes"
    d$N_fertilizer <- d$nrate
    d$P_fertilizer <- d$prate
    d$variety <- d$variety
    d$is_survey <- "no"
    d$treatment <- "Maize response to nitrogen and phosporus fertilizers"
    d$rep <- d$rep
    d$crop <- "maize"
    d$grain_weight <- (d$swt500)*2 ### [swt500	Weight of 500 seeds		g, the original value was multiplied by 2]
    d$biomass_total <- d$tdmm2*10 # Add total biomass (dry matter) in kg/ha )
    d$yield <- d$yield
        
 # Subset to columns of interest
    d <- d[,c("dataset_id", "country", "site", "trial_id", "latitude", "longitude", "season", "on_farm", "N_fertilizer", "P_fertilizer", "variety", "is_survey", "treatment", "rep", "crop", "grain_weight", "yield")]

  # all scripts must end like this
    carobiner::write_files(dset, d, path, dataset_id, group)
  TRUE
}  
