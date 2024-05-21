# R script for "carob"


carob_script <- function(path) {
  
  "

Summary results and individual trial results from the International Late Yellow Variety - ILYV, 
(Tropical Late Yellow Normal and QPM Synthetic Variety Trial - EVT13S) conducted in 2006

"
  #### Identifiers
  uri <- "hdl:11529/10563"
  group <- "maize_trials"
  
  
  #### Download data 
  ff <- carobiner::get_data(uri, path, group)
  
  ##### dataset level metadata 
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=0),
    #data_citation="Global Maize Program, 2019, International Early Yellow Hybrid Trial - IEYH0612, https://hdl.handle.net/11529/10563, CIMMYT Research Data & Software Repository Network,V1",
    data_institutions = "CIMMYT",
    publication= NA,
    project="Global Maize Program",
    data_type= "experiment",
		treatment_vars = "variety_code;longitude;latitude",
    carob_contributor= "Mitchelle Njukuya",
    carob_date="2024-03-12"
  )
  
  ##### PROCESS data records
  
  get_data <- function(fname, id,country,longitude,latitude,elevation) {
    f <- ff[basename(ff) == fname]
    r <- carobiner::read.excel(f) 
    r <-r[22:38,2:42]
    
    d <- data.frame( 
      trial_id = id,
      crop = "maize",
      
      on_farm = TRUE,
      striga_trial = FALSE, 
      striga_infected = FALSE,
      borer_trial = FALSE,
      yield_part = "grain",
      variety=r$BreedersPedigree1,
      yield=as.numeric(r$GrainYieldTons_FieldWt)*1000,
      asi=as.numeric(r$ASI),
      plant_height=as.numeric(r$PlantHeightCm),
      e_ht = as.numeric(r$EarHeightCm),
      rlper = as.numeric(r$RootLodgingPer),
      slper = as.numeric(r$StemLodgingPer),
      husk = as.numeric(r$BadHuskCoverPer),
      e_rot = as.numeric(r$EarRotTotalPer),
      moist = as.numeric (r$GrainMoisturePer),
      plant_density = as.numeric(r$PlantStand_NumPerPlot),
      e_asp = as.numeric(r$EarAspect1_5),
      p_asp = as.numeric(r$PlantAspect1_5),
      gls = r$GrayLeafSpot1_5,
      rust = r$CommonRust1_5,
      blight = r$LeafBlightTurcicum1_5,
      country=country,
      longitude=longitude,
      latitude=latitude,
      elevation = elevation)}
  
  d0 <- get_data("06CHTTEY10-1.xls",1,"Bolivia", -63.15, 17.7667, 398)  
  d0$location <- "El Vallecito"
  d0$planting_date <- "2006-12-06"
  d0$harvest_date  <- "2007-03-18"
  
  d1 <- get_data("06CHTTEY21-1.xls",2,"Mexico", -100.6833, 20.5333, 1771)  
  d1$location <- "Apasco el Gde"
  d1$planting_date <- "2006-05-10"
  d1$harvest_date  <- "2006-11-30"
  
  d2 <- get_data("06CHTTEY34-1.xls",3,"Mexico", -96.6667, 19.3333, 15)  
  d2$location <- "Cotaxtla Veracruz"
  d2$planting_date <- "2006-09-27"
  d2$harvest_date  <- "2007-01-30"
  
  d <- carobiner::bindr(d0, d1, d2 )
  
  
  carobiner::write_files(dset, d, path=path)
}





