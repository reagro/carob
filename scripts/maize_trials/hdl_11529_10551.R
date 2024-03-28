# R script for "carob"
#issues
#planting and harvest dates missing for 06EVT13S26-1.xls and 06EVT13S26-1.xls



carob_script <- function(path) {
  
  "Description:

Summary results and individual trial results from the International Late Yellow Variety - ILYV, 
(Tropical Late Yellow Normal and QPM Synthetic Variety Trial - EVT13S) conducted in 2006

"
  #### Identifiers
  uri <- "hdl:11529/10551"
  group <- "maize_trials"
  
  dataset_id <- carobiner::simple_uri(uri)
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  
  ##### dataset level metadata 
  dset <- data.frame(
    carobiner::extract_metadata(js, uri, group=group),
    data_citation="Global Maize Program, 2018, International Late Yellow Variety Trial - ILYV0604, https://hdl.handle.net/11529/10551, CIMMYT Research Data & Software Repository Network, V1",
    data_institutions = "CIMMYT",
    publication= NA,
    project="Global Maize Program",
    data_type= "experiment",
    carob_contributor= "Mitchelle Njukuya",
    carob_date="2024-03-28"
  )
  
  ##### PROCESS data records
  
  get_data <- function(fname, id,country,longitude,latitude,elevation) {
    f <- ff[basename(ff) == fname]
    r <- carobiner::read.excel(f) 
    r <-r[22:42,2:42]
    
    d <- data.frame( 
      trial_id = id,
      crop = "maize",
      dataset_id = dataset_id,
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
      country=country,
      longitude=longitude,
      latitude=latitude,
      elevation = elevation)}
  
  d0 <- get_data("06EVT13S9-1.xls",1,"Mexico", -96.6667, 19.3333, 15)  
  d0$location <- "Cotaxtla, Veracruz"
  d0$planting_date <- "2006-07-11"
  d0$harvest_date  <- "2006-11-06"
  
  d1 <- get_data("06EVT13S10-1.xls",2,"Mexico", -90.6833, 19.5167, NA)  
  d1$location <- "Champoton, Campeche"
  d1$planting_date <- "2006-05-01"
  d1$harvest_date  <- "2006-11-01"
  
  d2 <- get_data("06EVT13S12-1.xls",3,"India", 77.2667, 15.6333, 415)  
  d2$location <- "Adoni Mandal"
  d2$planting_date <- "2006-07-11"
  d2$harvest_date  <- "2006-11-10" 
  
  d3 <- get_data("06EVT13S18-1.xls",4,"Nigeria", 11.0667, 7.7, 648)  
  d3$location <- "Zaria"
  d3$planting_date <- NA
  d3$harvest_date  <- NA
  
  d4 <- get_data("06EVT13S26-1.xls",5,"Mexico", -96.6667, 19.3333, 15)  
  d4$location <- "Cotaxtla, Veracruz"
  d4$planting_date <- NA
  d4$harvest_date  <- NA
  
  d5 <- get_data("06EVT13S30-1.xls",6,"Ghana", -1.5833, 6.75, 720)  
  d5$location <- "Fumesua"
  d5$planting_date <- "2007-04-19"
  d5$harvest_date  <- "2007-08-09"
  
  d6 <- get_data("06EVT13S31-1.xls",7,"Ghana", -1.3667, 7.3833, 232)  
  d6$location <- "Ejura"
  d6$planting_date <- "2007-05-08"
  d6$harvest_date  <- "2007-12-09"
  
  d <- carobiner::bindr(d0, d1, d2, d3, d4, d5, d6)
  
  
  carobiner::write_files(dset, d, path=path)
}
