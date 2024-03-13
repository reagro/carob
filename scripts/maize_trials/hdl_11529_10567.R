# R script for "carob"


carob_script <- function(path) {
  
  "Description:

	Summary results and individual trial results from the International Late Both Hybrid - ILBH, (Transition Zone by Subtropical Intersynthetic Non-Conventional Hybrids - CHTTZLWY) conducted in 2006. "
  
  #### Identifiers
  uri <- "hdl:11529/10567"
  group <- "maize_trials"
  
  dataset_id <- carobiner::simple_uri(uri)
  
  #### Download data 
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  
  ##### dataset level metadata 
  dset <- data.frame(
    carobiner::extract_metadata(js, uri, group=group),
    data_citation="Global Maize Program, 2019, International Late Both Hybrid Trial - ILBH0651, https://hdl.handle.net/11529/10567, CIMMYT Research Data & Software Repository Network, V1",
    data_institutions = "CIMMYT",
    publication= NA,
    project="Global Maize Program",
    data_type= "experiment",
    carob_contributor= "Mitchelle Njukuya",
    carob_date="2024-03-12"
  )
  
  ##### PROCESS data records
  
 # get_data <- function(fname, id,country,longitude,latitude,elevation) {
    f <- ff[basename(ff) == "06CHTTZLWY17-1.xls"]
    r <- carobiner::read.excel(f) 
    r <-r[22:30,2:32]
    
    d <- data.frame(variety=r$BreedersPedigree1, yield=r$GrainYieldTons_FieldWt, anthesis=r$AnthesisDate,rlper=r$RootLodgingPer,moist=r$GrainMoisturePer,plant_density=r$PlantStand_NumPerPlot) 
    
    d$yield <-as.numeric(d$yield)*1000
    d$anthesis <- as.numeric(d$anthesis)
    d$plant_density <- as.numeric(d$plant_density)
    
    d$crop <- "maize"  
    d$yield_part = "grain"
    d$country <- "Mexico"
    d$location <- "San Jose Hurbide, Gto"
    d$longitude <- -100.3833
    d$latitude <- 22.9833
    d$elevation <- 2103
    d$planting_date <- "2006-05-02"
    d$harvest_date  <- "2006-12-05"
    
    d$dataset_id = dataset_id
    d$on_farm = TRUE
    d$striga_trial = FALSE
    d$striga_infected = FALSE
    d$borer_trial = FALSE
    d$trial_id <- paste0(d$dataset_id,"_",r$Entry)
      
  
  carobiner::write_files(dset, d, path=path)
}





