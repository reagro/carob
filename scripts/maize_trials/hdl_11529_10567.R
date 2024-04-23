# R script for "carob"


carob_script <- function(path) {
  
"Summary results and individual trial results from the International Late Both Hybrid - ILBH, (Transition Zone by Subtropical Intersynthetic Non-Conventional Hybrids - CHTTZLWY) conducted in 2006. "
  
	uri <- "hdl:11529/10567"
	group <- "maize_trials"
	ff <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "CIMMYT",
		publication= NA,
		project="Global Maize Program",
		data_type= "experiment",
		carob_contributor= "Mitchelle Njukuya",
		carob_date="2024-03-12"
	)
  
    f <- ff[basename(ff) == "06CHTTZLWY17-1.xls"]
    r <- carobiner::read.excel(f) 
    r <-r[22:30,2:32]
    
    d <- data.frame(
		variety=r$BreedersPedigree1, 
		yield=as.numeric(r$GrainYieldTons_FieldWt) * 1000, 
		anthesis_days=as.integer(round(as.numeric(r$AnthesisDate))),
		rlper=r$RootLodgingPer,
		moist=r$GrainMoisturePer,
		plant_density=as.numeric(r$PlantStand_NumPerPlot)
	) 
    
    d$crop <- "maize"  
    d$yield_part = "grain"
    d$country <- "Mexico"
    d$location <- "San Jose Hurbide, Gto"
    d$longitude <- -100.3833
    d$latitude <- 22.9833
    d$elevation <- 2103
    d$planting_date <- "2006-05-02"
    d$harvest_date  <- "2006-12-05"
    
    d$on_farm <- TRUE
    d$striga_trial <- FALSE
    d$striga_infected <- FALSE
    d$borer_trial <- FALSE
    d$trial_id <- "1"
		
	carobiner::write_files(dset, d, path=path)
}





