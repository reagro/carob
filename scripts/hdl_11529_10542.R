# R script for "carob"
# license: GPL (>=3)

## ISSUES
# if there are remaining issues, please put these in "meta$notes"

carob_script <- function(path) {

" International Intermediate White Hybrid Trial - IIWH0730 Summary results and individual trial results from the International Intermediate White Hybrid - IIWH, (Elite Subtropical Late White Normal and QPM Hybrid Trial, Basically Single Crosses Trial - CHTSW) conducted in 2007."


## Identifiers
	uri <- "hdl:11529/10542"
	group <- "varieties_maize"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = "International intermediate white hybrid trials",
		data_type = "experiment",
		treatment_vars = "variety;longitude;latitude",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-10-09",
		notes = "I double checked Mexico coordinates I cannot see where the error is originating from",
		design = NA
	)
	
## read data 
	
	f0 <- ff[basename(ff) == "07CHTSW11-1.xls"]
	
	r0 <- r <- carobiner::read.excel(f0)
	r0 <-r0[22:34, 2:46]
	
	d3 <- data.frame( 
	  trial_id = as.character(r0$Name),
	  variety=r0$BreedersPedigree1,
	  yield=as.numeric(r0$GrainYieldTons_GrainWt)*1000,
	  asi=as.numeric(r0$ASI),
	  plant_height=as.numeric(r0$PlantHeightCm),
	  ear_height = as.numeric(r0$EarHeightCm),
	  rlper = as.numeric(r0$RootLodging1_5),
	  slper = as.numeric(r0$StemLodgingPer),
	  husk = as.numeric(r0$BadHuskCover1_5),
	  e_rot = as.numeric(r0$EarRotTotalPer),
	  moist = as.numeric (r0$GrainMoisturePer),
	  plant_density = as.numeric(r0$PlantStand_NumPerPlot),
	  e_asp = as.numeric(r0$EarAspect1_5),
	  p_asp = as.numeric(r0$PlantAspect1_5),
	  gls = r0$GrayLeafSpot1_5,
	  rust = r0$CommonRust1_5,
	  blight = r0$LeafBlightTurcicum1_5)
	
	  d3$country <-"Mexico"
	  d3$longitude <- 20.4736865
	  d3$latitude <-  -103.4479314
	  d3$elevation <- 1550
	  d3$location <- "Tlajomulco, Jal"
	  d3$planting_date <- "2007-06-08"
	  d3$harvest_date  <- "2007-11-24"
	
	
	
	
	get_data <- function(fname, id, country, longitude, latitude, elevation) {
	  f <- ff[basename(ff) == fname]
	  r <- carobiner::read.excel(f) 
	  r <-r[22:34, 2:35]
	
	
	d <- data.frame( 
	  trial_id = as.character(r$Name),
	  variety=r$BreedersPedigree1,
	  yield=as.numeric(r$GrainYieldTons_FieldWt)*1000,
	  asi=as.numeric(r$ASI),
	  plant_height=as.numeric(r$PlantHeightCm),
	  ear_height = as.numeric(r$EarHeightCm),
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
	
	d0 <- get_data("07CHTSW1-1.xls",1,"Mexico", -103.5986529, 19.875 , 1340)  
	d0$location <- "Usmajac Mpio. de Sayula, Jal"
	d0$planting_date <- "2007-06-01"
	d0$harvest_date  <- "2007-12-12"
	
	d1 <- get_data("07CHTSW9-1.xls",2,"Egypt",  31.518, 30.733 , 10)  
	d1$location <- "Sids"
	d1$planting_date <- "2007-05-14"
	d1$harvest_date  <- "2007-09-15"
	
	d2 <- get_data("07CHTSW10-1.xls",4,"Egypt",   30.947,  31.087 , 20)  
	d2$location <- "Sakha"
	d2$planting_date <- "2007-05-14"
	d2$harvest_date  <- "2007-09-12"
	
	d4 <- get_data("07CHTSW24-1.xls",6,"Mexico",  -99.2222437 ,20.0919675, 2071)  
	d4$location <- "Tlaxcoapan, Hidalgo"
	d4$planting_date <- "2007-05-01"
	d4$harvest_date  <- "2007-10-04"
	
	d5 <- get_data("07CHTSW26-1.xls",7,"Mexico",    -98.843,  18.596 , 1350)  
	d5$location <- "Tepalcingo, Mor"
	d5$planting_date <- "2007-06-06"
	d5$harvest_date  <- "2007-11-20"
	
	
	d <- carobiner::bindr(d0, d1, d2, d3, d4, d5)
	
	  
	d$crop = "maize"
	d$on_farm = TRUE
	d$striga_trial = FALSE 
	d$striga_infected = FALSE
	d$borer_trial = FALSE
	d$yield_part = "grain"
	d$is_survey <- FALSE
	d$irrigated <- NA
	

	
	d$geo_from_source <- FALSE


# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

