# R script for "carob"


carob_script <- function(path) {

"Stress Tolerant Maize for Africa for Improved Livelihoods"

	uri <- "doi:10.25502/y1kz-yf32/d"
	group <- "maize_trials"
	ff  <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "International Institute of Tropical Agriculture (IITA)",
		publication= "doi:10.1371/journal.pone.0252506",
		project = NA,
		treatment = "variety",
		data_type= "experiment",
		carob_contributor= "Eduardo Garcia Bendito",
		carob_date="2024-05-07"
	)
	
	f <- ff[basename(ff) == "Phenotypic data of inbred lines evaluated under low-N conditions in 2017"]
	r <- read.csv(f)

	d <- data.frame(
		crop = "maize",
		trial_id = r$LOC,
		rep = r$REP,
		plot_lenght = 4, 
		season = "minor", # from pub
		planting_date = "2017-09-01", # from pub
		variety = r$ENTRY,
		yield = r$YIELD,
		plant_spacing = 75, # from pub
		row_spacing = 40, # from pub
		plant_density = 66666,
		anthesis_days = r$POLLEN,
		silking_days = r$DYSK,
		soil_N = ifelse(r$LOC == "FUMESUA", 0.11, 0.06)*10000 #from pub 
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$striga_trial <- FALSE
	d$borer_trial <- FALSE
	d$striga_infected <- FALSE

##### Location
	d$country <- "Ghana"
	d$location <- ifelse(r$LOC == "FUMESUA", "Kumasi", "Accra")
	d$site <- ifelse(r$LOC == "FUMESUA", "Crop Research Institute, Fumesua", "University of Ghana, WACCI Farm")
	d$elevation <- ifelse(r$LOC == "FUMESUA", 286, 97) # from pub
	d$latitude <- ifelse(r$LOC == "FUMESUA", 6.7175, 5.6601) # from pub
	d$longitude <- ifelse(r$LOC == "FUMESUA", -1.5326, -0.1919) # from pub

##### Fertilizers
	d$fertilizer_type <- "urea;KCl;TSP" # from pub
	d$N_fertilizer <- 30 # from pub
	d$N_splits <- as.integer(2)
	d$P_fertilizer <- 27.6 # Phosphorus (triple super phosphate, 46% P) applied at 60 kg ha-1
	d$K_fertilizer <- 36 # Potassium (muriate of potash, 60% K) applied at 60 kg ha-1 

	d$yield_part <- "grain"
	
	carobiner::write_files(path, dset, d)
}
