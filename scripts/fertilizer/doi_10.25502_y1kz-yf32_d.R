# R script for "carob"

## ISSUES
# EGB:
# Perhaps should go into "maize_trials"?


carob_script <- function(path) {

"
Stress Tolerant Maize for Africa for Improved Livelihoods
"

#### Identifiers
	uri <- "doi:10.25502/y1kz-yf32/d"
	group <- "fertilizer"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "International Institute of Tropical Agriculture (IITA)",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "doi:10.1371/journal.pone.0252506",
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		project = "CGIAR Research Program on Maize",
		data_type= "experiment",
		carob_contributor= "Eduardo Garcia Bendito",
		carob_date="2024-05-07"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Phenotypic data of inbred lines evaluated under low-N conditions in 2017"]
	r <- read.csv(f)

## process file(s)

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		crop = "maize",
		trial_id =  r$LOC,
		treatment = "Variety",
		rep = r$REP,
		plot_lenght = 4, # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
		season = "minor", # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
		planting_date = "2017-09-01", # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
		variety = r$ENTRY,
		yield = r$YIELD,
		plant_spacing = 75, # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
		row_spacing = 40, # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
		plant_density = 66666,
		anthesis_days = r$POLLEN,
		silking_days = r$DYSK,
		soil_N = ifelse(r$LOC == "FUMESUA", 0.11, 0.06)*10000 # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
	)

#### about the data #####
	d$on_farm <- FALSE
	d$is_survey <- FALSE

##### Location #####
	d$country <- "Ghana"
	d$location <- ifelse(r$LOC == "FUMESUA", "Kumasi", "Accra")
	d$site <- ifelse(r$LOC == "FUMESUA", "Crop Research Institute, Fumesua", "University of Ghana, WACCI Farm")
	d$elevation <- ifelse(r$LOC == "FUMESUA", 286, 97) # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
	d$latitude <- ifelse(r$LOC == "FUMESUA", 6.7175, 5.6601) # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
	d$longitude <- ifelse(r$LOC == "FUMESUA", -1.5326, -0.1919) # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002

##### Fertilizers #####
	d$fertilizer_type <- "urea; KCl;TSP" # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
	d$N_fertilizer <- 30 # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252506#sec002
	d$N_splits <- as.integer(2)
	d$P_fertilizer <- 27.6 # Phosphorus (triple super phosphate, 46% P) applied at 60 kg ha-1
	d$K_fertilizer <- 36 # Potassium (muriate of potash, 60% K) applied at 60 kg ha-1 

	d$yield_part <- "grain"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}
