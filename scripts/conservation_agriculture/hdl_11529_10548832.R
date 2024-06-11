# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
Grain yield data collected from Conservation Agriculture (CA) systems across 
experiments of varying experimental duration, established in trial locations of
Malawi, Mozambique, Zambia, and Zimbabwe under an increasingly variable climate.
Data contains different agro-environmental yield response moderators such as type
of crop diversifcation and amount of rainfall and aims to identify cropping systems
that may provide both short-term gains and longer-term sustainability.
"

#### Identifiers
	uri <- "hdl:11529/10548832"
	group <- "conservation_agriculture"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = "doi.org/10.1016/j.agee.2021.107812",
		project = NA,
		data_type = "experiment",
		treatment_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-31"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
	r <- carobiner::read.excel(f, sheet = "Raw Data")

## process file(s)

## select the variables of interest and assign them to the correct name
	d <- data.frame(
	  trial_id="1",
	  country="Malawi",
		site=r$Location,
		planting_date=r$Season,
		rep=r$Rep,
		rain=r$Rainfall,
		soil_N=r$Nitrogen,
		soil_P_total=r$Phosphorus,
		soil_K=r$Potassium,
		soil_clay=r$Clay,
		soil_SOC=r$OrgC,
		dmy_total=r$Biomass,
		yield=r$Grain*1000,
		yield_part="grain",
		crop="maize",
		longitude="33.63333",
		latitude="-13.98333",
		elevation="1146"
		
	)

	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$planting_date <- as.character(d$planting_date)
	d$rep <- as.integer(d$rep)
	d$longitude <- as.numeric(d$longitude)
	d$latitude <- as.numeric(d$latitude)
	d$elevation <- as.numeric(d$elevation)
	
	carobiner::write_files(path, dset, d)
}


