# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
This dataset contains information from on-farm validation trials conducted across 30 farmer fields in Local government areas of the sudan savanna AEZ in Kano. In each farmer field 10 plots (30 meter square each) were planted with a different combination of early maturing maize (10 varieties) under 3 different sowing densitie. Each plot is a different combination of variety and sowing data. Data was collected on tops weight at anthesis, cob yield and stover yield. (2017-12-10)
"

## Identifiers
	uri <- "hdl:11529/221663"
	group <- "maize_trials"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=3),
		data_institute = "CIMMYT",
		publication = NA,
		project = "TAMASA Nigeria. Variety validation trials in Sudan Savanna 2015",
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "variety;plant_density", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-07-09"
	)
	
## read data 

	f <- ff[basename(ff) == "NG_VT_Validation_SS_2016.xlsx"]
  r <- carobiner::read.excel(f, sheet = "DATA")

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		variety=r$Variety,
		plant_density=r$Density,
		yield=r$GY_ha
		
	)

## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( "1")))

## about the data (TRUE/FALSE)
	d$on_farm <- TRUE
	

### Location
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Nigeria"
	d$adm1 <-  "Kano"
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 8.516667
	d$latitude <- 12.000000

### Crop 
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"

### Time 
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date( 2015  ))
	d$harvest_date  <- as.character(as.Date( 2015   ))
	
	d$yield_part <- "grain"
	d$striga_trial<-FALSE
	d$borer_trial<-FALSE
	d$striga_infected<-FALSE
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

