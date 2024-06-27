# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Yaqui Valley. (2022-10-17)
"

#### Identifiers
	uri <- "hdl:11529/10548811"
	group <- "wheat_trials"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = "GREENSEEKER CALIBRATION",
		data_type = "experiment",
		treatment_vars = "N_fertilizer", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-06-25"
	)
	
##### PROCESS data records

# read data 
	sort_data <- function(sheetname) {
	  
	f <- ff[basename(ff) == "GreenSeeker Sonora 2000-2014.xlsx"]
  r <- carobiner::read.excel(f, sheet = "2000-2001",skip = 1, col_names = TRUE)
  names(r)[13] <- 'Rate N (kg/ha)'

	d1 <- data.frame(
	     trial_id="1",
	     country="Mexico",
		   adm2=r$Municipality,
		   site=r$Locality,
		   longitude="-110.6120",
		   latitude="27.6673",
		   crop="wheat",
		   planting_date=r$`Planting Date`,
		   land_prep_method=r$Tillage,
		   variety=r$Hibrid,
		   planting_method=r$`Planting method`,
		   #plant_density=r$`planting density (Kg/ha)`-need for conversion from kg/ha to plants/ha
		   rep=r$REP,
		    treatment=r$`Number of Treatment`,
		    Zn_fertilizer=r$`Rate Zn`,
 		   N_fertilizer=r$`Rate N (kg/ha)`,
		   yield_part="grain",
		   yield=r$...34
	)

	}
	
	d2 <- sort_data("2001-2002")
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE

	# d$planting_date <- as.character(as.Date(   ))
	# d$harvest_date  <- as.character(as.Date(    ))


# all scripts must end like this
	carobiner::write_files(path, dset, d)
}
