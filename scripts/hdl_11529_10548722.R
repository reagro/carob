# R script for "carob"

## ISSUES
# if there are remaining issues, please put these in "meta$notes"


carob_script <- function(path) {

"
This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Chihuahua. (2022-07-06)
"

## Identifiers
	uri <- "hdl:11529/10548722" 
	group <- "agronomy"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-09-03",
		notes = NA
	)
	
## read data 

	f <- ff[basename(ff) == "GreenSeeker Chihuahua 2016.xlsx"]
   r <- carobiner::read.excel(f, sheet = "Data-Buenaventura" )
   r1 <- r[-c(1, 2, 4), ]
   
	d <- data.frame(
		country = "Mexico",
		crop= "maize", 
		rep=as.integer(r1$Rep),
		treatment =r1$TRT,
		yield=as.numeric(r1$`Yield at 14% hum`))
		

	d$trial_id <- "1"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	
	d$longitude <- -106.069099
	d$latitude <-  28.632996
	d$geo_from_source <- FALSE

	d$planting_date <- "2016-05-12"
	d$irrigation_dates <- "2016-06-21"    
	d$land_prep_method <- "conventional"
	d$yield_part <- "grain"

	r1$`N at Planting` <- gsub("KgN","" , r1$`N at Planting`)
	r1$`N at Planting` <- as.numeric(r1$`N at Planting`)
	
	d$N_fertilizer <- (104*0.12)+ r1$`N at Planting`
	d$P_fertilizer <- (104*0.61)/2.29

# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

