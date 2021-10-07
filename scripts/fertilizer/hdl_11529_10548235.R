# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    CIMMYT and IITA. 2017. TAMASA Tanzania Agronomic Panel Survey for 2016. Version 1. Taking Maize Agronomy to Scale Project, CIMMYT and IITA. Tabular dataset (2016)

"

	uri <- "hdl:11529/10548235"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="hdl:11529/10548235",
	   data_citation = 'Masuki, Kenneth, 2019, "TZ TAMASA APS 2016 Yield MetaData", https://hdl.handle.net/11529/10548235, CIMMYT Research Data & Software Repository Network, V2, UNF:6:dF6+/EEer8HsSlxKsQJIVA== [fileUNF]',
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE,
	   has_soil=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "TZ_TAMASA_APS_2017_Yield_MetaData.xlsx"]

	d <- as.data.frame(readxl::read_excel(f, sheet = "Corrected-Raw-Data", n_max = 1835))
	d <- d[complete.cases(d[ , 13:14]),]
	tz <- sf::st_as_sf(geodata::gadm(country="TZA", level = 3, path = tempdir()), crs = "+proj=longlat +datum=WGS84")
	a <- sf::st_as_sf(d, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")
	a <- sf::st_join(a, tz, join = sf::st_intersects)
	
	d$country <- a$NAME_0
	d$adm1 <- a$NAME_1
	d$adm2 <- a$NAME_2
	d$adm3 <- a$NAME_3
	d$location <- d$Village
	d$site <- d$Hamlet
	d$trial_id <- paste0(d$HHID, "-", d$QID)
	d$latitude <- d$Latitude
	d$longitude <- d$Longitude
	d$start_date <- as.Date("01-05-2016", "%d-%m-%Y")
	d$end_date <- as.Date("01-12-2016", "%d-%m-%Y")
	d$on_farm <- "yes"
	d$is_survey <- "yes"
	d$crop <- "maize"
	d$yield <- d$`FWt of Cobs_all (kg)`*4 # FWt of Cobs_all (kg) = Fresh Weight of Cobs in Quadrat (25m2)
	
	# process file(s)
	d <- d[,c("country", "adm1", "adm2", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "crop", "yield")]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
