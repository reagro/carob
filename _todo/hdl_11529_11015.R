# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    Nutrient Ommission Trials (NOT's) conducted in two zones (West Showa and Jimma) in Ethiopia in 2015 and 2016. Trials comprise six nutrient management treatements, namely Control (zero fertilzer), PK, NK, PK, NPK, NPK+Ca+Mg+Zn+B. Trials were conducted on-farm with six plots per farm. Observations include soil analysis (0-20cm), biomass and grain yields

"

	uri <- "hdl:11529/11015"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="hdl:11529/11015",
	   data_citation = 'Peter Craufurd, 2017, "TAMASA Ethiopia. Nutrient omission trial (NOT) datasets for 2015 and 2016 seasons", https://hdl.handle.net/11529/11015, CIMMYT Research Data & Software Repository Network, V1',
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE,
	   has_soil=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "TAMASA_ET_NOT_2015_&_2016F.xlsx"]
	
	"NAME_1" = 'Oromia' AND "NAME_2" IN ('Mirab Shewa', 'Misraq Wellega', 'Jimma') AND NAME_3 IN ('Omo Nada', 'Kersa', 'Bako Tibe', 'Gobu Seyo')

	d <- as.data.frame(readxl::read_excel(f, sheet = "Corrected-Raw-Data", n_max = 1835))
	d <- d[complete.cases(d[ , 13:14]),]
	et <- sf::st_as_sf(geodata::gadm(country="ETH", level = 3, path = tempdir()), crs = "+proj=longlat +datum=WGS84")
	et <- et[et$NAME_1 == "Oromia" & et$NAME_2 %in% c("Mirab Shewa", "Misraq Wellega", "Jimma") & et$NAME_3 %in% c("Omo Nada", "Kersa", "Bako Tibe", "Gobu Seyo"),]
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
	d$start_date <- as.numeric("2016")
	d$end_date <- as.numeric("2016")
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
