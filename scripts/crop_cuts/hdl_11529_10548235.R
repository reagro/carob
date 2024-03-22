# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"CIMMYT and IITA. 2017. TAMASA Tanzania Agronomic Panel Survey for 2016. Version 1. Taking Maize Agronomy to Scale Project, CIMMYT and IITA. Tabular dataset (2016)"

	uri <- "hdl:11529/10548235"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project="TAMASA",
	   uri=uri,
	   publication=NA,
	   data_citation = 'Masuki, Kenneth, 2019, "TZ TAMASA APS 2016 Yield MetaData", hdl:11529/10548235, CIMMYT Research Data & Software Repository Network, V2, UNF:6:dF6+/EEer8HsSlxKsQJIVA== [fileUNF]',
	   data_institutions = "CIMMYT",
	   carob_contributor="Effie Ochieng'",
	   carob_date="2021-09-17",
	   data_type="survey"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)

	f <- ff[basename(ff) == "TZ_TAMASA_APS_2017_Yield_MetaData.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Corrected-Raw-Data", n_max = 1835)

	d <- data.frame(yield = r$`Grain yield (kg/ha@12.5%)`, dataset_id=dataset_id)
	d$country <- "Tanzania"
	d$adm1 <- r$Zone
	d$adm2 <- r$Region 
	d$adm3 <- r$District
	d$adm4 <- r$Ward 
	
	d$location <- r$Village
	d$site <- r$Hamlet
	d$trial_id <- paste0(r$HHID, "-", r$QID)
	d$latitude <- r$Latitude
	d$longitude <- r$Longitude
	d$elevation <- r$Altitude
	
	d$planting_date <- "2016-05-01"
	d$harvest_date <- "2016-12-01"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "maize"
	d$yield_part <- "grain"

	d <- d[!is.na(d$yield), ]
	
	carobiner::write_files(dset, d, path=path)
}
