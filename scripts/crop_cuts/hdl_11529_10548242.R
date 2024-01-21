# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    TAMASA Agronomy Panel Survey 2016/17 Season. This file contains the maize grain yield from approximately 578 maize fields in the Southern Highlands, Northern and Eastern Zones of Tanzania in collected May-August 2017. Maize grain yield data can be linked to associated maize yield and soil by the common HHID.

"

	uri <- "hdl:11529/10548242"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project="TAMASA",
	   uri=uri,
	   publication=NA,
	   data_citation = 'Craufurd, Peter; Karwani, George; Masuki, Kenneth, 2019, "TAMASA TZ APS 2017 CC MaizeYield v3", hdl:11529/10548242, CIMMYT Research Data & Software Repository Network, V2, UNF:6:FARtQ7xWh1m0+YYceI+wnw== [fileUNF]',
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   carob_date="2021-09-17",
	   data_type="survey"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)

	f <- ff[basename(ff) == "TAMASA_TZ_APS_2017_CC_MaizeYield.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Raw data", n_max = 1738)

## lon/lat reversed. but lat make no sense.		
	d <- data.frame(yield = r$`Grain yield (kg/ha@12.5%)`, 
				latitude = r$`QRcode Cobs`,
				longitude = r$Latitude)

	d$country <- "Tanzania"
	d$trial_id <- paste0(d$HHID, "-", d$QID)
	d$planting_date <- "2016-05-01"
	d$harvest_date <- "2016-12-01"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$dataset_id <- dataset_id

    d <- d[!is.na(d$longitude) & !is.na(d$latitude),]
    d <- d[!is.na(d$yield),]
	carobiner::write_files(dset, d, path=path)
}
