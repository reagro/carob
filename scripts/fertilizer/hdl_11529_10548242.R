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
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication=NA,
	   data_citation = 'Craufurd, Peter; Karwani, George; Masuki, Kenneth, 2019, "TAMASA TZ APS 2017 CC MaizeYield v3", https://hdl.handle.net/11529/10548242, CIMMYT Research Data & Software Repository Network, V2, UNF:6:FARtQ7xWh1m0+YYceI+wnw== [fileUNF]',
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE,
	   has_soil=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "TAMASA_TZ_APS_2017_CC_MaizeYield.xlsx"]

	d <- carobiner::read.excel(f, sheet = "Raw data", n_max = 1738)
	colnames(d) <- c("Country", "Zone", "Region", "District", "Ward", "Village", "Hamlet", "HHID", "Farmer Name", "drop", "QID",
	                 "QRcode Cobs", "Latitude", "Longitude", "Altitude", "Area by Farmer_est", "Area by GPS", "drop1",
	                 "Plant stands", "Total Number of Cobs", "FWt of Cobs_all (kg)", "FWt of Cobs SS (kg)", "Dry Wt of Cobs SS (kg)",
	                 "drop2", "Grain Wt SS (kg)", "Moisture_WB (%)", "Sheliing Factor", "Total Cob wt",
	                 "Total cob dry weight", "Grain dry weight (kg/25m2)", "Grain dry weight (kg/25m2 @12.5%)", "Grain yield (kg/ha@12.5%)",
	                 "...33", "...34")
	
	d$country <- "Tanzania"
	d$trial_id <- paste0(d$HHID, "-", d$QID)
	d$latitude <- d$Latitude
	d$longitude <- d$Longitude
	d$start_date <- "2016-05-01"
	d$end_date <- "2016-12-01"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "maize"
	# d$yield <- d$`FWt of Cobs_all (kg)`*4 # FWt of Cobs_all (kg) = Fresh Weight of Cobs in Quadrat (25m2)
	d$yield <- d$`Grain yield (kg/ha@12.5%)` # Grain yield at 12.5% moisture
	
	# process file(s)
	d <- d[,c("country", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "crop", "yield")]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)

}
