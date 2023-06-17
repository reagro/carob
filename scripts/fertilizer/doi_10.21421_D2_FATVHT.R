# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:


"

	uri <- "doi:10.21421/D2/FATVHT"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi:10.1155/2018/7676058",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="Split plot",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Data file of Sorghum N trial Kano Nigeria.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
	
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$adm2 <- ifelse(d$Location == "BUK", "Gezawa", "Minjibir")
	d$location <- d$Location
	d$trial_id <- paste0(dataset_id, '-', d$Location)
	d$latitude <- ifelse(d$Location == "BUK", 8.5922, 8.5978)
	d$longitude <- ifelse(d$Location == "BUK", 12.0034, 12.1733)
	d$start_date <- ifelse(d$Location == "BUK" & d$Year == 2014, as.character(as.Date("2014-07-19")),
	                       ifelse(d$Location == "BUK" & d$Year == 2015, as.character(as.Date("2014-07-20")),
	                              ifelse(d$Location == "Minjibir" & d$Year == 2014, as.character(as.Date("2014-07-07")),
	                                     as.character(as.Date("2014-07-04"))))) # As reported in the associated publication
	d$end_date <- as.character(as.Date(d$start_date) + d$`Days to Maturity`)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	
	d$treatment <- paste0("N", d$Nitrogen, "-P30-K30")

	d$rep <- as.integer(d$`Replication umber`)
	d$crop <- "sorghum"
	d$variety <- d$Sorghum
## RH	d$yield <- d$`Grain yield` + d$`Stalk yield`
	d$yield <- d$`Grain yield`
## assuming that "Stalk yield" also includes leaves	
	d$residue_yield <- d$`Stalk yield`
	d$grain_weight <- d$GW_1000grnM_g
	d$fertilizer_type <- "unknown" # Not reported in the associated publication
	d$N_splits <- 2 # As reported in the associated publication
	d$N_fertilizer <- d$Nitrogen
	d$P_fertilizer <- 30/2.29 # As reported in the associated publication. Converting P2O5 to P-elemental
	d$K_fertilizer <- 30/1.2051 # As reported in the associated publication Converting K2O to K-elemental
	d$OM_used <- FALSE
	d$plant_spacing <- 30 # As reported in the associated publication
	d$row_spacing <- 75 # As reported in the associated publication

	d <- d[,c(18:43)]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)

}
