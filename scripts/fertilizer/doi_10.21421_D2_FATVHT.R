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
	   publication="doi:10.21421/D2/FATVHT",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)[3]


	f <- ff[basename(ff) == "Data file of Sorghum N trial Kano Nigeria.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
	
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$adm2 <- ifelse(d$Location == "BUK", "Gezawa", "Minjibir")
	d$location <- d$Location
	d$trial_id <- paste0(dataset_id, '-', d$Location)
	d$latitude <- ifelse(d$Location == "BUK", 8.5922, 8.5978)
	d$longitude <- ifelse(d$Location == "BUK", 12.0034, 12.1733)
	d$start_date <- d$Year
	d$end_date <- d$Year
	d$on_farm <- "no"
	d$is_survey <- "no"
	
## RH	d$treatment <- ifelse(d$Nitrogen == 0 , 1, ifelse(d$Nitrogen == 20 , 2, ifelse(d$Nitrogen == 40 , 3, ifelse(d$Nitrogen == 60 , 4, ifelse(d$Nitrogen == 80 , 5, 6)))))
	d$treatment <- as.integer(as.factor(d$Nitrogen))

	d$rep <- d$`Replication umber`
	d$crop <- "sorghum"
	d$variety <- d$Sorghum
## RH	d$yield <- d$`Grain yield` + d$`Stalk yield`
	d$yield <- d$`Grain yield`
## assuming that "Stalk yield" also includes leaves	
	d$residue_yield <- d$`Stalk yield`
	d$grain_weight <- d$GW_1000grnM_g
	d$N_fertilizer <- d$Nitrogen
	d$P_fertilizer <- 30
	d$OM_used <- "no"

	d <- d[,c(18:37)]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)

}
