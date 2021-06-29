# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:


"

	uri <- "doi:10.21421/D2/AVKD0T"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi:10.21421/D2/AVKD0T",
	   contributor="fava",
	   experiment_type="___",
	   has_weather=FALSE,
	   has_management=TRUE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)[3]


	f <- ff[basename(ff) == "Data file of Long term millet trial ISC sadore Niger.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
	
	d$dataset_id <- dataset_id
	d$country <- "Niger"
	d$adm1 <- "Tillabéry"
	d$adm2 <- "Say"
	d$adm3 <- "Say"
	d$location <- "Sadoré"
	d$site <- "ICRISAT Sahelian Center (ISC)"
	d$trial_id <- paste0(dataset_id, '-', d$site, '-', d$Years)
	d$latitude <- 13.234295
	d$longitude <- 2.283155
	d$start_date <- d$Years
	d$end_date <- d$Years
	d$season <- "rainy season"
	d$on_farm <- "no"
	d$is_survey <- "no"
	d$treatment <- "Treatments replicated four times, involving various combinations of hand cultivation (HC), ridging with animal traction and planting on ridges (AT), limited P fertilizer application and rotation with sole cowpea (C)"
	d$rep <- d$`Replication number`
	d$crop <- "millet"
	d$yield <- d$`Stover yield` + d$`Grain weight` + d$`Pod weight`
	d$grain_weight <- d$`Grain weight` * 0.1875 # 0.1875 is the sampled area for yield (read dataset description)
	d$fertilizer_type <- "calcium ammonium nitrate"
	d$N_fertilizer <- ifelse(d$`N Application` == 1, 15, 0)
	d$OM_used <- "no"
	d$soil_type <- "Aeolian sands"
	d$soil_pH <- "4.5-5.0"
	d$soil_sand <- 95
	d$soil_SOC <- 0.4
	
	d <- d[,c(9:35)]

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
