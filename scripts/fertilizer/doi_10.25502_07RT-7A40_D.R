# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "Description:Maize response to N and P. Tofa, A., Kamara, A. Y., Babaji, B. A., Ademulegun, T. D., & Aliyu, K. T. (2021). Maize response to N and P [Data set]. International Institute of Tropical Agriculture (IITA). doi:10.25502/07RT-7A40/D"
    
####
	uri <- "doi:10.25502/07RT-7A40/d"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data. Internal annotation for CAROB 
	dset <- data.frame(
		dataset_id = dataset_id,
		data_citation = "Tofa, A., Kamara, A. Y., Babaji, B. A., Ademulegun, T. D., & Aliyu, K. T. (2021). Maize response to N and P [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/07RT-7A40/D",
		data_institutions = "IITA",
		group=group,
		uri=uri,
		publication=NA,
		carob_contributor="Henry Juarez",
		carob_date="2022-03-24",
		data_type="experiment",
		project=NA
	)
	
	## download and read data (Path is important)
		
	ff	 <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	
	# Process the trial/farm sites 
		
	f <- ff[basename(ff) == "N and P maize trial_2015_16.csv"]
	d <- read.csv(f)
	e <- d[,c(2,4)]
	colnames(e) <- c("site", "variety")
	
	# process file(s)
	e$dataset_id <- dataset_id
	e$country <- "Nigeria"
	e$trial_id <- "1" #paste0(dataset_id, "-", d$ID) ###
		 
	e$latitude <- 10.26858
	e$longitude <- 7.78896
 
	e$planting_date <- as.character(d$year)
	e$on_farm <- TRUE
	e$N_fertilizer <- as.numeric(d$nrate)
	e$P_fertilizer <- as.numeric(d$prate)
	##	RH presumably:
	e$K_fertilizer <- 0
	e$fertilizer_type <- "urea; DAP; NPK; SSP" # One of these

	e$treatment <- paste0("N", e$N, "P", e$P)

	e$variety <- d$variety
	e$is_survey <- FALSE
	e$crop <- "maize"
	e$yield_part <- "grain"
	# swt500 = Weight of 500 seeds (g), the original value was multiplied by 2
	e$grain_weight <- (d$swt500)*2 
	# total biomass (dry matter) to kg/ha
	e$dmy_total <- d$tdmm2 * 10

	e$yield <- d$yield
		
	e$flowering <- as.numeric(d$flw50)
	e$silking <- d$slk50
	
	# all scripts must end like this
	carobiner::write_files(dset, e, path=path)

}	
