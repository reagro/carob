# R script for "carob"


carob_script <- function(path) {
  
  "Maize response to N and P. Tofa, A., Kamara, A. Y., Babaji, B. A., Ademulegun, T. D., & Aliyu, K. T. (2021). Maize response to N and P [Data set]. International Institute of Tropical Agriculture (IITA). doi:10.25502/07RT-7A40/D"
    
####
	uri <- "doi:10.25502/07RT-7A40/d"
	group <- "fertilizer"
	ff	 <- carobiner::get_data(uri, path, group)
	## dataset level data. Internal annotation for CAROB 
	dset <- data.frame(
		data_institutions = "IITA",
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		publication=NA,
		carob_contributor="Henry Juarez",
		carob_date="2022-03-24",
		data_type="experiment",
		project=NA
	)
	
		
	
	# Process the trial/farm sites 
		
	f <- ff[basename(ff) == "N and P maize trial_2015_16.csv"]
	d <- read.csv(f)
	e <- d[,c(2,4)]
	colnames(e) <- c("site", "variety")
	
	# process file(s)
	
	e$country <- "Nigeria"
	e$trial_id <- "1" 
		 
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
		
	e$flowering_days <- as.numeric(d$flw50)
	e$silking_days <- d$slk50
	
	carobiner::write_files(dset, e, path=path)

}	
