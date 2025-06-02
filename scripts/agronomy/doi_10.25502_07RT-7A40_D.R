# R script for "carob"


carob_script <- function(path) {
  
"Maize response to N and P. Tofa, A., Kamara, A. Y., Babaji, B. A., Ademulegun, T. D., & Aliyu, K. T. (2021). Maize response to N and P [Data set]. International Institute of Tropical Agriculture (IITA). doi:10.25502/07RT-7A40/D"
    
	uri <- "doi:10.25502/07RT-7A40/d"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)
	meta <- data.frame(
		data_institute = "IITA",
		carobiner::get_metadata(uri, path, group, major=1, minor=0),
		publication=NA,
		carob_contributor="Henry Juarez",
		carob_date="2022-03-24",
		data_type="experiment",
		project=NA,
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer"
	)

	f <- ff[basename(ff) == "N and P maize trial_2015_16.csv"]
	r <- read.csv(f)
	d <- data.frame(
		location=r$loc,
		variety=r$variety,
		planting_date = as.character(r$year),
		on_farm = TRUE,
		N_fertilizer = as.numeric(r$nrate),
		P_fertilizer = as.numeric(r$prate),
		country = "Nigeria",
		trial_id = "1",
		# swt500 = Weight of 500 seeds (g)
		seed_weight = r$swt500 * 2, 
		# total biomass (dry matter) to kg/ha
		dmy_total = r$tdmm2 * 10,
		yield = r$yield,
		flowering_days = as.numeric(r$flw50),
		silking_days = r$silk50
	)
	
	d$latitude <- 10.26858
	d$longitude <- 7.78896
	d$geo_from_source <- FALSE
	d$K_fertilizer <- 0
	d$fertilizer_type <- "urea;DAP;NPK;SSP" # One of these
	d$treatment <- paste0("N", d$N_fertilizer, "P", d$P_fertilizer)
	d$is_survey <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$irrigated <- NA
	
	carobiner::write_files(meta, d, path=path)

}	
