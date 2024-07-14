# R script for "carob"

IRRI_LTE <- function(f) {

	r <- do.call(rbind, lapply(grep("\\.csv$", f, value=TRUE), \(i) read.csv(i, na.strings =".")))
	r <- unique(r)
	
	x <- data.frame(
		LTE_name = r$Expt,
		country="Philippines",
		adm1="Laguna",
		location="Los Baños",
		longitude = 121.2571,
		latitude = 14.1678,
		irrigated = TRUE,
		on_farm = FALSE, 
		is_survey = FALSE,
		site = r$Site,
		planting_date = as.character(r$Year),
		season = r$Season,
		crop = tolower(r$Crop),
		yield_part = "grain",
		LTE_ID = r$Crop_no,
		rep = r$Rep,
		yield = r$GYtha * 1000,
		N_fert_level = as.integer(gsub("F", "", r$Afactor)),
		variety_code = paste0(r$Year, "_", r$Bfactor),
		trial_id = paste0(r$Year, "_", r$Season)
	)
	x$N_fert_level <- c("zero", "low", "moderate", "high")[x$N_fert_level]
	i <- match(x$season, c("DS", "EWS", "LWS"))
	x$season <- c("dry", "early wet", "late wet")[x$season]
	x$N_fertilizer <- x$P_fertilizer <- x$K_fertilizer <- as.numeric(NA)
	x$N_fertilizer[x$N_fert_level == "zero"] <- 0
	x
}

