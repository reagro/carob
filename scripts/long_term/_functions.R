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
		rep = r$Rep,
		yield = r$GYtha * 1000,
		N_fert_level = as.integer(gsub("F", "", r$Afactor)),
		variety_code = paste0(r$Year, "_", r$Bfactor),
		trial_id = as.character(r$Crop_no)
	)
	x$N_fert_level <- c("zero", "low", "moderate", "high")[x$N_fert_level]
	i <- match(x$season, c("DS", "EWS", "LWS"))
	x$season <- c("dry", "early wet", "late wet")[i]
	x$N_fertilizer <- x$P_fertilizer <- x$K_fertilizer <- as.numeric(NA)
	x$N_fertilizer[x$N_fert_level == "zero"] <- 0
	x
}




#make_irri_lte <- function() {
#
#	ids <- c("TWMICE", "TRII7V", "GK6J36", "F9DFPD", "AF00KH", "LBXNBQ", "W2H2SL", "PL9FHO", "JPVKRX", "QYY2JY", "UZQBGX", "8G1DFQ", "26V1HS", "DRTSUI", "GANORJ", "GX0J15", "VNZJ5I", "WORMEW", "TV90PW", "75MPJJ", "WYCUMH", "ECKKQO", "UMYOUM", "RG78QM", "EQUNHF", "W3L3BL", "SGNHNA", "DBPKF6", "7CIDYV", "8YHLBG", "EFCTZR", "CDM9YD", "0DRM60", "ZRZUUJ", "5YCC3C", "F7YC0E", "NAUAPA", "5LRABO", "3HULF7", "EOLTRT", "MIIGR1", "MLHX7H", "XA7ORX", "NENY5L", "AKRLF4", "X3MA2Z", "O9QTTL", "1UP63T", "8R7D8R", "SVQ1XQ")
#	x <- readLines("doi_10.7910_DVN_26V1HS.R")
#	x[5] <- "\"This study contains grain yield data collected from IRRI's long term continuous cropping experiment, year\""
#	i <- grep("\turi <-", x)
#	for (id in ids) {
#		x[i] <- paste0("\turi <- \"doi:10.7910/DVN/", id, "\"")  
#		fname <- paste0("doi_10.7910_DVN_", id, ".R")
#		if (!file.exists(fname)) writeLines(x, fname)
#	}
#}

