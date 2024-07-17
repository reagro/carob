
get_elements_from_product <- function(fertab, products) {
	used <- unique(products) |> strsplit("; ") |> unlist() |> na.omit() |> unique() 	
	stopifnot(!all(fertab$name %in% used))
	fertab <- fertab[fertab$name %in% used, c("name", "N", "P", "K", "S")]
	fmat <- as.matrix(fertab[,-1]) / 100
	out <- matrix(0, ncol=4, nrow=length(products))
	colnames(out) <- paste0(c("N", "P", "K", "S"), "_fertilizer")
	for (fertilizer in used) {
		f <- fmat[fertab$name==fertilizer, ]		
		stopifnot(!any(is.na(f)))
		i <- grep(fertilizer, products)
		out[i, ] <- out[i, ] + rep(f, each=length(i))
	}
	out[is.na(products)] <- NA
	out
}



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



