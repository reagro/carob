
process_cip_lbvars <- function(filename, addvars=NULL) {      

	r <- carobiner::read.excel(filename, sheet="Fieldbook")		   
	minimal <- carobiner::read.excel(filename, sheet="Minimal")
	installation <- carobiner::read.excel(filename, sheet="Installation")
	
	m <- as.list(minimal$Value)
	names(m) <- gsub(" ", "_", minimal$Factor)
	
	n <- as.list(installation$Value)
	names(n) <- gsub(" ", "_", installation$Factor)
	
	for (v in addvars) if (is.null(r[[v]])) r[v] <- NA

	plot_size <- as.numeric(n$`Plot_size_(m2)`)
	plot_adj <- 10000 / plot_size
     
  	if (grepl("/", m$Begin_date)) m$Begin_date <- as.character(as.Date(m$Begin_date,format = "%d/%m/%Y"))
  	if (grepl("/", m$End_date)) m$End_date <- as.character(as.Date(m$End_date,format = "%d/%m/%Y"))
	 
	d <- data.frame(
		rep = as.integer(r$REP),
		variety = r$INSTN,
		yield = as.numeric(r$TTWP) * plot_adj,
		yield_marketable = as.numeric(r$MTWP) * plot_adj,
		AUDPC = as.numeric(r$AUDPC) / 100,
		rAUDPC = as.numeric(r$rAUDPC),
		country = m$Country,
		adm1 = m$Admin1,
		adm2 = m$Admin2,
		adm3 = m$Admin3,
		location = m$Locality,
		longitude = as.numeric(m$Longitude),
		latitude = as.numeric(m$Latitude),
		elevation = as.numeric(m$Elevation),
		planting_date = m$Begin_date,
		harvest_date = m$End_date,
		plant_density = as.numeric(n$`Planting_density_(plants/Ha)`),
		row_spacing = as.numeric(n$`Distance_between_rows_(m)`) * 100,
		plant_spacing = as.numeric(n$`Distance_between_plants_(m)`) * 100,
		trial_id = gsub(".xls|.xlsx", "", basename(filename))
	)

    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$crop <- "potato"
    d$pathogen <- "Phytophthora infestans"
    d$yield_part <- "tubers"
    d$geo_from_source <- TRUE
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d
}


