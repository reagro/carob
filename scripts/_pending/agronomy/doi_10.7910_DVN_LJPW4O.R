# R script for "carob"

#RH: needs a lot more work


carob_script <- function(path) {

"Response of maize to N and P in two trials in Uganda"

	uri <- "doi:10.7910/DVN/LJPW4O"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=5) ,
		publication=NA,
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2021-06-18",
		data_type = "experiment",
		data_institute="CIAT",
		project="AgMIP"
		treatment_vars = "N_fertilizer;P_fertilizer",
		response_vars = "yield"
	)


	f <- ff[basename(ff) == "9a Yield data.xlsx"]
	r <- carobiner::read.excel(f)

	# no season variable hence cannot be merged with r
	# biomass <- carobiner::read.excel(ff[basename(ff) == "2a Dry matter measurements.xlsx"])

	# Manure applied ("1a Cattle manure lab analysis.xlsx")
	# Missing information on amount of manure applied.
	# Have sent an email to Job Kihara to find out about the manure applied (2021/09/17)
	###########

	# Soil data ("8a Soil lab data.xlsx")
	soil <- carobiner::read.excel(ff[basename(ff) == "8a Soil lab data.xlsx"], skip = 12)

	d <- data.frame(
		country = "Uganda",
		adm1 = "Wakiso",
		adm2 = "Jinja",
		adm3 = ifelse(r$Site == "Kawanda", "Nabweru", "Busukuma"),
		trial_id = r$Site,
		latitude = ifelse(r$Site == "Kawanda", 0.4172778, 0.5256090),
		longitude = ifelse(r$Site == "Kawanda", 32.5355326, 32.6136960),
		planting_date = "2013-08-10",
		harvest_date = "2014",
		on_farm = FALSE,
		is_survey = TRUE,	
		crop = "maize",
		yield_part = "grain",
		variety = "Longe 10H",
		variety_code = "SC627"
	)
						  
						  
	# Adding Treatment information
	d1$fertilizer_type <- "urea"
	d1$N_fertilizer <- ifelse(r$Treatment %in% c(8,7,6,5,4), 200,
	                   ifelse(r$Treatment == 3, 60, 0))
	d1$N_splits <- NA
	d1$N_splits[d1$N_fertilizer > 0] <- 3L

	d1$P_fertilizer <- ifelse(r$Treatment %in% c(8,7,3,2), 90,
						 ifelse(r$Treatment %in% c(6), 50,
						   ifelse(r$Treatment %in% c(5), 20, 0)))
	d1$K_fertilizer <- ifelse(r$Treatment == 1, 0, 75)
	d1$Zn_fertilizer <- ifelse(r$Treatment == 1, 0, 75)
	

	d1$OM_used <- d1$Treatment == 8
	d1$OM_type <- ifelse(d1$Treatment == 8, "farmyard manure", NA)
	d1$OM_amount <- ifelse(d1$Treatment == 8, 5000, 0)
	d1$OM_N <- d1$OM_amount*(0.1796/100)*(0.755/100) # OM$K (%)
	d1$OM_P <- d1$OM_amount*(0.1796/100)*(200.67532467532467/1e+06) # OM$P (ppm)
	d1$OM_K <- d1$OM_amount*(0.1796/100)*(66.9072/1e+06) # OM$K (ppm)

	soil$Block <- sub("^\\D*(\\d+).*$", "\\1",  soil$`Client's ref`)
	soil$Plot <- sub('.*(?=.{2}$)', '', soil$`Client's ref`, perl=T)
	soil1 <- soil[,c(1,4:17)]
	# Consider only the first 30 cm
	soil1 <- soil1[soil1$`Depth (CM)` == "0-15" | soil1$`Depth (CM)` == "15-30", ]
	soil1$`P (ppm)`[soil1$`P (ppm)` == "trace"] = 0.01
	soil1$`P (ppm)` <- as.numeric(soil1$`P (ppm)`)
	soil1$Block <- as.numeric(soil1$Block)
	soil1$Plot <- as.numeric(soil1$Plot)
	soil1$`N (%)` <- soil1$`N (%)`*10
	soil1$`P (ppm)` <- soil1$`P (ppm)`/1000
	soil1$`K (ppm)` <- soil1$`K (ppm)`/1000
	soil2 <- aggregate(soil1[, c(3,5,6,9,10,11)], list(Site = soil1$Site, Block = soil1$Block, Plot = soil1$Plot), mean, na.rm = TRUE)
	soil2 <- soil2[order(soil2[,"Site"], soil2[,"Block"], soil2[,"Plot"]), ]
	d2 <- merge(d1, soil2, by = intersect(names(d1), names(soil2)), all.x = TRUE)
	d2$yield <- d2$`Grain yield (kg/plot -5.625m2)` * (10000/5.625)
	d2$fwy_residue <- d2$`Stover yield (kg/plot - 5.625m2)` * (10000/5.625)
	d2$irrigated <- TRUE
	d2$row_spacing <- 75
	d2$plant_spacing <- 25
	
	# process file(s)
	d <- carobiner::change_names(d2,
	     c("Site", "pH", "N (%)", "K (ppm)", "P (ppm)", "Sand (%)", "Clay (%)"),
	     c("site", "soil_pH", "soil_N", "soil_K", "soil_P_total", "soil_sand", "soil_clay"))
		 	 
	d <- d[,c("country", "adm1", "adm2", "adm3", "latitude", "longitude", "site", "planting_date", "harvest_date", "season", "on_farm", "is_survey", "crop", "variety", "variety_code", "dmy_total", "yield", "fwy_residue", "fertilizer_type", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "OM_used", "OM_type", "OM_amount", "OM_N", "OM_P", "OM_K", "soil_pH", "soil_N", "soil_K", "soil_P_total", "soil_sand", "soil_clay", "irrigated", "row_spacing","plant_spacing")]

	id <- ifelse(r$site == "Kawanda", seq(1,sum(r$site == "Kawanda")), 
										seq(1,sum(r$site == "Namulonge")))
	
	r$trial_id <- paste0(r$trial_id, "-", id)
	
	r$yield_part <- "grain"
	carobiner::write_files(meta, d, path=path)

}
