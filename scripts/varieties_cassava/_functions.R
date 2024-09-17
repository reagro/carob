
## there are more variables to be processed; depending on the dataset

process_cassava <- function(ff, location=NULL, adm1=NULL) {

	f <- grep("\\.csv$", ff, value=TRUE)
	r <- read.csv(f)
	
	if (is.null(r$fresh.root.yield.CO_334.0000013 )) r$fresh.root.yield.CO_334.0000013 <- NA
	
	d <- data.frame(
		planting_date = r$plantingDate,
		harvest_date = r$harvestDate,
		year = r$studyYear,
		plot_length = as.numeric(r$plotLength),
		plot_width = as.numeric(r$plotWidth),
		location = carobiner::fix_name(r$locationName, "title"),
		variety = r$germplasmName,
		rep = r$replicate,
		yield = r$fresh.root.yield.CO_334.0000013 * 1000
		
		#"fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016"
		#"plant.stands.harvested.counting.CO_334.0000010"              
		#"poundability.assessment.0.4.CO_334.0000074"
		#"root.number.counting.CO_334.0000011"                         
		#"rotted.storage.root.counting.CO_334.0000084"                  
		#"storage.root.size.visual.rating.1.7.CO_334.0000019"          
		#"taste.of.boiled.root.rating.1.3.CO_334.0000085"               
		#"notes"                                                       
	)
	if (!is.null(location)) {
		d$location <- location
		d$adm1 <- adm1
	}

	d$planting_date <- carobiner:::eng_months_to_nr(d$planting_date) |> as.Date() |> as.character()
	d$harvest_date <- carobiner:::eng_months_to_nr(d$harvest_date) |> as.Date() |> as.character()

	if (all(is.na(d$planting_date))) d$planting_date <- as.character(d$year)
	d$year <- NULL

	d$trial_id <- "1"
	d$longitude <- d$latitude <- as.numeric(NA)
	d$geo_from_source <- FALSE
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA
	
	geo <- data.frame(
		country = c("Nigeria", "Togo", "Ghana", "Togo", 
			"Niger", "Ghana", "Togo", "Togo", "Ghana", "Nigeria", "Burkina Faso", 
			"Burkina Faso", "Ghana", "Nigeria", "Benin", "Nigeria", "Ghana", 
			"Niger", "Benin", "Ghana", "Ghana", "Nigeria", "Nigeria", "Nigeria", 
			"Nigeria", "Nigeria", "Ghana", "Nigeria", "Nigeria", "Togo", 
			"Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria"), 
		location = c("Agbarho", "Adeta", "Assin Fosu", "Ativeme", 
			"Bengou", "Damongo", "Danyi", "Davie", "Ejura", "Ekekhen", "Fada", 
			"Farakoba", "Fumesua", "Ibadin", 
			"Ina", "Ivue", "Kumasi", "Lossa", "Niaouli", "Nyankpala", "Ohawu", 
			"Oki", "Okeredafe", "Okurekpo", "Onire", "Oteva", "Pokuase", 
			"Rivers", "Sohe", "Sotouboua", "Umuede", "Urhuo", "Usenu", "Ute", 
			"Warake"), 
		longitude = c(5.8664, 0.7368, -1.2769, 1.1118, 3.5932, -1.8201, 
			0.6943, 1.2162, -1.3559, 6.2487, 0.3542, -4.3409, -1.5214, 
			NA, 2.7265, 6.2717, -1.6233, 1.5754, 2.1369, -0.9815, 0.8967, 7.2865, 
			NA, 5.9515, 4.0315, NA, -0.2826, 6.8357, NA, 0.9472, 7.0992, NA, 6.2158, 
			5.6837, 6.1763), 
		latitude = c(5.5881, 7.1342, 5.7005, 6.421, 11.9906, 
			9.0851, 7.1596, 6.3681, 7.3847, 6.6222, 12.0502, 11.0828, 6.7108, 
			NA, 9.9668, 6.7392, 6.6986, 13.9207, 6.7436, 9.4005, 6.1313, 5.6312, 
			NA, 5.6955, 7.9812, NA, 5.6892, 5.0233, NA, 8.4848, 5.1170, NA, 6.7355, 6.3953, 
			6.9990)
	)
	
#geo[is.na(geo[,4]), ]
#   country  location longitude latitude
#14 Nigeria    Ibadin        NA       NA
#23 Nigeria Okeredafe        NA       NA
#26 Nigeria     Oteva        NA       NA
#29 Nigeria      Sohe        NA       NA
#32 Nigeria     Urhuo        NA       NA
	
	i <- match(d$location, geo$location)	
	d$country <- geo$country[i]
	d$longitude <- geo$longitude[i]
	d$latitude <- geo$latitude[i]

	d

}


#Ibadin, Edo