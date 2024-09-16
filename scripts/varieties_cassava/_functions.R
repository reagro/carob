
## there are more variables to be processed; depending on the dataset

process_cassava <- function(f, country) {

	r <- read.csv(f)
	
	if (is.null(r$fresh.root.yield.CO_334.0000013 )) r$fresh.root.yield.CO_334.0000013 <- NA
	
	d <- data.frame(
		planting_date = r$plantingDate,
		harvest_date = r$harvestDate,
		year = r$studyYear,
		#plotWidth plotLength
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

	d$planting_date <- carobiner:::eng_months_to_nr(d$planting_date) |> as.Date() |> as.character()
	d$harvest_date <- carobiner:::eng_months_to_nr(d$harvest_date) |> as.Date() |> as.character()

	if (all(is.na(d$planting_date))) d$planting_date <- as.character(d$year)
	d$year <- NULL

	d$country <- country
	d$trial_id <- "1"
	d$longitude <- d$latitude <- as.numeric(NA)
	d$geo_from_source <- FALSE
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA
	
	d

}
