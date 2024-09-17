## there are more variables to be processed; depending on the dataset
## the table below shows the variable name and frequency in the 132 datasets

#Variable, Count
#studyYear, 132
#studyName, 132
#studyDesign, 132
#studyDescription, 132
#studyDbId, 132
#rowNumber, 132
#replicate, 132
#programName, 132
#programDescription, 132
#programDbId, 132
#plotWidth, 132
#plotNumber, 132
#plotLength, 132
#plantNumber, 132
#plantingDate, 132
#plantedSeedlotTransactionWeight, 132
#plantedSeedlotTransactionDescription, 132
#plantedSeedlotTransactionCount, 132
#plantedSeedlotStockUniquename, 132
#plantedSeedlotStockDbId, 132
#plantedSeedlotCurrentWeightGram, 132
#plantedSeedlotCurrentCount, 132
#plantedSeedlotBoxName, 132
#observationUnitName, 132
#observationUnitDbId, 132
#observationLevel, 132
#notes, 132
#locationName, 132
#locationDbId, 132
#harvestDate, 132
#germplasmSynonyms, 132
#germplasmName, 132
#germplasmDbId, 132
#fieldTrialIsPlannedToCross, 132
#fieldTrialIsPlannedToBeGenotyped, 132
#fieldSize, 132
#entryType, 132
#colNumber, 132
#blockNumber, 132
#availableGermplasmSeedlotUniquenames, 132
#fresh.storage.root.weight.per.plot.CO_334.0000012, 105
#fresh.root.yield.CO_334.0000013, 97
#root.number.counting.CO_334.0000011, 91
#fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016, 77
#plant.stands.harvested.counting.CO_334.0000010, 59
#rotted.storage.root.counting.CO_334.0000084, 54
#taste.of.boiled.root.rating.1.3.CO_334.0000085, 51
#harvest.index.variable.CO_334.0000015, 49
#poundability.assessment.0.4.CO_334.0000074, 47
#boiled.storage.root.color.visual.1.3.CO_334.0000114, 45
#number.of.planted.stakes.per.plot.counting.CO_334.0000159, 41
#top.yield.CO_334.0000017, 40
#storage.root.size.visual.rating.1.7.CO_334.0000019, 37
#cassava.mosaic.disease.severity.3.month.evaluation.CO_334.0000192, 35
#sprouting.proportion.CO_334.0000008, 30
#cassava.mosaic.disease.incidence.3.month.evaluation.CO_334.0000196, 24
#dry.matter.content.percentage.CO_334.0000092, 17
#cassava.bacterial.blight.severity.3.month.evaluation.CO_334.0000175, 17
#cassava.mosaic.disease.severity.1.month.evaluation.CO_334.0000191, 15
#cassava.mosaic.disease.severity.6.month.evaluation.CO_334.0000194, 14
#dry.yield.CO_334.0000014, 13
#cassava.bacterial.blight.severity.6.month.evaluation.CO_334.0000176, 11
#sprout.count.at.one.month.CO_334.0000213, 10
#non.marketable.root.number.counting.CO_334.0000168, 9
#marketable.root.number.counting.CO_334.0000169, 9
#root.neck.length.visual.rating.0.7.CO_334.0000022, 8
#cassava.mosaic.disease.incidence.1.month.evaluation.CO_334.0000195, 7
#cassava.green.mite.severity.first.evaluation.CO_334.0000189, 7
#storage.root.shape.visual.rating.1.6.CO_334.0000020, 6
#non.marketable.root.weight.measurement.in.kg.CO_334.0000132, 6
#marketable.root.weight.measurement.in.kg.CO_334.0000131, 6
#storage.root.cortex.color.visual.rating.1.4.CO_334.0000115, 5
#ease.of.peeling.root.cortex.visual.rating.1.3.CO_334.0000308, 5
#cassava.bacterial.blight.incidence.6.month.evaluation.CO_334.0000179, 3
#cassava.anthractnose.disease.severity.in.3.month.CO_334.0000218, 3
#stem.number.counting.CO_334.0000129, 2
#starch.content.percentage.CO_334.0000071, 2
#leaf.retention.visual.rating.1.5.CO_334.0000048, 2
#cassava.mosaic.disease.incidence.6.month.evaluation.CO_334.0000198, 2
#staygreen.visual.scale.1.9.CO_334.0000224, 1
#plant.height.measurement.in.cm.CO_334.0000018, 1
#initial.vigor.assessment.1.7.CO_334.0000009, 1
#gari.weight.after.drying.in.kg.g.CO_334.0000246, 1
#gari.content.g.kg.CO_334.0000096, 1
#first.apical.branch.height.measurement.in.cm.CO_334.0000106, 1
#dry.matter.visual.rating.1.3.CO_334.0002012, 1
#cassava.mosaic.disease.severity.12.month.evaluation.CO_334.0000199, 1
#cassava.anthractnose.disease.severity.in.6.month.CO_334.0000184, 1
#branching.level.counting.CO_334.0000079, 1


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
		variety_alt = as.character(r$germplasmSynonyms),
		variety_code = as.character(r$germplasmDbId),
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