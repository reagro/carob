# R script for "carob"

carob_script <- function(path) {

"Data on sequential observations of agronomically important traits like Leaf biomass, Stem biomass, Total biomass, Leaf area, and Grain yield during cropping season for the year 2016 in Kharif for Sorghum at ICRISAT Patancheru campus research fields.Data on multi factorial treatments in 2016-17 Kharif Sorghum at Patancheru"
	
  uri <- "doi:10.21421/D2/BCYJVX"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "ICRISAT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;plant_density;irrigated",
		response_vars = "fwy_leaves, fwy_stems, fwy_total, yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-08-21",
		notes = NA
	)
	
	f1 <- ff[basename(ff) == "2.Experiment.xlsx"]
	r1 <- carobiner::read.excel(f1)

	d1 <- data.frame(
		trial_id=as.character(r1$Plot),
		variety_code=r1$Genotype, 
		planting_date=r1$`Sowing-date`,
		emergence_date=r1$`Emergence-date`,
		flowering_date=r1$`Flowering-date`,
		maturity_date=r1$`Maturity-date`,
		row_spacing=r1$`Row-spacing`,
		soil_type=r1$`Soil-type`,
		irrigated=TRUE,
		irrigation_number=as.integer(3) ,
		irrigation_amount=r1$`Irri1-amt`+ r1$`Irri2-amt`+ r1$`Irri3-amt`,
		N_fertilizer=r1$`Fert2-amt`+r1$`Fert3-amt`,
		P_fertilizer=r1$`Fert1-amt`,
		K_fertilizer =as.numeric(NA),
		fertilizer_type="urea;DAP"
	)
		
	f2 <- ff[basename(ff) == "1.Growth and development.xlsx"]
	r2 <- carobiner::read.excel(f2)
		
	d2 <- data.frame(
	  variety_code=r2$`Cultivar-short-name`,
	  trial_id=as.character(r2$`Plot-id`),
	  treatment=r2$`Treatment-combinations`,
	  fwy_leaves=r2$`TC-leaf-biom-ha`,
	  fwy_stems=as.numeric(r2$`TC-stem-biom-ha`),
	  fwy_residue=as.numeric(r2$`TC-stover-wt-ha`),
	  fwy_total=as.numeric(r2$`TC-tot-biom-ha`),
	  plant_density=r2$`TC-density`*10000,
	  LAI=as.numeric(r2$`TC-lai`),
	  yield=r2$`TC-grain-wt-ha`
	  
	)
	
	d2$treatment[d2$treatment=="WW_LD_HN"] <- "well watered_low density_high nitrogen"
	
	d <- merge(d1, d2, by=c("trial_id","variety_code"), all.x = TRUE)

	 # location details extracted from: https://www.geonames.org/search.html?q=patancheru&country=
	#conversions Source: https://www.vercalendario.info/en/how/convert-latitude-longitude-degrees-decimals.html
	d$country <- "India"	
	d$location <- "Patancheru"
	d$adm1 <- carobiner::fix_name("Telanyana")
	d$adm2 <- carobiner::fix_name("Sangareddy")
	d$longitude <- 78.2294
	d$latitude <- 17.53
	d$geo_from_source <- FALSE

  d$crop <- "sorghum"
  d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	
	#fixing dates
	#project dates where stated as Start: 2016-07-11 ; End: 2016-11-11
	#d$harvest_date <- as.character(2016-11)
	
	d$flowering_date[d$flowering_date=="no panical"] <- NA
	d$planting_date <- as.Date(d$planting_date, "%d/%m/%Y")
	d$emergence_date <- as.Date(d$emergence_date, "%d/%m/%Y")
	d$maturity_date <- as.Date(d$maturity_date, "%d/%m/%Y")
	d$flowering_date <- as.Date(d$flowering_date, "%d/%m/%Y")

	carobiner::write_files(path, meta, d)
}



