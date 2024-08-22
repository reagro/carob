# R script for "carob"

## Fertilizer amounts are wrong. 
## 100 DAP has both N and P (but not 100). Urea is not 100% N. 
## It seems that this file has mulitple observations in time for some variables. If so, these are higly valuable and need to be captured as "timevars"

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
		planting_date=as.character(as.Date(r1$`Sowing-date`, "%d/%m/%Y")),
		emergence_date=as.character(as.Date(r1$`Emergence-date`, "%d/%m/%Y")),
		flowering_date=as.character(as.Date(r1$`Flowering-date`, "%d/%m/%Y")),
		maturity_date=as.character(as.Date(r1$`Maturity-date`, "%d/%m/%Y")),
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
	r2 <- suppressWarnings(carobiner::read.excel(f2))
		
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

## location details extracted from: https://www.geonames.org/search.html?q=patancheru&country=
## RH: use Google Maps to find lon/lat for the ICRISAT campus instead 
	
	d$country <- "India"	
	d$location <- "Patancheru"
	d$site <- "ICRISAT"
	d$adm1 <- "Telanyana"
	d$adm2 <- "Sangareddy"
	d$longitude <- NA #78.2294
	d$latitude <- NA #17.53
	d$geo_from_source <- FALSE

  d$crop <- "sorghum"
  d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	
	d$flowering_date[d$flowering_date=="no panical"] <- NA

	carobiner::write_files(path, meta, d)
}



