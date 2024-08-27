# R script for "carob"

carob_script <- function(path) {

"Climate change and soil fertility decline are major threats to smallholder farmers' food and nutrition security in southern Africa, and cropping systems that improve soil health are needed to address these challenges. Cropping systems that invest in soil organic matter, such as no-tillage (NT) with crop residue retention, have been proposed as potential solutions. However, a key challenge for assessing the sustainability of NT systems is that soil carbon (C) stocks develop over long timescales, and there is an urgent need to identify trajectory indicators of sustainability and crop productivity. Here we examined the effects of NT as compared with conventional tillage without residue retention on relationships between soil characteristics and maize (Zea mays L.) productivity in long-term on-farm and on-station trials in Zimbabwe. Our results show that relationships between soil characteristics and maize productivity, and the effects of management on these relationships, varied with soil type. Total soil nitrogen (N) and C were strong predictors of maize grain yield and above-ground biomass (i.e., stover) in the clayey soils, but not in the sandy soils, under both managements. This highlights context-specific benefits of management that fosters the accumulation of soil C and N stocks. Despite a strong effect of NT management on soil C and N in sandy soils, this accrual was not sufficient to support increased crop productivity in these soils. We suggest that sandy soils should be the priority target of NT with organic resource inputs interventions in southern Africa, as mineral fertilizer inputs alone will not halt the soil fertility decline. This will require a holistic management approach and input of C in various forms (e.g., biomass from cover crops and tree components, crop residues, in combination with mineral fertilizers). Clayey soils on the other hand have greater buffering capacity against detrimental effects of soil tillage and low C input."

	uri <- "hdl:11529/10548835"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

		meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield;dmy_total", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-08-22",
		notes = NA
	)
	
	f <- ff[basename(ff) == "DATA 2006 - 2017.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "*"))

	d <- data.frame(
		country = "Zimbabwe",
		location= r$`Site name`,
		land_prep_method=tolower(r$Treatment),
		soil_pH = as.numeric(r$`pH (H2O) -repeat`),
		soil_EC= as.numeric(r$`Electrical conductivity (µS/cm)`) * 0.001,
		soil_N = as.numeric(r$`N concentration (mg/g Soil)`) * 1000,
		soil_CEC= as.numeric(r$`CEC (meq/100g)`) ,
		soil_C= as.numeric(r$`C concentration (mg/g Soil)`),
		yield=as.numeric(r$`Mean grain yield (kg/ha)`),
		fwy_total=as.numeric(r$`Mean biomass yield (kg/ha)`)
	)
    
	
	d$land_prep_method <- gsub("ripper", "ripping", d$land_prep_method)
	  d$trial_id <- as.character(as.integer(as.factor(d$location)))
	
  	d$on_farm <- TRUE
  	d$is_survey <- FALSE
  	d$irrigated <- FALSE
  	d$yield_part <- "grain"
  	d$crop <- "maize"
  	
    geo <- data.frame(
      	location = c("Hereford", "Shamva", "Madziva"), 
  	    longitude = c(31.45, 31.5655, 31.5312), 
      	latitude = c(-17.4333, -17.3058, -16.914)
    )
   	d$geo_from_source <- FALSE
    d <- merge(d, geo, by="location", all.x=TRUE)
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    d$planting_date <- as.character(NA) 
    
   carobiner::write_files(path, meta, d)
}

