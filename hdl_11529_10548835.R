# R script for "carob"

## ISSUES
# if there are remaining issues, please put these in "meta$notes"


carob_script <- function(path) {

"
Climate change and soil fertility decline are major threats to smallholder farmers' food and nutrition security in southern Africa, and cropping systems that improve soil health are needed to address these challenges. Cropping systems that invest in soil organic matter, such as no-tillage (NT) with crop residue retention, have been proposed as potential solutions. However, a key challenge for assessing the sustainability of NT systems is that soil carbon (C) stocks develop over long timescales, and there is an urgent need to identify trajectory indicators of sustainability and crop productivity. Here we examined the effects of NT as compared with conventional tillage without residue retention on relationships between soil characteristics and maize (Zea mays L.) productivity in long-term on-farm and on-station trials in Zimbabwe. Our results show that relationships between soil characteristics and maize productivity, and the effects of management on these relationships, varied with soil type. Total soil nitrogen (N) and C were strong predictors of maize grain yield and above-ground biomass (i.e., stover) in the clayey soils, but not in the sandy soils, under both managements. This highlights context-specific benefits of management that fosters the accumulation of soil C and N stocks. Despite a strong effect of NT management on soil C and N in sandy soils, this accrual was not sufficient to support increased crop productivity in these soils. We suggest that sandy soils should be the priority target of NT with organic resource inputs interventions in southern Africa, as mineral fertilizer inputs alone will not halt the soil fertility decline. This will require a holistic management approach and input of C in various forms (e.g., biomass from cover crops and tree components, crop residues, in combination with mineral fertilizers). Clayey soils on the other hand have greater buffering capacity against detrimental effects of soil tillage and low C input.
"


	uri <- "hdl:11529/10548835"
	group <- "agronomy"


	ff  <- carobiner::get_data(uri, path, group)

 
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institute = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "conventional;ripper;direct seeding",
		response_vars = "yield;dmy_total", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-08-22",
		notes = NA
	)
	
## read data 

	f <- ff[basename(ff) == "DATA 2006 - 2017.xlsx"]
	r <- carobiner::read.excel(f)

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		country = "Zimbabwe",
		location= r$`Site name`,
		treatment=tolower(r$Treatment),
		soil_pH = as.numeric(r$`pH (H2O) -repeat`),
		soil_EC= r$`Electrical conductivity (µS/cm)`,
		soil_N=r$`N concentration (mg/g Soil)` ,
		soil_CEC= as.numeric(r$`CEC (meq/100g)`) ,
		soil_C= as.numeric(r$`C concentration (mg/g Soil)`),
		yield=as.numeric(r$`Mean grain yield (kg/ha)`),
		dmy_total=as.numeric(r$`Mean biomass yield (kg/ha)`))
    
	
    d$soil_EC <- as.numeric(d$soil_EC) *0.001
    d$soil_N <- as.numeric(d$soil_N) *1000
	

	 d$trial_id <- "1"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	

	

  d$longitude[d$location =="Hereford"]<- 31.45 
	d$latitude[d$location =="Hereford"]<- -17.4333
	
	d$longitude[d$location =="Shamva"]<- 31.56554240
	d$latitude[d$location =="Shamva"]<- -17.30588810

	d$longitude[d$location =="Madziva"]<- 31.531223
	d$latitude[d$location =="Madziva"]<-  -16.914036
	

	d$geo_from_source <- FALSE


	d$yield_part <- "grain"
	d$crop <- "maize"
	

# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

