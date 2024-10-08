# R script for "carob"
# license: GPL v3

# 1. variables such as herbicide_efficacy need to be captured
# 2. some more herbicide_product names need to be resolved
# 3. weed species need to be captured
# 4. "time-records" need to be captured; For example Wdden_m2_04WAP; Wdden_m2_06WAP; Wdden_m2_08WAP; Wdden_m2_10WAP; Wdden_m2_12WAP
# TWControl_04WAP..TWControl_12WAP, Broadleaf_04WAP..Broadleaf_12WAP, Grass_04WAP..Grass_12WAP



carob_script <- function(path) {

"The ‘Sustainable Weed Management Technologies for Nigeria’ was a 5-year project that was developed and assessed with smallholder farmer participation modern, relevant and appropriate cassava weed management technologies suitable for sustainable intensification in major agro-ecological (humid rainforest, forest transition savanna and southern Guinea savanna) and socio-economic conditions of Nigeria. An important goal of the project was to help smallholder cassava growers achieve sustainable increases in their productivity and incomes through the development and adoption of improved weed control methods. The project evaluated enhanced cassava agronomy, including judicious, safe use of herbicides, toward improved weed management, across 4 states in Nigeria where cassava is central to food security and livelihoods of 4.5 million farm families.

Though Nigeria is still the global leader in the overall production of cassava with about 50 million tons on 3.8 million hectares, average yields in Nigeria are only about half of those in leading countries in Asia, and less than half of those typical from researcher-run trials in Nigeria. Diverse factors are responsible for low productivity on about 4.5 million cassava farms, but poor weed management is generally among the principal factors. Weed control in the humid tropics is always a challenge, but compared to most other field crops, weed control in cassava systems is much more demanding. The crop is in the field for a long time (12 to 18 months), and is sown at wide spacing, resulting in ample opportunity for weeds to occupy space under the cassava canopy and reduce productivity. Although weeds are one of the most important constraints to improving cassava productivity; for high yields, good weed control needs to be coupled with improved varieties sown in the right densities at the right time. Adequate plant nutrition and pest control are also important; however, such inputs will not result in better yields if weeds are not controlled.

Hand weeding is the predominant weed control practice on smallholder cassava farms. Conventionally, farmers weed cassava three times, but in cassava farms where perennial weeds, such as Imperata, are predominant, extra hoe weeding may be required. Weeding takes 50 to 80% of the total labor budget. Up to 200-500 hours of labor for mostly women and children per ha are needed to prevent economic cassava root losses in Nigeria. IITA and its partners are therefore, through this project conducted research that developed innovative weed management practices, combining improved varieties, proper planting dates, plant populations, and plant nutrition, all coupled to intercropping and tillage options, through well-focused trials in the three agro-ecologies where cassava dominates in Nigeria. Herbicides, meeting globally accepted conventions and safety thresholds appropriate for smallholders, were tested for efficacy and economic merit. Multi-location on-station/off-station trials were followed with participatory farmer evaluations. Extension manuals and other tools for farmer and applicator learning were developed.

Results from this project showed that with appropriate weed management couple with best cassava agronomy cassava growers in can more than double the national yield average in Nigeria."


	uri <- "doi:10.25502/7k15-wy47/d"
	group <- "pest_disease"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institute = "IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_used;weeding_done",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-10-03"
	)
	
	f1 <- ff[basename(ff) == "Onfarm2017_All_Locations_DataFile_Rft.csv"]
	f2 <- ff[basename(ff) == "Onfarm2017_All_Locations_WeedSpecies_Rft.csv"]
	#f3 <- ff[basename(ff) == "Codelist_Onfarm2017_All_Locations_WeedSpecies_Rft.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	
	
	d <- data.frame(
		location = r1$Location,
		site = r1$Site,
		intercrops = r1$Cropsystem,
		treatment = r1$Treatment,
		latitude = r1$Latitude,
		longitude = r1$Longitude,
		herbicide_product = r1$Treatment,
		plant_density = r1$Standha_Cas,
		fwy_stems = r1$WtStemtha,
		yield_marketable = r1$NoOKRootsha,
		fw_yield = r1$RootYldtha * 1000,
		planting_date = as.Date(r1$Date_Planted_Cas,"%m/%d/%Y"),
		harvest_date = as.Date(r1$Date_Harvested_Cas,"%m/%d/%Y"),
		mze_planting_date = as.Date(r1$Date_Planted_Mz,"%m/%d/%Y"),
		mze_harvest_date = as.Date(r1$Date_Harvested_Mz,"%m/%d/%Y"),
		mze_plant_density = r1$Standha_Mz,
		cob_density = r1$MzCobCount_ha,
		mze_yield = r1$MzGrainYld_tha * 1000,
		soil_pH_KCl = r1$pH_KCl,
		soil_SOC = r1$OC_pct,
		soil_N = r1$N_pct,
		soil_P_Mehlich = r1$Meh_P_ppm,
		soil_Ca = r1$CMOL_perKg * 200,
		soil_sand = r1$SAND_pct,
		soil_silt = r1$SILT_pct,
		soil_clay = r1$CLAY_pct
	)
  
	#treatment variables
	d$herbicide_used <- ifelse(grepl("FarmerPractise|Untreated", r1$Pretreat) & grepl("Shorthandledhoe|FarmerPractise|Untreated", r1$Posttreat), FALSE, TRUE)
	d$weeding_done <- ifelse(grepl("Fa_Prt|_SHh$", r1$Treatment), TRUE, FALSE)
	
	#total weed biomass is stated as dry mass in meta data
	#d$weed_biomass <- r1$WdBiomass15_gm2/0.1
	
	#herbicide_products
	d$herbicide_product <- gsub("_", ";", d$herbicide_product)
	d$herbicide_product <- gsub("La","isoxaflutole;aclonifene",d$herbicide_product) # Lagon

	d$herbicide_product <- gsub("Fi","flumioxazin",d$herbicide_product)
	d$herbicide_product <- gsub("Pr|Prt","atrazine;s-metolachlor",d$herbicide_product)
	d$herbicide_product <- gsub("Ga","s-metolachlor;terbuthylazine",d$herbicide_product)
	d$herbicide_product <- gsub("Se","metribuzin",d$herbicide_product)
	d$herbicide_product <- gsub("Me","isoxaflutole;indaziflam",d$herbicide_product) #merlintotal
	d$herbicide_product <- gsub("Mo","flufenacet;diflufenican;flurtamone", d$herbicide_product) # movon

	d$herbicide_product <- gsub("MPw","foramsulfuron;iodosulfuron;thiencarbazone-methyl;cyprosulfamide", d$herbicide_product) #maisterpower
	d$herbicide_product <- gsub("M61","foramsulfuron;iodosulfuron-methyl-sodium", d$herbicide_product) #maister61WG
	d$herbicide_product <- gsub("FuC","fluazifop-butyl;lactofen",d$herbicide_product) # C=Cobra
	d$herbicide_product <- gsub("SmC","clethodim;lactofen",d$herbicide_product)
	d$herbicide_product <- gsub("RUp","glyphosate",d$herbicide_product)

## Why this is "none"?	
#	d$herbicide_product <- gsub("Fa_Prt|NP_ZPo|NP_Zpo","none",d$herbicide_product)
# Why "" ?
	d$herbicide_product <- gsub(";SHh","",d$herbicide_product)
	
	#total herbicide efficacy @ 12 weeks 
	#d$herbicide_efficacy <- r1$TWControl_12WAP
	#herbicide efficacy on different weed species @ 12 weeks
	#d$herbicide_efficacy_grass <- r1$Grass_12WAP
	#d$herbicide_efficacy_broadleaf <- r1$Broadleaf_12WAP
	#d$herbicide_efficacy_sedge <- r1$Sedge_12WAP
	
	#weed species
	d1 <- data.frame(
	  location = r2$Location,
	  site = r2$Site,
	  treatment = r2$Treatment,
	  weed_species = r2$WeedSpecies,
	  weed_density = r2$Wdden_m2_12WAP*10000)
	  
	#merge datasets
	d <- merge(d, d1, by=c("location","site","treatment"), all.x = TRUE)
	
	#sorting intercrops
	d$intercrops <- gsub("Intercrop","cassava;maize",d$intercrops)	
	d$intercrops <- gsub("Monocrop","cassava",d$intercrops) 
	
	csva <- d[grepl("cassava",d$intercrops),]
	csva$crop <- "cassava"
	csva$yield_part <- "roots"
	csva$intercrops <- gsub("cassava;|cassava","",csva$intercrops)
	csva$yield <- NA
	
	mze <- d[grepl("maize",d$intercrops),]
	mze$crop <- "maize"
	mze$yield_part <- "grain"
	mze$intercrops <- gsub(";maize","",mze$intercrops)
	mze$planting_date <- mze$harvest_date <- mze$plant_density <-NULL
	mze$planting_date <- mze$mze_planting_date
	mze$harvest_date <- mze$mze_harvest_date
	mze$plant_density <- mze$mze_plant_density
	mze$yield <- mze$mze_yield
	
	d <- rbind(csva, mze)
	
	d$mze_harvest_date <- d$mze_planting_date <- d$mze_plant_density <- d$mze_yield <- NULL
	  
	d$country <- "Nigeria"
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
   
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA)
  
	d$planting_date <- as.character(d$planting_date)
	d$harvest_date <- as.character(d$harvest_date)

	d$yield_marketable[d$yield_marketable < 150000] <- NA
	d$intercrops[d$intercrops==""] <- "none"
	d$trial_id <- as.character(as.integer(as.factor(1)))
	
	carobiner::write_files(path, meta, d)
}


