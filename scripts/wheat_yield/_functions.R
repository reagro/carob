
proc_wheat <- function(ff) {

# not used
#	fgeno <- ff[basename(ff) == "29 HRWYT_Genotypes_Data.xls"]
#	fgrn <- ff[basename(ff) == "29 HRWYT_GrnYld.xls"]
#	geno <- read.csv(fgeno, sep = "\t")
#	grn <- read.csv(fgrn, sep = "\t")
	


	fenv <- ff[grep("EnvData.xls", basename(ff))]
	floc <- ff[grep("Loc_data.xls", basename(ff))]
	fraw <- ff[grep("RawData.xls", basename(ff))]

	loc <- read.csv(floc, sep = "\t")
	raw <- read.csv(fraw, sep = "\t", fileEncoding = "latin1")
	env <- read.csv(fenv, sep = "\t")
	
	raw$country <- carobiner::fix_name(raw$Country, "title")

	raw$location <- gsub(" - ", ", ", raw$Loc_desc)

	raw$site <- raw$location
	raw$trial_id <- raw$Trial.name

 	raw <- raw[,c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Gen_name", "Trait.name", "Value")]
	raw <- reshape(raw, idvar = c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), timevar = "Trait.name", direction = "wide")
	colnames(raw) <- gsub("Value.","", colnames(raw))

	loc$latitude <- loc$Lat_degress + loc$Lat_minutes / 60 
	loc$longitude <- loc$Long_degress + loc$Long_minutes / 60 
	
	W <- grep("W", loc$Longitud, ignore.case=TRUE)
	loc$longitude[W] <- -loc$longitude[W]
	S <- grep("S", loc$Latitud, ignore.case=TRUE)
	loc$latitude[S] <- -loc$latitude[S]
	
	# Merge raw and loc tables to get latlon variables
	raw <- merge(raw, loc[, c("Loc_no", "longitude", "latitude")], by ="Loc_no", all.x = T)
	
	
	renv <- merge(raw,env, by = c("Loc_no"), all.x = TRUE)[,c("Loc_no", "Rep", "Sub_block", "Plot", "Gen_name", "Trait.name","Value")]
	renv <- reshape(renv, idvar = c("Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), timevar = "Trait.name", direction = "wide")
	colnames(renv) <- gsub("Value.","", colnames(renv))

# Merge raw with renv
	renv <- merge(raw,renv, by = c("Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), all.x = TRUE)

# Process in carob format
	renv$start_date <- as.character(as.Date(renv$SOWING_DATE, "%b %d %Y"))
	renv$end_date <- as.character(as.Date(renv$HARVEST_FINISHING_DATE, "%b %d %Y"))
	
# other variables
	renv$on_farm <- FALSE
	renv$is_survey <- FALSE
	renv$irrigated <- renv$IRRIGATED != "NO"
	renv$row_spacing <- as.numeric(renv$SPACE_BTN_ROWS_SOWN)
	renv$rep <- renv$Rep
	renv$crop <- "wheat"
	renv$variety_code <- renv$Gen_name
	renv$variety_type <- "high-yield"
	
	m <- matrix(byrow=TRUE, ncol=2, 
		c(
		"AVENA-VICIA", "oats; vetch", 
		"AVENA / VICIA", "oats; vetch", 
		"BAJRA", "pearl millet", 
		"CEREALS", "CEREAL", 
		"CHECK PEA", "chickpea", 
		"COE PEA", "cowpea", 
		"COJENUS", "pigeon pea", 
		"CORN", "maize", 
		"CROTOTERIA (ABONO VERDE)", "crotalaria", 
		"GLYCIN MAX", "soybean", 
		"LAGUME CROP(SOYABEAN)", "soybean", 
		"LEGUMES", "legume", 
		"LEGUME", "legume", 
		"LINSEED", "flax",
		"GREEM  MANURE", "green manure", 
		"MAIZE", "maize", 
		"MAIZ", "maize", 
		"MUNG-PULSES", "mung bean", 
		"MONG BEAN", "mung bean", 
		"OILSEED", "rapeseed", 
		"OLISEED", "rapeseed", 
		"OIL SEED", "rapeseed", 
		"OIL CROP", "rapeseed", 
		"ORYZA SATIVA L.", "rice", 
		"ORYZA SATIVA", "rice", 
		"V. RADIATA MOONG", "mung bean", 
		"PADDY", "rice", 
		"PAPPER CROP", "pepper", 
		"PEAS", "pea", 
		"PISUM SATIVUM", "pea",
		"PULSES", "pulse", 
		"PULSE", "pulse", 
		"RAPHANUS  SPP", "raphanus spp.", 
		"SESBANIA SP.", "sesbania", 
		"SOJA", "soybean", 
		"SOYA", "soybean", 
		"SOY BEAN", "soybean", 
		"SUNHIMP", "sunhemp", 
		"TRIGO", "wheat", 
		"TRIFOLIUM ALEXANDRIUM", "clover", 
		"TRITICALE", "triticale", 
		"ZEA MAYS", "maize", 
		"CEREAL", "cereal", 		
		"CROP", NA)
	)
	
	
	prcrop <- renv$USE_OF_FIELD_SPECIFY_CROP
	for (i in 1:nrow(m)) {
		prcrop <- gsub(m[i,1], m[i,2], prcrop)
	}
	renv$previous_crop <- tolower(prcrop)
	
	# Convert yield in ton/ha to kg/ha
	renv$yield <- as.numeric(renv$GRAIN_YIELD)*1000 
	renv$grain_weight <- as.numeric(renv$`1000_GRAIN_WEIGHT`)
	
	# Extract columns with NPK fertilizers
	
	n <- grep("FERTILIZER_%N", colnames(renv))
	p <- grep("FERTILIZER_%P", colnames(renv))
	k <- grep("FERTILIZER_%K", colnames(renv))
	
	renvn <- renv[,n]
	renvp <- renv[,p]
	renvk <- renv[,k]
		
	# function to replace na with 0
	isna <- function(x){
		ifelse(is.na(x) == TRUE, 0,x)
	}
	
	# Nitrogen levels
	revns <- as.data.frame(sapply(renvn, isna))
	
	revns <- data.frame(apply(revns, 2, as.numeric))
	
	renv$N_fertilizer <- apply(revns, 1, sum)
	
	renv$N_splits <- ifelse(renv$`FERTILIZER_KG/HA_3` > 0, 3,
	                       ifelse(renv$`FERTILIZER_KG/HA_2` > 0, 2, 1))


	# Phosphorus levels
	revps <- as.data.frame(sapply(renvp, isna))
	
	revps <- data.frame(apply(revps, 2, as.numeric))
		
	renv$P_fertilizer <- apply(revps, 1, sum)
	
	# Potassium levels
	revks <- as.data.frame(sapply(renvk, isna))
	
	revks <- data.frame(apply(revks, 2, as.numeric))
		
	renv$K_fertilizer <- apply(revks, 1, sum)
	
	# Subset for relevant columns
	
	carob_variables <- c("country", "location", "site", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "rep","crop", "variety_code", "variety_type", "previous_crop", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer",  "irrigated", "row_spacing", "yield", "grain_weight")
	
	renv[, carob_variables]
}
