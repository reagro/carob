
# to do: extract more variables of interest. 
# unique(raw$Trait.name)
# GRAIN_YIELD, 1000_GRAIN_WEIGHT, DAYS_TO_HEADING, PLANT_HEIGHT, AGRONOMIC_SCORE, SELECTED_CHECK_MARK, DAYS_TO_MATURITY, H_TRITICI_REPENTIS, LODGING_PERCENT_HARVESTED_AREA, STRIPE_RUST_ON_LEAF, POWDERY_MILDEW, TEST_WEIGHT, LEAF_RUST, YRWarriorRace, SPIKE_LENGTH, GERMINATION_%, CHLOROPHYLL, GRAIN_PROTEIN, Normalized Difference Vegetation Index, GRAIN APPEARANCE SCORE, PHENOL REACTION SCORE, Canopy Temperature, ABOVE_GROUND_BIOMASS, STEM_RUST, FeConcentration, ZnConcentration, TILLERS BY METER, HELMINTHOSPORIUM_SATIVUM_LEAF, GRAINS/SPIKE, TILLERS BY M2, Blast intensity, Blast severity, SPIKES_M2, GLUTEN_CONTENT, GRAIN_MOISTURE, SEDIMENTATION_INDEX
 


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

	# 'Cid', 'Sid', 	
	vars <- c( 'Trait.name', 'Value', 'Trial.name', 'Loc_no', 'Country', 'Loc_desc', 'Cycle', 'Gen_name', 'Rep', 'Sub_block', 'Plot')
	raw <- raw[, vars]
	
	raw$Value <- trimws(raw$Value)
	raw$Value[raw$Value %in% c("-", ".")] <- ""
	raw$Value[raw$Value == ""] <- NA
	raw$Value <- as.numeric(raw$Value)
	raw <- aggregate(Value ~ ., data=raw, mean, na.rm=TRUE)

	raw <- reshape(raw, idvar=vars[-c(1:2)], timevar = "Trait.name", direction = "wide")
	colnames(raw) <- gsub("Value.","", colnames(raw))

	loc$latitude <- loc$Lat_degress + loc$Lat_minutes / 60 
	loc$longitude <- loc$Long_degress + loc$Long_minutes / 60 
	
	W <- grep("W", loc$Longitud, ignore.case=TRUE)
	loc$longitude[W] <- -loc$longitude[W]
	S <- grep("S", loc$Latitud, ignore.case=TRUE)
	loc$latitude[S] <- -loc$latitude[S]
	
	# Merge raw and loc tables to get latlon variables
	raw <- merge(raw, loc[, c("Loc_no", "longitude", "latitude")], by ="Loc_no", all.x = TRUE)

	envvars <- c('Trait.name', 'Value', 'Trial.name', 'Loc_no', 'Country', 'Cycle')
	env <- unique(env[,envvars])
	# take the first in case of duplicates
	env <- aggregate(Value ~ ., data=env, \(i) i[1])
	env <- reshape(env, idvar=envvars[-c(1:2)], timevar = "Trait.name", direction = "wide")
	colnames(env) <- gsub("Value.","", colnames(env))

	r <- merge(raw, env, by = c("Country", "Loc_no", "Trial.name", "Cycle"), all.x = TRUE)

	r$trial_id <- r$Trial.name
	r$country <- carobiner::fix_name(r$Country, "title")
	r$location <- gsub(" - ", ", ", r$Loc_desc)

# Process in carob format
	r$start_date <- as.character(as.Date(r$SOWING_DATE, "%b %d %Y"))
	r$end_date <- as.character(as.Date(r$HARVEST_FINISHING_DATE, "%b %d %Y"))
	
# other variables
	r$on_farm <- FALSE
	r$is_survey <- FALSE
	r$irrigated <- r$IRRIGATED != "NO"
	r$row_spacing <- as.numeric(r$SPACE_BTN_ROWS_SOWN)
	r$rep <- r$Rep
	r$crop <- "wheat"
	r$variety_name <- r$Gen_name
	r$variety_code <- r$Gen_name
	r$variety_type <- "high-yield"

	# note that the order is important
	# to avoid partial matching, first the more complex names
	m <- matrix(byrow=TRUE, ncol=2, c(
		"ALFALFA", "LUCERNE", 
		"AMAN RCIE", "rice",
		"AVENA+VICIA", "oats; vetch", 
		"AVENA-VICIA", "oats; vetch", 
		"AVENA / VICIA", "oats; vetch", 
		"AVENA", "oats", 
		"BAJRA", "pearl millet", 
		"CEREALS", "CEREAL", 
		"CHECK PEA", "chickpea", 
		"CHICK PEN", "chickpea", 
		"COE PEA", "cowpea", 
		"COJENUS", "pigeon pea", 
		"CORN", "maize", 
		"COTTAN", "cotton",
		"CROTOTERIA (ABONO VERDE)", "crotalaria", 
		"FALLOWED", "no crop",
		"FIELD PEAS", "pea",
		"GLYCIN MAX", "soybean", 
		"HARICOT BEAN", "common bean", 
		"LAB.LAB", "lablab",
		"LAGUME CROP(SOYABEAN)", "soybean", 
		"LEGUMES", "legume", 
		"LEGUME", "legume", 
		"LINSEED", "flax",
		"GREEM  MANURE", "green manure", 
		"MAIZE", "maize", 
		"MAIZ/SOJA", "maize; soybean",
		"MAIZ", "maize", 
		"MUNG-PULSES", "mung bean", 
		"MONG BEAN", "mung bean", 
		"OILSEED", "rapeseed", 
		"OLISEED", "rapeseed", 
		"OIL SEED", "rapeseed", 
		"OIL CROPS", "rapeseed", 
		"OIL CROP", "rapeseed", 
		"ORYZA SATIVA L.", "rice", 
		"ORYZA SATIVA", "rice", 
		"PADDY", "rice", 
		"PAPPER CROP", "pepper", 
		"PATATO", "potato",
		"PEAS", "pea", 
		"PISUM SATIVUM", "pea",
		"PULSES", "pulse", 
		"PULSE", "pulse", 
		"PURPERUREUS", "lablab",
		"RAPHANUS  SPP", "raphanus spp.", 
		"RAPHA NUS SPP", "raphanus spp.", 
		"SESBANIA SP.", "sesbania", 
		"SOY BEAN", "soybean", 
		"SOYBEANS", "soybean", 
		"SOYBEAN", "soybean", 
		"SOYA BEANS", "soybean", 
		"SOYA BEAN", "soybean", 
		"SUGAR CAME", "sugarcane",
		"SOJA", "soybean", 
		"SOYA", "soybean", 
		"SUNHEMP (FLAX)", "sunhemp",
		"SUNHIMP (FLAX)", "sunhemp",
		"SUNHIMP", "sunhemp", 
		"SUNHAMP", "sunhemp",
		"SUMHEMP", "sunhemp",
		"SWEET  POTATOS", "sweetpotato",
		"TRIGO", "wheat", 
		"TRIFOLIUM ALEXANDRIUM", "clover", 
		"TRIFOLIUM ALEXANDIUM", "clover", 
		"TRITICALE", "triticale", 
		"VIGNA RADIATA", "mung bean", 
		"V. RADIATA MOONG", "mung bean", 
		"ZEA MAYS", "maize", 
		"CEREAL", "cereal", 		
		"BEANS", "common bean",
		"CROP", NA)
	)

 # Update once I get clarification

	
	prcrop <- r$USE_OF_FIELD_SPECIFY_CROP
	for (i in 1:nrow(m)) {
		prcrop <- gsub(m[i,1], m[i,2], prcrop)
	}
	r$previous_crop <- tolower(prcrop)
	
	# Convert yield in ton/ha to kg/ha
	r$yield <- as.numeric(r$GRAIN_YIELD) * 1000 
	r$grain_weight <- as.numeric(r$`1000_GRAIN_WEIGHT`)
	
	# Extract columns with NPK fertilizers
	fertfun <- function(x, v) {
		i <- grep(v, colnames(x))
		rn <- x[,i]
		rn[rn==0] <- NA
		fert <- apply(rn, 1, \(i) sum(as.numeric(i), na.rm=T))
		test <- grepl("P2O5", colnames(rn))
		if (all(test)) {
			fert <- fert / 2.29
		} else if (any(test)) { stop("?") }
		test <- grepl("K2O", colnames(rn))
		if (all(test)) {
			fert <- fert / 1.21
		} else if (any(test)) { stop("?") }
		splits <- apply(rn, 1, \(i) sum(as.numeric(i) > 0, na.rm=T))
		cbind(fert, splits)
	}
	
	x <- fertfun(r, "FERTILIZER_%N")
	r$N_fertilizer <- x[,1]
	r$N_splits <- x[,2]

	r$P_fertilizer <- fertfun(r, "FERTILIZER_%P")[,1]
	r$K_fertilizer <- fertfun(r, "FERTILIZER_%K")[,1]
	
	# Subset for relevant columns
	
	cvars <- c("country", "location", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "rep","crop", "variety_code", "variety_type", "previous_crop", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer",  "irrigated", "row_spacing", "yield", "grain_weight")
	
	r[, cvars]
}
