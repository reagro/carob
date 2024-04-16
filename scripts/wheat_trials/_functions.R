
# to do: extract more variables of interest. 
# not included from raw$Trait.name
# PLANT_HEIGHT, AGRONOMIC_SCORE, SELECTED_CHECK_MARK, DAYS_TO_MATURITY, H_TRITICI_REPENTIS, LODGING_PERCENT_HARVESTED_AREA, TEST_WEIGHT, YRWarriorRace, SPIKE_LENGTH, GERMINATION_%, CHLOROPHYLL, GRAIN_PROTEIN, Normalized Difference Vegetation Index, GRAIN APPEARANCE SCORE, PHENOL REACTION SCORE, Canopy Temperature, ABOVE_GROUND_BIOMASS, TILLERS BY METER, GRAINS/SPIKE, TILLERS BY M2, SPIKES_M2, GLUTEN_CONTENT, GRAIN_MOISTURE, SEDIMENTATION_INDEX

# also can use more variables from env data such as irrigation 


proc_wheat <- function(ff) {

# not used
#	fgeno <- ff[basename(ff) == "29 HRWYT_Genotypes_Data.xls"]
#	fgrn <- ff[basename(ff) == "29 HRWYT_GrnYld.xls"]
#	geno <- read.csv(fgeno, sep = "\t")
#	grn <- read.csv(fgrn, sep = "\t")

	fenv <- ff[grep("EnvData.xls", basename(ff))]
	floc <- ff[grep("Loc_data.xls", basename(ff))]
	fraw <- ff[grep("RawData.xls", basename(ff))]

	if (carobiner::is_excel(floc)) {
		loc <- carobiner::read.excel(floc)
		if (basename(fraw) %in% c("4TH HTWYT_RawData.xlsx", "5TH HTWYT_RawData.xlsx")) {
			suppressWarnings(raw <- carobiner::read.excel(fraw, na=c("", "-")))
		} else {
			raw <- carobiner::read.excel(fraw, na=c("", "-", "."))
		}
		env <- carobiner::read.excel(fenv)
		colnames(raw) <- gsub(" ", ".", colnames(raw))
		colnames(env) <- gsub(" ", ".", colnames(env))
	} else {
		loc <- read.csv(floc, sep = "\t", fileEncoding = "latin1")
		raw <- read.csv(fraw, sep = "\t", fileEncoding = "latin1")	
		env <- read.csv(fenv, sep = "\t", fileEncoding = "latin1")
	}

	# 'Cid', 'Sid', 	
	vars <- c( 'Trait.name', 'Value', 'Trial.name', 'Loc_no', 'Country', 'Loc_desc', 'Cycle', 'Gen_name', 'Rep', 'Plot')
	raw <- raw[, vars]
	
	raw$Value <- trimws(raw$Value)
	raw$Value[raw$Value %in% c("-", ".")] <- ""
	raw$Value[raw$Value == ""] <- NA
	raw$Value[raw$Value %in% c("NORMAL", "SPARSE", "DENSE")] <- NA
	raw$Value[raw$Value %in% c("2*", "1/2*", "7+9", "7+8", "7+8/17+18", "17+18", "13+16", "5+10", "2+12", "5+10/2+12")] <- NA
	raw$Value[raw$Value %in% c("+", "+,-", "1B", "1B/1R", "1B", "1B", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B", "MIX", "1B/1R", "1B/1R", "1B", "1B", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B/1R", "1B", "1B/1R", "1B", "1B/1R", "1B/1R", "MIX", "1B/1R", "1B", "1B/1R", "1B/1R", "1B")] <- NA 
	raw$Value[raw$Value %in% c("1B/1B", "7+9/17+18", "2.1+12", "2.1+10")] <- NA   

	raw$Value <- as.numeric(raw$Value)
	i <- colSums(!is.na(raw))
	raw <- raw[, i>0]
	
	raw <- aggregate(Value ~ ., data=raw, mean, na.rm=TRUE)

	i <- match(c("Trait.name", "Trial.name"), names(raw))
	
	vars <- vars[vars %in% colnames(raw)]
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
	r$Country <- NULL
	r$location <- gsub(" - ", ", ", r$Loc_desc)

# Process in carob format
	r$planting_date <- as.Date(r$SOWING_DATE, "%b %d %Y")

	if (!is.null(r$HARVEST_STARTING_DATE)) {
		r$harvest_date <- as.Date(r$HARVEST_STARTING_DATE, "%b %d %Y")
	} else {
		r$harvest_date <- as.Date(r$HARVEST_FINISHING_DATE, "%b %d %Y")
	}
	
	r$heading <- r$DAYS_TO_HEADING
	if (!is.null(r$heading)) {
		season <- as.numeric(r$harvest_date - r$planting_date)	
		h <- which((r$heading > 150) & (r$heading > (season + 15)))
		r$heading[h] <- NA
	}
	
# other variables
	if (is.null(r$IRRIGATED))  {
		r$IRRIGATED <- as.logical(NA)	
	} else {
		r$IRRIGATED <- r$IRRIGATED != "NO"
	}
	
	r$Rep <- as.integer(r$Rep)
	r$planting_date <- as.character(r$planting_date)
	i <- is.na(r$planting_date)
	r$planting_date[i] <- as.character(r$Cycle[i])
	r$harvest_date <- as.character(r$harvest_date)

	r$on_farm <- FALSE
	r$is_survey <- FALSE
	if (is.null(r$SPACE_BTN_ROWS_SOWN)) {
		r$row_spacing <- as.numeric(NA)	
	} else {
		r$row_spacing <- as.numeric(r$SPACE_BTN_ROWS_SOWN)
	}
	r$crop <- "wheat"
	r$variety_code <- r$Gen_name

	m <- matrix(byrow=TRUE, ncol=2, c(
		"VICIA FABA", "faba bean", 
		"FABABEEN", "faba bean", 
		"FABAE BEAN", "faba bean", 
		"FABA VULGARIS", "faba bean", 
		"G'NUT", "groundnut",
		"PEARL MILLET", "pearl millet",
		"SEAOME", "sesame",
		"SUGAR BEATTREFOIL", "sugar beet",
	
		"CHICK-PEA", "chickpea",
		"COW PEA", "cowpea",
		"MOUNG BEAN", "mung bean",
		"BLACK-GRAM", "black gram",
		
		"INTERCROPING", NA,
		"CLASLERBEEN", "guar",  # cluster bean
		"P. / LEGUME", "legume",
		"LEGUME: FABA BEAN", "faba bean",
		"LEGUME, FABA BEAN", "faba bean",
		"AJOS", "garlic",
		"AJO", "garlic",
		"ALFA ALFA", "LUCERNE",
		"ALFALFA", "LUCERNE",
		"GORSYHIUM HIRSULU", "cotton",
		"G-HIRTUSUM", "cotton",
		"ALGODON", "cotton",		
		"ALGODONERO", "cotton",			  
		"ALGODAON", "cotton",
		"ALUBIAS", "common bean",
		"ALLIUM CEPA", "onion",
		"AMAN RICE", "rice",
		"AMAN RCIE", "rice",
		"AMARANTO", "amaranth",
		"AMARANTHUS", "amaranth",
		"ARVEJAS PARA CONGELADO", "pea",
		"ARVEJA", "pea",
		"AVENA+VICI", "oats; vetch", 
		"AVENA+VICIA", "oats; vetch", 
		"AVENA + VICIA", "oats; vetch", 
		"AVENA-VICIA", "oats; vetch",
		"AVENA VICIA", "oats; vetch", 
		"AVENA / VICIA", "oats; vetch", 
		"AVENA", "oats",
		"AVENA SATIVA", "oats",
		"BAIRA", "pearl millet",
		"BAJRA", "pearl millet",
		"BAJWA", "pearl millet",
		"BEANS&POTATOES", "common bean; potato",
		"BERSEEM (FODDER)", "berseem clover",
		"TRIFOLIUM ALEXANDRINUM", "berseem clover",
		"BLEND", NA,
		"BRASICA NAPUS", "rapeseed",
		"CARTAMO", "safflower",
		"CANOLA", "rapeseed",
		"CEBADA", "barley",
		"CEBOLLA", "onion",
		"CERAL", "cereal",
		"CEREALS", "cereal",
		"CEREAL (MAGI)", "cereal",  #ragi?
		"CEREAL (MAYIL)", "cereal", #rice?
		"CEAREALS", "cereal",
		"CEREAL (RICE)", "rice",
		"CEREAL  (RICE)", "rice",
		"CICER ARIETINUM", "chickpea",
		"CICER  ARIETINUM", "chickpea",
		"CHECK PEA", "chickpea",
		"CHECKPEA", "chickpea",
		"CHICK PEN", "chickpea",
		"CHICK PEA", "chickpea",
		"CHIKPEA", "chickpea",
		"CHOCHO", "tarwi",
		"CHAINCHA", "common bean", #chaucha
		"CLASTERBEAN", "guar", 
		"CLUSTERBEAN", "guar", 
		"CLASVERBEEN", "guar", 
		"COE PEA", "cowpea", 
		"COJENUS", "pigeon pea", 
		"COJANUS", "pigeon pea", 
		"COJENUS COJON", "pigeon pea", 
		"CAJONES", "pigeon pea", 		
		"COMPOSITEA", NA, 
		"COMPOSITAE", NA, 
		"CORN", "maize",
		"MAIZE CERIAL", "maize",
		"COSTAN", "cotton",
		"SUGAR  BEET-MAIZE", "sugar beet; maize",	
		"COTTON/VEGETABLE", "cotton; vegetables",
		"COTLON", "cotton",
		"COTTAN", "cotton",
		"COTTOON", "cotton",
		"COWPEAS", "cowpea",
		"CROTOTERIA (ABONO VERDE)", "crotalaria", 		
		"CROTOTERIA", "crotalaria",
		"DEHNEHA", NA,
		"DURUM WHEAT", "durum wheat",
		"BREAD WHEAT", "wheat",
		"TRIGO HARI", "wheat",
		"WEED FREE FALLOW", "no crop",
		"FELLOW", "no crop",		
		"FALLOW", "no crop",
		"WEEDY FALLOW", "no crop",
		"FALLOWED", "no crop",
		"FABABEAN", "faba bean",
		"FABA BEEN", "faba bean",
		"FABA VULGARIS", "faba bean",
		"FABACEAE", "faba bean",
		"VICIA FABA", "faba bean",
		"FEET FOLLOWED BY MU", NA,
		"FIELD PEAS", "pea",
		"FIELD BEANS", "lablab",
		"FODDER", "forage legume",
		"FOOD LEGUMES", "legume",
		"FOOD LEGUME", "legume",		
		"LEGUMS", "legume",
		"FRIJOL", "common bean",
		"GIRASOL", "sunflower",
		"G. HIRSUTUM", "cotton",
		"GLYCIN MAX", "soybean",
		"GIYCIN MAX", "soybean",
		"GRASSES", "forage legume",
		"GREEN GRAME","mung bean",
		"GREEN MANUITY", "green manure",
		"GUISANTE", "pea",
		"HABAS", "faba bean",
		"MARICOT BEAN", "common bean",
		"HARICOT BEAN", "common bean",
		"IRISH POTATO", "potato",
		"LABLAB (LEGUME)", "lablab",
		"LAB.LAB", "lablab",
		"LAB-LAB", "lablab",
		"LABLAB PURPUREUS (COVER CROP)", "lablab",
		"LABLAB PURPUREUS", "lablab",
		"LAGUME CROP(SOYABEAN)", "soybean",
		"LEAVES VEGETABLE FOLLOWED BY MAIZE", "vegetables; maize",
		"LECHUGA", "lettuce",
		"LEGUMES", "legume", 
		"LEGUME", "legume",
		"LEGUMINOUS", "legume",
		"LEGUMINOSA", "legume",
		"LEGUMINOSAS", "legume",
		"LEGUMINOSAE", "legume",
		"LENTILS", "lentil",
		"LINSEED", "flax",
		"LOLIUM", "rye grass",
		"LUPINS", "white lupin",
		"LUPINUS ALBUS", "white lupin",
		"LUPINO", "white lupin", #?
		"GRAMINEAE", "maize; rice",
		"AREEN MANURE", "green manure", 
		"GREEN MANUDE", "green manure", 
		"GREEN MANEME", "green manure", 
		"GREE MANURE", "green manure", 
		"GREEN MANURING", "green manure", 
		"GREEM MANURE", "green manure", 
		"GREEN MANWE", "green manure", 
		"GREEN MANWERE", "green manure",
		"GREEN MAMURE", "green manure", 		
		"GREEN MAMUDE", "green manure", 
		"GREEN MONMING", "green manure", 
		"GREEN GRAIN", "mung bean",
		"GRUNDNUT", "groundnut",
		"GROUND NUT", "groundnut",
		"HELIANTHUS ANNUUS", "sunflower",
		"HELIANTHUS ANNUS", "sunflower",
		"MAIN SEASON", NA, 
		"MAIZE", "maize", 
		"MAIZ/SOJA", "maize; soybean",
		"MAIZ", "maize",
		"MAIZ FODDER", "maize",
		"MAIZE (FODDER)", "maize",
		"MAÍZ", "maize",
		"MAIZ, BAJO", "maize",
		"MAIZE,MUNG BEAN", "maize; mung bean",
		"MAIZE-WHEAT", "maize; wheat",
		"MALINBEAN", NA,  # mung bean??
		"MEDCIN PLANTS", NA,
		"MILICIMA ATERRINA", NA,
		"MONSOON", "rice",
		"MONSSON RICE", "rice",
		"MUNJE WHEAT", "wheat",		
		"MUNG-PULSES", "mung bean",
		"MUNG- PULSE", "mung bean",
		"MAIN BAEN", "mung bean", 
		"MAIN BEAN", "mung bean", 
		"MUNG BEANS", "mung bean", 
		"MUNG BAEN", "mung bean", 
		"MUNG BEEN", "mung bean", 
		"MUNG", "mung bean",
		"MUSTAFARA", NA,
		"MONG BEAN", "mung bean",
		"MOONG BEAN", "mung bean",		
		"MONG (V. RADIATA)", "mung bean",
		"MONG", "mung bean",
		"MOONG", "mung bean",
		"MOONGBEAN", "mung bean",
		"MUNGHEAN", "mung bean",
		"MUNGBEAM", "mung bean",
		"MUNGBEAN", "mung bean",
		"MANG BEAN", "mung bean",
		"MANG BEEB", "mung bean",
		"MANGBEAN", "mung bean",
		"MANN BEAN", "mung bean",
		"MANDY SETERIA", "foxtail millet",
		"MIAZE", "maize",
		"O. SATIVA", 'rice',
		"OAT", "oats", 
		"OILSEED", "rapeseed", 
		"OLISEED", "rapeseed", 
		"OIL SEED", "rapeseed",
		"OIL CROPS", "rapeseed", 
		"OIL CROP", "rapeseed", 
		"ORYZA SATIVA L.", "rice", 
		"ORYZA SATIVA", "rice",
		"PADDY", "rice",
		"PADDY (RICE)", "rice",
		"PAADY", "rice",
		"PALSAN", "jute",
		"PAPA", "potato", 
		"PAPPER CROP", "pepper", 
		"PATATO", "potato",
		"POTATOES", "potato",
		"POTATO AND RAPE SEED", "potato; rapeseed",
		"PEAS", "pea",
		"PERCO", NA,
		"PENNISETUM TYPHOIDES", "pearl millet",
		"PEAEL MILLELE", "pearl millet",
		"PEARL MILL", "pearl millet",
		"PEARLI MILLER", "pearl millet",
		"PHACELIA TANACETIFOLIA", 'phacelia',
		"PHASEOLUS VULGARIS", "common bean",
		"PHASCOWS", "common bean", # maybe
		"PISUM SATIVUM", "pea",
		"PNLEES", NA,
		"POROTO", "common bean", 
		"PULSEC", "pulse", 
		"PEA FOR GRAIN", "pea", 
		"PULSES", "pulse", 
		"PULSE", "pulse", 
		"PULSE CROP", "pulse", 		
		"PURPERUREUS", "lablab",
		"QUINUA", "quinoa", 
		"RAPE", "rapeseed",
		"RAPE SEED", "rapeseed",
		"RAPE SEAT", "rapeseed",
		"RAPHANUS SPP", "radish",
		"RAPHANUS SPP.", "radish", 
		"RAPHA NUS SPP", "radish",
		"RAPHANUS SATIRUS L", "radish",
		"UPLAND RIC", "rice",
		"LOWLAND RICE", "rice",
		"RIC", "rice",
		"RICE (CEREALS)", "rice",
		"RICE-CEREAL", "rice",
		"RICE, CEREAL", "rice",
		"RICE SEED PRODUCTION", 'rice',
		"RICE/PADDY", "rice",
		"RICE/GRAIMINEAE", "rice",
		"RICE WHEAT", "rice; wheat",		
		"ROOT", "root crop", 
		"SAF FLOWER", "safflower", 
		"DHAINCHA", "sesbania",
		"CESBANIA", "sesbania",
		"SESBANIA I", "sesbania",
		"SESBANIA INDICA", "sesbania",
		"SESBANIA SP.", "sesbania",
		"SESBANIA\\", "sesbania",
		"SESBANIA ACULEATA", "sesbania",
		"SEED PRODUCTION", NA,
		"SINAPIS", "mustard",
		"SIGA", NA,
		"SOGO", "sorghum",
		"SORGO", "sorghum", 
		"SORGO FORR", "sorghum", 
		"SORGO FORRAGE", "sorghum", 
		"FORAGE SORGHUM", "sorghum", 
		"SAYABEAN", "soybean", 
		"SOJA BEANS", "soybean", 
		"SOYBEAN JS-335", "soybean", 
		"SOYBEAN-JS 335", "soybean", 
		"SOY BEAN", "soybean", 
		"SOYBEANS", "soybean", 
		"SOYBEAN", "soybean",
		"SOYABEAN", "soybean",
		"SOYABEEN", "soybean",
		"SOYBAEAN", "soybean", 
		"SOYA BEANS", "soybean", 
		"SOYA BEAN", "soybean",
		"SOYASEAN", "soybean",
		"SOY BEAN", "soybean",
		"SOYBEA", "soybean",
		"SUGAR CANE", "sugarcane",
		"SUGAR CAME", "sugarcane",
		"SUGAR BEET-MAIZE", "sugar beet; maize",
		"SUGAR BEET", "sugar beet",
		"SUGERBEET", "sugar beet",
		"SUGARR BEETS", "sugar beet",
		"SIJA", "soybean",
		"SOJA", "soybean",
		"SOJA CICLO CORTO", "soybean",
		"SOYA", "soybean",
		"SOYBEN", "soybean",
		"SOYA-OIL SEED", "soybean",
		"SOYA- OIL SEED", "soybean",
		"SUNFLOVER", "sunflower",
		"SUNFLOWER (HELIANTHUS ANNUUS L)", "sunflower",
		"SUNFLOWER; COMPOSITAE", "sunflower",
		"SUNHANP", "sunn hemp",
		"SAMHAMP", "sunn hemp",
		"SANNHEMP", "sunn hemp",
		"SANPHEMP", "sunn hemp",
		"SANDAEMP", "sunn hemp",
		"SUNNHAMP", "sunn hemp",
		"SUNHEMO", "sunn hemp",
		"SRUNHEMP", "sunn hemp",
		"SUNHEMP (FLAX)", "sunn hemp",
		"SUNHIMP (FLAX)", "sunn hemp",
		"SUN-HAMP", "sunn hemp", 
		"SUNHAMP", "sunn hemp",
		"SUNHAMN", "sunn hemp", 
		"SUN-HAMP", "sunn hemp", 
		"SANHAMP", "sunn hemp", 
		"SUN HEMP", "sunn hemp", 
		"SUNNHEMP", "sunn hemp", 
		"SUNHIMP", "sunn hemp", 
		"SUNHAMP", "sunn hemp",
		"SUNHEMP", "sunn hemp",
		"SUNHEOP", "sunn hemp",
		"SUMHEMP", "sunn hemp",
		"SUNHEAD", "sunn hemp",
		"SUN HEUF", "sunn hemp",
		"SUNYHEMP", "sunn hemp",
		"SYNHEMP", "sunn hemp",
		"SYNHEME", "sunn hemp",
		"SYMHEMP", "sunn hemp",
		"SUPER BEAT", "sugar beet",
		"SWEET POTATOS", "sweetpotato",
		"TRAMANRICE", "rice",
		"TREBOL BLANCO", "white clover", 
		"TREBOL ROJO", "red clover",
		"TREFOIL CLOVER", "clover",		
		"TREBOL-SOJA", "clover; soybean",
		"TRIGO", "wheat", 
		"TRIGO HARINERO", "wheat",
		"TRIFOLIUM ALEXANDRIUM", "berseem clover", 
		"TRIFOLIUM ALEXANDIUM", "berseem clover", 
		"TRIFOLIUM REPENS", "white clover", 
		"TRYPHOLIUM SUBTERRANEUM", "subterranean clover",
		"TRITICALE", "triticale",
		"TRITICUM", "wheat",
		"T. AESTIVUM", "wheat",
		"UPLAND RICE", "rice",
		"URDBEAN (BLACK GRAM)", "black gram",
		"URDBEAN", "black gram",
		"URDBEAN- PULSES", "black gram",
		"URDBEAN-PULSES", "black gram",
		"VICIA LABA", "faba bean",
		"VECIA FABA", "faba bean", 
		"VIGNA RADIATA", "mung bean", 
		"VIGRA RADIATA", "mung bean", 
		"V. RADIATA MOONG", "mung bean", 
		"VES", "vetch",
		"VEZA", "vetch",
		"VEGETABLE", "vegetables",
		"VEGATEABLES", "vegetables",
		"WATER MELON", "watermelon",
		"WEEDS", "no crop",
		"ZEA MAYS", "maize",
		"CEREAL", "cereal", 		
		"BEANS", "common bean",
		"BEAN", "common bean",
		"CROP", NA)
	)

	prcrop <- r$USE_OF_FIELD_SPECIFY_CROP
	if (is.null(prcrop)) {
		r$previous_crop <- as.character(NA)	
	} else {
		prcrop <- trimws(gsub("\\s+", " ", prcrop))
		h <- cbind(1:nrow(r), match(prcrop, m[,1])) |> na.omit()
		prcrop[h[,1]] <- m[h[,2], 2]
		r$previous_crop <- tolower(prcrop)
	}
	# Convert yield in ton/ha to kg/ha
	r$yield <- as.numeric(r$GRAIN_YIELD) * 1000 
	if (is.null(r$`1000_GRAIN_WEIGHT`)) {
		r$grain_weight <- as.numeric(NA)	
	} else {
		r$grain_weight <- as.numeric(r$`1000_GRAIN_WEIGHT`)
	}
	
	# Extract columns with NPK fertilizers
	fertfun <- function(x, v) {
		i <- grep(v, colnames(x))
		#if (length(i) == 0) return(cbind(NA, NA))
		rn <- x[,i,drop=FALSE]
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
	r$N_splits <- as.integer(x[,2])

	r$P_fertilizer <- fertfun(r, "FERTILIZER_%P")[,1]
	r$K_fertilizer <- fertfun(r, "FERTILIZER_%K")[,1]
	
	r$soil_type <- r$SOIL_CLASIFICATION
	r$soil_SOC <- if (is.null(r$SOIL_PERCENT_ORGANIC_MATTER)) {NULL} else {as.numeric(r$SOIL_PERCENT_ORGANIC_MATTER) * 0.58}
	
	r$SOIL_PH <- if(is.null(r$SOIL_PH_ACTUAL_VALUE)) {NULL} else {as.numeric(r$SOIL_PH_ACTUAL_VALUE)}
	
	r$plant_height <- r$PLANT_HEIGHT 
	r$sterility <- r$STERILITY_INDEX
		
	r$bird_damage <- r$BIRD_DAMAGE_PER_PLOT
	r$blast_intensity <- r$`Blast intensity`
	r$blast_severity  <- r$`Blast severity`
					
	r$country <- ifelse(r$country== "Dem Rep of Congo", "Democratic Republic of the Congo", r$country)
	r$country <- ifelse(r$country== "U A Emirates", "United Arab Emirates", r$country)
	r$country <- ifelse(r$country== "Swaziland", "Eswatini", r$country)
	
	# Exclude Null countries
	r <- r[r$country!="Null",]

	# more could be done. But we should not keep ALL CAPS
	r$location <- carobiner::fix_name(r$location, "title")
	r$location <- gsub(" Ltd$", " LTD", r$location)
	
	i <- which(r$location == "Aurangabad, Gangapur, Ajeet Seeds LTD")
	r$longitude[i] <- 75.0822
	r$latitude[i] <- 19.6927
	
	i <- which(r$location == "Black Sea  A.R.I.")
	r$longitude[i] <- 36.4889
	r$latitude[i] <- 41.2335

	i <- which(r$location == "Sanliurfa-Akcakale,  A.R.I")
	r$latitude[i] <- 36.72

	i <- r$location == "Bagh-E-Zakherah Taloqan, Aria"
	r$longitude[i] <- 69.48988

	i <- r$location == "Bardc, Quetta"
	r$latitude[i] <- 30.193

	i <- which(r$location == "Exp.Farm Kasapa, U.Lubumbashi")
	r$location[i] <- "Exp. Farm Kasapa, University of Lubumbashi"
	r$longitude[i] <- 27.4144
	r$latitude[i] <- -11.5657

	i <- which(r$location == "Lyamungo")
	r$longitude[i] <- 37.249
	r$latitude[i] <- -3.248

	i <- grep("Karaj", r$location)
	r$latitude[i] <- 35.802

	r$yield_part <- "grain"
	# records without yield are not very useful
	#r <- r[!is.na(r$yield), ]	
		
	# set all colnames to lowercase and subset	
	colnames(r) <- tolower(colnames(r))

	# Subset for relevant columns
	cvars <- c("country", "location", "trial_id", "latitude", "longitude", "planting_date", "harvest_date", "on_farm", "is_survey", "rep", "crop", "variety_code", "previous_crop", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "soil_type", "soil_om", "soil_ph",  "irrigated", "row_spacing", "yield_part", "yield", "grain_weight", "heading", "height","powdery_mildew", "stem_rust", "leaf_rust", "sterility_index", "fusarium_scab_spike", "helminthosporium_sativum_leaf", "septoria_tritici_blotch", "septoria_species", "blast_severity", "blast_intensity", "stripe_rust_on_leaf", "h_tritici_repentis", "feconcentration", "znconcentration")

	# they may not be all available
	r <- r[, cvars[cvars %in% names(r)]]
	r <- r[!is.na(r$yield), ]

	#fix colnames with uppercase
	r <- carobiner::change_names(r, 
		c("soil_ph", "h_tritici_repentis", "feconcentration", "znconcentration","n_fertilizer", "n_splits", "p_fertilizer", "k_fertilizer", "soil_om"),
		c("soil_pH", "H_tritici_repentis", "grain_Fe", "grain_Zn",  "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "soil_OM"), must_have=FALSE)

	r

}


