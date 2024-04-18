
# to do: extract more variables of interest. 
# not yet included from raw$Trait.name
# AGRONOMIC_SCORE, SELECTED_CHECK_MARK, LODGING_PERCENT_HARVESTED_AREA, TEST_WEIGHT, YRWarriorRace, SPIKE_LENGTH, GERMINATION_%, CHLOROPHYLL, GRAIN_PROTEIN, Normalized Difference Vegetation Index, GRAIN APPEARANCE SCORE, PHENOL REACTION SCORE, Canopy Temperature, ABOVE_GROUND_BIOMASS, TILLERS BY METER, GRAINS/SPIKE, TILLERS BY M2, SPIKES_M2, GLUTEN_CONTENT, GRAIN_MOISTURE, SEDIMENTATION_INDEX

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
	colnames(r) <- tolower(colnames(r))
	
	d <- data.frame(
		crop = "wheat",
		yield_part = "grain",
		on_farm = FALSE,
		is_survey = FALSE,
		trial_id = r$trial.name,
		country = carobiner::fix_name(r$country, "title"),
		location = gsub(" - ", ", ", r$loc_desc),
		planting_date = as.Date(r$sowing_date, "%b %d %Y"),
		rep = as.integer(r$rep),
		yield = as.numeric(r$grain_yield) * 1000, 
		variety_code = r$gen_name,
		longitude = r$longitude,
		latitude = r$latitude
	)
	
	if (!is.null(r$harvest_starting_date)) {
		d$harvest_date <- as.Date(r$harvest_starting_date, "%b %d %Y")
	} else {
		d$harvest_date <- as.Date(r$harvest_finishing_date, "%b %d %Y")
	}
	
	d$heading_days <- r$days_to_heading
	if (!is.null(d$heading_days)) {
		season <- as.numeric(d$harvest_date - d$planting_date)	
		h <- which((d$heading_days > 150) & (d$heading_days > (season + 15)))
		d$heading_days[h] <- NA
	}
	d$planting_date <- as.character(d$planting_date)
	d$harvest_date <- as.character(d$harvest_date)
	i <- is.na(d$planting_date)
	d$planting_date[i] <- as.character(r$cycle[i])
	
	d$maturity_days <- r$days_to_maturity
		
	if (is.null(r$irrigated))  {
		d$irrigated <- as.logical(NA)	
	} else {
		# should look for yes?
		d$irrigated <- tolower(r$irrigated) == "yes"
	}

	if (is.null(r$space_btn_rows_sown)) {
		d$row_spacing <- as.numeric(NA)	
	} else {
		d$row_spacing <- as.numeric(r$space_btn_rows_sown)
	}


	m <- matrix(byrow=TRUE, ncol=2, c(
		"b napus", "rapeseed",
		"maiz/papa", "maize;potato",
		"rough pro", NA, 
		"zallon", NA,
		"granpea", "grass pea",
		"grasspea", "grass pea",
		"berseen", "berseem clover",
		"egyption", "berseem clover",
		"vicia faba", "faba bean", 
		"faba vulgaris, vicia faba", "faba bean", 
		"fababeen", "faba bean", 
		"fabae bean", "faba bean", 
		"faba vulgaris", "faba bean", 
		"g'nut", "groundnut",
		"pearl millet", "pearl millet",
		"pearl-mill", "pearl millet",
		"seaome", "sesame",
		"sugar beattrefoil", "sugar beet",	
		"chick-pea", "chickpea",
		"cow pea", "cowpea",
		"moung bean", "mung bean",
		"black-gram", "black gram",
		"intercroping", NA,
		"claslerbeen", "guar",  # cluster bean
		"p. / legume", "legume",
		"legums.", "legume",
		"legume: faba bean", "faba bean",
		"legume, faba bean", "faba bean",
		"ajos", "garlic",
		"ajo", "garlic",
		"alfa alfa", "lucerne",
		"alfalfa", "lucerne",
		"gorsyhium hirsulu", "cotton",
		"g-hirtusum", "cotton",
		"algodon", "cotton",		
		"algodonero", "cotton",			  
		"algodaon", "cotton",
		"alubias", "common bean",
		"allium cepa", "onion",
		"aman rice", "rice",
		"aman rcie", "rice",
		"amaranto", "amaranth",
		"amaranthus", "amaranth",
		"arvejas para congelado", "pea",
		"arveja", "pea",
		"avena+vici", "oats; vetch", 
		"avena+vicia", "oats; vetch", 
		"avena + vicia", "oats; vetch", 
		"avena-vicia", "oats; vetch",
		"avena vicia", "oats; vetch", 
		"avena / vicia", "oats; vetch", 
		"avena", "oats",
		"avena sativa", "oats",
		"baira", "pearl millet",
		"bajra", "pearl millet",
		"bajwa", "pearl millet",
		"beans&potatoes", "common bean; potato",
		"berseem (fodder)", "berseem clover",
		"trifolium alexandrinum", "berseem clover",
		"blend", NA,
		"brasica napus", "rapeseed",
		"cartamo", "safflower",
		"canola", "rapeseed",
		"cebada", "barley",
		"cebolla", "onion",
		"ceral", "cereal",
		"cereals", "cereal",
		"cereal (magi)", "cereal",  #ragi?
		"cereal (mayil)", "cereal", #rice?
		"ceareals", "cereal",
		"cereales", "cereal",
		"cereal (rice)", "rice",
		"cereal  (rice)", "rice",
		"cicer arietinum", "chickpea",
		"cicer  arietinum", "chickpea",
		"check pea", "chickpea",
		"checkpea", "chickpea",
		"chick pen", "chickpea",
		"chick pea", "chickpea",
		"chikpea", "chickpea",
		"chocho", "tarwi",
		"chaincha", "common bean", #chaucha
		"clasterbean", "guar", 
		"clusterbean", "guar", 
		"clasverbeen", "guar", 
		"coe pea", "cowpea", 
		"cojenus", "pigeon pea", 
		"cojanus", "pigeon pea", 
		"cojenus cojon", "pigeon pea", 
		"cajones", "pigeon pea", 		
		"compositea", NA, 
		"compositae", NA, 
		"corn", "maize",
		"maize cerial", "maize",
		"costan", "cotton",
		"sugar  beet-maize", "sugar beet; maize",	
		"cotton/vegetable", "cotton; vegetables",
		"cotlon", "cotton",
		"cottan", "cotton",
		"cottoon", "cotton",
		"cowpeas", "cowpea",
		"crototeria (abono verde)", "crotalaria", 		
		"crototeria", "crotalaria",
		"dehneha", NA,
		"durum wheat", "durum wheat",
		"bread wheat", "wheat",
		"trigo hari", "wheat",
		"weed free fallow", "no crop",
		"fellow", "no crop",		
		"fallow", "no crop",
		"weedy fallow", "no crop",
		"fallowed", "no crop",
		"fababean", "faba bean",
		"faba been", "faba bean",
		"faba vulgaris", "faba bean",
		"fabaceae", "faba bean",
		"vicia faba", "faba bean",
		"feet followed by mu", NA,
		"field peas", "pea",
		"peas barby blend", "pea",
		"field beans", "lablab",
		"fodder", "forage legume",
		"food legumes", "legume",
		"food legume", "legume",		
		"legums", "legume",
		"frijol", "common bean",
		"girasol", "sunflower",
		"g. hirsutum", "cotton",
		"glycin max", "soybean",
		"giycin max", "soybean",
		"grasses", "forage legume",
		"green grame","mung bean",
		"green manuity", "green manure",
		"guisante", "pea",
		"habas", "faba bean",
		"maricot bean", "common bean",
		"haricot bean", "common bean",
		"irish potato", "potato",
		"lablab (legume)", "lablab",
		"lab.lab", "lablab",
		"lab-lab", "lablab",
		"lablab purpureus (cover crop)", "lablab",
		"lablab purpureus", "lablab",
		"lagume crop(soyabean)", "soybean",
		"leaves vegetable followed by maize", "vegetables; maize",
		"lechuga", "lettuce",
		"legumes", "legume", 
		"legume", "legume",
		"leguminous", "legume",
		"leguminosa", "legume",
		"leguminosas", "legume",
		"leguminosae", "legume",
		"lentils", "lentil",
		"linseed", "flax",
		"lolium", "rye grass",
		"lupins", "white lupin",
		"lupinus albus", "white lupin",
		"lupino", "white lupin", #?
		"gramineae", "maize; rice",
		"areen manure", "green manure", 
		"green manude", "green manure", 
		"green maneme", "green manure", 
		"gree manure", "green manure", 
		"green manuring", "green manure", 
		"greem manure", "green manure", 
		"green manwe", "green manure", 
		"green manwere", "green manure",
		"green mamure", "green manure", 		
		"green mamude", "green manure", 
		"green monming", "green manure", 
		"green grain", "mung bean",
		"grundnut", "groundnut",
		"ground nut", "groundnut",
		"helianthus annuus", "sunflower",
		"helianthus annus", "sunflower",
		"main season", NA, 
		"maize", "maize", 
		"maiz/soja", "maize; soybean",
		"maiz", "maize",
		"maiz fodder", "maize",
		"maize (fodder)", "maize",
		"maíz", "maize",
		"maiz, bajo", "maize",
		"maize,mung bean", "maize; mung bean",
		"maize-wheat", "maize; wheat",
		"malinbean", NA,  # mung bean??
		"medcin plants", NA,
		"milicima aterrina", NA,
		"monsoon", "rice",
		"monsson rice", "rice",
		"munje wheat", "wheat",		
		"mung-pulses", "mung bean",
		"mung- pulse", "mung bean",
		"main baen", "mung bean", 
		"main bean", "mung bean", 
		"mung beans", "mung bean", 
		"mung baen", "mung bean", 
		"mung been", "mung bean", 
		"mung", "mung bean",
		"mustafara", NA,
		"mong bean", "mung bean",
		"moong bean", "mung bean",		
		"mong (v. radiata)", "mung bean",
		"mong", "mung bean",
		"moong", "mung bean",
		"moongbean", "mung bean",
		"munghean", "mung bean",
		"mungbeam", "mung bean",
		"mungbean", "mung bean",
		"mang bean", "mung bean",
		"mang beeb", "mung bean",
		"mangbean", "mung bean",
		"mann bean", "mung bean",
		"mandy seteria", "foxtail millet",
		"miaze", "maize",
		"o. sativa", 'rice',
		"oat", "oats", 
		"oilseed", "rapeseed", 
		"oliseed", "rapeseed", 
		"oil seed", "rapeseed",
		"oil crops", "rapeseed", 
		"oil crop", "rapeseed", 
		"oryza sativa l.", "rice", 
		"oryza sativa", "rice",
		"paddy", "rice",
		"paddy (rice)", "rice",
		"paady", "rice",
		"palsan", "jute",
		"papa", "potato", 
		"papper crop", "pepper", 
		"patato", "potato",
		"potatoes", "potato",
		"potato and rape seed", "potato; rapeseed",
		"peas", "pea",
		"perco", NA,
		"pennisetum typhoides", "pearl millet",
		"peael millele", "pearl millet",
		"pearl mill", "pearl millet",
		"pearli miller", "pearl millet",
		"phacelia tanacetifolia", 'phacelia',
		"phaseolus vulgaris", "common bean",
		"phascows", "common bean", # maybe
		"pisum sativum", "pea",
		"pnlees", NA,
		"poroto", "common bean", 
		"pulsec", "pulse", 
		"pea for grain", "pea", 
		"pulses", "pulse", 
		"pulse", "pulse", 
		"pulse crop", "pulse", 		
		"purperureus", "lablab",
		"quinua", "quinoa", 
		"rape", "rapeseed",
		"rape seed", "rapeseed",
		"rape seat", "rapeseed",
		"raphanus spp", "radish",
		"raphanus spp.", "radish", 
		"rapha nus spp", "radish",
		"raphanus satirus l", "radish",
		"upland ric", "rice",
		"lowland rice", "rice",
		"ric", "rice",
		"rice (cereals)", "rice",
		"rice-cereal", "rice",
		"rice, cereal", "rice",
		"rice seed production", 'rice',
		"rice/paddy", "rice",
		"rice/graimineae", "rice",
		"rice wheat", "rice; wheat",		
		"root", "root crop", 
		"saf flower", "safflower", 
		"dhaincha", "sesbania",
		"cesbania", "sesbania",
		"sesbania i", "sesbania",
		"sesbania indica", "sesbania",
		"sesbania sp.", "sesbania",
		"sesbaniasp", "sesbania",
		"sesbania\\", "sesbania",
		"sesbania aculeata", "sesbania",
		"sesbania as gm", "sesbania",
		"seed production", NA,
		"sinapis", "mustard",
		"siga", NA,
		"sogo", "sorghum",
		"sorgo", "sorghum", 
		"sorgo forr", "sorghum", 
		"sorgo forrage", "sorghum", 
		"forage sorghum", "sorghum", 
		"sayabean", "soybean", 
		"soja beans", "soybean", 
		"soybean js-335", "soybean", 
		"soybean-js 335", "soybean", 
		"soy bean", "soybean", 
		"soybeans", "soybean", 
		"soybean", "soybean",
		"soyabean", "soybean",
		"soyabeen", "soybean",
		"soybaean", "soybean", 
		"soya beans", "soybean", 
		"soya bean", "soybean",
		"soyasean", "soybean",
		"soy bean", "soybean",
		"soybea", "soybean",
		"sugar cane", "sugarcane",
		"sugar came", "sugarcane",
		"sugar beet-maize", "sugar beet; maize",
		"sugar beet", "sugar beet",
		"sugerbeet", "sugar beet",
		"sugarr beets", "sugar beet",
		"sugar beat", "sugar beet",
		"sija", "soybean",
		"soja", "soybean",
		"soja ciclo corto", "soybean",
		"soya", "soybean",
		"soyben", "soybean",
		"soya-oil seed", "soybean",
		"soya- oil seed", "soybean",
		"sunflover", "sunflower",
		"sunflower (helianthus annuus l)", "sunflower",
		"sunflower; compositae", "sunflower",
		"sunhanp", "sunn hemp",
		"samhamp", "sunn hemp",
		"sannhemp", "sunn hemp",
		"sanphemp", "sunn hemp",
		"sandaemp", "sunn hemp",
		"sunnhamp", "sunn hemp",
		"sunhemo", "sunn hemp",
		"srunhemp", "sunn hemp",
		"sunhemp (flax)", "sunn hemp",
		"sunhimp (flax)", "sunn hemp",
		"sun-hamp", "sunn hemp", 
		"sunhamp", "sunn hemp",
		"sunhamn", "sunn hemp", 
		"sun-hamp", "sunn hemp", 
		"sanhamp", "sunn hemp", 
		"sun hemp", "sunn hemp", 
		"sunnhemp", "sunn hemp", 
		"sunhimp", "sunn hemp", 
		"sunhamp", "sunn hemp",
		"sunhemp", "sunn hemp",
		"sunheop", "sunn hemp",
		"sumhemp", "sunn hemp",
		"sunhead", "sunn hemp",
		"sun heuf", "sunn hemp",
		"sunyhemp", "sunn hemp",
		"synhemp", "sunn hemp",
		"synheme", "sunn hemp",
		"symhemp", "sunn hemp",
		"super beat", "sugar beet",
		"sweet potatos", "sweetpotato",
		"tramanrice", "rice",
		"trebol blanco", "white clover", 
		"trebol rojo", "red clover",
		"trefoil", "clover",		
		"trefoil clover", "clover",		
		"trebol-soja", "clover; soybean",
		"trigo", "wheat", 
		"trigo harinero", "wheat",
		"trifolium alexandrium", "berseem clover", 
		"trifolium alexandium", "berseem clover", 
		"trifolium repens", "white clover", 
		"trypholium subterraneum", "subterranean clover",
		"triticale", "triticale",
		"triticum", "wheat",
		"t. aestivum", "wheat",
		"upland rice", "rice",
		"urdbean (black gram)", "black gram",
		"urdbean", "black gram",
		"urdbean- pulses", "black gram",
		"urdbean-pulses", "black gram",
		"vicia laba", "faba bean",
		"vecia faba", "faba bean", 
		"vigna radiata", "mung bean", 
		"vigra radiata", "mung bean", 
		"v. radiata moong", "mung bean", 
		"ves", "vetch",
		"beza", "vetch",
		"veza", "vetch",
		"vegetable", "vegetables",
		"vegateables", "vegetables",
		"water melon", "watermelon",
		"weeds", "no crop",
		"zea mays", "maize",
		"cereal", "cereal", 		
		"beans", "common bean",
		"bean", "common bean",
		"crop", NA)
	)

	if (is.null(r$use_of_field_specify_crop)) {
		d$previous_crop <- as.character(NA)	
	} else {
		prcrop <- trimws(gsub("\\s+", " ", tolower(r$use_of_field_specify_crop)))
		h <- cbind(1:nrow(r), match(prcrop, m[,1])) |> na.omit()
		prcrop[h[,1]] <- m[h[,2], 2]
		d$previous_crop <- prcrop
	}
	if (is.null(r$`1000_grain_weight`)) {
		d$grain_weight <- as.numeric(NA)	
	} else {
		d$grain_weight <- as.numeric(r$`1000_grain_weight`)
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
	
	x <- fertfun(r, "fertilizer_%n")
	d$N_fertilizer <- x[,1]
	d$N_splits <- as.integer(x[,2])

	d$P_fertilizer <- fertfun(r, "fertilizer_%p")[,1]
	d$K_fertilizer <- fertfun(r, "fertilizer_%k")[,1]
	
	if (!is.null(r$soil_percent_organic_matter)) {
		d$soil_SOC <- as.numeric(r$soil_percent_organic_matter) * 0.58
	}
	if (!is.null(r$soil_ph_actual_value)) {
		d$soil_pH <- as.numeric(r$soil_ph_actual_value)
	}
					
	d$country[d$country== "Dem Rep of Congo"] <- "Democratic Republic of the Congo"
	d$country[d$country== "U A Emirates"] <- "United Arab Emirates"
	d$country[d$country== "Swaziland"] <- "Eswatini"

	# more could be done. But we should not keep ALL CAPS
	d$location <- carobiner::fix_name(d$location, "title")
	d$location <- gsub(" Ltd$", " LTD", d$location)
	
	i <- which(d$location == "Aurangabad, Gangapur, Ajeet Seeds LTD")
	d$longitude[i] <- 75.0822
	d$latitude[i] <- 19.6927
	
	i <- which(d$location == "Black Sea  A.R.I.")
	d$longitude[i] <- 36.4889
	d$latitude[i] <- 41.2335

	i <- which(d$location == "Sanliurfa-Akcakale,  A.R.I")
	d$latitude[i] <- 36.72

	i <- d$location == "Bagh-E-Zakherah Taloqan, Aria"
	d$longitude[i] <- 69.48988

	i <- d$location == "Bardc, Quetta"
	d$latitude[i] <- 30.193

	i <- which(d$location == "Exp.Farm Kasapa, U.Lubumbashi")
	d$location[i] <- "Exp. Farm Kasapa, University of Lubumbashi"
	d$longitude[i] <- 27.4144
	d$latitude[i] <- -11.5657

	i <- which(d$location == "Lyamungo")
	d$longitude[i] <- 37.249
	d$latitude[i] <- -3.248

	i <- grep("Karaj", d$location)
	d$latitude[i] <- 35.802

	tolow <- function(x) if (is.null(x)) NULL else tolower(x)

	d$plant_height <- r$plant_height
	d$sterility_index <- r$sterility_index		
	d$bird_damage <- tolow(r$bird_damage)
	d$hail_damage <- tolow(r$hail_damage)
	d$soil_type <- tolow(r$soil_clasification)
	if (!is.null(r$soil_aluminium_toxicity)) d$soil_aluminium_toxicity <- tolower(r$soil_aluminium_toxicity) == "yes"

	d$blast_intensity <- r$`blast intensity`
	d$blast_severity <- r$`blast severity`
	d$powdery_mildew <- r$powdery_mildew
	d$stem_rust <- r$stem_rust
	d$leaf_rust <- r$leaf_rust
	d$sterility_index <- r$sterility_index
	d$fusarium_scab_spike <- r$fusarium_scab_spike
	d$helminthosporium_sativum_leaf <- r$helminthosporium_sativum_leaf
	d$septoria_tritici_blotch <- r$septoria_tritici_blotch 
	d$septoria_species <- r$septoria_species 
	d$blast_severity <- r$blast_severity 
	d$blast_intensity <- r$blast_intensity
	d$stripe_rust_on_leaf <- r$stripe_rust_on_leaf 
	d$H_tritici_repentis <- r$h_tritici_repentis
	d$grain_Fe <- r$feconcentration
	d$grain_Zn <- r$znconcentration

	if (!is.null(r$herbicide)) d$herbicide_used <- tolower(r$herbicide) == "yes"
	if (!is.null(r$pesticide)) d$insecticide_used <- tolower(r$pesticide) == "yes"
	d$herbicide_damage <- tolow(r$herbicide_damage)
	#d$herbicide_product <- tolow(r$`herbicide_product(s)`)
	#if (!is.null(d$herbicide_product)) {
	#	d$herbicide_product <- gsub(" and ", ";", d$herbicide_product)
	#	d$herbicide_product <- gsub("  ", " ", d$herbicide_product)
	#}
	
	d <- d[d$country != "Null", ]
	d <- d[!is.na(d$yield), ]
	d
}


