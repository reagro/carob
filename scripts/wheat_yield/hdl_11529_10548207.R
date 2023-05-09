# R script for "carob"

## ISSUES
# ....
# specify path parameter

carob_script <- function(path) {

"Description:
	CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2007)"
	
	uri <- "hdl:11529/10548207"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_yield"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication = NA,
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2019, '15th High Rainfall Wheat Yield Trial', https://hdl.handle.net/11529/10548207, CIMMYT Research Data & Software Repository Network, V2",
	   data_institutions = "CIMMYT",
	   carob_contributor="Andrew Sila",
	   
	   ## something like randomized control...
	   experiment_type="On-station experiment",
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=TRUE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
	dset$license <- carobiner::get_license(js)


	env <- ff[basename(ff) == "15TH HRWYT_EnvData.xls"]
	geno <- ff[basename(ff) == "15TH HRWYT_Genotypes_Data.xls"]
	grn <- ff[basename(ff) == "15TH HRWYT_GrnYld.xls"]
	loc <- ff[basename(ff) == "15TH HRWYT_Loc_data.xls"]
	raw <- ff[basename(ff) == "15TH HRWYT_RawData.xls"]

## Read data referenced by the above pathnames

	env <- read.csv(env, sep = "\t")
	geno <- read.csv(geno, sep = "\t")
	grn <- read.csv(grn, sep = "\t")
	loc <- read.csv(loc, sep = "\t")
	raw <- read.csv(raw, sep = "\t")
	
## process file(s)
	proper <- function(x){paste0(toupper(substr(x, 1,1)), tolower(substr(x,2, nchar(x))))}

	raw$country <- proper(raw$Country)
	raw$location <- gsub(" - ","_",raw$Loc_desc)
	raw$location <- gsub("_-","_",raw$location)
	raw$location <- gsub(" ","_",raw$location)
	raw$location <- gsub("\\.","",raw$location)
	raw$location <- gsub("_-","_",raw$location)
	raw$location <- gsub("-","_",raw$location)

	#raw$site <- merge(raw,loc, by = "Loc_no", all.x = TRUE)[,"Loc..Description"]
	
	raw$site <- raw$location
	raw$trial_id <- raw$Trial.name

# Select variables and reshape raw table
 	raw <- raw[,c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Gen_name", "Trait.name", "Value")]
	raw <- reshape(raw, idvar = c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Gen_name"), timevar = "Trait.name", direction = "wide")

	colnames(raw) <- gsub("Value.","", colnames(raw))
	
	loc$latitude <- loc$Lat_degress + loc$Lat_minutes / 60 
	loc$longitude <- loc$Long_degress + loc$Long_minutes / 60 
	loc$longitude <- ifelse(loc$Longitud == "W", -loc$longitude, loc$longitude)
	loc$latitude <- ifelse(loc$Latitud == "S", -loc$latitude, loc$latitude)
	
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
	
# Rename South africa, South and North korea and United states
renv$country <- ifelse(renv$country == "South africa", "South Africa", renv$country)
renv$country <- ifelse(renv$country == "South korea", "South Korea", renv$country)
renv$country <- ifelse(renv$country == "North korea", "North Korea", renv$country)
renv$country <- ifelse(renv$country == "United states", "United States", renv$country)
renv$country <- ifelse(renv$country == "Null", "Unknown", renv$country)



# other variables
	renv$on_farm <- FALSE
	renv$is_survey <- FALSE
	renv$irrigated <- ifelse(renv$IRRIGATED == "NO", FALSE, TRUE)
	renv$row_spacing <- as.numeric(renv$SPACE_BTN_ROWS_SOWN)
	renv$rep <- renv$Rep
	renv$crop <- "wheat"
	renv$variety_code <- renv$Gen_name
	renv$variety_type <- "high-yield"
	# previous crop details
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "ZEA MAYS", "maize", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "MAIZ", "maize", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "COJENUS", "pigeon pea", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "AMAN RCIE", "rice", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "OILSEED", "rapeseed", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "OIL CROPS", "rapeseed", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "SOJA", "soybean", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "SOYA", "soybean", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "GREEM  MANURE", "green manure", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "MUNG-PULSES", "mung bean", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "MONG BEAN", "mung bean", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "TRIGO", "fenugreek", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "AMAN RICE", "rice", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "ORYZA SATIVA", "rice", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "PADDY", "rice", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "CEREALS", "CEREAL", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "COE PEA", "cowpea", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "SUNHIMP", "sunhemp", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "SESBANIA SP.", "sesbania", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "TRIFOLIUM ALEXANDIUM", "clover", renv$USE_OF_FIELD_SPECIFY_CROP)
	renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "CORN", "maize", renv$USE_OF_FIELD_SPECIFY_CROP)
		renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "VIGNA RADIATA", "mung bean", renv$USE_OF_FIELD_SPECIFY_CROP)

		renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "PISUM SATIVUM", "pea", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification
			renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "AVENA+VICIA", "vetch", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification
			renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "VES", "vetch", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification
			renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "AVENA+VICIA", "vetch", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification
		renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "VEGATEABLES", "vegetables", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification
			renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "RAPHANUS  SPP", "vegetables", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification
		renv$USE_OF_FIELD_SPECIFY_CROP <- ifelse(renv$USE_OF_FIELD_SPECIFY_CROP == "LAB.LAB", "lablab", renv$USE_OF_FIELD_SPECIFY_CROP) # Update once I get clarification



	# Is corn and maize crop same?
	renv$previous_crop <-  tolower(renv$USE_OF_FIELD_SPECIFY_CROP)
	
	# Replace previous_crop with NA entry by no crop
	renv$previous_crop <- ifelse(is.na(renv$previous_crop) == TRUE,'no crop', renv$previous_crop )
	
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
	
	tocarob <- renv[, carob_variables]
	
	tocarob$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset,tocarob, path, dataset_id, group)
}
