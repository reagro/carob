# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2020)

"

	uri <- "hdl:11529/10548587"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi.org/10.1016/j.fcr.2020.107742",
	   data_citation = "Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2021, '28th High Rainfall Wheat Yield Trial', https://hdl.handle.net/11529/10548587, CIMMYT Research Data & Software Repository Network, V1",
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="Station experimnt",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)


	raw.data <- ff[basename(ff) == "28TH HRWYT_RawData.xls"]
	loc.data <- ff[basename(ff) == "28TH HRWYT_Loc_data.xls"]
	env.data <- ff[basename(ff) == "28TH HRWYT_EnvData.xls"]


	d <- read.table(raw.data, comment.char="", sep="\t", header=TRUE)
	loc <- read.table(loc.data, sep = "\t", header=TRUE)
	env <- read.csv(env.data, sep = "\t")
	loc$latitude <- loc$Lat_degress + loc$Lat_minutes / 60 
	loc$longitude <- loc$Long_degress + loc$Long_minutes / 60 
	loc$longitude <- ifelse(loc$Longitud == "W", -loc$longitude, loc$longitude)
	
	d$country <- tools::toTitleCase(tolower(as.character(d$Country)))
	d$location <- gsub("\\ -.*","",d$Loc_desc)
	d$site <- merge(d,loc, by = c("Loc_no"))[,"Loc..Description"]
	d$trial_id <- d$Trial.name
	
	# Sub-setting relevant columns and reformatting dataset to "wide" for easier handling
 	d <- d[,c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot", "Trait.name", "Value")]
	d <- reshape(d, idvar = c("country", "location", "site", "trial_id", "Loc_no", "Rep", "Sub_block", "Plot"), timevar = "Trait.name", direction = "wide")
	colnames(d)[9:19] <- gsub(".*Value.", "", colnames(d)[9:19])
	
	# Join latitude and longitude data
	d <- merge(d, loc[, c("Loc_no", "longitude", "latitude")], by ="Loc_no", all.x = T)
	
	# Join with additional variables of interest, Sub-set only those new variables and format df to "wide" 
	m <- merge(d,env, by = c("Loc_no"), all.x = TRUE)[,c("Loc_no", "Rep", "Sub_block", "Plot", "Trait.name","Value")]
	dd <- reshape(m, idvar = c("Loc_no", "Rep", "Sub_block", "Plot"), timevar = "Trait.name", direction = "wide")
	colnames(dd)[5:62] <- gsub(".*Value.", "", colnames(dd)[5:62])
	
	# Re-join d <-> dd
	ddd <- merge(d,dd, by = c("Loc_no", "Rep", "Sub_block", "Plot"), all.x = TRUE)
	
	# Start processing the dataset into carob
	ddd$start_date <- as.Date(dd$SOWING_DATE, "%b %d %Y")
	ddd$end_date <- as.Date(dd$HARVEST_FINISHING_DATE, "%b %d %Y")
	ddd$on_farm <- FALSE
	ddd$is_survey <- FALSE
	# dd$treatment <- 
	ddd$rep <- ddd$Rep
	ddd$crop <- "wheat"
	ddd$previous_crop <- ifelse(ddd$USE_OF_FIELD_SPECIFY_CROP == "MAIZ", "maize", NA)
	ddd$yield <- as.numeric(ddd$GRAIN_YIELD)*1000 # Yield in ton/ha
	ddd$grain_weight <- ddd$`1000_GRAIN_WEIGHT`
	
	# Nitrogen levels
	zz <- c("FERTILIZER_%N_2", "FERTILIZER_%P2O5_1", "FERTILIZER_%N_1", "FERTILIZER_KG/HA_1", "FERTILIZER_KG/HA_2", "FERTILIZER_%N_3", "FERTILIZER_%P2O5_3", "FERTILIZER_%K2O_1", "FERTILIZER_%K2O_2", "FERTILIZER_%K2O_3", "FERTILIZER_%P2O5_2", "FERTILIZER_KG/HA_3")
	ddd[zz][is.na(ddd[zz])] <- 0
	ddd$`FERTILIZER_KG/HA_1` <- as.numeric(ddd$`FERTILIZER_KG/HA_1`)
	ddd$`FERTILIZER_KG/HA_2` <- as.numeric(ddd$`FERTILIZER_KG/HA_2`)
	ddd$`FERTILIZER_KG/HA_3` <- as.numeric(ddd$`FERTILIZER_KG/HA_3`)
	ddd$`FERTILIZER_%N_1` <- as.numeric(ddd$`FERTILIZER_%N_1`)/100
	ddd$`FERTILIZER_%N_2` <- as.numeric(ddd$`FERTILIZER_%N_2`)/100
	ddd$`FERTILIZER_%N_3` <- as.numeric(ddd$`FERTILIZER_%N_3`)/100
	ddd$N_fertilizer <- (ddd$`FERTILIZER_KG/HA_1`*ddd$`FERTILIZER_%N_1`) + (ddd$`FERTILIZER_KG/HA_2`*ddd$`FERTILIZER_%N_2`) + (ddd$`FERTILIZER_KG/HA_3`*ddd$`FERTILIZER_%N_3`)
	ddd$N_splits <- ifelse(ddd$`FERTILIZER_KG/HA_3` > 0, 3,
	                       ifelse(ddd$`FERTILIZER_KG/HA_2` > 0, 2, 1))
	
	# Phosphorous levels
	ddd$`FERTILIZER_%P2O5_1` <- as.numeric(ddd$`FERTILIZER_%P2O5_1`)/100
	ddd$`FERTILIZER_%P2O5_2` <- as.numeric(ddd$`FERTILIZER_%P2O5_2`)/100
	ddd$`FERTILIZER_%P2O5_3` <- as.numeric(ddd$`FERTILIZER_%P2O5_3`)/100
	ddd$P_fertilizer <- (ddd$`FERTILIZER_KG/HA_1`*ddd$`FERTILIZER_%P2O5_1`) + (ddd$`FERTILIZER_KG/HA_2`*ddd$`FERTILIZER_%P2O5_2`) + (ddd$`FERTILIZER_KG/HA_3`*ddd$`FERTILIZER_%P2O5_3`)
	
	# Potassium levels
	ddd$`FERTILIZER_%K2O_1` <- as.numeric(ddd$`FERTILIZER_%K2O_1`)/100
	ddd$`FERTILIZER_%K2O_2` <- as.numeric(ddd$`FERTILIZER_%K2O_2`)/100
	ddd$`FERTILIZER_%K2O_3` <- as.numeric(ddd$`FERTILIZER_%K2O_3`)/100
	ddd$K_fertilizer <- (ddd$`FERTILIZER_KG/HA_1`*ddd$`FERTILIZER_%K2O_1`) + (ddd$`FERTILIZER_KG/HA_2`*ddd$`FERTILIZER_%K2O_2`) + (ddd$`FERTILIZER_KG/HA_3`*ddd$`FERTILIZER_%K2O_3`)
	
	# Other variables
	ddd$irrigated <- ifelse(ddd$IRRIGATED == "NO", FALSE, TRUE)
	ddd$row_spacing <- as.numeric(ddd$SPACE_BTN_ROWS_SOWN)
	
	# Subset for relevant columns
	d <- ddd[, c("country", "location", "site", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "rep",
	           "crop", "previous_crop", "yield", "grain_weight", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer",  "irrigated", "row_spacing")]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}

