# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Rainfall Wheat Yield Trial (HRWYT) contains very top-yielding advance lines of spring bread wheat (Triticum aestivum) germplasm adapted to high rainfall, Wheat Mega-environment 2 (ME2HR). (2020)

"

	uri <- "hdl:11529/10548587"
	dataset_id <- agro::get_simple_URI(uri)
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
	   experiment_type="variety",
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

	d <- read.csv(raw.data, sep = "\t")
	loc <- read.csv(loc.data, sep = "\t")
	env <- read.csv(env.data, sep = "\t")

	d$country <- tools::toTitleCase(tolower(as.character(d$Country)))
	d$location <- gsub("\\ -.*","",d$Loc_desc)
	d$site <- merge(d,loc, by = c("Loc_no"))[,"Institute.Name"]
	d$trial_id <- d$Trial.name
	d$latitude <- ifelse(merge(d,loc, by = c("Loc_no"))[,"Latitud"] == "N",
	                     merge(d,loc, by = c("Loc_no"))[,"Lat_degress"],
	                     merge(d,loc, by = c("Loc_no"))[,"Lat_degress"] * -1) +
	  ((merge(d,loc, by = c("Loc_no"))[,"Lat_minutes"])/60)
	d$longitude <- ifelse(merge(d,loc, by = c("Loc_no"))[,"Longitude"] == "E",
	                      merge(d,loc, by = c("Loc_no"))[,"Long_degress"],
	                      merge(d,loc, by = c("Loc_no"))[,"Long_degress"] * -1) +
	  ((merge(d,loc, by = c("Loc_no"))[,"Long_minutes"])/60)
	
	d$start_date <- as.Date(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "SOWING_DATE", select = "Value.y"))), "%b %d %Y", tz = "GMT")
	d$end_date <- as.Date(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "HARVEST_FINISHING_DATE", select = "Value.y"))), "%b %d %Y", tz = "GMT")
	d$on_farm <- "no"
	d$is_survey <- "no"
	d$treatment <- ""
	d$rep <- d$Rep
	d$crop <- "wheat"
	d$yield <- d[d$Trait.name == "GRAIN_YIELD", "Value"]
	d$grain_weight <- d[d$Trait.name == "1000_GRAIN_WEIGHT", "Value"]
	d$fertilizer_type <- "?"
	# Fertilizer amounts
	fert1 <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_KG/HA_1", select = "Value.y"))))
	fert2 <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_KG/HA_2", select = "Value.y"))))
	# Nitrogen levels
	n1l <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_%N_1", select = "Value.y"))))
	n2l <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_%N_2", select = "Value.y"))))
	d$N_fertilizer <- ifelse(d$Rep == 1,
	                         (n1l/100) * fert1,
	                         (n2l/100) * fert2)
  # P levels
	p1l <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_%P2O5_1", select = "Value.y"))))
	p2l <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_%P2O5_2", select = "Value.y"))))
	d$P_fertilizer <- ifelse(d$Rep == 1,
	                         (p1l/100) * fert1,
	                         (p2l/100) * fert2)
	# K levels
	k1l <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_%K2O_1", select = "Value.y"))))
	k2l <- as.numeric(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "FERTILIZER_%K2O_2", select = "Value.y"))))
	d$K_fertilizer <- ifelse(d$Rep == 1,
	                         (k1l/100) * fert1,
	                         (k2l/100) * fert2)
	d$irrigated <- "no"
	d$row_spacing <- as.integer(as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "SPACE_BTN_ROWS_SOWN", select = "Value.y"))))
	
	# Subset for relevant columns
	d <- d[, c("trial_id", "country", "location", "site", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "treatment", "rep",
	           "crop", "yield", "grain_weight", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer",
	           "irrigated", "row_spacing")]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
