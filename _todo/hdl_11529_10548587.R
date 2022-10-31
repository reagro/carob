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
	group <- "variety_performance"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="",
	   data_citation = "",
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
	c <- read.csv("/home/jovyan/CAROB/egb-carob/data/raw/variety_performance/hdl_11529_10548587/28TH HRWYT_Genotypes_Data.xls", sep = "\t")

	d <- read.csv(raw.data, sep = "\t")
	loc <- read.csv(loc.data, sep = "\t")
	env <- read.csv(env.data, sep = "\t")

	d$country <- d$Country
	d$location <- gsub("\\ -.*","",d$Loc_desc)
	d$site <- merge(d,loc, by = c("Loc_no"))[,"Institute.Name"]
	# d$trial_id <- 
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
	# d$variety <- "?"
	# d$variety_code <- "?"
	# d$variety_type <- "?"
	d$yield <- d[d$Trait.name == "GRAIN_YIELD", "Value"]
	d$grain_weight <- d[d$Trait.name == "1000_GRAIN_WEIGHT", "Value"]
	# d$fertilizer_type <- "?"
	d$N_fertilizer <- as.vector(unlist(subset(merge(d,env, by = c("Loc_no"), all = TRUE)[,c("Trait.name.y","Value.y")], Trait.name.y == "SOWING_DATE", select = "Value.y")))

	# process file(s)
	d <- carobiner::change_names(d, from, to)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
