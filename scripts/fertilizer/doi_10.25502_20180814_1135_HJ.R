# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "doi:10.25502/20180814/1135/HJ"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="",
	   data_citation = "",
	   data_institutions = "",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="___",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group)
	dset$license <- carobiner::get_license(js)

  # 1st get field and location data
	f <- ff[basename(ff) == "Kiberashi_DT2010_field.csv"]
	d <- read.csv(f)
	d <- d[complete.cases(d[ , 6:7]),]
	tz <- sf::st_as_sf(raster::getData('GADM', country='TZA', level = 3, path = "data/other"), crs = "+proj=longlat +datum=WGS84")
	a <- sf::st_as_sf(d, coords = c("Flong", "Flat"), crs = "+proj=longlat +datum=WGS84")
	a <- sf::st_join(a, tz, join = sf::st_intersects)
	d$country <- a$NAME_0
	d$adm1 <- a$NAME_1
	d$adm2 <- a$NAME_2
	d$adm3 <- a$NAME_3
	d$location <- d$Village
	d$site <- d$Site
	d$trial_id <- paste0(dataset_id, "-", d$ID)
	d$latitude <- d$Flat
	d$longitude <- d$Flong
	d$start_date <- format(as.Date(js$result$coverage_start_date), "%Y-%m-%d")
	d$end_date <- format(as.Date(js$result$coverage_end_date), "%Y-%m-%d")
	d$season <- "rain"
	d$on_farm <- "yes"
	d$is_survey <- "no"
	d$crop <- tolower(d$TCrop)
	d$variety <- d$TCVariety
	d$intercrops <- d$CS
	d$previous_crop <- d$PCrop1
	
	# 2nd get agronomic data
	f1 <- ff[basename(ff) == "Kiberashi_DT2010_plot.csv"]
	d1 <- read.csv(f1)
	d1$yield <- d1$TGrainYld
	d1$residue_yield <- d1$Adj.TStoverYld*1000
	d1$treatment <- d1$TrtDesc
	d1$rep <- d1$Rep
	d1$fertilizer_type <- d1$TrtDesc
	d1$N_fertilizer <- ifelse(d1$TrtDesc == "Control", 0,
	                             ifelse(d1$TrtDesc == "PK", 0, 100))
	d1$N_splits <- paste(d1$N_fertilizer*0.25,d1$N_fertilizer*0.375,d1$N_fertilizer*0.375, sep = " | ")
	d1$P_fertilizer <- ifelse(d1$TrtDesc == "Control", 0,
	                          ifelse(d1$TrtDesc == "NK", 0, 30))
	d1$K_fertilizer <- ifelse(d1$TrtDesc == "Control", 0,
	                          ifelse(d1$TrtDesc == "NP", 0, 60))
	d1$Zn_fertilizer <- ifelse(d1$TrtDesc == "NPK+MN", 3, 0)
	d1$S_fertilizer <- ifelse(d1$TrtDesc == "NPK+MN", 5, 0)
	d1$OM_used <- "yes"
	d1$OM_type <- "Manure"
	d1$OM_applied <- 1000
	d2 <- merge(d, d1, by = "FieldID", all.x = TRUE)
	d <- d2[,c("country", "adm1", "adm2", "adm3", "location", "site", "trial_id", "latitude", "longitude",
	            "start_date", "end_date", "season",
	            "on_farm", "is_survey",
	            "treatment", "rep", "crop", "variety", "intercrops", "previous_crop",
	            "yield", "residue_yield",
	            "fertilizer_type", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "S_fertilizer",
	            "OM_used", "OM_type", "OM_applied")]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
