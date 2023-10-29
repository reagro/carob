# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveilllance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments).
"

	uri <- "doi:10.25502/20180814/1135/HJ"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication=NA,
	   data_citation = "Huising, J. (2018). Africa Soil Information System - Phase 1, Kiberashi [Data set]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/20180814/1135/HJ",
	   data_institutions = "IITA",
	   carob_contributor="Eduardo Garcia Bendito",
	   carob_date="2021-10-06",
	   data_type="on-farm experiments",
	   project=NA
 	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group)
	dset$license <- carobiner::get_license(js)

  # 1st get field and location data
	f <- ff[basename(ff) == "Kiberashi_DT2010_field.csv"]
	d <- read.csv(f)
	d <- d[complete.cases(d[ , 6:7]),]
	d$country <- "Tanzania"
	d$location <- d$Site
	d$site <- d$Village
	d$trial_id <- paste0(dataset_id, "-", d$ID)
	d$latitude <- d$Flat
	d$longitude <- d$Flong
	d$planting_date <- format(as.Date(js$result$coverage_start_date), "%Y-%m-%d")
	d$harvest_date <- format(as.Date(js$result$coverage_end_date), "%Y-%m-%d")
	d$season <- "rainy"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$crop <- tolower(d$TCrop)
	d$variety <- d$TCVariety
	## should be crop names
	##d$intercrops <- d$CS
	d$previous_crop <- d$PCrop1
	
	# 2nd get agronomic data
	f1 <- ff[basename(ff) == "Kiberashi_DT2010_plot.csv"]
	d1 <- read.csv(f1)
	d1$yield <- d1$TGrainYld*1000
	d1$residue_yield <- d1$Adj.TStoverYld*1000
	d1$treatment <- d1$TrtDesc
	d1$rep <- d1$Rep
## RH: this is the treatment, not the _type_ of fertilizer 
##	d1$fertilizer_type <- d1$TrtDesc
	d1$N_fertilizer <- ifelse(d1$TrtDesc == "Control", 0,
	                   ifelse(d1$TrtDesc == "PK", 0, 100))

## RH: this is nice, but the field only stores how many splits there were
##	d1$N_splits <- paste(d1$N_fertilizer*0.25,d1$N_fertilizer*0.375,d1$N_fertilizer*0.375, sep = " | ")

	d1$N_splits <- NA
	d1$N_splits[d1$N_fertilizer > 0] <- 3L
	
	d1$P_fertilizer <- ifelse(d1$TrtDesc == "Control", 0,
	                   ifelse(d1$TrtDesc == "NK", 0, 30))
	d1$K_fertilizer <- ifelse(d1$TrtDesc == "Control", 0,
	                   ifelse(d1$TrtDesc == "NP", 0, 60))
	d1$Zn_fertilizer <- ifelse(d1$TrtDesc == "NPK+MN", 3, 0)
	d1$S_fertilizer <- ifelse(d1$TrtDesc == "NPK+MN", 5, 0)
	d1$OM_used <- TRUE
	d1$OM_type <- "farmyard manure"
	d1$OM_applied <- 1000
	d2 <- merge(d, d1, by = "FieldID", all.x = TRUE)

	p <- carobiner::fix_name(gsub("/", "; ", d2$previous_crop), "lower")
	p <- gsub("mangoes", "mango", p)
	p <- gsub("beans", "common bean", p)
	d2$previous_crop <- p

	d <- d2[,c("country", "location", "site", "trial_id", "latitude", "longitude",
	            "planting_date", "harvest_date", "season", "on_farm", "is_survey",
	            "treatment", "rep", "crop", "variety", "previous_crop",
	            "yield", "residue_yield", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "S_fertilizer", "OM_used", "OM_type", "OM_applied")]
	d$dataset_id <- dataset_id

	d$yield_part <- "grain"

	d <- d[!is.na(d$yield), ]
	carobiner::write_files(dset, d, path=path)

}
