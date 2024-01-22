# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
Agronomy and yield survey of approximately 70 maize fields in one 10 x 10km2 area in Bako in 2015 conducted by EIAR and CIMMYT. Replicated crop cuts of 16m2 in farmers fields along with addition data on agronomy, household characteristics, fertilizer use, variety, and soil analysis.
"

	uri <- "hdl:11529/11020"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication=NA,
	   data_citation = "T Balemi; M Kebede; T Tufa; G Gurumu, 2017. TAMASA Ethiopia. Yield, soil and agronomy data from 70 farmers’ maize fields in Bako, Ethiopia, 2015 season. https://hdl.handle.net/11529/11020, CIMMYT Research Data & Software Repository Network, V1",
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   carob_date="2022-11-02",
	   data_type= "farm survey",
	   project=NA
 	)

## download and read data 

	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)

	f <- ff[basename(ff) == "TAMASA_ET_CC_2015_BakoF.xlsx"]

	# suppress variable renaming message
	r <- carobiner::read.excel(f, sheet = "Raw_Data", trim_ws = TRUE, n_max = 100)

	d <- data.frame(country="Ethiopia", location=r$`Name of the Village`)
	d$trial_id <- paste0("TAMASABako2015_", gsub("\\.", "-", r$`plot ID`))

##RH: ??  needs to be georeferenced by location
##	jsgeo <- js$data$latestVersion$metadataBlocks$geospatial$fields$value[[3]]
##	xmin <- as.numeric(jsgeo$westLongitude[4][[1]])
##	xmax <- as.numeric(jsgeo$eastLongitude[4][[1]])
##	ymin <- as.numeric(jsgeo$southLongitude[4][[1]])
##	ymax <- as.numeric(jsgeo$northLongitude[4][[1]])
##	d$longitude <- xmin + ((xmin - xmax)/2)
##	d$latitude <- ymax + ((ymin - ymax)/2)
##	# d$longitude <- 37.115
##	# d$latitude <- 9.085
## RH: GPS lon/lat probably exist

	d$planting_date <- as.character(as.Date(r$`Planting Date`))
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$treatment <- "none"
	d$rep <- ifelse(gsub("^[^.]*.", "", as.character(r$`plot ID`)) == "", "1", 
					gsub("^[^.]*.", "", as.character(r$`plot ID`)))
	d$rep <- as.integer(d$rep)			
	d$crop <- "maize"
	d$yield_part <- "grain"
	
	d$variety_code <- r$`Type of Variety`
	d$variety_type <- r$`Seed type (Local vs Improved)`
	d$previous_crop <- tolower(r$`Previous/precursor crop`)
	d$previous_crop[d$previous_crop  == "beans"] <- "common bean" 
	d$previous_crop[d$previous_crop  == "other"] <- NA

#	d$crop_rotation <- d$`If crop rotation is practiced`
	d$yield <- r$`Average yield kg/ha or (Q1+Q2)/2`
	fr <- toupper(gsub("\n", "", gsub("\r", "", r$`Type of Inorganic Fertilizer`)))
	fr <- gsub(" ", "", fr)
	fr <- gsub("\\.,|,|AND|&|-", "; ", fr)
	fr <- gsub("UREA", "urea", fr)
	fr <- gsub("URE", "urea", fr)
	
	fr <- gsub("SPS", "SSP", fr)
	fr <- gsub("NPS", "SSP", fr)
	fr <- gsub("NSP", "SSP", fr)
	d$fertilizer_type <- fr

	d$N_fertilizer <- 0
	i <- grep("urea", d$fertilizer_type)
	d$N_fertilizer[i] <- (r$`Amount of Inorganic Fertilizer (kg)`[i] * 0.46)

	i <- grep("DAP", d$fertilizer_type)
	# summing because you can have DAP _and_ urea
	d$N_fertilizer[i] <- (d$N_fertilizer[i] + r$`Amount of Inorganic Fertilizer (kg)`[i] * 0.18)
	d$N_fertilizer <- d$N_fertilizer / r$`Farm size (ha)`
	
	d$P_fertilizer <- 0
	d$P_fertilizer[i] <- (r$`Amount of Inorganic Fertilizer (kg)`[i] * 0.2)
	i <- grep("SSP", d$fertilizer_type)
	d$P_fertilizer[i] <- (r$`Amount of Inorganic Fertilizer (kg)`[i] * 0.1659)
	d$P_fertilizer <- d$P_fertilizer / r$`Farm size (ha)`
	d$K_fertilizer <- 0

	d$OM_used <- r$`Apply Organic Fertilizer ?` == "Yes"
	d$OM_type <- r$`Type of Organic Fertilizer applied`
	d$OM_type[grepl("manure", tolower(d$OM_type))] <- "farmyard manure"
	d$OM_type[grepl("Farm", d$OM_type)] <- "farmyard manure"
	# Assuming 50kg Manure Bags
	d$OM_applied <- 0
	bags <- which(r$`Unit for Organic Fertilizer` == "Bags")
	kgs <- which(r$`Unit for Organic Fertilizer` == "Kg")
	d$OM_applied[bags] <- as.numeric(r$`Amount of  Organic Fertilizer applied`[bags] * 50)
	d$OM_applied[kgs] <- as.numeric(r$`Amount of  Organic Fertilizer applied`[kgs])
			
	d$soil_type <- r$`Soil type`
	d$soil_pH <- r$pH
	d$soil_SOC <- r$`Carbon (%)`*10 # convert to g/kg
	d$soil_N <- r$`Nitrogen (%)`* 10000 # convert to mg/kg
	d$soil_K <- r$`K (mg kg-1)` # mg/kg
	d$soil_P_total <- r$`P (mg kg-1)`
	
#	d <- d[,c("country", "site", "trial_id", "longitude", "latitude", "planting_date", "on_farm", "is_survey", "treatment", "rep", "crop", "variety_code", "variety_type", "previous_crop",
#	          "yield", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "OM_used", "OM_type", "OM_applied", "soil_type", "soil_pH", "soil_SOC",
#	          "soil_N", "soil_K", "soil_P_total")]
	
	d$dataset_id <- dataset_id
	d$yield_part <- "grain"
	
	d <- d[!is.na(d$yield), ]
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)

}
