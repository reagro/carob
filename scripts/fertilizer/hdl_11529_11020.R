# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "hdl:11529/11020"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication=NA,
	   data_citation = "Balemi T. and Kebede M., Tufa T., and Gurumu G.. 2017. TAMASA Ethiopia.  Yield, soil and agronomy data from 70 farmers’ maize fields  in Bako, Ethiopia, 2015 season. International Maize and Wheat Improvement Centre (CIMMYT), Ethiopia.",
	   data_institutions = "CIMMYT",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type= "on-farm observations",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "TAMASA_ET_CC_2015_BakoF.xlsx"]

	# suppress variable renaming message
	suppressMessages(
		d <- readxl::read_excel(f, sheet = "Raw_Data", trim_ws = TRUE, n_max = 100)
	)
	
	d <- as.data.frame(d)
	
	d$country <- "Ethiopia"
	d$site <- d$`Name of the Village`
	d$trial_id <- "TAMASA-Bako-2015"
	d$start_date <- format(d$`Planting Date`, "%Y-%m-%d")
	d$on_farm <- "yes"
	d$is_survey <- "no"
	d$treatment <- "none"
	d$rep <- ifelse(gsub("^[^.]*.","",as.character(d$`plot ID`)) == "", "1", gsub("^[^.]*.","",as.character(d$`plot ID`)))
	d$crop <- "maize"
	d$variety_code <- d$`Type of Variety`
	d$variety_type <- d$`Seed type (Local vs Improved)`
	d$previous_crop <- d$`Previous/precursor crop`
	d$crop_rotation <- d$`If crop rotation is practiced`
	d$yield <- d$`Average yield kg/ha or (Q1+Q2)/2`
	d$fertilizer_type <- toupper(gsub("\n", "", gsub("\r", "", d$`Type of Inorganic Fertilizer`)))
	d$N_fertilizer <- ifelse(grepl( "UREA", d$fertilizer_type, fixed = TRUE), d$`Amount of Inorganic Fertilizer (kg)` * 0.46,
	                         ifelse(grepl( "DAP", d$fertilizer_type, fixed = TRUE), d$`Amount of Inorganic Fertilizer (kg)` * 0.18,
	                                d$`Amount of Inorganic Fertilizer (kg)` * 0.19))
	d$P_fertilizer <- ifelse(grepl( "DAP", d$fertilizer_type, fixed = TRUE), d$`Amount of Inorganic Fertilizer (kg)` * 0.2, d$`Amount of Inorganic Fertilizer (kg)` * 0.1659)
	d$OM_used <- d$`Apply Organic Fertilizer ?`
	d$OM_type <- d$`Type of Organic Fertilizer applied`
	# Assuming 50kg Manure Bags
	d$OM_applied <- 0
	bags <- which(d$`Unit for Organic Fertilizer` == "Bags")
	kgs <- which(d$`Unit for Organic Fertilizer` == "Kg")
	d$OM_applied[bags] <- as.numeric(d$`Amount of  Organic Fertilizer applied`[bags] * 50)
	d$OM_applied[kgs] <- as.numeric(d$`Amount of  Organic Fertilizer applied`[kgs])
			
	d$soil_type <- d$`Soil type`
	d$soil_pH <- d$pH
	d$soil_SOC <- d$`Carbon (%)`
	d$soil_N <- d$`Nitrogen (%)`
	d$soil_K <- d$`K (mg kg-1)`
	d$soil_P_total <- d$`P (mg kg-1)`
	
	d <- d[,c("country", "site", "trial_id", "start_date", "on_farm", "is_survey", "treatment", "rep", "crop", "variety_code", "variety_type", "previous_crop",
	          "crop_rotation", "yield", "fertilizer_type", "N_fertilizer", "P_fertilizer", "OM_used", "OM_type", "OM_applied", "soil_type", "soil_pH", "soil_SOC",
	          "soil_N", "soil_K", "soil_P_total")]
	
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
