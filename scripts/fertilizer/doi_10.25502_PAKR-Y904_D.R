# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
"
Description:
   Characterising soils of the maize belt in Nigeria to determine limiting nutrients based on which new fertilizer formulations are developed that are tested on farmer's fields in validation trials in a large number of locations against the commonly used NPK 15-15-15 fertilizer
"
  
	uri <- "doi:10.25502/pakr-y904/d"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	  ## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		publication=NA,
		data_citation = "Huising, J. (2019). OCP validation trials for maize fertilizers, OCP - Nigeria [Data set]. International Institute of Tropical Agriculture (IITA). doi:10.25502/PAKR-Y904/D",
		data_institutions = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-04-10",
		data_type="experiment",
		project=NA    
	)
  
	## download and read data 
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	
	f <- ff[basename(ff) == "OCP_Yld-data&covariates_complete.csv"] 
	
	# read the dataset
	d <- read.csv(f)

# Team BUK1,BUK2,BUK3 data are already included in doi_10.25502_RGB5_GA15_D.R
	d <- d[!(d$team %in% c("BUK1", "BUK2", "BUK3")), ] 
	
	# process file(s)
	d <- d[, c('state', 'lga', 'sid', 'lat', 'lon', 'trt', 'ayld', 'PH', 'SND', 'SOC')]
	colnames(d) <- c("adm1", "adm2", "rep", "latitude", "longitude", "treatment",
			"yield", "soil_pH", "soil_sand", "soil_SOC")
	# Add columns
	d$plant_spacing <- 25 # get from VT protocol OCP Project Document 
	d$row_spacing <- 75	 # get from VT protocol OCP Project Document
	d$dataset_id <- dataset_id
	d$country <- "Nigeria"
	d$crop <- "maize"
	d$yield_part <- "grain"	
	d$variety <- "Sammaz 15" # get from VT protocol OCP Project Document
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	

	# see annex 2 pdf

	# NPK Apply	15-15-15 means 15% N, 15% P2O5, 15% K2O
	#OCPF1	(N) : 11% (P2O5): 21% (k2O): 22% S: 5%	Zn: 1% B2O3: 0.8%
	#OCPF2 (N) : 14% (P205): 31% (k2O): 0%	 S: 9%	Zn: 0.9% B2O3: 1%
	
	# RH each product was appliet at 150 kg/ha 
	# in addition: 100 kg urea
	d$N_fertilizer <- ifelse(d$treatment == "Control", 0, 
					ifelse(d$treatment == "OCPF1", 11,
					ifelse(d$treatment == "OCPF2", 14, 15))) * 1.50 + 100 * .46
		
	d$P_fertilizer <- ifelse(d$treatment == "Control", 0, 
					ifelse(d$treatment == "OCPF1", 21,
					ifelse(d$treatment == "OCPF2", 31, 15))) * 1.50 / 2.29

	d$K_fertilizer <- ifelse(d$treatment == "Control", 0, 
					ifelse(d$treatment == "OCPF1", 22,
					ifelse(d$treatment == "OCPF2", 0, 15))) * 1.50 / 1.2051
	
	d$Zn_fertilizer <- ifelse(d$treatment == "Control", 0, 
					ifelse(d$treatment == "OCPF1", 1,
					ifelse(d$treatment == "OCPF2", 0.9, 0))) * 1.50
	
	d$S_fertilizer <- ifelse(d$treatment == "Control", 0, 
					ifelse(d$treatment == "OCPF1", 5,
					ifelse(d$treatment == "OCPF2", 9, 0))) * 1.50

	d$B_fertilizer <- ifelse(d$treatment == "Control", 0, 
					ifelse(d$treatment == "OCPF1", 0.8,
					ifelse(d$treatment == "OCPF2", 1, 0))) * 1.50


	# planting dates from VT protocol pdf

	d$planting_date <- "2017-04"  # april/may
	i <- !(d$adm1 %in% c("Nassarawa", "Plateau", "Taraba"))
	d$planting_date[i] <- "2017-06"
	d$harvest_date <- "2017-11-01"
	d$season <- "2017"
	
	d$trial_id <- as.character(as.integer(as.factor(paste0(paste(d$longitude, d$latitude)))))

	d$yield <- as.numeric(d$yield)
	d$soil_SOC <- d$soil_SOC / 10

	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
	
}

