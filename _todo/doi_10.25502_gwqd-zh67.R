# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    Assessment of Varieties (Selections from Mokwa) of Cassava for high yield, disease resistance in Uniform Yield Trial (20 clones) in Mokwa 2000/2001 Breeding Season.

"

	uri <- "doi:10.25502/gwqd-zh67"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "pest_diseases"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="pests_diseases",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "2020-04-02T100448phenotype_download.csv"]

	d <- data.frame(read.csv2(f, sep = ","))
	
	# Start formatting dataset
	d$country <- "Nigeria"
	d$adm1 <- "Oyo"
	d$adm2 <- "Ibadan"
	d$location <- d$locationName
	d$trial_id <- paste0(dataset_id, '-', d$studyDbId)
	d$latitude <- 9.23942
	d$longitude <- 5.31025
	d$start_date <- format(as.Date(as.character(d$plantingDate), format = "%Y-%b-%d"), format = "%Y-%m-%d")
	d$end_date <- format(as.Date(as.character(d$harvestDate), format = "%Y-%b-%d"), format = "%Y-%m-%d")
	d$on_farm <- "no"
	d$is_survey <- "no"
	d$rep <- d$replicate
	d$crop <- "cassava"
	d$variety <- d$germplasmName
	d$variety_code <- d$germplasmDbId
	d$bacterial_blight <- d$cassava.bacterial.blight.incidence.6.month.evaluation.CO_334.0000179
	d
	
	d$yield <- d$PodKgHa
	d$residue_yield <- d$FdWtKgHa
	d$grain_weight <- d$Seed.weight
	d$P_fertilizer <- ifelse(d$Fertilizer == "F1", 0, 20)
	d$spacing <- d$Spacing
	
	
	# process file(s)
	d <- d[,c("country", "adm1", "adm2", "trial_id", "latitude", "longitude", "start_date", "end_date", "on_farm", "is_survey", "rep", "crop", "variety", "yield", "residue_yield", "grain_weight", "P_fertilizer", "spacing")]
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
