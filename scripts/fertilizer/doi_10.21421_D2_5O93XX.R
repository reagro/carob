# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    [copy the abstract from the repo]

"

	uri <- "doi:10.21421/D2/5O93XX"
	dataset_id <- agro::get_simple_URI(uri)
	group <- ""
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="doi:10.21421/D2/5O93XX",
	   contributor="Eduardo Garcia Bendito",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Data file of Groundnut to plant density and phosphorous application in Wudil 2012-13.xlsx"]

	d <- data.frame(readxl::read_excel(f))
	
	# Start formatting dataset
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$adm2 <- "Wudil"
	d$trial_id <- paste0(dataset_id, '-', d$Location)
	d$latitude <- 11.793702
	d$longitude <- 8.838846
	d$start_date <- 2012
	d$end_date <- 2013
	d$on_farm <- "no"
	d$is_survey <- "no"
	d$rep <- d$Replication.number
	d$crop <- "groundnut"
	d$variey <- d$Variety
	d$yield <- d$PodKgHa
	d$residue_yield <- d$FdWtKgHa
	d$grain_weight <- d$Seed.weight
	d$P_fertilizer <- ifelse(d$Fertilizer == "F1", 0, 20)
	
	
	# process file(s)
	d <- carobiner::change_names(d, from, to)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
