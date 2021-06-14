# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:


"

	uri <- "doi:10.7910/DVN/LJPW4O"
	dataset_id <- agro::get_simple_URI(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="",
	   contributor="blabla",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group) # WHICH VERSION OF THE DATSET IS DOWNLOADED???
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=5) 
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "9a Yield data.xlsx"]

	d <- as.data.frame(readxl::read_excel(f))
	d$dataset_id <- dataset_id
	d$country <- "Uganda"
	d$region <- ""
	d$adm1 <- "Wakiso"
	d$adm2 <- "Jinja"
	d$adm3 <- ifelse(d$Site == "Kawanda", "Nabweru", "Busukuma")
	d$trial_id <- paste0("AgMIP", '_', d$Site)
	# SHOULD WE DERIVE FROM GADM???
	# d$latitude
	# d$longitude
	d$start_date <- format(as.Date('01/01/2013', format="%d/%m/%Y"),"%Y") # Correct date format (to date)
	d$end_date <- format(as.Date('01/01/2014', format="%d/%m/%Y"),"%Y")  # Correct date format (to date)
	
	# process file(s)
	d <- carobiner::change_names(d, from, to)
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
	TRUE
}
