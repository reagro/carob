# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:B3C0 is the initial cycle of recombination of group B3- Population B. This population has been derived from population A. R genes have been eliminated from this group and horizontal resistance has been retained. Currently, these clones are being used on the variety selection in developing countries. This group of clones were planted in a randomized complete block design (RCBD) with 2 replicates at Comas, located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru. The trials were established at Comas due to the high disease pressure of late blight present in the area and used to screen selected potato genotypes for resistance to this disease. 
"

	uri <- "doi:10.21223/P3/TNNRYW"
	dataset_id <- carobiner::simple_uri(uri)
	#dataset_id <- agro::get_simple_uri(uri)
	group <- "lateblight"
		## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gastelo, Manuel; Bonierbale, Merideth; Landeo, Juan; Diaz, Luis, 2016. Dataset for: Advanced clones of B3C0, group B3, population B in Comas-Peru. https://doi.org/10.21223/P3/TNNRYW, International Potato Center, V1",
		
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)   
		publication= NA,
		data_institutions = "CIP",
		carob_contributor="Henry Juarez",
	   
	   ## something like randomized control...
		experiment_type="___",
		has_weather=FALSE,
		has_soil=FALSE,
		has_management=FALSE
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
  ## Major and minor version are important. This case is 1.3
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=3)
	dset$license <- carobiner::get_license(js)

##Read data 
	f <- ff[basename(ff) == "PTLB200112_VIENA_B3C0COM02-01.xls"]
	d <- carobiner::read.excel(f, sheet = "Fieldbook") 

	d$record_id <- as.integer(1:nrow(d))
	lbvars <- c('LB1', 'LB2', 'LB3', 'LB4', 'LB5')
	
	x <- reshape(d[, c("record_id", lbvars)], direction="long", varying =lbvars, v.names="severity", timevar="step")
	dates <- as.character(as.Date(c("2002-02-03", "2002-02-13", "2002-02-21", "2002-02-28", "2002-03-07")))
	x$time <- dates[x$step]
	x$step <- x$id <- NULL
	
	d[, lbvars] <- NULL	
	d <- carobiner::change_names(d, 
		c("REP", "INSTN", "TTYNA"),
		c("rep", "variety", "yield"))

	d$rep <- as.integer(d$rep)
	d$yield <- d$yield * 1000
	d$AUDPC <- d$AUDPC / 100
	
	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- "none"

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Peru"
	d$site <- "Viena"
	d$adm1 <- "Junin"
	d$adm2 <- "Concepcion"
	d$adm3 <- "Mariscal Castilla"
	d$elevation <- 2415
## each site must have corresponding longitude and latitude
	d$longitude <- -75.1314
	d$latitude <- -11.5237
	d$trial_id <- "1"
##### Crop #####
## normalize variety names
	d$crop <- "potato"
	d$pathogen <- "Phytophthora infestans"
	

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format. Date in "yyyy-mm-dd", "yyyy-mm", or "yyyy" format
	d$start_date <- as.character(as.Date("2001-12-10"))
	d$end_date  <- as.character(as.Date("2002-04-02"))


	d$PLOT <- NULL

# all scripts must end like this
	carobiner::write_files(dset, d, timerecs=x, path=path)
}

